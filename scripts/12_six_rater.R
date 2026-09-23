#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# 12_six_rater.R -- agreement across the full six-rater panel
#
#   Rscript scripts/12_six_rater.R \
#     --human results/ratings_9n99_Human.tsv \
#     --llm   results/ratings_9n99_main.csv \
#     --label 9n99
#
# Computes, on one aligned set of units:
#   * alpha within the human panel, within the model panel, and pooled
#   * alpha between the two panel majorities (the pre-registered criterion,
#     supplementary S8.3)
#   * every one of the 15 rater pairs, as Cohen's kappa and as pairwise alpha
#   * the same for the CQ subclassification, at FACTOR level -- the only level
#     both rater types coded
#
# READS NO JOURNAL TEXT.
# ---------------------------------------------------------------------------

repo <- normalizePath(file.path(dirname(sub("^--file=", "",
        commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])), ".."))
setwd(repo)
source("test_for_consistency/R/agreement.R")
source("R/six_rater.R")

a <- commandArgs(trailingOnly = TRUE)
getopt <- function(f, d = NULL) { i <- match(f, a); if (is.na(i) || i == length(a)) d else a[i + 1L] }
H_IN  <- getopt("--human", "results/ratings_9n99_Human.tsv")
L_IN  <- getopt("--llm",   "results/ratings_9n99_main.csv")
LAB   <- getopt("--label", "9n99")
OMIT  <- getopt("--omit",  "results/Omit_ids.txt")
RMAP  <- getopt("--remap", "results/remap_9n99.csv")
B     <- as.integer(getopt("--boot", "4000"))
if (identical(OMIT, "none")) OMIT <- NULL

set.seed(20260904)
if (identical(RMAP, "none") || !file.exists(RMAP)) RMAP <- NULL
al <- align_raters(H_IN, L_IN, omit_file = OMIT, remap_file = RMAP)
u <- al$units
n <- nrow(u)

hr <- function(s) { cat("\n", strrep("=", 74), "\n", s, "\n", strrep("=", 74), "\n", sep = "") }

# --- what we are standing on --------------------------------------------
hr(sprintf("SIX-RATER PANEL -- %s", LAB))
cat(sprintf("aligned units       %d\n", n))
cat(sprintf("students            %s\n", paste(sort(unique(u$student)), collapse = ", ")))
cat(sprintf("weeks               %s\n", paste(sort(unique(u$week)), collapse = ", ")))
cat(sprintf("human raters        %s\n", paste(al$humans, collapse = ", ")))
cat(sprintf("model raters        %s  (%d replicate%s)\n",
            paste(al$models, collapse = ", "), al$n_replicates,
            if (al$n_replicates == 1L) "" else "s"))
if (length(al$omit$requested)) {
  cat(sprintf("\nOMITTED by list     %s\n", OMIT))
  cat(sprintf("  %d ids requested; %d matched the human file, %d the model file\n",
              length(al$omit$requested), length(al$omit$human),
              length(al$omit$model)))
  cat(sprintf("  %d fell inside an analysed block and left the denominator\n",
              length(intersect(al$omit$human, al$omit$model))))
  if (length(al$omit$unmatched)) {
    cat("  WARNING -- these ids matched NEITHER file, so they excluded nothing:\n")
    cat(paste("   ", al$omit$unmatched, collapse = "\n"), "\n")
  }
}
if (length(al$divergent$blocks)) {
  cat("\nEXCLUDED -- segmentation differs between the two files:\n")
  for (b in al$divergent$blocks) {
    d <- al$divergent$detail[[b]]
    cat(sprintf("  student %-4s week %-3s human %d units, model %d units, %d ids shared\n",
                sub(" .*", "", b), sub(".* ", "", b), d["human"], d["model"], d["shared"]))
  }
  cat("  The whole block is dropped, not just the unshared ids: where the two\n")
  cat("  files split a block into different questions, a shared id can point at\n")
  cat("  different sentences, and matching on it would compare unrelated text.\n")
}

m_all <- cbind(al$h_code, al$l_code)
KIND <- c(rep("human", ncol(al$h_code)), rep("model", ncol(al$l_code)))
cat(sprintf("\nmissing cells       %d of %d\n", sum(is.na(m_all)), length(m_all)))

# --- panel-level alpha ---------------------------------------------------
show <- function(lbl, m, lv = PRIMARY_LEVELS, boot = TRUE) {
  k <- krippendorff_alpha(m, lv)
  ci <- if (boot) bootstrap_ci(m, stat_alpha, lv, B = B) else list(lower = NA, upper = NA)
  cat(sprintf("  %-30s alpha %6.3f  [%.3f, %.3f]  n=%d raters=%d\n",
              lbl, k$alpha, ci$lower, ci$upper, k$n_units, ncol(m)))
  data.frame(what = lbl, alpha = k$alpha, lo = ci$lower, hi = ci$upper,
             n_units = k$n_units, n_raters = ncol(m), stringsAsFactors = FALSE)
}

hr("PRIMARY CODE -- four categories, nominal")
NH <- ncol(al$h_code); NM <- ncol(al$l_code)
res <- list()
# A one-rater side has no internal agreement to measure. Reporting alpha on a
# single column would be meaningless rather than merely uninteresting, so it is
# skipped and said out loud.
if (NH > 1L) res[[length(res) + 1L]] <- show(sprintf("among the %d humans", NH), al$h_code)
if (NM > 1L) res[[length(res) + 1L]] <- show(sprintf("among the %d models", NM), al$l_code)
res[[length(res) + 1L]] <- show(sprintf("all %d raters pooled", NH + NM), m_all)

h_mod <- panel_modal(al$h_code, tie = NA_character_)
l_mod <- panel_modal(al$l_code, tie = NA_character_)
xlab <- if (NH > 1L) "human panel vs model panel" else
        sprintf("%s vs model panel", al$humans[1])
res[[length(res) + 1L]] <- show(xlab, cbind(humans = h_mod, models = l_mod))
res <- do.call(rbind, res)

if (NH > 1L) {
  crit <- res$alpha[res$what == xlab]
  hlo  <- res$lo[grepl("^among the .* humans$", res$what)]
  hhi  <- res$hi[grepl("^among the .* humans$", res$what)]
  # DIRECTION MATTERS. The question S8.3 asks is whether the model panel is at
  # least as consistent with the human panel as the humans are with each other.
  # Landing ABOVE the human interval answers that with room to spare; only
  # landing BELOW it is a failure. A two-sided "inside the interval" test would
  # report the best possible result as a failed criterion.
  verdict <- if (crit >= hlo) "MET" else "NOT MET"
  where <- if (crit < hlo) "lies BELOW" else if (crit > hhi) "lies ABOVE" else "lies INSIDE"
  cat(sprintf("\nS8.3 criterion: alpha(panels) = %.3f %s the human interval [%.3f, %.3f]  -> %s\n",
              crit, where, hlo, hhi, verdict))
  if (crit > hhi) {
    cat("  Above the interval, not below: the two panels agree with each other MORE\n")
    cat("  than the three people agree among themselves. The criterion asks for at\n")
    cat("  least parity, so this passes -- but it also says the model panel is the\n")
    cat("  more internally consistent instrument here, which is worth discussing\n")
    cat("  rather than celebrating: agreeing closely is not the same as being right.\n")
  }
} else {
  cat("\nS8.3 criterion NOT EVALUABLE: it asks whether model-human agreement falls\n")
  cat("inside the interval for agreement AMONG humans, and with one human rater\n")
  cat("that interval does not exist. The figure above is how well the model panel\n")
  cat("tracks one person, which is a weaker claim -- it cannot distinguish the\n")
  cat("models being right from the models sharing that person's particular reading.\n")
}

# --- marking rate: does the scope gate behave the same for both types? ---
hr("SCOPE GATE -- share of sentences given any code other than Not marked")
mk <- apply(m_all, 2L, function(v) mean(v != "Not marked", na.rm = TRUE))
for (i in seq_along(mk)) {
  cat(sprintf("  %-12s %-6s %5.1f%%   (%d of %d)\n", colnames(m_all)[i], KIND[i],
              100 * mk[i], sum(m_all[, i] != "Not marked", na.rm = TRUE),
              sum(!is.na(m_all[, i]))))
}
cat(sprintf("\n  humans %.1f%%  vs  models %.1f%%   difference %.1f pp\n",
            100 * mean(mk[KIND == "human"]), 100 * mean(mk[KIND == "model"]),
            100 * abs(mean(mk[KIND == "human"]) - mean(mk[KIND == "model"]))))

DETECT <- c("Relevant", "Not marked")
to_det <- function(m) ifelse(is.na(m), NA_character_,
                             ifelse(m == "Not marked", "Not marked", "Relevant"))
cat("\ndetection only (marked vs not), the scope gate as its own variable:\n")
for (nm in c("human", "model")) {
  if (sum(KIND == nm) < 2L) {
    cat(sprintf("  %-6s only one rater, so there is no internal agreement to report\n",
                paste0(nm, "s")))
    next
  }
  mm <- to_det(m_all[, KIND == nm, drop = FALSE])
  cat(sprintf("  %-6s alpha %6.3f   AC1 %6.3f\n", paste0(nm, "s"),
              krippendorff_alpha(mm, DETECT)$alpha, gwet_ac1(mm, DETECT)$ac1))
}

# --- every rater pair ----------------------------------------------------
hr(sprintf("ALL %d RATER PAIRS -- who agrees with whom", choose(NH + NM, 2)))
pairs <- utils::combn(colnames(m_all), 2, simplify = FALSE)
pw <- do.call(rbind, lapply(pairs, function(p) {
  x <- m_all[, p[1]]; y <- m_all[, p[2]]
  ok <- !is.na(x) & !is.na(y)
  ka <- krippendorff_alpha(m_all[, p], PRIMARY_LEVELS)
  ck <- cohens_kappa(x[ok], y[ok], PRIMARY_LEVELS)
  ta <- KIND[match(p[1], colnames(m_all))]; tb <- KIND[match(p[2], colnames(m_all))]
  data.frame(a = p[1], b = p[2],
             type = if (ta == tb) paste0(ta, "-", tb) else "human-model",
             alpha = ka$alpha, kappa = ck$kappa,
             pct = percent_agreement(x[ok], y[ok]), n = sum(ok),
             stringsAsFactors = FALSE)
}))
pw <- pw[order(-pw$alpha), ]
cat(sprintf("  %-12s %-12s %-13s %6s %6s %6s %4s\n",
            "rater A", "rater B", "pair type", "alpha", "kappa", "%agree", "n"))
for (i in seq_len(nrow(pw))) {
  cat(sprintf("  %-12s %-12s %-13s %6.3f %6.3f %5.1f%% %4d\n",
              pw$a[i], pw$b[i], pw$type[i], pw$alpha[i], pw$kappa[i],
              100 * pw$pct[i], pw$n[i]))
}
cat("\nbest pair overall      ", sprintf("%s + %s  (alpha %.3f)", pw$a[1], pw$b[1], pw$alpha[1]), "\n")
for (tp in intersect(c("human-human", "model-model", "human-model"), unique(pw$type))) {
  s <- pw[pw$type == tp, ]
  cat(sprintf("best %-16s %s + %s  (alpha %.3f)   |  mean over %d pairs %.3f\n",
              tp, s$a[1], s$b[1], s$alpha[1], nrow(s), mean(s$alpha)))
}
s <- pw[nrow(pw), ]
cat(sprintf("weakest pair overall   %s + %s  (alpha %.3f)\n", s$a, s$b, s$alpha))

# --- CQ subclassification, at the level both types coded -----------------
hr("CQ SUBCLASSIFICATION -- factor level, the only level both types coded")
l_fac <- if (!is.null(al$l_type)) al$l_type else {
  stop("model file has no cq_type column; cannot compare at factor level")
}
cq_all <- cbind(al$h_cq, l_fac)
FACS <- unname(FACTOR_CANON[!duplicated(FACTOR_CANON)])
cat(sprintf("units where at least 2 raters assigned a factor: %d\n",
            n_pairable_units(cq_all)))
cat("\nrestricted to units the whole panel called Positive (a clean comparison,\n")
cat("but a small and self-selected one -- read it as indicative only):\n")
pos <- rowSums(m_all == "Positive", na.rm = TRUE) == ncol(m_all)
cat(sprintf("  units all %d called Positive: %d\n", NH + NM, sum(pos)))
cqres <- list()
if (n_pairable_units(cq_all) >= 5L) {
  if (NH > 1L) cqres[[length(cqres) + 1L]] <- show("factor, humans", al$h_cq, FACS)
  if (NM > 1L) cqres[[length(cqres) + 1L]] <- show("factor, models", l_fac, FACS)
  cqres[[length(cqres) + 1L]] <- show(sprintf("factor, all %d", NH + NM), cq_all, FACS)
  # A panel's factor verdict exists only where that panel's PRIMARY majority is
  # Positive. Unmasked, the modal factor is taken over whoever happened to say
  # Positive -- possibly one rater -- so units the panel called Not marked would
  # still contribute, and the base could exceed the units both called Positive.
  hf <- panel_modal(al$h_cq, tie = NA_character_)
  lf <- panel_modal(l_fac, tie = NA_character_)
  hf[is.na(h_mod) | h_mod != "Positive"] <- NA_character_
  lf[is.na(l_mod) | l_mod != "Positive"] <- NA_character_
  cqres[[length(cqres) + 1L]] <- show("factor, panel vs panel", cbind(humans = hf, models = lf), FACS)
  okf <- !is.na(hf) & !is.na(lf)
  cat(sprintf("    -> %d of %d matched exactly (%.0f%%); %d sentences were called Positive by both panels\n",
              sum(hf[okf] == lf[okf]), sum(okf), 100 * mean(hf[okf] == lf[okf]),
              sum(h_mod == "Positive" & l_mod == "Positive", na.rm = TRUE)))
}
cat("\nfactor marginals (how often each rater used each factor):\n")
print(apply(cq_all, 2L, function(v) table(factor(v, levels = FACS))))

cat("\nmodel CQ ITEM level, models only (humans did not code items):\n")
it_lv <- sort(unique(as.character(al$l_item[!is.na(al$l_item)])))
if (length(it_lv) >= 2L && n_pairable_units(al$l_item) >= 5L) {
  k <- krippendorff_alpha(al$l_item, it_lv)
  cat(sprintf("  item, models                 alpha %6.3f  n=%d  distinct items used %d\n",
              k$alpha, k$n_units, length(it_lv)))
}

# --- outputs -------------------------------------------------------------
dir.create("results", showWarnings = FALSE)
f1 <- sprintf("results/sixrater_alpha_%s.csv", LAB)
f2 <- sprintf("results/sixrater_pairwise_%s.csv", LAB)
f3 <- sprintf("results/sixrater_units_%s.csv", LAB)
utils::write.csv(if (length(cqres)) rbind(cbind(res, level = "primary"),
                   cbind(do.call(rbind, cqres), level = "cq_factor")) else
                 cbind(res, level = "primary"), f1, row.names = FALSE)
utils::write.csv(pw, f2, row.names = FALSE)
utils::write.csv(cbind(u[, c("unit_id", "student", "week", "question", "sentence")],
                       as.data.frame(al$h_code), humans = panel_modal(al$h_code),
                       as.data.frame(al$l_code), models = panel_modal(al$l_code),
                       human_factor = panel_modal(al$h_cq, tie = NA_character_),
                       model_factor = panel_modal(l_fac, tie = NA_character_),
                       model_item = panel_modal(al$l_item, tie = NA_character_)),
                 f3, row.names = FALSE, na = "")
cat("\nwrote", f1, "\n     ", f2, "\n     ", f3, "\n")
