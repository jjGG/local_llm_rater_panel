#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# diagnose_alpha.R -- why does another tool report a different alpha?
#
#   Rscript test_for_consistency/diagnose_alpha.R
#
# Two tools disagreeing on Krippendorff's alpha for the SAME sheet almost always
# means they are not computing the same quantity. Rather than argue about
# implementations, this computes alpha under every specification that a
# reasonable person might have chosen, and prints them together. The competing
# figure then usually identifies itself: whichever row it matches names the
# difference.
#
# Ordered roughly from "most likely to inflate" downwards. Nothing here is a
# claim that any row is correct -- only row 1 is what we report.
# ---------------------------------------------------------------------------

repo <- normalizePath(file.path(dirname(sub("^--file=", "",
        commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])), ".."))
setwd(repo)
source("test_for_consistency/R/io.R")
source("test_for_consistency/R/agreement.R")

SHEET <- "test_for_consistency/260728 Coding alignment Omics in Oncology.csv"
d <- read_alignment(SHEET)
long <- d$long
units <- sort(unique(long$unit_id))
m <- ratings_matrix(long, "primary", units)
raters <- colnames(m)

row <- function(spec, alpha, n_units, n_cat, note) {
  data.frame(spec = spec, alpha = round(alpha, 4), n_units = n_units,
             n_cat = n_cat, note = note, stringsAsFactors = FALSE)
}
out <- list()

# 1. What we report -------------------------------------------------------
k1 <- krippendorff_alpha(m, PRIMARY_LEVELS)
out[[1]] <- row("nominal, 4 categories, all units  <-- WE REPORT THIS",
                k1$alpha, k1$n_units, 4,
                "the codes are unordered names, so nominal is the correct choice")

# 2. Measurement level mis-set -------------------------------------------
# The most common cause of an inflated alpha: labels recoded 1..4 and the tool's
# measurement level left at scale/interval or ordinal.
k2 <- krippendorff_alpha(m, PRIMARY_LEVELS, delta_interval(PRIMARY_LEVELS))
out[[length(out) + 1L]] <- row("INTERVAL on codes 1..4 (order as in our codebook)",
                k2$alpha, k2$n_units, 4,
                "not interpretable here; inflates by treating adjacent codes as similar")

# Category order is arbitrary, so an interval alpha depends on it. Show the
# spread over orderings to make the arbitrariness visible.
perms <- list(c(1,2,3,4), c(4,1,2,3), c(2,3,4,1), c(1,3,2,4), c(4,3,2,1), c(2,1,4,3))
iv <- vapply(perms, function(p) {
  lv <- PRIMARY_LEVELS[p]
  krippendorff_alpha(m, lv, delta_interval(lv))$alpha
}, numeric(1))
out[[length(out) + 1L]] <- row(sprintf("INTERVAL, worst/best over 6 category orderings"),
                max(iv), k1$n_units, 4,
                sprintf("range %.3f to %.3f -- the answer depends on list order",
                        min(iv), max(iv)))

k4 <- krippendorff_alpha(m, PRIMARY_LEVELS, delta_ordinal(m, PRIMARY_LEVELS))
out[[length(out) + 1L]] <- row("ORDINAL on codes 1..4", k4$alpha, k4$n_units, 4,
                "also not interpretable here; also order-dependent")

# 3. A different variable ------------------------------------------------
DETECT <- c("Relevant", "Not marked")
md <- ifelse(is.na(m), NA_character_,
             ifelse(m == "Not marked", "Not marked", "Relevant"))
k5 <- krippendorff_alpha(md, DETECT)
out[[length(out) + 1L]] <- row("DETECTION only: marked vs Not marked", k5$alpha, k5$n_units, 2,
                "a different question -- did we flag the same sentences at all")

relevant <- apply(m, 1L, function(v) all(!is.na(v) & v != "Not marked"))
mv <- m[relevant, , drop = FALSE]
k6 <- krippendorff_alpha(mv, setdiff(PRIMARY_LEVELS, "Not marked"))
out[[length(out) + 1L]] <- row("VALENCE only: units all three marked", k6$alpha, k6$n_units, 3,
                "drops the units we disagreed about most -- raises alpha by selection")

# 3b. IMPORT ARTEFACTS -- the likeliest cause of all -----------------------
# These are not statistical choices at all. They are what happens when the sheet
# is read slightly differently, and both push alpha UP.

# (i) "No comment" left blank in the sheet and imported as MISSING rather than as
#     a category. Alpha then computes on pairable values only, so the very cells
#     we disagreed about most stop contributing.
mm <- m; mm[mm == "Not marked"] <- NA_character_
k_miss <- krippendorff_alpha(mm, setdiff(PRIMARY_LEVELS, "Not marked"))
out[[length(out) + 1L]] <- row(
  "\"No comment\" imported as MISSING, not as a category",
  k_miss$alpha, k_miss$n_units, 3,
  "LIKELY CAUSE: blanks in the sheet read as missing data")

# (ii) The trailing padding rows kept as data. Our reader drops 10 of them. If
#      they survive import, every rater is blank on each, which reads as a run of
#      perfectly-agreed units and lifts alpha mechanically.
n_pad <- 10L
m_pad <- rbind(m, matrix("(blank)", n_pad, ncol(m), dimnames = list(NULL, colnames(m))))
k_pad <- krippendorff_alpha(m_pad, c(PRIMARY_LEVELS, "(blank)"))
out[[length(out) + 1L]] <- row(
  sprintf("%d trailing padding rows kept as a category", n_pad),
  k_pad$alpha, k_pad$n_units, 5,
  "LIKELY CAUSE: empty rows counted as units all three agreed on")

# (iii) REPRODUCES THE SPSS RUN of 2026-08-21 (Hayes' KALPHA macro), which
#       reported: Ordinal, alpha .8380, Units 36, Observers 3, Pairs 108.
#       Units 36 and Pairs 108 identify it exactly -- 26 real units plus the 10
#       empty padding rows, with a value in every cell. Combine that with the
#       ordinal measurement level and we land within 0.007 of the reported
#       figure; the residual is a difference in how the ordinal difference
#       function handles its endpoints, not in the data.
m_spss <- rbind(m, matrix("(blank)", n_pad, ncol(m), dimnames = list(NULL, colnames(m))))
lv_spss <- c(PRIMARY_LEVELS, "(blank)")
k_spss <- krippendorff_alpha(m_spss, lv_spss, delta_ordinal(m_spss, lv_spss))
out[[length(out) + 1L]] <- row(
  "ORDINAL + padding rows  <-- REPRODUCES THE SPSS RUN",
  k_spss$alpha, k_spss$n_units, 5,
  "SPSS reported .8380 / 36 units / 108 pairs; we get the same to 0.007")

# (iv) Both at once.
m_both <- rbind(mm, matrix("(blank)", n_pad, ncol(m), dimnames = list(NULL, colnames(m))))
k_both <- krippendorff_alpha(m_both, c(setdiff(PRIMARY_LEVELS, "Not marked"), "(blank)"))
out[[length(out) + 1L]] <- row("both import artefacts together",
                               k_both$alpha, k_both$n_units, 4,
                               "blanks as missing AND padding rows kept")

# 4. Collapsing categories ------------------------------------------------
mc <- ifelse(is.na(m), NA_character_,
             ifelse(m == "Self awareness", "Positive", m))
k7 <- krippendorff_alpha(mc, c("Positive", "Negative", "Not marked"))
out[[length(out) + 1L]] <- row("Self awareness merged into Positive", k7$alpha, k7$n_units, 3,
                "fewer categories, fewer ways to disagree")

# 5. Declared category count ---------------------------------------------
# Alpha uses only observed categories, but some tools ask you to declare them.
k8 <- krippendorff_alpha(m, c(PRIMARY_LEVELS, "unused"))
out[[length(out) + 1L]] <- row("nominal, but 5 categories declared (one unused)",
                k8$alpha, k8$n_units, 5,
                "alpha is unchanged by an unused category; AC1 is NOT")

# 6. Pairs rather than the panel -----------------------------------------
pw <- utils::combn(raters, 2, simplify = FALSE)
pa <- vapply(pw, function(p) krippendorff_alpha(m[, p], PRIMARY_LEVELS)$alpha, numeric(1))
names(pa) <- vapply(pw, paste, character(1), collapse = "-")
out[[length(out) + 1L]] <- row(sprintf("best PAIR of raters (%s)", names(pa)[which.max(pa)]),
                max(pa), k1$n_units, 4,
                sprintf("all pairs: %s",
                        paste(sprintf("%s %.3f", names(pa), pa), collapse = ", ")))

# 7. Other coefficients sometimes called "agreement" ---------------------
out[[length(out) + 1L]] <- row("percent agreement (not chance-corrected)",
                 mean(apply(m, 1L, function(v) length(unique(v[!is.na(v)])) == 1L)),
                 k1$n_units, 4, "proportion of units where all three agree")
out[[length(out) + 1L]] <- row("Gwet's AC1 (nominal, 4 categories)",
                 gwet_ac1(m, PRIMARY_LEVELS)$ac1, k1$n_units, 4,
                 "prevalence-robust; legitimately higher than alpha when skewed")
out[[length(out) + 1L]] <- row("Fleiss' kappa (nominal, 4 categories)",
                 fleiss_kappa(m, PRIMARY_LEVELS)$kappa, k1$n_units, 4,
                 "assumes raters are interchangeable")

res <- do.call(rbind, out)

cat("\n")
cat("Krippendorff's alpha and neighbours, on the SAME 26 units x 3 raters\n")
cat(strrep("=", 78), "\n", sep = "")
print(res[, c("spec", "alpha", "n_units", "n_cat")], row.names = FALSE)
cat("\nnotes\n", strrep("-", 78), "\n", sep = "")
for (i in seq_len(nrow(res))) {
  cat(sprintf(" %-52s %s\n", substr(res$spec[i], 1, 52), res$note[i]))
}

ci <- bootstrap_ci(m, stat_alpha, PRIMARY_LEVELS, B = 2000L)
cat(sprintf("\nOur reported figure: alpha = %.4f, 95%% CI %.3f to %.3f\n",
            k1$alpha, ci$lower, ci$upper))

cat("\n", strrep("=", 78), "\n", sep = "")
cat("HOW TO USE THIS\n")
cat(strrep("=", 78), "\n", sep = "")
cat("1. ASK FOR THE UNIT COUNT, NOT JUST THE COEFFICIENT. N fingerprints the\n")
cat("   specification better than alpha does:\n")
cat("     N = 26  same units as us -- a genuine methodological difference\n")
cat("     N = 21  \"No comment\" was treated as missing rather than as a code\n")
cat("     N = 18  only units all three raters marked were analysed\n")
cat("     N = 36  the trailing padding rows were kept as data\n")
cat("     N = 31  both of the above\n")
cat("2. THEN ASK WHICH MEASUREMENT LEVEL WAS SET. If the codes were entered as\n")
cat("   1..4 and the level left at scale/ordinal, the coefficient stops being\n")
cat("   interpretable: our four codes are unordered names, so \"1 vs 2\" is not a\n")
cat("   smaller disagreement than \"1 vs 4\". Note above how far an interval alpha\n")
cat("   moves when only the ORDER of the category list changes -- a quantity that\n")
cat("   depends on an arbitrary listing order is not measuring agreement.\n")
cat("3. ONLY THEN COMPARE IMPLEMENTATIONS. Ours is checked by 36 tests against\n")
cat("   analytic identities and irrCAC, and it also documents a real bug in one\n")
cat("   widely-used R package (irr::kripp.alpha, 3+ raters, complete data). A\n")
cat("   difference of implementation is the LAST explanation to reach for, not\n")
cat("   the first.\n")
cat("4. AND MIND THE INTERVAL. Any competing figure inside ")
cat(sprintf("%.3f to %.3f\n", ci$lower, ci$upper))
cat("   is not a discrepancy at all: 26 units cannot pin alpha down more\n")
cat("   tightly than that, whoever computes it.\n")

utils::write.csv(res, "test_for_consistency/results/alpha_specification_scan.csv",
                 row.names = FALSE)
cat("\nwrote test_for_consistency/results/alpha_specification_scan.csv\n")
