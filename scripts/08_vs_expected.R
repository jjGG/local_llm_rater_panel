#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# 08_vs_expected.R -- each model against a written reference reading
#
#   Rscript scripts/08_vs_expected.R --label example_sections
#   Rscript scripts/08_vs_expected.R --label example_sections --expected <file.csv>
#
# The reference is a CSV with unit_id / expected_code / expected_item, as
# produced by tools/make_expected_labels.R.
#
# WHAT THIS IS AND IS NOT. The reference for the invented example set was written
# by whoever wrote the sentences, at the same time as the sentences. That is a
# weaker thing than an independent coding: it inherits whatever the author already
# believed the codebook meant, and it was never blind. So this script does NOT
# report "accuracy" and does not rank the models. It reports WHERE a model and the
# reference diverge, and it deliberately separates:
#
#   detection  marked vs Not marked -- the scope gate, which decides most units
#   choice     which of the three, among units both sides marked
#   cq         factor and item, among units both sides called Positive
#
# and it flags UNANIMOUS divergence separately, because when all models agree
# against the reference the reference is the more likely thing to be wrong.
#
# For genuine inter-rater reliability among the models, use scripts/07_alpha.R.
# ---------------------------------------------------------------------------

repo <- normalizePath(file.path(dirname(sub("^--file=", "",
        commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])), ".."))
setwd(repo)
source("R/codebook.R")
source("test_for_consistency/R/agreement.R")

a <- commandArgs(trailingOnly = TRUE)
getopt <- function(f, d = NULL) { i <- match(f, a); if (is.na(i) || i == length(a)) d else a[i + 1L] }

label   <- getopt("--label")
profile <- getopt("--profile", "main")
exp_in  <- getopt("--expected")
B_BOOT  <- as.integer(getopt("--boot", "2000"))

cb  <- load_codebook("config/codebook.yml")
cfg <- load_run_config("config/run.yml")
out_dir <- cfg$paths$results
NM <- "Not marked"

if (is.null(label)) stop("give --label <name>, e.g. --label example_sections", call. = FALSE)
rat_path <- file.path(out_dir, sprintf("ratings_%s_%s.csv", label, profile))
if (!file.exists(rat_path)) stop("not found: ", rat_path, call. = FALSE)

if (is.null(exp_in)) {
  guess <- c(sprintf("data/example_journals_sections/expected_labels.csv"),
             sprintf("data/expected_labels_%s.csv", label))
  hit <- guess[file.exists(guess)]
  if (!length(hit)) {
    stop("no expected-labels file found. Pass --expected <file.csv>, or build one:\n",
         "  Rscript tools/make_expected_labels.R", call. = FALSE)
  }
  exp_in <- hit[1]
  message("using reference: ", exp_in)
}

r <- utils::read.csv(rat_path, stringsAsFactors = FALSE, na.strings = c("NA", ""))
e <- utils::read.csv(exp_in,   stringsAsFactors = FALSE, na.strings = c("NA", ""))
stopifnot(all(c("unit_id", "expected_code") %in% names(e)))

# Restrict to the panel under analysis. The modal answer depends on WHICH models
# vote, so leaving a retired model in the file would report a panel nobody uses.
split_csv <- function(x) if (is.null(x)) NULL else trimws(strsplit(x, ",")[[1]])
only <- split_csv(getopt("--only")); excl <- split_csv(getopt("--exclude"))
if (!is.null(only) || !is.null(excl)) {
  unknown <- setdiff(c(only, excl), unique(r$model))
  if (length(unknown)) {
    stop("no such model in ", basename(rat_path), ": ", paste(unknown, collapse = ", "),
         "\n  present: ", paste(unique(r$model), collapse = ", "), call. = FALSE)
  }
  if (!is.null(only)) r <- r[r$model %in% only, , drop = FALSE]
  if (!is.null(excl)) r <- r[!r$model %in% excl, , drop = FALSE]
  cat(sprintf("PANEL restricted to %d model(s): %s\n", length(unique(r$model)),
              paste(sort(unique(r$model)), collapse = ", ")))
}

write_tbl <- function(df, name) {
  utils::write.csv(df, file.path(out_dir, name), row.names = FALSE, na = "")
  invisible(df)
}
stem <- sprintf("%s_%s", label, profile)
# A restricted analysis gets its own file names, so it cannot be confused with
# the full-panel one.
if (!is.null(only) || !is.null(excl)) {
  stem <- sprintf("%s_p%d", stem, length(unique(r$model)))
}

common <- intersect(unique(r$unit_id), e$unit_id)
if (!length(common)) stop("no shared unit_id between ratings and reference", call. = FALSE)
cat(sprintf("reference %s | %d unit(s) shared with the ratings\n",
            basename(exp_in), length(common)))

r <- r[r$unit_id %in% common, ]
e <- e[match(common, e$unit_id), ]
models <- sort(unique(r$model))

# One row per (unit, model), aligned to the reference.
pick <- function(mid, col) {
  d <- r[r$model == mid, ]
  d[[col]][match(common, d$unit_id)]
}

# --- Per model ------------------------------------------------------------
rows <- list(); diverge <- list()
for (mid in models) {
  got  <- pick(mid, "codes")
  item <- if ("cq_item" %in% names(r)) pick(mid, "cq_item") else rep(NA_character_, length(common))
  fac  <- if ("cq_type" %in% names(r)) pick(mid, "cq_type") else rep(NA_character_, length(common))

  usable <- !is.na(got)
  g_mark <- usable & got != NM
  e_mark <- e$expected_code != NM

  # Detection: a 2x2 on the scope gate. Reported with Cohen's kappa AND raw
  # counts, because with 74 of 107 units unmarked the raw rate looks flattering
  # on its own.
  tp <- sum(g_mark & e_mark, na.rm = TRUE)
  fp <- sum(g_mark & !e_mark, na.rm = TRUE)
  fn <- sum(!g_mark & e_mark & usable, na.rm = TRUE)
  tn <- sum(!g_mark & !e_mark & usable, na.rm = TRUE)
  k_det <- cohens_kappa(ifelse(g_mark[usable], "marked", NM),
                        ifelse(e_mark[usable], "marked", NM), c("marked", NM))

  both <- usable & g_mark & e_mark
  rows[[length(rows) + 1L]] <- data.frame(
    model = mid, n_units = sum(usable), n_failed = sum(!usable),
    exact = sum(usable & got == e$expected_code, na.rm = TRUE),
    # detection
    over_marked = fp, missed = fn, det_kappa = k_det$kappa,
    # choice, given both marked
    choice_n = sum(both), choice_agree = sum(both & got == e$expected_code, na.rm = TRUE),
    # cq, given both said Positive
    cq_n = sum(both & got == "Positive" & e$expected_code == "Positive" & !is.na(fac)),
    cq_factor_agree = sum(both & got == "Positive" & !is.na(fac) &
                          fac == e$expected_cq_type, na.rm = TRUE),
    cq_item_agree = sum(both & got == "Positive" & !is.na(item) &
                        item == e$expected_item, na.rm = TRUE),
    stringsAsFactors = FALSE)

  bad <- which(usable & got != e$expected_code)
  if (length(bad)) {
    diverge[[length(diverge) + 1L]] <- data.frame(
      unit_id = common[bad], model = mid, expected = e$expected_code[bad],
      got = got[bad], expected_item = e$expected_item[bad], got_item = item[bad],
      arguable = e$arguable[bad] %||% FALSE, stringsAsFactors = FALSE)
  }
}
per_model <- do.call(rbind, rows)
write_tbl(per_model, sprintf("expected_per_model_%s.csv", stem))

div <- if (length(diverge)) do.call(rbind, diverge) else NULL
if (!is.null(div)) write_tbl(div, sprintf("expected_divergences_%s.csv", stem))

# --- The panel's modal answer --------------------------------------------
# Does the ensemble track the reference better than its members? This is the one
# comparison that motivates using several models rather than the best one.
modal <- vapply(common, function(u) {
  v <- r$codes[r$unit_id == u]; v <- v[!is.na(v)]
  if (!length(v)) return(NA_character_)
  tb <- sort(table(v), decreasing = TRUE)
  if (length(tb) > 1L && tb[1] == tb[2]) NA_character_ else names(tb)[1]
}, character(1), USE.NAMES = FALSE)
n_tie <- sum(is.na(modal))
panel <- data.frame(
  rater = "panel modal", n_units = sum(!is.na(modal)), n_ties = n_tie,
  exact = sum(!is.na(modal) & modal == e$expected_code),
  over_marked = sum(!is.na(modal) & modal != NM & e$expected_code == NM),
  missed = sum(!is.na(modal) & modal == NM & e$expected_code != NM),
  stringsAsFactors = FALSE)
write_tbl(panel, sprintf("expected_panel_%s.csv", stem))

# --- How many models diverge, per unit -----------------------------------
# Counted against the models that ANSWERED that unit, not against the size of
# the panel. Getting this wrong hides the most important rows: with one model
# failing every call, "all 4 disagree" can never be true and the unanimous
# divergences -- the ones that indict the reference -- go unreported.
n_answered <- vapply(common, function(u)
  sum(!is.na(r$codes[r$unit_id == u])), integer(1), USE.NAMES = FALSE)

byunit <- NULL
if (!is.null(div)) {
  byunit <- do.call(rbind, lapply(unique(div$unit_id), function(u) {
    d <- div[div$unit_id == u, ]
    data.frame(unit_id = u, expected = d$expected[1],
               n_diverging = nrow(d),
               n_answered = n_answered[match(u, common)],
               models_said = paste(sort(unique(d$got)), collapse = " / "),
               arguable = isTRUE(d$arguable[1]), stringsAsFactors = FALSE)
  }))
  byunit$unanimous <- byunit$n_diverging == byunit$n_answered
  byunit <- byunit[order(-byunit$n_diverging, byunit$unit_id), ]
  write_tbl(byunit, sprintf("expected_by_unit_%s.csv", stem))
}
unan <- if (!is.null(byunit)) byunit[byunit$unanimous, ] else NULL
if (!is.null(unan) && nrow(unan)) {
  write_tbl(unan, sprintf("expected_unanimous_divergence_%s.csv", stem))
} else unan <- NULL

# --- Console -------------------------------------------------------------
cat("\n================ EACH MODEL VS THE REFERENCE READING ================\n")
p <- per_model
p$exact_pct <- round(100 * p$exact / p$n_units)
print(p[, c("model", "n_units", "n_failed", "exact", "exact_pct",
            "over_marked", "missed", "det_kappa")], row.names = FALSE, digits = 3)
cat("\nover_marked = reference says Not marked, model marked it (the failure the\n")
cat("codebook is written against). missed = the reverse.\n")

cat("\n---------------- the two decisions, separated ----------------\n")
p$choice_pct <- ifelse(p$choice_n > 0, round(100 * p$choice_agree / p$choice_n), NA)
p$cq_fac_pct <- ifelse(p$cq_n > 0, round(100 * p$cq_factor_agree / p$cq_n), NA)
p$cq_item_pct <- ifelse(p$cq_n > 0, round(100 * p$cq_item_agree / p$cq_n), NA)
print(p[, c("model", "choice_n", "choice_agree", "choice_pct",
            "cq_n", "cq_factor_agree", "cq_fac_pct", "cq_item_agree", "cq_item_pct")],
      row.names = FALSE)
cat("\nchoice = which of the three, among units BOTH sides marked.\n")
cat("cq = factor and item, among units both sides called Positive.\n")
cat("The factor-minus-item gap is disagreement about which item inside an\n")
cat("agreed factor -- expected, since several sentences match no item well.\n")

cat("\n---------------- the panel's modal answer ----------------\n")
print(panel, row.names = FALSE)
cat(sprintf("A modal answer better than every single model is the argument for\n"))
cat("using a panel; no better, and the extra models are only costing time.\n")

if (!is.null(unan)) {
  cat("\n================ EVERY ANSWERING MODEL DISAGREES WITH THE REFERENCE ================\n")
  print(unan[, c("unit_id", "expected", "models_said", "n_diverging",
                 "n_answered", "arguable")], row.names = FALSE)
  cat("\nREAD THESE AS CANDIDATE ERRORS IN THE REFERENCE, not in the models.\n")
  cat("Several independent readings against one is not evidence for the one.\n")
} else {
  cat("\nNo unit where every answering model disagrees with the reference.\n")
}

if (!is.null(byunit)) {
  cat("\n---------------- divergence concentrated on few units? ----------------\n")
  print(table(models_diverging = byunit$n_diverging))
  cat(sprintf("%d of %d units drew any disagreement at all.\n",
              nrow(byunit), length(common)))
  cat("Disagreement piled onto a handful of units is a definition problem;\n")
  cat("spread thinly across many is noise. They need different responses.\n")
}

cat(sprintf("\nwrote expected_*_%s.csv to %s\n", stem, out_dir))
cat("This is agreement with ONE written reading, not accuracy. For reliability\n")
cat("among the models, which needs no reference at all, use scripts/07_alpha.R.\n")
