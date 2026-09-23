#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# 24_mock_pooled.R -- a pooled rater table for the INVENTED journals
#
#   Rscript scripts/24_mock_pooled.R
#
# Builds the same long table that scripts/17_pool.R produces for the real
# cohort, but from the five invented journals of data/example_journals_sections/
# and the written reference coding that ships beside them. The point is a
# worked example that anyone can regenerate from a clone: every input is in the
# repository, so the figures in the README are reproducible rather than
# decorative.
#
# The reference coding enters as a rater named `reference`, of kind "human".
# It is one person's written adjudication of 107 invented sentences, not a
# panel, so it supports no human-panel agreement figure -- only a comparison
# between it and the models. That asymmetry is real and is left visible.
#
# READS NO REAL JOURNAL TEXT. The only text involved is invented.
# ---------------------------------------------------------------------------

repo <- normalizePath(file.path(dirname(sub("^--file=", "",
        commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])), ".."))
setwd(repo)
source("R/six_rater.R")

a <- commandArgs(trailingOnly = TRUE)
getopt <- function(f, d = NULL) { i <- match(f, a); if (is.na(i) || i == length(a)) d else a[i + 1L] }
RAT <- getopt("--ratings", "results/ratings_example_sections_main.csv")
EXP <- getopt("--expected", "data/example_journals_sections/expected_labels.csv")
OUT <- getopt("--out", "mock_results/pooled_units_example_sections.csv")

l <- read_ratings(RAT)
e <- read_ratings(EXP)
l <- l[l$replicate == min(l$replicate), , drop = FALSE]   # one run per model

short <- function(m) {
  m <- sub(":latest$", "", m); m <- sub("^DeepSeek.*", "DeepSeek", m)
  sub("^gpt-oss.*", "gpt-oss", m)
}
u <- parse_units(unique(l$unit_id))
u <- u[order(u$student, u$week, u$question, u$sentence), ]

grab <- function(who, col) {
  s <- l[l$model == who, ]; s[[col]][match(u$unit_id, s$unit_id)]
}
models <- unique(l$model)
rows <- do.call(rbind, lapply(models, function(m) data.frame(
  batch = "example_sections", unit_id = u$unit_id, student = u$student,
  week = u$week, rater = short(m), rater_kind = "model",
  code = grab(m, "codes"), cq_factor = canon_factor(grab(m, "cq_type")),
  cq_item = grab(m, "cq_item"), stringsAsFactors = FALSE)))

ref <- data.frame(
  batch = "example_sections", unit_id = u$unit_id, student = u$student,
  week = u$week, rater = "reference", rater_kind = "human",
  code = e$expected_code[match(u$unit_id, e$unit_id)],
  cq_factor = canon_factor(e$expected_cq_type[match(u$unit_id, e$unit_id)]),
  cq_item = e$expected_item[match(u$unit_id, e$unit_id)],
  stringsAsFactors = FALSE)
rows <- rbind(ref, rows)
rows$cq_factor[rows$code != "Positive" | is.na(rows$code)] <- NA_character_
rows$cq_item[rows$code != "Positive" | is.na(rows$code)] <- NA_character_

bad <- setdiff(unique(rows$code), c(PRIMARY_LEVELS, NA))
if (length(bad)) stop("code outside the codebook: ", paste(bad, collapse = ", "))

# model panel majority, as its own pseudo-rater
M <- vapply(short(models), function(r) rows$code[rows$rater == r],
            character(nrow(u)))
FM <- vapply(short(models), function(r) rows$cq_factor[rows$rater == r],
             character(nrow(u)))
IM <- vapply(short(models), function(r) rows$cq_item[rows$rater == r],
             character(nrow(u)))
mj <- panel_modal(M, tie = NA_character_)
mf <- panel_modal(FM, tie = NA_character_); mf[is.na(mj) | mj != "Positive"] <- NA
mi <- panel_modal(IM, tie = NA_character_); mi[is.na(mj) | mj != "Positive"] <- NA
rows <- rbind(rows, data.frame(
  batch = "example_sections", unit_id = u$unit_id, student = u$student,
  week = u$week, rater = "model panel", rater_kind = "panel",
  code = mj, cq_factor = mf, cq_item = mi, stringsAsFactors = FALSE))

rows$n_raters <- NA_integer_; rows$n_agree <- NA_integer_
dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)
utils::write.csv(rows, OUT, row.names = FALSE, na = "")

cat(sprintf("%d units | 1 reference coding + %d models | %d rows\n",
            nrow(u), length(models), nrow(rows)))
cat("raters:", paste(unique(rows$rater), collapse = ", "), "\n")
cat("wrote", OUT, "\n")
