#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# 17_pool.R -- one long table of labels across every rating batch
#
#   Rscript scripts/17_pool.R --batches config/batches.csv \
#                             --out results/pooled_units.csv
#
# Each batch is a (human file, model file) pair that has already been aligned,
# remapped and omit-filtered by R/six_rater.R, so pooling here cannot reintroduce
# the id problems those steps solved.
#
# ONE STUDENT MUST APPEAR IN ONE BATCH ONLY. Students 8 and 94 were rated twice:
# once in the `test2` batch and again in `Sep09`, from the same journals
# segmented into different questions (identical sentence counts per week, almost
# no shared ids). Pooling both would enter those two students twice under
# different ids and silently double their weight in every cohort figure, so
# `test2` is simply not listed in the batch manifest. This script refuses to run
# if any student still appears in more than one batch.
#
# READS NO JOURNAL TEXT.
# ---------------------------------------------------------------------------

repo <- normalizePath(file.path(dirname(sub("^--file=", "",
        commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])), ".."))
setwd(repo)
source("R/six_rater.R")

a <- commandArgs(trailingOnly = TRUE)
getopt <- function(f, d = NULL) { i <- match(f, a); if (is.na(i) || i == length(a)) d else a[i + 1L] }
BATCH <- getopt("--batches", "config/batches.csv")
ONLY  <- getopt("--only", NA)
OUT   <- getopt("--out", "results/pooled_units.csv")

bs <- utils::read.csv(BATCH, stringsAsFactors = FALSE, na.strings = c("NA", ""))
# --only restricts the pool to named batches, so a report can be built on one
# cohort without editing the manifest or hand-trimming the output.
if (!is.na(ONLY)) {
  want <- trimws(unlist(strsplit(ONLY, "\\s*,\\s*")))
  miss <- setdiff(want, bs$batch)
  if (length(miss)) stop("--only names batches not in ", BATCH, ": ",
                         paste(miss, collapse = ", "))
  bs <- bs[bs$batch %in% want, , drop = FALSE]
  cat(sprintf("restricted to batch(es): %s\n", paste(want, collapse = ", ")))
}
cat(sprintf("batches: %d\n\n", nrow(bs)))

blank <- function(x) is.null(x) || is.na(x) || !nzchar(x)
out <- list()
for (i in seq_len(nrow(bs))) {
  b <- bs[i, ]
  al <- align_raters(b$human_file, b$llm_file,
                     omit_file  = if (blank(b$omit_file))  NULL else b$omit_file,
                     remap_file = if (blank(b$remap_file)) NULL else b$remap_file)
  u <- al$units
  cat(sprintf("%-8s %3d units | students %-22s | humans %-22s | models %s\n",
              b$batch, nrow(u), paste(unique(u$student), collapse = ","),
              paste(al$humans, collapse = ","), paste(al$models, collapse = ",")))

  # one row per unit per rater, plus the model panel's majority as its own rater
  # cq_item travels too, NA for the people: they coded the factor, the models the
  # item. Carrying both lets one pooled figure show each side at the level it
  # actually worked at, instead of flattening the models to match.
  mk <- function(mat, kind, cq, item = NULL) {
    do.call(rbind, lapply(colnames(mat), function(r) data.frame(
      batch = b$batch, unit_id = u$unit_id, student = u$student, week = u$week,
      rater = r, rater_kind = kind, code = mat[, r],
      cq_factor = if (is.null(cq)) NA_character_ else cq[, r],
      cq_item = if (is.null(item)) NA_character_ else item[, r],
      stringsAsFactors = FALSE)))
  }
  rows <- rbind(mk(al$h_code, "human", al$h_cq),
                mk(al$l_code, "model", al$l_type, al$l_item))
  # Each side's own majority, as its own pseudo-rater. Having both means the
  # human-consensus vs model-consensus question can be asked without rebuilding
  # anything -- but only where a side actually has more than one rater to take a
  # majority of, so it is emitted conditionally rather than silently duplicating
  # a lone rater's column under a grander name.
  side <- function(mat, cqm, lab) {
    if (ncol(mat) < 2L) return(NULL)
    mj <- panel_modal(mat, tie = NA_character_)
    mf <- panel_modal(cqm, tie = NA_character_)
    mf[is.na(mj) | mj != "Positive"] <- NA_character_
    mi <- if (identical(lab, "model panel")) {
      x <- panel_modal(al$l_item, tie = NA_character_)
      x[is.na(mj) | mj != "Positive"] <- NA_character_; x
    } else NA_character_
    data.frame(batch = b$batch, unit_id = u$unit_id, student = u$student,
               week = u$week, rater = lab, rater_kind = "panel",
               code = mj, cq_factor = mf, cq_item = mi, stringsAsFactors = FALSE)
  }
  rows <- rbind(rows, side(al$l_code, al$l_type, "model panel"),
                side(al$h_code, al$h_cq, "human panel"))
  out[[i]] <- rows
}
p <- do.call(rbind, out)

# --- the check that makes pooling safe -----------------------------------
sb <- unique(p[, c("student", "batch")])
dup <- sb$student[duplicated(sb$student)]
if (length(dup)) {
  stop("student(s) ", paste(unique(dup), collapse = ", "),
       " appear in more than one batch. Pooling would count them twice.\n",
       "Decide which batch supersedes and remove the other from ", BATCH)
}
if (anyDuplicated(p[, c("unit_id", "rater")])) stop("duplicate unit x rater rows")

p$cq_factor[p$code != "Positive" | is.na(p$code)] <- NA_character_

# --- consensus across every rater who saw the unit -----------------------
# One label per sentence, by strict majority of all raters on that sentence.
# A sentence with no strict majority gets NA rather than an arbitrary pick, and
# is counted and reported rather than quietly dropped.
#
# Panel size need not be the same in every batch, and a consensus of six is a
# firmer thing than a consensus of four, so n_raters travels with every row and
# any analysis that treats them alike has to say so.
real <- p[p$rater_kind %in% c("human", "model"), ]
cons <- do.call(rbind, lapply(split(real, real$unit_id), function(d) {
  v <- d$code[!is.na(d$code)]
  tb <- sort(table(v), decreasing = TRUE)
  lab <- if (!length(tb) || (length(tb) > 1L && tb[1] == tb[2])) NA_character_ else names(tb)[1]
  fv <- d$cq_factor[!is.na(d$cq_factor)]
  ft <- sort(table(fv), decreasing = TRUE)
  fl <- if (!length(ft) || (length(ft) > 1L && ft[1] == ft[2])) NA_character_ else names(ft)[1]
  data.frame(batch = d$batch[1], unit_id = d$unit_id[1], student = d$student[1],
             week = d$week[1], rater = "consensus", rater_kind = "consensus",
             code = lab, cq_factor = if (identical(lab, "Positive")) fl else NA_character_,
             cq_item = NA_character_,
             n_raters = nrow(d), n_agree = if (length(tb)) as.integer(tb[1]) else 0L,
             stringsAsFactors = FALSE)
}))
p$n_raters <- NA_integer_; p$n_agree <- NA_integer_
p <- rbind(p, cons)

cat(sprintf("\nconsensus: %d sentences | panel size %s | no majority on %d (%.1f%%)\n",
            nrow(cons), paste(sort(unique(cons$n_raters)), collapse = " or "),
            sum(is.na(cons$code)), 100 * mean(is.na(cons$code))))
cat("consensus labels:\n"); print(table(cons$code, useNA = "ifany"))
cat("\nstrength of the consensus (raters backing the winning label):\n")
print(table(agreeing = cons$n_agree, of = cons$n_raters))

cat(sprintf("\npooled: %d rows | %d units | %d students | weeks %s\n",
            nrow(p), length(unique(p$unit_id)), length(unique(p$student)),
            paste(sort(unique(p$week)), collapse = "/")))
cat("\nunits per student x week\n")
uu <- unique(p[, c("student", "week", "unit_id")])
print(table(student = uu$student, week = uu$week))
cat("\nraters and their coverage (students rated)\n")
cv <- tapply(p$student, p$rater, function(x) length(unique(x)))
for (r in names(cv)) cat(sprintf("  %-14s %d of %d students\n", r, cv[[r]],
                                 length(unique(p$student))))
cat("\nRaters that do not cover every student cannot enter a cohort trend\n")
cat("without confounding rater with student; scripts/18_trajectory.R uses only\n")
cat("those with full coverage.\n")

utils::write.csv(p, OUT, row.names = FALSE, na = "")
cat("\nwrote", OUT, "\n")
