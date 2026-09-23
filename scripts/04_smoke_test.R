#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# 04_smoke_test.R -- check the prompts against the synthetic fixture
#
#   Rscript scripts/04_smoke_test.R                      # fastest model only
#   Rscript scripts/04_smoke_test.R --models all
#   Rscript scripts/04_smoke_test.R --models qwen3:14b --refresh
#
# data/units_synthetic.csv is invented sentence-level text (no student data)
# with an `expected_codes` / `expected_cq` column per unit. It deliberately
# covers all four primary codes and all four CQ factors, including the two
# self-referential factors (Behavioural, Metacognitive) that a careless
# "self-referential means Self awareness" rule collapses into Self awareness.
#
# THE GATE IS OVER-MARKING. Since codebook v5 most sentences must come back
# "Not marked", and the characteristic failure of a language model here is to
# find something positive in every sentence. So 18 of the 30 fixture sentences
# are things that must NOT be marked -- praise for a lecture, complaints about
# workload, plain logistics -- and a model that marks too many of them fails.
#
# The fixture's marked share (40%) is far higher than a real journal's, because
# it also has to exercise every code and every CQ factor. Do not read it as an
# expected base rate.
#
# This is a prompt regression test, not a model benchmark: a failure means the
# codebook wording lets a reasonable coder go wrong, so fix the wording rather
# than the expectation. Run it after every edit to config/codebook.yml.
# ---------------------------------------------------------------------------

repo <- normalizePath(file.path(dirname(sub("^--file=", "",
        commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])), ".."))
setwd(repo)
source("R/codebook.R"); source("R/units.R"); source("R/llm.R"); source("R/rate.R")

a <- commandArgs(trailingOnly = TRUE)
getopt <- function(f, d = NULL) { i <- match(f, a); if (is.na(i) || i == length(a)) d else a[i + 1L] }
has <- function(f) f %in% a

FIXTURE <- "data/units_synthetic.csv"
cb  <- load_codebook("config/codebook.yml")
cfg <- load_run_config("config/run.yml")

which_models <- getopt("--models", "fastest")
if (identical(which_models, "fastest")) {
  cfg$active_models <- cfg$active_models[1]      # run.yml is ordered fastest-first
} else if (!identical(which_models, "all")) {
  want <- trimws(strsplit(which_models, ",")[[1]])
  cfg$active_models <- Filter(function(m) m$id %in% want, cfg$models)
  if (!length(cfg$active_models)) stop("no model matches: ", which_models, call. = FALSE)
}

expected <- utils::read.csv(FIXTURE, stringsAsFactors = FALSE, na.strings = c("NA", ""))
if (!all(c("expected_codes", "expected_cq") %in% names(expected))) {
  stop(FIXTURE, " must carry expected_codes and expected_cq columns", call. = FALSE)
}
units <- read_units(FIXTURE)

# Canonicalise an expected code set the same way the runner canonicalises the
# model's answer, so " | "-joined sets compare regardless of order or spacing.
canon_set <- function(s) {
  vapply(s, function(x) {
    if (is.na(x)) return(NA_character_)
    v <- trimws(strsplit(x, "|", fixed = TRUE)[[1]])
    v <- v[nzchar(v)]
    code_set_string(v[order(match(v, cb$primary_labels))], CODE_SEP)
  }, character(1), USE.NAMES = FALSE)
}
expected$expected_codes <- canon_set(expected$expected_codes)

preflight(cfg)
cat(sprintf("\ncodebook v%s | context_mode %s | %d unit(s) | %s\n\n", cb$version,
            cfg$context_mode, nrow(units),
            paste(vapply(cfg$active_models, function(m) m$id, character(1)), collapse = ", ")))

ratings <- rate_units(units, cb, cfg, profile = "main", refresh = has("--refresh"))

# Persist under a distinct name so a miss can be diagnosed from the rationale,
# without clobbering a real 02_rate.R run's output.
dir.create(cfg$paths$cache, showWarnings = FALSE, recursive = TRUE)
smoke_out <- file.path(cfg$paths$cache, "smoke_test_with_rationales.csv")
utils::write.csv(ratings, smoke_out, row.names = FALSE, na = "")

keep_cols <- c("unit_id", "model", "codes", "cq_type",
               intersect("cq_item", names(ratings)))
res <- merge(ratings[, keep_cols],
             expected[, c("unit_id", "expected_codes", "expected_cq")],
             by = "unit_id")

NM <- "Not marked"
res$got_marked  <- !is.na(res$codes) & res$codes != NM
res$want_marked <- res$expected_codes != NM
res$exact_ok    <- !is.na(res$codes) & res$codes == res$expected_codes

# The scheme is really two decisions, and pooling them hides which one broke:
#   detection  is this sentence about intercultural experience at all?
#   choice     given that it is, which of the three?
# Over-marking (marking a sentence that must stay unmarked) is the failure the
# codebook is written against, so it is the only hard gate.
OVER_MARK_MAX <- 0.25

fail <- character(0)
for (mid in unique(res$model)) {
  r <- res[res$model == mid, ]
  r <- r[order(r$unit_id), ]

  n_nm <- sum(!r$want_marked)
  over <- sum(!r$want_marked & r$got_marked)
  miss <- sum(r$want_marked & !r$got_marked)
  both <- r[r$want_marked & r$got_marked, ]
  wrong_choice <- sum(both$expected_codes != both$codes, na.rm = TRUE)
  cq_check <- r[!is.na(r$expected_cq) & !is.na(r$cq_type), ]
  cq_ok <- sum(cq_check$cq_type == cq_check$expected_cq)

  cat(sprintf("== %s\n", mid))
  cat(sprintf("   exact code        %2d/%2d\n", sum(r$exact_ok), nrow(r)))
  cat(sprintf("   OVER-MARKED       %2d/%2d of the must-not-mark sentences (%.0f%%, gate %.0f%%)\n",
              over, n_nm, 100 * over / max(1L, n_nm), 100 * OVER_MARK_MAX))
  cat(sprintf("   missed marking    %2d/%2d of the must-mark sentences\n",
              miss, sum(r$want_marked)))
  cat(sprintf("   wrong choice      %2d/%2d (marked correctly, wrong one of the three)\n",
              wrong_choice, nrow(both)))
  cat(sprintf("   cq factor         %2d/%2d of those reaching stage 2\n",
              cq_ok, nrow(cq_check)))

  bad <- r[!r$exact_ok, ]
  if (nrow(bad)) {
    for (i in seq_len(nrow(bad))) {
      kind <- if (!bad$want_marked[i] && bad$got_marked[i]) "OVER "
              else if (bad$want_marked[i] && !bad$got_marked[i]) "MISS "
              else "CHOICE"
      cat(sprintf("   %-6s %-18s expected %-15s got %-15s\n",
                  kind, bad$unit_id[i], bad$expected_codes[i],
                  bad$codes[i] %||% "NA"))
    }
  }
  cq_bad <- cq_check[cq_check$cq_type != cq_check$expected_cq, ]
  if (nrow(cq_bad)) {
    for (i in seq_len(nrow(cq_bad))) {
      cat(sprintf("   CQ     %-18s expected %-15s got %-15s%s\n",
                  cq_bad$unit_id[i], cq_bad$expected_cq[i], cq_bad$cq_type[i],
                  if ("cq_item" %in% names(cq_bad)) paste0(" (", cq_bad$cq_item[i], ")") else ""))
    }
  }

  if (over / max(1L, n_nm) > OVER_MARK_MAX) fail <- c(fail, mid)
  cat("\n")
}

if ("cq_item" %in% names(res) && any(!is.na(res$cq_item))) {
  cat("== CQ items chosen ==\n")
  print(table(model = res$model, item = res$cq_item))
  cat("\n")
}

if (length(fail)) {
  cat(sprintf("GATE FAILED -- %d model(s) marked more than %.0f%% of the sentences that\n",
              length(fail), 100 * OVER_MARK_MAX))
  cat("must stay unmarked: ", paste(fail, collapse = ", "), "\n", sep = "")
  cat("Treat this as a codebook-wording bug first: read the rationale in\n")
  cat("  ", smoke_out, "\n", sep = "")
  cat("and check whether a decision rule actually licensed the wrong answer\n")
  cat("before changing the expectation. The scope gate is decision rule 1 in\n")
  cat("config/codebook.yml.\n")
  quit(status = 1L)
}
cat("scope gate holds for every model tested.\n")
