#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# 09_ask.R -- rate one sentence or paragraph with the whole panel, right now
#
#   Rscript scripts/09_ask.R                      # reads ask.txt
#   Rscript scripts/09_ask.R --text "I enjoyed working with the other group."
#   Rscript scripts/09_ask.R --file somewhere.txt
#
# For the "we were wondering how the models would code THIS sentence" question.
# Put the text in ask.txt, run the command, read the table. The script creates
# ask.txt with instructions the first time you run it.
#
# It uses the SAME codebook, prompts, splitter, two-stage protocol and cache as
# a real run, so the answer here is the answer a real run would give -- otherwise
# the tool would be worse than useless. In particular:
#   - a paragraph is SPLIT INTO SENTENCES and each sentence is rated separately,
#     because the sentence is the unit. The paragraph rides along as context.
#   - a single sentence is rated with no context, which is the harder task.
#
# Options:
#   --text "..."     rate this text instead of reading a file
#   --file PATH      read from somewhere other than ask.txt
#   --models a,b     only these models (default: every enabled one)
#   --whole          treat the input as ONE unit, do not split into sentences
#   --quiet          just the table, no rationales
#
# NOTHING IS WRITTEN. This is a scratch probe, not a dataset: no results/ file,
# no manifest. Answers still land in the per-call cache, so asking the same
# thing twice is instant.
#
# PRIVACY. ask.txt is gitignored and blocked by the pre-commit hook, because the
# obvious thing to paste into it is a real student sentence. Every model this
# script calls runs on hardware we control -- the local Ollama daemon or the FGCZ
# vLLM -- so the text itself never leaves the premises.
#
# THE REMAINING RISK IS NOT THIS SCRIPT, IT IS THE TERMINAL IT PRINTS TO.
# If you are probing REAL journal text, run this yourself in your own shell. Do
# not ask a cloud AI assistant to run it for you and do not paste the text or
# the output into a chat with one: the sentence and the models' rationales would
# then leave the premises after all, by a route this repository cannot block.
# Invented text is unrestricted.
# ---------------------------------------------------------------------------

repo <- normalizePath(file.path(dirname(sub("^--file=", "",
        commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])), ".."))
setwd(repo)
source("R/codebook.R"); source("R/units.R"); source("R/llm.R"); source("R/rate.R")

a <- commandArgs(trailingOnly = TRUE)
getopt <- function(f, d = NULL) { i <- match(f, a); if (is.na(i) || i == length(a)) d else a[i + 1L] }
has <- function(f) f %in% a

ASK <- getopt("--file", "ask.txt")
TEMPLATE <- c(
  "# Put the sentence or paragraph you want the models to code below this block.",
  "# Lines starting with # are ignored, so these instructions can stay.",
  "#",
  "# A paragraph is split into sentences and each sentence is coded separately,",
  "# because the sentence is the unit of analysis. The paragraph is shown to the",
  "# model as context. Pass --whole to code the text as a single unit instead.",
  "#",
  "# Then run:   Rscript scripts/09_ask.R",
  "",
  "I was hesitant about the group work at first, but I really enjoyed working",
  "with the students from the partner university in the end.")

txt <- getopt("--text")
if (is.null(txt)) {
  if (!file.exists(ASK)) {
    writeLines(TEMPLATE, ASK)
    cat(sprintf("created %s with an example in it.\n\n", ASK))
    cat("Edit that file, then run this again:\n  Rscript scripts/09_ask.R\n")
    quit(status = 0L)
  }
  lines <- readLines(ASK, warn = FALSE, encoding = "UTF-8")
  lines <- lines[!grepl("^[[:space:]]*#", lines)]
  txt <- trimws(paste(lines, collapse = " "))
  if (!nzchar(txt)) {
    stop(ASK, " has no text in it (only comments). Put a sentence or paragraph there.",
         call. = FALSE)
  }
}
txt <- trimws(gsub("[[:space:]]+", " ", txt))

cb  <- load_codebook("config/codebook.yml")
cfg <- load_run_config("config/run.yml")

only <- getopt("--models")
if (!is.null(only)) {
  want <- trimws(strsplit(only, ",")[[1]])
  cfg$active_models <- Filter(function(m) m$id %in% want, cfg$models)
  if (!length(cfg$active_models)) stop("no model matches: ", only, call. = FALSE)
}
model_ids <- vapply(cfg$active_models, function(m) m$id, character(1))

# --- Build the units, exactly as the pipeline would ----------------------
sents <- if (has("--whole")) txt else split_sentences(txt)
if (!length(sents)) stop("nothing to rate", call. = FALSE)

units <- data.frame(
  unit_id = sprintf("ask_s%02d", seq_along(sents)),
  student_id = "ask", week = 0L, text = sents,
  question = NA_character_,
  context = if (length(sents) > 1L && !has("--whole")) txt else NA_character_,
  stringsAsFactors = FALSE)
units <- validate_units(units, "<ask>")

cat(sprintf("\ncodebook v%s | context_mode %s | %d model(s): %s\n",
            cb$version, cfg$context_mode, length(model_ids),
            paste(model_ids, collapse = ", ")))
cat(sprintf("%d sentence(s) from %d character(s) of input\n\n",
            nrow(units), nchar(txt)))

preflight(cfg, strict = FALSE)
ratings <- rate_units(units, cb, cfg, profile = "main")

# --- Report ---------------------------------------------------------------
short <- function(m) {
  m <- sub(":latest$", "", m); m <- sub("^DeepSeek.*", "DeepSeek", m)
  substr(m, 1, 14)
}
#' Make a model's prose printable in a C locale.
#'
#' Models write curly quotes, en dashes and ellipses. In a C locale R prints
#' those as "<U+2019>" escapes, which makes a rationale hard to read at exactly
#' the moment you are trying to read it. Transliterating to ASCII is display-only
#' and touches nothing that is stored.
plain <- function(x) {
  if (is.na(x)) return(x)
  out <- iconv(x, "UTF-8", "ASCII//TRANSLIT")
  if (is.na(out)) x else out
}

blank_na <- function(x, width = NULL) {
  x <- if (length(x) != 1L || is.na(x)) "" else as.character(x)
  if (!is.null(width)) substr(x, 1, width) else x
}
modal <- function(v) {
  v <- v[!is.na(v)]
  if (!length(v)) return(c(NA_character_, "0", "0"))
  tb <- sort(table(v), decreasing = TRUE)
  tied <- length(tb) > 1L && tb[1] == tb[2]
  c(if (tied) "TIE" else names(tb)[1], as.character(tb[1]), as.character(length(v)))
}

for (i in seq_len(nrow(units))) {
  u <- units$unit_id[i]
  cat(strrep("=", 78), "\n", sep = "")
  cat(sprintf("sentence %d of %d\n", i, nrow(units)))
  cat("  \"", units$text[i], "\"\n\n", sep = "")

  d <- ratings[ratings$unit_id == u, ]
  d <- d[match(model_ids, d$model), ]
  cat(sprintf("  %-15s %-15s %-6s %-6s %s\n",
              "model", "code", "item", "factor", "conf"))
  cat("  ", strrep("-", 60), "\n", sep = "")
  for (j in seq_len(nrow(d))) {
    cat(sprintf("  %-15s %-15s %-6s %-6s %s\n",
                short(d$model[j]),
                if (is.na(d$codes[j])) "-- FAILED --" else d$codes[j],
                blank_na(d$cq_item[j]),
                blank_na(d$cq_type[j], 6),
                if (is.na(d$conf_primary[j])) "" else sprintf("%.2f", d$conf_primary[j])))
  }
  mc <- modal(d$codes); mi <- modal(d$cq_item)
  cat("  ", strrep("-", 60), "\n", sep = "")
  cat(sprintf("  %-15s %-15s %s\n", "CONSENSUS",
              sprintf("%s (%s/%s)", mc[1], mc[2], mc[3]),
              if (is.na(mi[1])) "" else sprintf("item %s (%s/%s)", mi[1], mi[2], mi[3])))
  if (identical(mc[1], "TIE")) {
    cat("  ^ no majority. With an odd panel this only happens when a model fails\n")
    cat("    to answer, or when the models split evenly across three codes.\n")
  }

  if (!has("--quiet")) {
    cat("\n  why:\n")
    for (j in seq_len(nrow(d))) {
      rat <- plain(d$rationale_primary[j])
      if (!is.na(rat) && nzchar(rat)) {
        cat(sprintf("   %-14s %s\n", short(d$model[j]),
                    paste(strwrap(rat, width = 58, prefix = "", initial = ""),
                          collapse = "\n                  ")))
      }
      rq <- plain(d$rationale_cq[j])
      if (!is.na(rq) && nzchar(rq)) {
        cat(sprintf("   %-14s [item] %s\n", "",
                    paste(strwrap(rq, width = 52),
                          collapse = "\n                         ")))
      }
    }
  }
  cat("\n")
}

failed <- ratings[!ratings$ok, ]
if (nrow(failed)) {
  cat(sprintf("%d call(s) failed -- run again to retry (successes are cached):\n",
              nrow(failed)))
  print(unique(failed[, c("model", "unit_id", "error")]), row.names = FALSE)
}

cat(sprintf("%.0f s, %d cached. Nothing was written to results/.\n",
            sum(ratings$secs), sum(ratings$cached)))
if (is.null(getopt("--text"))) {
  cat(sprintf("Edit %s and run again to try another one.\n", ASK))
}
