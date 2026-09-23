#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# 06_show_units.R -- per-paragraph codes from every model, side by side
#
#   Rscript scripts/06_show_units.R                       # infers the label
#   Rscript scripts/06_show_units.R --label synth5
#   Rscript scripts/06_show_units.R --label synth5 --text  # show the passages
#   Rscript scripts/06_show_units.R --label synth5 --only-disagreed
#
# Prints one block per student and writes the same table to
# results/per_unit_codes_<label>_<profile>.{md,csv}.
#
# --text echoes passage text. Safe for the synthetic journals; for real
# journals that is student personal data, so keep it in your own terminal.
# ---------------------------------------------------------------------------

repo <- normalizePath(file.path(dirname(sub("^--file=", "",
        commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])), ".."))
setwd(repo)
source("R/codebook.R")

a <- commandArgs(trailingOnly = TRUE)
getopt <- function(f, d = NULL) { i <- match(f, a); if (is.na(i) || i == length(a)) d else a[i + 1L] }
has <- function(f) f %in% a

profile <- getopt("--profile", "main")
label   <- getopt("--label")
units_f <- getopt("--units")

cb  <- load_codebook("config/codebook.yml")
cfg <- load_run_config("config/run.yml")
out_dir <- cfg$paths$results

avail <- list.files(out_dir, pattern = sprintf("^ratings_.*_%s\\.csv$", profile))
if (is.null(label)) {
  if (length(avail) == 1L) {
    label <- sub(sprintf("^ratings_(.*)_%s\\.csv$", profile), "\\1", avail)
  } else {
    stop(if (length(avail)) paste0("pass --label <name>, one of: ",
           paste(sub(sprintf("^ratings_(.*)_%s\\.csv$", profile), "\\1", avail),
                 collapse = ", "))
         else "no ratings found -- run scripts/02_rate.R first", call. = FALSE)
  }
}
stem <- sprintf("%s_%s", label, profile)
r <- utils::read.csv(file.path(out_dir, sprintf("ratings_%s.csv", stem)),
                     stringsAsFactors = FALSE, na.strings = c("NA", ""))

# Passage text lives in the units file, never in results/. Locate it only if
# asked for, so the default output carries no journal text at all.
txt <- NULL
if (has("--text")) {
  cand <- c(units_f, file.path("data", sprintf("units_%s.csv", label)))
  cand <- cand[!vapply(cand, is.null, logical(1))]
  hit <- cand[file.exists(unlist(cand))]
  if (length(hit)) {
    u <- utils::read.csv(hit[1], stringsAsFactors = FALSE)
    txt <- setNames(u$text, u$unit_id)
  } else {
    message("--text: no units file found (tried ", paste(cand, collapse = ", "), ")")
  }
}

# Compact single letters, so six models fit on one line.
ABBR <- c("Positive" = "P", "Negative" = "N", "Self awareness" = "S", "Not marked" = "-")
CQ_ABBR <- c("Metacognitive" = "Meta", "Cognitive" = "Cog",
             "Motivational" = "Motiv", "Behavioural" = "Behav")
short_codes <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return("??")
    paste(ABBR[trimws(strsplit(v, "|", fixed = TRUE)[[1]])], collapse = "+")
  }, character(1), USE.NAMES = FALSE)
}
short_model <- function(m) {
  m <- sub(":latest$", "", m)
  m <- sub("^DeepSeek.*", "deepsk", m)
  m <- sub("^gpt-oss.*", "gptoss", m)
  m <- sub("^qwen3\\.6.*", "qwen36", m)
  m <- sub("^qwen3:14b$", "qwen14", m)
  m <- sub("^gemma4.*", "gemma", m)
  m <- sub("^llama3\\.1.*", "llama", m)
  substr(m, 1, 6)
}

r$code_short <- short_codes(r$codes)
r$mshort <- short_model(r$model)
models <- unique(r$mshort[order(r$model)])
units <- unique(r$unit_id)

wide <- matrix("", length(units), length(models), dimnames = list(units, models))
cqw  <- matrix("", length(units), length(models), dimnames = list(units, models))
has_item <- "cq_item" %in% names(r) && any(!is.na(r$cq_item))
for (i in seq_len(nrow(r))) {
  wide[r$unit_id[i], r$mshort[i]] <- r$code_short[i]
  # Show the item code when there is one: it is both shorter than the factor
  # name and strictly more informative, since the factor follows from it.
  if (has_item && !is.na(r$cq_item[i])) {
    cqw[r$unit_id[i], r$mshort[i]] <- r$cq_item[i]
  } else if (!is.na(r$cq_type[i])) {
    cqw[r$unit_id[i], r$mshort[i]] <- CQ_ABBR[[r$cq_type[i]]]
  }
}

# Consensus = modal code set; ties reported as "tie".
consensus <- apply(wide, 1L, function(v) {
  v <- v[nzchar(v)]
  if (!length(v)) return("")
  tb <- sort(table(v), decreasing = TRUE)
  if (length(tb) > 1L && tb[1] == tb[2]) sprintf("tie") else
    sprintf("%s %d/%d", names(tb)[1], tb[1], length(v))
})
n_distinct <- apply(wide, 1L, function(v) length(unique(v[nzchar(v)])))

meta <- unique(r[, c("unit_id", "student_id", "week")])
meta <- meta[match(units, meta$unit_id), ]
keep <- if (has("--only-disagreed")) n_distinct > 1L else rep(TRUE, length(units))

md <- c(sprintf("# Per-unit codes -- %s (profile %s)", label, profile), "",
        sprintf("P = Positive, N = Negative, S = Self awareness, - = Not marked. `+` joins a multi-label set."),
        sprintf("%s shown only where a model assigned one (fires on: %s).",
                if (has_item) "CQ item (factor follows from it)" else "CQ factor",
                paste(cb$cq_applies_to, collapse = ", ")), "")

hdr <- sprintf("%-14s %s  %-12s %s", "unit", paste(sprintf("%-7s", models), collapse = ""),
               "consensus", "cq (per model)")
for (s in unique(meta$student_id)) {
  idx <- which(meta$student_id == s & keep)
  if (!length(idx)) next
  cat(sprintf("\n=== %s ===\n", s))
  cat(hdr, "\n")
  cat(strrep("-", nchar(hdr)), "\n")
  md <- c(md, sprintf("## %s", s), "",
          paste0("| unit | ", paste(models, collapse = " | "), " | consensus | CQ |"),
          paste0("|", strrep("---|", length(models) + 3L)))
  for (i in idx) {
    uid <- units[i]
    cqs <- cqw[i, ]; cqtxt <- if (any(nzchar(cqs))) paste(cqs[nzchar(cqs)], collapse = "/") else ""
    flag <- if (n_distinct[i] > 1L) "*" else " "
    cat(sprintf("%-14s %s  %-12s %s%s\n", uid,
                paste(sprintf("%-7s", wide[i, ]), collapse = ""),
                consensus[i], cqtxt, flag))
    if (!is.null(txt) && uid %in% names(txt)) {
      cat(sprintf("               \"%s\"\n", substr(txt[[uid]], 1, 96)))
    }
    md <- c(md, paste0("| ", uid, " | ", paste(wide[i, ], collapse = " | "), " | ",
                       consensus[i], " | ", cqtxt, " |"))
  }
  md <- c(md, "")
}

cat(sprintf("\n%d of %d unit(s) had at least one model disagree (marked *)\n",
            sum(n_distinct > 1L), length(units)))
cat(sprintf("all six identical on %d unit(s)\n", sum(n_distinct == 1L)))

f_md <- file.path(out_dir, sprintf("per_unit_codes_%s.md", stem))
writeLines(md, f_md)
tab <- data.frame(unit_id = units, student_id = meta$student_id, week = meta$week,
                  wide, consensus = consensus, n_distinct = n_distinct,
                  check.names = FALSE, stringsAsFactors = FALSE)
f_csv <- file.path(out_dir, sprintf("per_unit_codes_%s.csv", stem))
utils::write.csv(tab, f_csv, row.names = FALSE, na = "")
cat(sprintf("wrote %s\n      %s\n", f_md, f_csv))
