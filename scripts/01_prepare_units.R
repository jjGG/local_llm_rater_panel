#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# 01_prepare_units.R -- build or validate the coding-unit table
#
# One text file per student -- the complete reflection journal. Two layouts are
# recognised, and which one a file uses is detected and reported.
#
# `sections` -- THE REAL DOCUMENTS. Week headings give the time dimension,
# bullet-marked lines are the journal's own questions and are never rated, and
# the text under each question is the student's answer:
#
#     Week 1
#     - What did you expect from the collaboration?
#     The answer. It may run over several sentences.
#
#     - How did the first joint session go?
#     Another answer.
#
#     Week 4
#     ...
#
# `marker` -- paragraphs prefixed with an inline week marker, which is what the
# invented example journals in this repository use:
#
#     w1.1: first paragraph of week one. It may hold several sentences.
#
#     w4.1: a paragraph from week four ...
#
# THE UNIT IS A SENTENCE. Every answer paragraph is split into sentences and each
# sentence becomes one unit with its own class label, carrying its week and the
# question it answers. The parent paragraph is kept as `context`, so a sentence
# that means nothing on its own can still be coded in situ. Pass --paragraphs for
# one unit per paragraph.
#
# Anything that is NOT rated -- week headings, the questions themselves, front
# matter before the first week heading -- is listed with its reason. Read that
# list: it is how you check the document was understood the way you meant it.
#
# Whole folder at once (file name becomes student_id, so name them 99.txt etc):
#   Rscript scripts/01_prepare_units.R --dir test_journal --out data/units_all.csv
#
# Single file:
#   Rscript scripts/01_prepare_units.R --txt test_journal/99.txt --student 99
#
# Validate a units table you built yourself:
#   Rscript scripts/01_prepare_units.R --units data/units_all.csv
#
# Options:
#   --format F           sections | marker | auto (default auto, and it says
#                        which one it chose)
#   --paragraphs         one unit per paragraph instead of per sentence. The
#                        2026-07-28 human pilot was coded this way.
#   --min-chars N        drop units shorter than N characters (default 0)
#   --week N             week for text with no heading/marker at all. In
#                        `sections` layout this also keeps front matter.
#   --detect-questions   `marker` layout only: treat bulleted lines and short
#                        "?" lines as the journal's own prompts rather than as
#                        units. OFF by default there, because a real paragraph
#                        ending in "?" would otherwise never be rated. In
#                        `sections` layout question detection is always on --
#                        that is what the layout means.
#   --preview            print each unit's id, week, length and opening words
#
# --preview echoes journal text to your terminal. Fine locally; never paste it
# anywhere else.
# ---------------------------------------------------------------------------

repo <- normalizePath(file.path(dirname(sub("^--file=", "",
        commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])), ".."))
setwd(repo)
source("R/codebook.R"); source("R/units.R")

a <- commandArgs(trailingOnly = TRUE)
getopt <- function(flag, default = NULL) {
  i <- match(flag, a); if (is.na(i) || i == length(a)) default else a[i + 1L]
}
has <- function(flag) flag %in% a

units_in  <- getopt("--units")
txt_in    <- getopt("--txt")
dir_in    <- getopt("--dir")
out_path  <- getopt("--out")
student   <- getopt("--student")
min_chars <- as.integer(getopt("--min-chars", "0"))
week_def  <- suppressWarnings(as.integer(getopt("--week", NA)))
detect_q  <- has("--detect-questions")
lvl       <- if (has("--paragraphs")) "paragraph" else "sentence"
fmt       <- getopt("--format", "auto")
if (!fmt %in% c("auto", "sections", "marker")) {
  stop("--format must be auto, sections or marker", call. = FALSE)
}

if (is.null(units_in) && is.null(txt_in) && is.null(dir_in)) {
  stop("give one of:\n",
       "  --dir <folder>            one .txt per student\n",
       "  --txt <file> --student ID  a single journal\n",
       "  --units <file.csv>        validate an existing units table",
       call. = FALSE)
}

#' Report what the splitter discarded. Silent drops on a curated file would
#' quietly shrink the dataset, so anything not turned into a unit is listed.
report_drops <- function(u, label) {
  dr <- attr(u, "dropped")
  ni <- attr(u, "n_inherited_week") %||% 0L
  if (length(dr)) {
    cat(sprintf("  %s: %d block(s) NOT turned into units:\n", label, length(dr)))
    for (d in dr) cat(sprintf("     [%s] %s\n", d$reason, substr(d$text, 1, 60)))
    cat("     ^ check this list. Week headings and the journal's own questions\n")
    cat("       belong here; a student's answer does not.\n")
  }
  if (ni > 0L) {
    cat(sprintf("  %s: %d paragraph(s) had no week marker and inherited the previous week\n",
                label, ni))
  }
  # A question recognised only because it ended in "?" may really be a student
  # sentence that has now been silently excluded from rating, so it is called out.
  nf <- attr(u, "n_fallback_questions") %||% 0L
  if (nf > 0L) {
    cat(sprintf("  %s: %d question(s) had NO bullet and were recognised only by the\n",
                label, nf))
    cat("     trailing '?'. Verify none of those is actually a student sentence.\n")
  }
  nq <- attr(u, "n_answers_without_question") %||% 0L
  if (nq > 0L) {
    cat(sprintf("  %s: %d answer paragraph(s) sit under no question at all\n", label, nq))
  }
  if (!isTRUE(attr(u, "stable_ids"))) {
    cat(sprintf("  %s: ids come from document order, so editing the text renumbers\n", label))
    cat("     the units after the edit. Freeze the documents before rating.\n")
  }
}

if (!is.null(dir_in)) {
  files <- list.files(dir_in, pattern = "\\.txt$", full.names = TRUE, ignore.case = TRUE)
  if (!length(files)) stop("no .txt files in ", dir_in, call. = FALSE)
  cat(sprintf("%d file(s) in %s\n", length(files), dir_in))

  parts <- lapply(files, function(f) {
    sid <- tools::file_path_sans_ext(basename(f))
    u <- read_journal(f, student_id = sid, format = fmt, min_chars = min_chars,
                      default_week = week_def, detect_questions = detect_q,
                      unit_level = lvl)
    cat(sprintf("  %-24s student_id=%-10s %3d unit(s)  [%s layout]\n",
                basename(f), sid, nrow(u), attr(u, "format")))
    report_drops(u, basename(f))
    u
  })
  units <- do.call(rbind, parts)
  if (anyDuplicated(units$unit_id)) {
    stop("duplicate unit_id across files -- are two files named the same student?",
         call. = FALSE)
  }
  if (is.null(out_path)) out_path <- "data/units_all.csv"
  utils::write.csv(units, out_path, row.names = FALSE, na = "")
  cat(sprintf("\nwrote %d unit(s) from %d student(s) to %s\n",
              nrow(units), length(files), out_path))

} else if (!is.null(txt_in)) {
  if (is.null(student)) student <- tools::file_path_sans_ext(basename(txt_in))
  units <- read_journal(txt_in, student_id = student, format = fmt,
                        min_chars = min_chars, default_week = week_def,
                        detect_questions = detect_q, unit_level = lvl)
  cat(sprintf("%s: %s layout\n", basename(txt_in), attr(units, "format")))
  report_drops(units, basename(txt_in))
  if (is.null(out_path)) out_path <- file.path("data", sprintf("units_%s.csv", student))
  utils::write.csv(units, out_path, row.names = FALSE, na = "")
  cat(sprintf("wrote %d unit(s) to %s\n", nrow(units), out_path))

} else {
  units <- read_units(units_in)
  cat(sprintf("%s validates: %d unit(s)\n", units_in, nrow(units)))
}

cfg <- load_run_config("config/run.yml")
check_context_available(units, cfg$context_mode)

cat("\nunits per student x week (the time dimension):\n")
print(table(student = units$student_id, week = units$week))
expected_weeks <- c(1L, 4L, 7L, 10L)
missing_weeks <- setdiff(expected_weeks, unique(units$week))
extra_weeks <- setdiff(unique(units$week), expected_weeks)
if (length(missing_weeks)) {
  cat(sprintf("NOTE: no units for week(s) %s. The journal has four sections\n",
              paste(missing_weeks, collapse = ", ")))
  cat("      (1, 4, 7, 10), so a missing one usually means a heading was not\n")
  cat("      recognised -- check the dropped list above.\n")
}
if (length(extra_weeks)) {
  cat(sprintf("NOTE: unexpected week value(s) %s -- probably a misread heading.\n",
              paste(sort(extra_weeks), collapse = ", ")))
}
if ("question_no" %in% names(units) && any(!is.na(units$question_no))) {
  cat("\nunits per week x question:\n")
  print(table(week = units$week, question = units$question_no))
  qs <- unique(units[, c("week", "question_no", "question")])
  qs <- qs[order(qs$week, qs$question_no), ]
  cat("\nquestions found (these are NOT rated; each groups the sentences under it):\n")
  for (i in seq_len(nrow(qs))) {
    cat(sprintf("  w%-3s q%-2s %s\n", qs$week[i], qs$question_no[i],
                substr(qs$question[i] %||% "(none)", 1, 66)))
  }
}
cat(sprintf("\ntext length (chars): min %d, median %d, max %d\n",
            min(nchar(units$text)), as.integer(stats::median(nchar(units$text))),
            max(nchar(units$text))))
if ("n_sent_in_para" %in% names(units) && any(!is.na(units$n_sent_in_para))) {
  cat(sprintf("\n%d sentence unit(s) from %d paragraph(s), %.1f per paragraph\n",
              nrow(units), length(unique(units$para_id)),
              nrow(units) / length(unique(units$para_id))))
  # A "sentence" far too long for one is the signature of a missing full stop;
  # far too short, of an abbreviation the splitter does not know. Both are
  # segmentation bugs and both are fixable only before rating starts.
  susp <- units[nchar(units$text) > 400L | nchar(units$text) < 15L, ]
  if (nrow(susp)) {
    cat(sprintf("%d sentence(s) look like a segmentation problem (>400 or <15 chars).\n",
                nrow(susp)))
    cat("Check these before rating -- re-splitting later changes every unit_id:\n")
    for (i in seq_len(min(12L, nrow(susp)))) {
      cat(sprintf("  %-24s %4dc  %s\n", susp$unit_id[i], nchar(susp$text[i]),
                  substr(susp$text[i], 1, 50)))
    }
    if (nrow(susp) > 12L) cat(sprintf("  ... and %d more\n", nrow(susp) - 12L))
    cat("A too-long one usually means a missing full stop; a too-short one, an\n")
    cat("abbreviation missing from SENTENCE_ABBREV in R/units.R.\n")
  }
} else {
  long <- units[nchar(units$text) > 800L, ]
  if (nrow(long)) {
    cat(sprintf("%d paragraph unit(s) over 800 chars, which often hold several\n",
                nrow(long)))
    cat("  distinct statements:", paste(long$unit_id, collapse = ", "), "\n")
  }
}
cat(sprintf("context_mode '%s' -> %s\n", cfg$context_mode,
            if (cfg$context_mode == "unit") "passage only, no extra context sent"
            else paste("will also send:", cfg$context_mode)))

if (has("--preview")) {
  cat("\n-- preview (local terminal only) --\n")
  for (i in seq_len(nrow(units))) {
    cat(sprintf("%-18s w%-3s %4dc  %s...\n", units$unit_id[i], units$week[i],
                nchar(units$text[i]), substr(units$text[i], 1, 55)))
  }
} else {
  cat("\n(use --preview to eyeball the segmentation)\n")
}
