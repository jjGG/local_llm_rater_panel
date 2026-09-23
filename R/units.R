# ---------------------------------------------------------------------------
# units.R -- read, validate and (optionally) build the coding-unit table
#
# A "unit" is one stretch of text that gets exactly one primary code. Units are
# fixed in advance, mechanically; no model ever decides what a unit is.
#
# SINCE 2026-08-20 THE UNIT IS A SENTENCE. Complete journals come in and every
# sentence becomes one unit, carrying its own class label. Paragraph-level units
# are still supported (`unit_level = "paragraph"`) because the 2026-07-28 human
# pilot was coded that way.
#
# Canonical units file columns
#   unit_id       optional; generated if absent (see below)
#   student_id    required, pseudonymous (e.g. 99) -- never a name
#   week          required, integer (1, 4, 7, 10) -- the time dimension
#   text          required, the sentence (or passage) to code
#   question      optional, the journal's own question this answers
#                 (required for context_mode = unit_question*)
#   question_no   optional, that question's position within the week
#   context       optional, surrounding text -- for sentence units this is the
#                 whole parent paragraph (required for context_mode = *context)
#   para_id       optional, which paragraph the sentence came from
#   sent_in_para  optional, the sentence's position inside that paragraph
#
# UNIT IDS -- three shapes, deliberately distinguishable, so a run at one
# granularity can never be silently joined to labels at another:
#   sentence, sections layout   <student>_w<week>_q<qq>_s<ss>
#   sentence, marker layout     <student>_w<week>_p<pp>_s<ss>
#   paragraph                   <student>_w<week>_u<nn>  (the human pilot's shape)
#
# ALIGNMENT WARNING: the human alignment sheet has no ids of its own -- ids come
# from row order (test_for_consistency/R/io.R). Keep rows in the sheet's order
# and never re-sort. The pilot sheet is PARAGRAPH-level, so comparing it with a
# sentence-level run requires either re-coding the humans by sentence or
# aggregating sentences back up to their paragraph via `para_id`.
# ---------------------------------------------------------------------------

REQUIRED_UNIT_COLS <- c("student_id", "week", "text")

# Optional columns that are carried through if present, so provenance from the
# sentence splitter is not lost on the way to the ratings table.
OPTIONAL_UNIT_COLS <- c("question", "question_no", "context", "para_id",
                        "sent_in_para", "n_sent_in_para")

#' Read a units table from .csv / .tsv (or .xlsx when readxl is installed)
read_units <- function(path) {
  if (!file.exists(path)) stop("units file not found: ", path, call. = FALSE)
  ext <- tolower(tools::file_ext(path))

  d <- switch(ext,
    csv = read_delim_bom(path, ","),
    tsv = read_delim_bom(path, "\t"),
    txt = read_delim_bom(path, "\t"),
    xlsx = {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("reading .xlsx needs readxl: install.packages('readxl')", call. = FALSE)
      }
      as.data.frame(readxl::read_excel(path), stringsAsFactors = FALSE)
    },
    stop("unsupported units file type: .", ext, call. = FALSE))

  validate_units(d, path)
}

#' read.csv that survives the UTF-8 BOM Excel writes (see test_for_consistency)
read_delim_bom <- function(path, sep) {
  con <- file(path, "rb"); on.exit(close(con), add = TRUE)
  enc <- if (identical(readBin(con, "raw", 3L), as.raw(c(0xEF, 0xBB, 0xBF)))) {
    "UTF-8-BOM"
  } else {
    "UTF-8"
  }
  d <- utils::read.delim(path, sep = sep, stringsAsFactors = FALSE,
                         quote = "\"", colClasses = "character",
                         fileEncoding = enc, na.strings = c("NA", "", " "))
  names(d) <- trimws(sub("^X\\.+", "", names(d)))
  d
}

validate_units <- function(d, source = "<data.frame>") {
  missing <- setdiff(REQUIRED_UNIT_COLS, names(d))
  if (length(missing)) {
    stop("units file ", source, " is missing column(s): ",
         paste(missing, collapse = ", "),
         "\n  found: ", paste(names(d), collapse = ", "),
         "\n  see data/units_TEMPLATE.csv", call. = FALSE)
  }

  d$text <- trimws(d$text)
  empty <- which(is.na(d$text) | !nzchar(d$text))
  if (length(empty)) {
    stop("empty text in row(s): ", paste(empty, collapse = ", "), call. = FALSE)
  }

  d$week <- suppressWarnings(as.integer(d$week))
  if (anyNA(d$week)) {
    stop("non-integer week in row(s): ", paste(which(is.na(d$week)), collapse = ", "),
         call. = FALSE)
  }
  d$student_id <- trimws(as.character(d$student_id))
  if (anyNA(d$student_id) || any(!nzchar(d$student_id))) {
    stop("missing student_id", call. = FALSE)
  }

  # Generate ids the same way test_for_consistency/R/io.R does, so the LLM
  # ratings and the human ratings join on unit_id without further mapping.
  if (!"unit_id" %in% names(d) || all(is.na(d$unit_id))) {
    grp <- paste(d$student_id, d$week, sep = "_w")
    n_in <- stats::ave(seq_along(grp), grp, FUN = seq_along)
    d$unit_id <- sprintf("%s_w%02d_u%02d", d$student_id, d$week, n_in)
    message("generated unit_id from row order within (student_id, week)")
  }
  if (anyDuplicated(d$unit_id)) {
    stop("duplicate unit_id: ",
         paste(unique(d$unit_id[duplicated(d$unit_id)]), collapse = ", "), call. = FALSE)
  }

  # Fingerprint the text so a silently edited passage cannot masquerade as a
  # cache hit later on.
  d$text_sha1 <- vapply(d$text, function(x) digest::digest(x, algo = "sha1"), character(1))

  for (opt in c("question", "context")) {
    if (!opt %in% names(d)) d[[opt]] <- NA_character_
  }

  keep <- c("unit_id", "student_id", "week", "text", "text_sha1",
            intersect(OPTIONAL_UNIT_COLS, names(d)))
  d[, unique(keep), drop = FALSE]
}

#' Check the units table carries what the chosen context_mode needs
check_context_available <- function(units, mode) {
  need <- switch(mode, unit = character(0), unit_context = "context",
                 unit_question = "question",
                 unit_question_context = c("question", "context"))
  for (col in need) {
    n_missing <- sum(is.na(units[[col]]) | !nzchar(trimws(units[[col]] %||% "")))
    if (n_missing == nrow(units)) {
      stop("context_mode='", mode, "' needs a populated '", col, "' column", call. = FALSE)
    }
    if (n_missing > 0L) {
      # A one-sentence paragraph has no context to add beyond the sentence
      # itself, so for sentence units this is expected rather than a problem.
      one_sentence <- identical(col, "context") &&
        "n_sent_in_para" %in% names(units) &&
        all(units$n_sent_in_para[is.na(units$context)] == 1L, na.rm = TRUE)
      if (one_sentence) {
        message(sprintf("context_mode='%s': %d of %d units are single-sentence paragraphs, so they carry no separate context",
                        mode, n_missing, nrow(units)))
      } else {
        warning(sprintf("context_mode='%s': '%s' is empty for %d of %d units; those units
  will be sent with less context than the rest", mode, col, n_missing, nrow(units)),
                call. = FALSE)
      }
    }
  }
  invisible(TRUE)
}


# --- Deterministic sentence segmentation ---------------------------------

#' Tokens whose trailing full stop does NOT end a sentence.
#'
#' Deliberately a fixed, visible list rather than a language model or an
#' external tokeniser: the unit list must be reproducible from this repository
#' alone, and reviewable by the raters. Add to it when a journal proves it
#' incomplete -- and re-check the affected ids, since splitting differently
#' changes them.
SENTENCE_ABBREV <- c(
  "e.g", "i.e", "etc", "cf", "vs", "viz", "al", "ca", "approx", "incl", "excl",
  "resp", "eq", "fig", "no", "nr", "pp", "vol", "ch", "sec", "min", "max",
  "dr", "prof", "mr", "mrs", "ms", "st", "jr", "sr",
  "a.m", "p.m", "u.s", "u.k", "e.u", "ph.d", "b.sc", "m.sc", "b.a", "m.a")

#' Split one paragraph into sentences.
#'
#' Purely mechanical and deterministic. Abbreviations, decimal numbers, initials
#' and ellipses are shielded first, then a boundary is taken only where terminal
#' punctuation is followed by whitespace and something that can start a sentence.
#'
#' Known limits, worth stating in the methods section rather than hiding:
#'   - a missing full stop between two sentences leaves them as one unit
#'   - a list written with semicolons stays one unit
#'   - an unlisted abbreviation splits mid-sentence
#' All three are visible in the units file, which is why that file is reviewed
#' before rating rather than after.
split_sentences <- function(text, abbrev = SENTENCE_ABBREV) {
  if (is.na(text)) return(character(0))
  t <- trimws(gsub("[[:space:]]+", " ", text))
  if (!nzchar(t)) return(character(0))

  DOT <- "\x01"   # stands in for a full stop that must not end a sentence

  # Ellipsis first, so its dots are not mistaken for three boundaries.
  t <- gsub("\\.\\.\\.", paste0(rep(DOT, 3L), collapse = ""), t)
  # Decimals and version-like numbers: 1.5, 10.4.2
  t <- gsub("([0-9])\\.([0-9])", paste0("\\1", DOT, "\\2"), t)
  # Single-letter initials: "J. Grossmann"
  t <- gsub("\\b([A-Za-z])\\.", paste0("\\1", DOT), t)
  # Listed abbreviations, longest first so "ph.d" wins over "d". The match is
  # kept via a backreference and only the trailing dot is replaced -- writing
  # the abbreviation back literally would lowercase "Dr." to "dr.", and the
  # lost capital would then also defeat the sentence-start lookahead.
  # Any interior dots have already been shielded by the initials rule above,
  # hence the [. DOT] class.
  for (ab in abbrev[order(-nchar(abbrev))]) {
    pat <- paste0("(?i)\\b(", gsub(".", paste0("[.", DOT, "]"), ab, fixed = TRUE), ")\\.")
    t <- gsub(pat, paste0("\\1", DOT), t, perl = TRUE)
  }

  # A boundary is terminal punctuation (optionally followed by a closing quote
  # or bracket), then whitespace, then something that can open a sentence. The
  # lookbehind uses two fixed-length alternatives, which PCRE allows.
  parts <- unlist(strsplit(
    t, "(?<=[.!?]|[.!?][\"')”’\\]])\\s+(?=[\"'(“‘\\[]?[A-Z0-9])",
    perl = TRUE), use.names = FALSE)

  parts <- trimws(gsub(DOT, ".", parts, fixed = TRUE))
  parts[nzchar(parts)]
}


# --- The journal document: week sections, bullet questions, answers --------

# Characters Word leaves behind where a bullet used to be, as UNICODE CODE
# POINTS rather than literals.
#
# This is not fussiness. A multibyte regex class like [•●] fails outright in
# some locales, and even a plain string comparison against a literal "•" fails
# when the source file's encoding and the session's locale disagree -- which is
# exactly what happened here: every bullet question was missed and fell through
# to the weaker "ends in a question mark" test. Code points are immune to both.
#
#   2022 bullet  25CF black circle  25AA black small square  2023 triangular
#   2043 hyphen bullet  25E6 white bullet  2027 hyphenation point
#   F0B7 Word's Symbol-font bullet, which survives .docx conversion as a
#        private-use character
BULLET_UNAMBIGUOUS <- c(0x2022, 0x25CF, 0x25AA, 0x2023, 0x2043, 0x25E6,
                        0x2027, 0xF0B7)

# These are also used as bullets, but each of them can legitimately START a
# sentence, so they only count as a bullet when whitespace follows.
#
# This distinction is not pedantry. Word uses "o" for second-level bullets, so
# with a naive test an answer beginning "Overall, I think ..." is classified as a
# journal question and dropped from rating -- silent data loss, in the direction
# that is hardest to notice. Same for "-5 degrees" and "*emphasis*".
#   00B7 middle dot  2212 minus  2013 en dash  2014 em dash
BULLET_IF_SPACED <- c(0x00B7, 0x2212, 0x2013, 0x2014,
                      utf8ToInt("*"), utf8ToInt("-"), utf8ToInt("+"),
                      utf8ToInt("o"), utf8ToInt("O"))

BULLET_CODEPOINTS <- c(BULLET_UNAMBIGUOUS, BULLET_IF_SPACED)

#' A line as a vector of Unicode code points.
#'
#' Everything below inspects lines through this, never through substr(). This
#' session's locale is "C", where substr() counts BYTES: substr(x, 1, 1) on a
#' line starting with "•" returns a third of a character, and any comparison
#' against it silently fails. utf8ToInt() on the whole string is correct whether
#' or not the string is marked UTF-8, so it is the only safe entry point.
line_codepoints <- function(s) {
  s <- trimws(s)
  if (!nchar(s)) return(integer(0))
  tryCatch(utf8ToInt(s), error = function(e) integer(0),
           warning = function(w) integer(0))
}

SPACE_CODEPOINTS <- c(0x20, 0x09, 0xA0, 0x2007, 0x202F)   # space, tab, nbsp, figure, narrow

#' Does this line start with a bullet character?
#'
#' An unambiguous bullet symbol counts on its own; a character that could also
#' open a sentence counts only when whitespace follows it.
starts_with_bullet <- function(line) {
  cp <- line_codepoints(line)
  if (!length(cp)) return(FALSE)
  if (cp[1] %in% BULLET_UNAMBIGUOUS) return(TRUE)
  cp[1] %in% BULLET_IF_SPACED && length(cp) >= 2L && cp[2] %in% SPACE_CODEPOINTS
}

# A section heading naming the week: "Week 1", "WEEK 04 - getting started",
# "Reflection week 7". Kept short so a sentence merely mentioning week 4 in
# passing cannot be mistaken for a heading.
#
# Every pattern here that uses a non-capturing group is matched with perl = TRUE.
# R's default (TRE) engine accepts "(?:...)" in grepl but mis-numbers the groups
# in sub(), which silently returns the wrong text -- so the engine is not
# optional, it is part of the pattern's correctness.
WEEK_HEADING_RE <- "^[[:space:]]*(?:reflection[[:space:]]*)?(?:week|w)[[:space:]]*0?([0-9]{1,2})\\b"
QUESTION_NUM_RE <- "^(?:[0-9]{1,2}[.)]|[Qq][0-9]{1,2}[.):])"

#' Is this line a section heading that names the week?
is_week_heading <- function(line) {
  l <- trimws(line)
  nchar(l) <= 60L &&
    grepl(WEEK_HEADING_RE, l, ignore.case = TRUE, perl = TRUE) &&
    # A heading is a heading, not a sentence that happens to start with "Week 4".
    !grepl("[.!?][\"')]?$", l)
}

#' The week number from a heading line. Takes the first run of digits rather
#' than a back-reference, so it cannot depend on group numbering.
week_of_heading <- function(line) {
  suppressWarnings(as.integer(sub("^[^0-9]*([0-9]{1,2}).*$", "\\1", trimws(line))))
}

#' Is this line one of the journal's own bullet-marked questions?
#'
#' The bullet is the primary signal, because that is how the questions are
#' marked in the documents. A short line ending in "?" is accepted as a fallback
#' and counted separately, so a question that lost its bullet in the conversion
#' from Word is still recognised and the fallback's use is visible rather than
#' silent.
is_question_line <- function(line) {
  l <- trimws(line)
  if (!nchar(l)) return(FALSE)
  if (starts_with_bullet(l)) return(TRUE)
  # "1." / "1)" / "Q2:" numbering
  if (grepl(paste0(QUESTION_NUM_RE, "[[:space:]]"), l, perl = TRUE)) return(TRUE)
  nchar(l) <= 300L && grepl("\\?[[:space:]]*$", l)
}

#' Was this line marked as a question, rather than merely ending in "?"
has_question_marker <- function(line) {
  starts_with_bullet(line) || grepl(QUESTION_NUM_RE, trimws(line), perl = TRUE)
}

#' Strip the bullet or numbering from a question line.
#'
#' Drops the first CODE POINT, not the first byte -- substr() here would cut a
#' multibyte bullet in half and leave the remaining bytes glued to the text.
strip_bullet <- function(line) {
  l <- trimws(line)
  while (starts_with_bullet(l)) {
    cp <- line_codepoints(l)
    if (length(cp) < 2L) return("")
    l <- trimws(intToUtf8(cp[-1]))
  }
  trimws(sub(paste0(QUESTION_NUM_RE, "[[:space:]]*"), "", l, perl = TRUE))
}

#' Read a full reflection journal document into units.
#'
#' The document layout, one file per student:
#'
#'     Week 1
#'     - What did you expect from the collaboration?
#'     The student's answer. It may run over several sentences.
#'
#'     - How did the first meeting go?
#'     Another answer.
#'
#'     Week 4
#'     - ...
#'
#' Week headings set the time dimension, bullet-marked lines are the journal's
#' OWN questions and are never rated, and everything else is answer text. Each
#' answer paragraph is split into sentences and each sentence becomes one unit,
#' carrying its week and the question it answers.
#'
#' Processing is LINE-based rather than block-based on purpose: in these
#' documents a question and its answer are often on consecutive lines with no
#' blank line between them, which a block-based reader would merge into one unit.
#'
#' IDS COME FROM ORDER. `<student>_w<week>_q<qq>_s<ss>`, the sentence index
#' running within the question. There are no explicit numbers in these documents
#' to anchor them, so editing an answer renumbers the sentences after it in that
#' question -- which invalidates their cache entries and breaks any join. Freeze
#' the documents before rating.
journal_to_units <- function(path, student_id, min_chars = 0L,
                             default_week = NA_integer_,
                             unit_level = c("sentence", "paragraph")) {
  unit_level <- match.arg(unit_level)
  if (!file.exists(path)) stop("journal file not found: ", path, call. = FALSE)
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")

  week <- default_week
  question <- NA_character_
  q_no <- 0L
  p_in_q <- 0L
  buf <- character(0)             # the answer paragraph being accumulated
  out <- list()
  dropped <- list()
  n_fallback_q <- 0L
  n_no_question <- 0L

  # Emit whatever is in the paragraph buffer as units.
  #
  # Anything before the first week heading is FRONT MATTER -- a title, the
  # student's name, a course code -- and is dropped rather than rated. Passing
  # default_week says "there are no headings, it is all this week", and then it
  # is kept.
  flush <- function() {
    if (!length(buf)) return(invisible(NULL))
    para <- trimws(gsub("[[:space:]]+", " ", paste(buf, collapse = " ")))
    buf <<- character(0)
    if (!nchar(para)) return(invisible(NULL))
    if (is.na(week)) {
      dropped[[length(dropped) + 1L]] <<-
        list(reason = "front matter, before the first week heading", text = para)
      return(invisible(NULL))
    }
    p_in_q <<- p_in_q + 1L
    if (is.na(question)) n_no_question <<- n_no_question + 1L
    out[[length(out) + 1L]] <<- list(week = week, q_no = q_no, p_no = p_in_q,
                                     question = question, para = para)
    invisible(NULL)
  }

  for (ln in lines) {
    if (!nzchar(trimws(ln))) { flush(); next }

    if (is_week_heading(ln)) {
      flush()
      week <- week_of_heading(ln)
      q_no <- 0L; p_in_q <- 0L; question <- NA_character_
      dropped[[length(dropped) + 1L]] <-
        list(reason = sprintf("week %s heading", week), text = trimws(ln))
      next
    }

    if (is_question_line(ln)) {
      flush()
      if (!has_question_marker(ln)) n_fallback_q <- n_fallback_q + 1L
      question <- strip_bullet(ln)
      q_no <- q_no + 1L
      p_in_q <- 0L
      dropped[[length(dropped) + 1L]] <-
        list(reason = "journal question", text = question)
      next
    }

    buf <- c(buf, trimws(ln))
  }
  flush()

  if (!length(out)) {
    stop("no answer text found in ", path,
         "\n  Expected week headings ('Week 1'), bullet-marked questions and",
         "\n  answer text. If the file instead uses inline 'w1.1:' markers, use",
         "\n  --format marker.", call. = FALSE)
  }

  n_front <- sum(vapply(dropped, function(x)
    grepl("^front matter", x$reason), logical(1)))
  if (n_front > 0L) {
    message(sprintf("%s: %d paragraph(s) before the first week heading treated as front matter and NOT rated (pass --week N to keep them)",
                    basename(path), n_front))
  }

  rows <- list()
  for (o in out) {
    pid <- sprintf("%s_w%02d_q%02d_p%02d", student_id, o$week, o$q_no, o$p_no)
    if (identical(unit_level, "paragraph")) {
      rows[[length(rows) + 1L]] <- data.frame(
        unit_id = pid, student_id = student_id, week = o$week, text = o$para,
        question = o$question, question_no = o$q_no, context = NA_character_,
        para_id = pid, sent_in_para = NA_integer_, n_sent_in_para = NA_integer_,
        stringsAsFactors = FALSE)
      next
    }
    sents <- split_sentences(o$para)
    keep <- nchar(sents) >= min_chars
    if (any(!keep)) {
      for (s in sents[!keep]) {
        dropped[[length(dropped) + 1L]] <-
          list(reason = sprintf("sentence shorter than min_chars (%d)", min_chars),
               text = s)
      }
      sents <- sents[keep]
    }
    if (!length(sents)) next
    rows[[length(rows) + 1L]] <- data.frame(
      unit_id = NA_character_, student_id = student_id, week = o$week,
      text = sents, question = o$question, question_no = o$q_no,
      context = if (length(sents) > 1L) o$para else NA_character_,
      para_id = pid, sent_in_para = seq_along(sents),
      n_sent_in_para = length(sents), stringsAsFactors = FALSE)
  }
  if (!length(rows)) stop("no units left in ", path, call. = FALSE)
  d <- do.call(rbind, rows)

  # Sentence ids number within (week, question), across that question's
  # paragraphs, so the id says which question the sentence answers.
  if (identical(unit_level, "sentence")) {
    grp <- sprintf("%s_w%02d_q%02d", d$student_id, d$week, d$question_no)
    s_no <- stats::ave(seq_len(nrow(d)), grp, FUN = seq_along)
    d$unit_id <- sprintf("%s_s%02d", grp, s_no)
  }

  n_para <- length(unique(d$para_id))
  message(sprintf("%s: %d question(s), %d answer paragraph(s) -> %d %s unit(s)",
                  basename(path), length(unique(paste(d$week, d$question_no))),
                  n_para, nrow(d), unit_level))

  d <- validate_units(d, path)
  attr(d, "dropped") <- dropped
  attr(d, "n_inherited_week") <- 0L
  # Ids are order-derived in this format: the documents carry no explicit
  # numbering to anchor them to.
  attr(d, "stable_ids") <- FALSE
  attr(d, "unit_level") <- unit_level
  attr(d, "n_fallback_questions") <- n_fallback_q
  attr(d, "n_answers_without_question") <- n_no_question
  d
}

#' Which of the two journal layouts is this file?
#'
#' `marker`   paragraphs prefixed with an inline week marker (w1.1: ...) --
#'            the format the invented example journals in this repository use
#' `sections` week headings plus bullet-marked questions -- the real documents
detect_journal_format <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) stop("empty journal file: ", path, call. = FALSE)

  marker_re <- "^[wW](?:eek)?[[:space:]]*[0-9]{1,2}(?:\\.[0-9]{1,3})?[[:space:]]*[:.)-]"
  n_marker <- sum(grepl(marker_re, trimws(lines), perl = TRUE))
  n_heading <- sum(vapply(lines, is_week_heading, logical(1)))
  n_bullet <- sum(vapply(lines, is_question_line, logical(1)))

  # A "w1:" marker line is also a short line starting with "week", so the
  # marker test has to win when both match.
  if (n_marker >= max(1L, n_heading)) "marker"
  else if (n_heading || n_bullet) "sections"
  else stop("cannot tell the layout of ", path,
            "\n  Found no inline 'w1.1:' markers and no 'Week N' headings.",
            "\n  Pass --format marker or --format sections to force one.",
            call. = FALSE)
}

#' Read one journal file in whichever of the two layouts it uses.
#'
#' Arguments are named explicitly rather than passed through `...`, because the
#' two readers do not take the same options and a silently swallowed argument
#' here would mean a units file built with settings nobody chose.
read_journal <- function(path, student_id, format = "auto", min_chars = 0L,
                         default_week = NA_integer_, unit_level = "sentence",
                         detect_questions = FALSE) {
  fmt <- if (identical(format, "auto")) detect_journal_format(path) else format
  u <- switch(fmt,
    marker = txt_to_units(path, student_id = student_id, min_chars = min_chars,
                          default_week = default_week,
                          detect_questions = detect_questions,
                          unit_level = unit_level),
    sections = journal_to_units(path, student_id = student_id,
                               min_chars = min_chars, default_week = default_week,
                               unit_level = unit_level),
    stop("unknown journal format: ", fmt,
         "\n  expected 'marker', 'sections' or 'auto'", call. = FALSE))
  attr(u, "format") <- fmt
  u
}


# --- Deterministic segmentation of a plain-text journal -------------------

#' Split a journal .txt into units on blank lines
#'
#' Purely mechanical: one blank-line-separated paragraph becomes one unit, in
#' file order. No model is involved, so the unit list is reproducible and
#' reviewable -- which is the point of fixing units before rating.
#'
#' Intended input, one file per student:
#'
#'     w1: first paragraph to be rated ...
#'
#'     w1: second paragraph ...
#'
#'     w4: a paragraph from week four ...
#'
#' The leading week marker accepts `w1:`, `W10:`, `week 4 -`, and may carry an
#' explicit unit number (`w1.3:`) which is used for the unit id instead of the
#' position -- see `stable ids` below. A paragraph with no marker inherits the
#' previous paragraph's week, and that is reported.
#'
#' STABLE IDS. Without an explicit number, ids come from row order
#' (`01_w01_u03`). Inserting or reordering a paragraph therefore renumbers
#' everything after it, which silently breaks cache reuse and any join to human
#' labels. Numbering the paragraphs yourself (`w1.1:`, `w1.2:`) makes ids
#' immune to editing. Recommended for anything you will edit more than once.
#'
#' DROPS NOTHING BY DEFAULT. Every non-empty block becomes a unit. Question-
#' prompt detection is opt-in, because a genuine student paragraph ending in
#' "?" would otherwise be absorbed as a prompt and never rated; likewise
#' `min_chars` defaults to 0 so a terse but real paragraph is not discarded.
#' Whatever is dropped is listed in the "dropped" attribute of the result.
#'
#' @param min_chars blocks shorter than this are dropped (0 = keep everything).
#' @param detect_questions treat bulleted lines, and short lines ending in "?",
#'   as the journal's own question prompts rather than as units. Only enable
#'   this for files that actually interleave the questions with the answers.
txt_to_units <- function(path, student_id, min_chars = 0L, default_week = NA_integer_,
                         detect_questions = FALSE,
                         unit_level = c("sentence", "paragraph")) {
  unit_level <- match.arg(unit_level)
  if (!file.exists(path)) stop("journal file not found: ", path, call. = FALSE)
  raw <- readLines(path, warn = FALSE, encoding = "UTF-8")

  # split into blank-line separated blocks
  blank <- !nzchar(trimws(raw))
  grp <- cumsum(blank)
  blocks <- vapply(split(raw[!blank], grp[!blank]),
                   function(x) trimws(paste(x, collapse = " ")), character(1))
  blocks <- blocks[nzchar(blocks)]

  week <- default_week
  current_q <- NA_character_
  out <- list()
  dropped <- list()
  n_inherited <- 0L
  explicit_no <- integer(0)

  # Bullet handling is shared with the `sections` reader via starts_with_bullet /
  # strip_bullet, which work on Unicode code points. The previous local
  # character-literal list silently failed in a C locale, where substr() counts
  # bytes rather than characters.
  WEEK_RE <- "^[wW](?:eek)?[[:space:]]*([0-9]{1,2})(?:\\.([0-9]{1,3}))?[[:space:]]*[:.)-][[:space:]]*"

  for (b in blocks) {
    b <- trimws(gsub("[[:space:]]+", " ", b))
    raw <- b
    this_no <- NA_integer_

    # Inline week marker on the paragraph itself: "w1: ", "W10: ", "w1.3: ".
    m <- regmatches(b, regexec(WEEK_RE, b))[[1]]
    if (length(m)) {
      week <- as.integer(m[2])
      if (length(m) >= 3L && nzchar(m[3])) this_no <- as.integer(m[3])
      b <- trimws(sub(WEEK_RE, "", b))
    } else {
      n_inherited <- n_inherited + 1L
    }

    # Standalone heading line: "Reflection W4" / "Week 7"
    h <- regmatches(b, regexpr("(?i)\\b(reflection[[:space:]]*w|week[[:space:]]*)([0-9]{1,2})\\b",
                               b, perl = TRUE))
    if (length(h) && nchar(b) < 40L) {
      week <- as.integer(gsub("\\D", "", h))
      dropped[[length(dropped) + 1L]] <- list(reason = "week heading", text = raw)
      next
    }

    if (isTRUE(detect_questions)) {
      if (starts_with_bullet(b) || (nchar(b) < 400L && grepl("\\?[[:space:]]*$", b))) {
        current_q <- strip_bullet(b)
        dropped[[length(dropped) + 1L]] <- list(reason = "question prompt", text = raw)
        next
      }
    }

    if (nchar(b) < min_chars) {
      dropped[[length(dropped) + 1L]] <-
        list(reason = sprintf("shorter than min_chars (%d)", min_chars), text = raw)
      next
    }

    explicit_no <- c(explicit_no, this_no)
    out[[length(out) + 1L]] <- data.frame(
      student_id = student_id, week = week, text = b,
      question = current_q, context = NA_character_, stringsAsFactors = FALSE)
  }

  if (!length(out)) stop("no units found in ", path, " -- check the file layout", call. = FALSE)
  d <- do.call(rbind, out)

  if (anyNA(d$week)) {
    warning(sum(is.na(d$week)), " unit(s) have no week marker and none to inherit; ",
            "pass default_week or prefix the paragraphs with 'w1: '", call. = FALSE)
    d$week[is.na(d$week)] <- 0L
  }

  # Explicit paragraph numbers give ids that survive editing. Used only when
  # every paragraph has one, so ids are never a mix of the two schemes.
  stable <- !anyNA(explicit_no)
  para_no <- if (stable) {
    explicit_no
  } else {
    stats::ave(seq_len(nrow(d)), paste(d$student_id, d$week), FUN = seq_along)
  }
  if (stable) {
    message("using explicit week.number markers for unit ids (stable across edits)")
  }
  d$para_id <- sprintf("%s_w%02d_p%02d", d$student_id, d$week, para_no)
  if (anyDuplicated(d$para_id)) {
    stop("duplicate week.number marker(s): ",
         paste(unique(d$para_id[duplicated(d$para_id)]), collapse = ", "),
         call. = FALSE)
  }

  if (identical(unit_level, "paragraph")) {
    d$unit_id <- sprintf("%s_w%02d_u%02d", d$student_id, d$week, para_no)
    d$sent_in_para <- NA_integer_
    d$n_sent_in_para <- NA_integer_
  } else {
    # One row per sentence. The parent paragraph travels along as `context`, so
    # a sentence that is meaningless alone ("It was great.") can still be coded
    # in situ -- see context_mode in config/run.yml.
    rows <- list()
    n_short <- 0L
    for (i in seq_len(nrow(d))) {
      sents <- split_sentences(d$text[i])
      if (!length(sents)) next
      keep <- nchar(sents) >= min_chars
      if (any(!keep)) {
        n_short <- n_short + sum(!keep)
        for (s in sents[!keep]) {
          dropped[[length(dropped) + 1L]] <-
            list(reason = sprintf("sentence shorter than min_chars (%d)", min_chars),
                 text = s)
        }
        sents <- sents[keep]
      }
      if (!length(sents)) next
      rows[[length(rows) + 1L]] <- data.frame(
        unit_id = sprintf("%s_s%02d", d$para_id[i], seq_along(sents)),
        student_id = d$student_id[i], week = d$week[i], text = sents,
        question = d$question[i],
        # No point repeating a one-sentence paragraph as its own context.
        context = if (length(sents) > 1L) d$text[i] else NA_character_,
        para_id = d$para_id[i], sent_in_para = seq_along(sents),
        n_sent_in_para = length(sents), stringsAsFactors = FALSE)
    }
    if (!length(rows)) stop("no sentences found in ", path, call. = FALSE)
    n_para <- nrow(d)
    d <- do.call(rbind, rows)
    message(sprintf("%s: %d paragraph(s) -> %d sentence unit(s) (%.1f per paragraph)",
                    basename(path), n_para, nrow(d), nrow(d) / n_para))
  }

  d <- validate_units(d, path)
  attr(d, "dropped") <- dropped
  attr(d, "n_inherited_week") <- n_inherited
  attr(d, "stable_ids") <- stable
  attr(d, "unit_level") <- unit_level
  d
}
