# ---------------------------------------------------------------------------
# io.R -- read and normalise the human rater alignment sheet
#
# Expected wide layout (as exported from the 2026-07-28 alignment meeting):
#
#   ID, Serena, scqType, Thomas, tcqType, Jonas, jcqType, Week
#
# One row per coding unit. For each rater there is a primary-code column named
# after the rater and a CQ-type column named <first letter lowercased>cqType.
# The CQ type is only filled when the primary code is "Positive".
#
# IMPORTANT: the sheet carries no unit identifier and no journal text -- rows
# are aligned across raters by their position in the file. Unit ids are
# therefore generated from row order within (ID, Week). Never re-sort the sheet.
# ---------------------------------------------------------------------------

# Canonical label vocabularies. Codebook source: ColorCode.docx.
#
# The fourth code was called "No comment" up to codebook v4 and is called
# "Not marked" from v5. The pilot sheet still uses the old name, which the
# aliases below fold onto the new one -- so this module keeps reading the
# 2026-07-28 sheet unchanged while sharing one vocabulary with the LLM arm.
PRIMARY_LEVELS <- c("Positive", "Negative", "Self awareness", "Not marked")
CQ_LEVELS      <- c("Metacognitive", "Cognitive", "Motivational", "Behavioural")

# Spelling variants seen in the sheets, mapped onto the canonical labels.
LABEL_ALIASES <- c(
  "self-awareness"                    = "Self awareness",
  "self awareness"                    = "Self awareness",
  "selfawareness"                     = "Self awareness",
  "self-awareness or cultural background" = "Self awareness",
  "cultural background"               = "Self awareness",
  "no comment"                        = "Not marked",
  "nocomment"                         = "Not marked",
  "none"                              = "Not marked",
  "not marked"                        = "Not marked",
  "notmarked"                         = "Not marked",
  "not-marked"                        = "Not marked",
  "unmarked"                          = "Not marked",
  "positive"                          = "Positive",
  "negative"                          = "Negative",
  "behavioural"                       = "Behavioural",
  "behavioral"                        = "Behavioural",
  "behav"                             = "Behavioural",
  "metacognitive"                     = "Metacognitive",
  "meta-cognitive"                    = "Metacognitive",
  "metacog"                           = "Metacognitive",
  "cognitive"                         = "Cognitive",
  "cog"                               = "Cognitive",
  "motivational"                      = "Motivational",
  "motiv"                             = "Motivational"
)


#' Directory of the executing script, so paths work from any working directory
script_dir <- function() {
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f)) normalizePath(dirname(f[1])) else normalizePath(getwd())
}

#' Collapse whitespace and drop empty strings to NA
clean_chr <- function(x) {
  x <- gsub(" ", " ", as.character(x))    # non-breaking space
  x <- trimws(gsub("[[:space:]]+", " ", x))
  x[x == "" | x %in% c("NA", "-", "?")] <- NA_character_
  x
}

#' Map a raw label onto its canonical form; unknown values raise an error so a
#' typo in the sheet can never silently become a new category.
normalise_label <- function(x, allowed, what = "label") {
  x <- clean_chr(x)
  key <- tolower(x)
  hit <- LABEL_ALIASES[key]
  out <- ifelse(!is.na(hit), unname(hit), x)

  bad <- unique(out[!is.na(out) & !(out %in% allowed)])
  if (length(bad)) {
    stop("unrecognised ", what, ": ", paste(sprintf('"%s"', bad), collapse = ", "),
         "\n  allowed: ", paste(allowed, collapse = ", "),
         "\n  add an entry to LABEL_ALIASES in R/io.R if this is a spelling variant.",
         call. = FALSE)
  }
  factor(out, levels = allowed)
}


#' Locate the alignment sheet inside a directory
find_alignment_csv <- function(dir = script_dir(), pattern = "Coding alignment.*\\.csv$") {
  f <- list.files(dir, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
  if (!length(f)) stop("no alignment CSV matching /", pattern, "/ in ", dir, call. = FALSE)
  if (length(f) > 1L) {
    f <- f[order(file.mtime(f), decreasing = TRUE)]
    message("several alignment CSVs found; using the newest: ", basename(f[1]))
  }
  f[1]
}


#' Read the wide alignment sheet into a tidy long table
#'
#' @param path CSV path; auto-detected when NULL.
#' @param raters Rater names, matching the primary-code column names.
#' @return list(long, wide, raters, path) where `long` has one row per
#'   unit x rater with columns unit_id, student_id, week, unit_in_week, rater,
#'   primary, cq_type.
read_alignment <- function(path = NULL, raters = c("Serena", "Thomas", "Jonas")) {
  if (is.null(path)) path <- find_alignment_csv()

  # Excel exports these sheets with a UTF-8 BOM. read.csv() would otherwise
  # fold it into the first column name as "X...ID", so detect it from the raw
  # bytes and let the connection layer strip it.
  con <- file(path, "rb"); on.exit(close(con), add = TRUE)
  enc <- if (identical(readBin(con, "raw", 3L), as.raw(c(0xEF, 0xBB, 0xBF)))) {
    "UTF-8-BOM"
  } else {
    "UTF-8"
  }

  raw <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = TRUE,
                         colClasses = "character", fileEncoding = enc,
                         na.strings = c("NA", "", " ", "  "))
  # Belt and braces: undo any residual name mangling from a BOM or stray dots.
  names(raw) <- trimws(sub("^X\\.+", "", names(raw)))

  for (nm in names(raw)) raw[[nm]] <- clean_chr(raw[[nm]])

  required <- c("ID", "Week", raters)
  missing <- setdiff(required, names(raw))
  if (length(missing)) {
    stop("alignment sheet is missing column(s): ", paste(missing, collapse = ", "),
         "\n  found: ", paste(names(raw), collapse = ", "), call. = FALSE)
  }

  cq_col <- setNames(paste0(tolower(substr(raters, 1, 1)), "cqType"), raters)
  missing_cq <- cq_col[!cq_col %in% names(raw)]
  if (length(missing_cq)) {
    stop("expected CQ-type column(s) not found: ", paste(missing_cq, collapse = ", "),
         call. = FALSE)
  }

  # Drop the trailing padding rows that spreadsheet exports leave behind:
  # anything with no ID and no rating from anyone.
  has_content <- !is.na(raw$ID) |
    Reduce(`|`, lapply(raw[raters], function(x) !is.na(x)))
  n_dropped <- sum(!has_content)
  d <- raw[has_content, , drop = FALSE]

  if (anyNA(d$ID) || anyNA(d$Week)) {
    stop("rows with a rating but no ID/Week -- fix the sheet before analysing:\n  rows ",
         paste(which(is.na(d$ID) | is.na(d$Week)), collapse = ", "), call. = FALSE)
  }

  week <- suppressWarnings(as.integer(d$Week))
  if (anyNA(week)) stop("non-integer Week value(s) in the sheet", call. = FALSE)

  # Stable, human-readable unit ids from row order within (ID, Week)
  grp <- paste(d$ID, week, sep = "_w")
  unit_in_week <- stats::ave(seq_along(grp), grp, FUN = seq_along)
  unit_id <- sprintf("%s_w%02d_u%02d", d$ID, week, unit_in_week)
  if (anyDuplicated(unit_id)) stop("generated duplicate unit_id -- check the sheet", call. = FALSE)

  long <- do.call(rbind, lapply(raters, function(rt) {
    data.frame(
      unit_id      = unit_id,
      student_id   = d$ID,
      week         = week,
      unit_in_week = unit_in_week,
      rater        = rt,
      primary      = normalise_label(d[[rt]], PRIMARY_LEVELS, "primary code"),
      cq_type      = normalise_label(d[[cq_col[[rt]]]], CQ_LEVELS, "CQ type"),
      stringsAsFactors = FALSE
    )
  }))

  # Protocol check: a CQ type is only meaningful for a Positive code.
  offenders <- long[!is.na(long$cq_type) & long$primary != "Positive", ]
  if (nrow(offenders)) {
    warning("CQ type assigned where the primary code is not Positive (",
            nrow(offenders), " cell(s)): ",
            paste(sprintf("%s/%s", offenders$unit_id, offenders$rater),
                  collapse = ", "),
            "\n  these cells are kept as-is; decide whether the sheet needs fixing.",
            call. = FALSE)
  }

  message(sprintf("read %s: %d units x %d raters (%d padding row(s) dropped)",
                  basename(path), length(unit_id), length(raters), n_dropped))

  list(long = long,
       wide = data.frame(unit_id = unit_id, student_id = d$ID, week = week,
                         unit_in_week = unit_in_week, stringsAsFactors = FALSE),
       raters = raters, path = path)
}


#' Pivot the long table to the units x raters matrix the coefficients expect
#'
#' @param value "primary" or "cq_type".
#' @param units Optional vector of unit_ids to restrict to (preserves order).
ratings_matrix <- function(long, value = c("primary", "cq_type"), units = NULL) {
  value <- match.arg(value)
  if (is.null(units)) units <- unique(long$unit_id)
  raters <- unique(long$rater)

  m <- matrix(NA_character_, nrow = length(units), ncol = length(raters),
              dimnames = list(units, raters))
  idx <- cbind(match(long$unit_id, units), match(long$rater, raters))
  keep <- !is.na(idx[, 1])
  m[idx[keep, , drop = FALSE]] <- as.character(long[[value]])[keep]
  m
}
