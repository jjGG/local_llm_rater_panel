# ---------------------------------------------------------------------------
# six_rater.R -- load and align a human rating sheet against a model run
#
# Sourced by scripts/12_six_rater.R and scripts/13_pairwise.R. Kept separate so
# both scripts see exactly the same units; a figure and a coefficient computed
# on different subsets would be worse than either alone.
#
# READS NO JOURNAL TEXT. Everything here works from unit_id strings and label
# columns. The units file, which holds the sentences, is never opened.
# ---------------------------------------------------------------------------

PRIMARY_LEVELS <- c("Not marked", "Positive", "Negative", "Self awareness")

# The four Ang & Van Dyne factors, and every spelling we have actually received.
# "MetaCognitivenitive" is a spreadsheet autofill artefact in the 9n99 sheet;
# "Behavioral" is the US spelling. Folded on read so the raters' own files stay
# exactly as they delivered them.
FACTOR_CANON <- c(
  "Metacognitive"       = "Metacognitive",
  "MetaCognitive"       = "Metacognitive",
  "MetaCognitivenitive" = "Metacognitive",
  "Meta-cognitive"      = "Metacognitive",
  "Cognitive"           = "Cognitive",
  "Motivational"        = "Motivational",
  "Behavioural"         = "Behavioural",
  "Behavioral"          = "Behavioural")

FACTOR_PREFIX <- c(Metacognitive = "MC", Cognitive = "COG",
                   Motivational = "MOT", Behavioural = "BEH")

canon_factor <- function(v) {
  out <- unname(FACTOR_CANON[trimws(as.character(v))])
  bad <- !is.na(v) & trimws(as.character(v)) != "" & is.na(out)
  if (any(bad)) {
    stop("unrecognised CQ factor spelling: ",
         paste(unique(trimws(as.character(v))[bad]), collapse = ", "),
         "\nadd it to FACTOR_CANON in R/six_rater.R if it is a real variant")
  }
  out
}

#' Read a rating file, sniffing the separator from the extension
#'
#' Spreadsheet exports arrive with a UTF-8 BOM, CRLF endings and a trailing
#' comma on the header row. Left alone the BOM becomes part of the first column
#' name, so `unit_id` silently stops existing and every downstream lookup
#' returns NA; the trailing comma invents an empty column. Both are stripped
#' here rather than asking anyone to clean the file by hand.
read_ratings <- function(path) {
  sep <- if (grepl("\\.tsv$|\\.txt$", path, ignore.case = TRUE)) "\t" else ","
  d <- utils::read.delim(path, sep = sep, stringsAsFactors = FALSE,
                         na.strings = c("NA", ""), check.names = FALSE)
  names(d)[1] <- sub("^﻿", "", names(d)[1])
  names(d) <- trimws(names(d))
  drop <- !nzchar(names(d)) | vapply(d, function(x) all(is.na(x)), logical(1)) &
          grepl("^(X|V)[0-9]*$", names(d))
  if (any(drop)) d <- d[, !drop, drop = FALSE]
  # Trim EVERY character column, not a hand-picked few. Different raters export
  # from different tools: one file arrives with a leading space on every CQ
  # value and whitespace-only cells that `na.strings` does not catch, so " " and
  # " Motivational" would otherwise become distinct levels from "" and
  # "Motivational" and quietly break both the factor vocabulary and the counts.
  for (cl in names(d)) {
    if (is.character(d[[cl]])) {
      v <- trimws(d[[cl]])
      v[!nzchar(v)] <- NA_character_
      d[[cl]] <- v
    }
  }
  d
}

#' Split a rater's uncertainty marker off a code
#'
#' A trailing "?" means the rater looked and declined to commit. That is neither
#' the code they hedged towards nor a positive judgement of "Not marked", so it
#' becomes missing: the sentence simply has one fewer rater, which every
#' coefficient and the consensus already handle natively. Forcing it either way
#' would put a judgement in a rater's mouth that they explicitly withheld.
strip_uncertain <- function(v) {
  hit <- !is.na(v) & grepl("\\?\\s*$", v)
  v[hit] <- NA_character_
  attr(v, "n_uncertain") <- sum(hit)
  v
}

#' Parse student / week / question / sentence out of the unit ids
parse_units <- function(ids) {
  ids <- sort(unique(ids))
  k <- regmatches(ids, regexec("^(.+)_w([0-9]+)_q([0-9]+)_s([0-9]+)$", ids))
  if (!all(lengths(k) == 5L)) {
    stop("unit_id not in <student>_w<week>_q<question>_s<sentence> form: ",
         paste(utils::head(ids[lengths(k) != 5L], 3), collapse = ", "))
  }
  data.frame(unit_id = ids,
             student  = vapply(k, `[`, character(1), 2),
             week     = as.integer(vapply(k, `[`, character(1), 3)),
             question = as.integer(vapply(k, `[`, character(1), 4)),
             sentence = as.integer(vapply(k, `[`, character(1), 5)),
             stringsAsFactors = FALSE)
}

#' Find student x week blocks whose unit sets are not identical in both files
#'
#' A partial overlap inside a block is the dangerous case, not the harmless one.
#' If one file segmented a block into different questions, an id such as
#' 9_w01_q01_s01 exists on both sides but points at DIFFERENT sentences, and
#' matching on the id would silently compare unrelated text. We therefore drop
#' the whole block rather than the non-shared ids, and say so loudly.
divergent_blocks <- function(h_ids, l_ids) {
  hb <- parse_units(h_ids); lb <- parse_units(l_ids)
  hb$blk <- paste(hb$student, hb$week); lb$blk <- paste(lb$student, lb$week)
  blks <- union(hb$blk, lb$blk)
  bad <- character(0)
  info <- list()
  for (b in blks) {
    a <- sort(hb$unit_id[hb$blk == b]); z <- sort(lb$unit_id[lb$blk == b])
    if (!identical(a, z)) {
      bad <- c(bad, b)
      info[[b]] <- c(human = length(a), model = length(z),
                     shared = length(intersect(a, z)))
    }
  }
  list(blocks = bad, detail = info)
}

#' Read a one-column list of unit_ids to exclude
#'
#' Accepts a file with or without a `unit_id` header, blank lines and `#`
#' comments allowed. These are units that turned out not to be student prose at
#' all -- question prompts that survived segmentation -- so they are not data
#' and must leave the denominator, not merely be recoded.
read_omit <- function(path) {
  if (is.null(path) || !nzchar(path)) return(character(0))
  if (!file.exists(path)) stop("omit file not found: ", path)
  v <- trimws(readLines(path, warn = FALSE))
  v <- v[nzchar(v) & !startsWith(v, "#")]
  if (length(v) && identical(tolower(v[1]), "unit_id")) v <- v[-1]
  unique(v)
}

#' Build the aligned six-rater tables
#'
#' Returns unit metadata plus units x rater matrices for the primary code, the
#' human CQ factor and the model CQ item, all sharing one row order.
align_raters <- function(human_file, llm_file,
                         omit_file       = NULL,
                         remap_file      = NULL,
                         human_rater_col = "rater",
                         human_code_col  = "codes",
                         human_cq_col    = "cq_type_ifPos",
                         llm_code_col    = "codes",
                         llm_item_col    = "cq_item",
                         llm_type_col    = "cq_type",
                         drop_divergent  = TRUE) {

  # `human_file` may name several files -- one per rater, which is how they
  # arrive -- either as a vector or as one ";"-separated string from the batch
  # manifest. They are stacked, so a batch's human panel is however many people
  # actually coded it.
  hf <- unlist(strsplit(as.character(human_file), "\\s*;\\s*"))
  hf <- hf[nzchar(hf)]
  hum <- do.call(rbind, lapply(hf, function(f) {
    d <- read_ratings(f)
    keep <- intersect(c("unit_id", human_rater_col, human_code_col, human_cq_col),
                      names(d))
    miss <- setdiff(c("unit_id", human_rater_col, human_code_col), keep)
    if (length(miss)) stop(f, " has no column(s): ", paste(miss, collapse = ", "))
    d <- d[, keep, drop = FALSE]
    if (!human_cq_col %in% names(d)) d[[human_cq_col]] <- NA_character_
    d
  }))
  n_unc <- 0L
  hum[[human_code_col]] <- {
    v <- strip_uncertain(hum[[human_code_col]])
    n_unc <- attr(v, "n_uncertain"); attributes(v) <- NULL; v
  }
  llm <- read_ratings(llm_file)

  # Dropped BEFORE the divergence check, because an omitted id is a statement
  # that the unit is not data -- it should not influence whether a block counts
  # as comparably segmented.
  # A block the two files numbered differently is rescued by renaming the model
  # ids to their human counterparts (see scripts/15_remap.R for how the pairing
  # is derived). Applied before anything else, so the rest of this function sees
  # one consistent id space. Model units with no counterpart fall away here,
  # which is what should happen to a sentence the other file does not contain.
  remap <- if (is.null(remap_file) || !nzchar(remap_file)) NULL else {
    if (!file.exists(remap_file)) stop("remap file not found: ", remap_file)
    r <- utils::read.csv(remap_file, stringsAsFactors = FALSE)
    if (!all(c("human_unit_id", "model_unit_id") %in% names(r)))
      stop("remap file needs columns human_unit_id and model_unit_id")
    r[nzchar(r$human_unit_id) & nzchar(r$model_unit_id), , drop = FALSE]
  }
  remapped <- 0L
  if (!is.null(remap) && nrow(remap)) {
    if (anyDuplicated(remap$model_unit_id) || anyDuplicated(remap$human_unit_id))
      stop("remap file is not one-to-one")
    # Only the blocks the remap actually covers are touched; a model id that is
    # already a valid shared id must not be renamed out from under itself.
    touched <- unique(sub("_q[0-9]+_s[0-9]+$", "", remap$model_unit_id))
    blk <- sub("_q[0-9]+_s[0-9]+$", "", llm$unit_id)
    hit <- blk %in% touched
    i <- match(llm$unit_id[hit], remap$model_unit_id)
    llm$unit_id[hit] <- ifelse(is.na(i), NA_character_,
                               remap$human_unit_id[i])
    # distinct UNITS renamed, not data rows -- there is one row per model per
    # unit, so counting rows would report three times the truth.
    remapped <- length(unique(llm$unit_id[hit & !is.na(llm$unit_id)]))
    llm <- llm[!is.na(llm$unit_id), , drop = FALSE]
  }

  omit <- read_omit(omit_file)
  omit_hit <- list(human = intersect(omit, unique(hum$unit_id)),
                   model = intersect(omit, unique(llm$unit_id)))
  omit_miss <- setdiff(omit, union(unique(hum$unit_id), unique(llm$unit_id)))
  if (length(omit)) {
    hum <- hum[!hum$unit_id %in% omit, , drop = FALSE]
    llm <- llm[!llm$unit_id %in% omit, , drop = FALSE]
  }

  for (cl in c("unit_id", "model", llm_code_col)) {
    if (!cl %in% names(llm)) stop("model file has no column `", cl, "`")
  }

  # Models may be run at several replicates; collapse to one label per
  # model x unit by modal vote before any rater-level comparison.
  if ("replicate" %in% names(llm)) {
    n_rep <- length(unique(llm$replicate))
  } else n_rep <- 1L

  dv <- divergent_blocks(hum$unit_id, llm$unit_id)
  keep_blk <- function(d) {
    b <- paste(sub("_w.*$", "", d$unit_id),
               as.integer(sub("^.*_w([0-9]+)_.*$", "\\1", d$unit_id)))
    !(b %in% dv$blocks)
  }
  if (drop_divergent && length(dv$blocks)) {
    hum <- hum[keep_blk(hum), , drop = FALSE]
    llm <- llm[keep_blk(llm), , drop = FALSE]
  }

  ids <- intersect(unique(hum$unit_id), unique(llm$unit_id))
  u <- parse_units(ids)
  # Student ids are mixed-width strings ("00", "01", "8", "11"), so a plain
  # sort puts 8 after 11. Order numerically where the id is a number, and fall
  # back to alphabetical for anything that is not.
  snum <- suppressWarnings(as.numeric(u$student))
  u$student_ord <- if (anyNA(snum)) match(u$student, sort(unique(u$student)))
                   else snum
  u <- u[order(u$student_ord, u$week, u$question, u$sentence), ]
  u$ord <- stats::ave(seq_len(nrow(u)), u$student, FUN = seq_along)
  u$panel_lab <- factor(paste("student", u$student),
                        levels = paste("student", unique(u$student)))

  hum <- hum[hum$unit_id %in% ids, , drop = FALSE]
  llm <- llm[llm$unit_id %in% ids, , drop = FALSE]

  H <- sort(unique(hum[[human_rater_col]]))
  L <- unique(llm$model)

  # one value per rater x unit; modal over replicates for the models
  modal1 <- function(v) {
    v <- v[!is.na(v)]
    if (!length(v)) return(NA_character_)
    tb <- sort(table(v), decreasing = TRUE)
    if (length(tb) > 1L && tb[1] == tb[2]) NA_character_ else names(tb)[1]
  }
  grab_h <- function(who, col) {
    s <- hum[hum[[human_rater_col]] == who, , drop = FALSE]
    if (anyDuplicated(s$unit_id)) {
      stop("rater ", who, " has duplicate rows for a unit in the human file")
    }
    s[[col]][match(u$unit_id, s$unit_id)]
  }
  grab_l <- function(who, col) {
    s <- llm[llm$model == who, , drop = FALSE]
    vapply(u$unit_id, function(i) modal1(s[[col]][s$unit_id == i]),
           character(1), USE.NAMES = FALSE)
  }

  h_code <- vapply(H, grab_h, character(nrow(u)), col = human_code_col)
  l_code <- vapply(L, grab_l, character(nrow(u)), col = llm_code_col)
  h_cq   <- canon_factor(vapply(H, grab_h, character(nrow(u)), col = human_cq_col))
  dim(h_cq) <- dim(h_code); dimnames(h_cq) <- dimnames(h_code)
  l_item <- vapply(L, grab_l, character(nrow(u)), col = llm_item_col)
  l_type <- if (llm_type_col %in% names(llm)) {
    x <- canon_factor(vapply(L, grab_l, character(nrow(u)), col = llm_type_col))
    dim(x) <- dim(l_code); dimnames(x) <- dimnames(l_code); x
  } else NULL

  short <- function(m) {
    m <- sub(":latest$", "", m); m <- sub("^DeepSeek.*", "DeepSeek", m)
    sub("^gpt-oss.*", "gpt-oss", m)
  }
  colnames(l_code) <- colnames(l_item) <- short(L)
  if (!is.null(l_type)) colnames(l_type) <- short(L)

  # a CQ value is only meaningful where that same rater said Positive
  h_cq[h_code != "Positive" | is.na(h_code)] <- NA_character_
  l_item[l_code != "Positive" | is.na(l_code)] <- NA_character_
  if (!is.null(l_type)) l_type[l_code != "Positive" | is.na(l_code)] <- NA_character_

  bad <- setdiff(unique(c(h_code, l_code)), c(PRIMARY_LEVELS, NA))
  if (length(bad)) stop("primary code outside the codebook: ",
                        paste(bad, collapse = ", "))

  list(units = u, h_code = h_code, l_code = l_code,
       h_cq = h_cq, l_item = l_item, l_type = l_type,
       humans = H, models = colnames(l_code), n_replicates = n_rep,
       divergent = dv, n_remapped = remapped, n_uncertain = n_unc,
       human_files = hf,
       omit = list(requested = omit, human = omit_hit$human,
                   model = omit_hit$model, unmatched = omit_miss))
}

#' Modal label across the columns of a matrix, ties marked
panel_modal <- function(m, tie = "(tie)") {
  apply(m, 1L, function(v) {
    v <- v[!is.na(v)]
    if (!length(v)) return(NA_character_)
    tb <- sort(table(v), decreasing = TRUE)
    if (length(tb) > 1L && tb[1] == tb[2]) tie else names(tb)[1]
  })
}
