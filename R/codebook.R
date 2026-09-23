# ---------------------------------------------------------------------------
# codebook.R -- load the coding scheme and render it into prompt fragments
#
# config/codebook.yml is the single source of truth for the label vocabularies.
# Both the prompts and the label validation come from here, so the model can
# never be offered a category the analysis does not recognise.
# ---------------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

#' Collapse the folded-block whitespace YAML leaves behind
squish <- function(x) trimws(gsub("[[:space:]]+", " ", paste(x, collapse = " ")))

#' Read questionnaire item wording from a local, uncommitted file.
#'
#' Expected format is the scale as distributed -- one item per line, the item
#' code first, wording after it, wrapped lines allowed:
#'
#'     MC1 I am conscious of the cultural knowledge I use when ...
#'     MOT5 I am confident that I can get accustomed to the shopping
#'     conditions in a different culture.
#'
#' A line continues the previous item only when that item does not yet end in
#' terminal punctuation. That rule is what keeps section headings ("Cognitive
#' CQ:") and rulers ("-----") from being glued onto the item above them, without
#' needing a list of every heading the file might contain.
parse_item_text <- function(path) {
  if (!file.exists(path)) {
    stop("item wording file not found: ", path,
         "\n\n  config/codebook.yml holds only the item CODES, because the",
         "\n  Cultural Intelligence Scale is copyrighted and is not",
         "\n  redistributed with this repository. Put the scale in that file",
         "\n  (one item per line, code first) -- see setting/README.md.",
         call. = FALSE)
  }
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  code_re <- "^[[:space:]]*([A-Za-z]{2,8}[0-9]{1,2})[[:space:]]+(.+)$"

  out <- list(); cur <- NULL
  for (ln in lines) {
    l <- trimws(ln)
    if (!nzchar(l)) { cur <- NULL; next }
    if (grepl("^[-=_]+$", l)) { cur <- NULL; next }        # ruler
    m <- regmatches(l, regexec(code_re, l))[[1]]
    if (length(m) == 3L) {
      cur <- toupper(m[2])
      out[[cur]] <- squish(m[3])
      next
    }
    # Continuation only while the item is still an unfinished sentence.
    if (!is.null(cur) && !grepl("[.!?][\"')]?$", out[[cur]])) {
      out[[cur]] <- squish(paste(out[[cur]], l))
    } else {
      cur <- NULL
    }
  }
  if (!length(out)) {
    stop("no items found in ", path,
         "\n  expected lines beginning with an item code, e.g. 'MC1 I am ...'",
         call. = FALSE)
  }
  data.frame(code = names(out), text = unlist(out, use.names = FALSE),
             stringsAsFactors = FALSE)
}

#' Fill in missing item wording from the local file, matching on item code
merge_item_text <- function(items, path) {
  found <- parse_item_text(path)
  need <- !nzchar(items$text)
  hit <- match(items$code[need], found$code)

  if (anyNA(hit)) {
    missing <- items$code[need][is.na(hit)]
    stop("no wording found for item(s): ", paste(missing, collapse = ", "),
         "\n  looked in: ", path,
         "\n  found ", nrow(found), " item(s): ",
         paste(found$code, collapse = ", "),
         "\n  the codes in config/codebook.yml must match the codes in that file.",
         call. = FALSE)
  }
  items$text[need] <- found$text[hit]

  blank <- !nzchar(items$text)
  if (any(blank)) {
    stop("empty wording for item(s): ", paste(items$code[blank], collapse = ", "),
         call. = FALSE)
  }
  message(sprintf("loaded wording for %d item(s) from %s (not in version control)",
                  sum(need), path))
  items
}

load_codebook <- function(path = "config/codebook.yml") {
  if (!file.exists(path)) stop("codebook not found: ", path, call. = FALSE)
  cb <- yaml::read_yaml(path)

  # `subclassification` is the v3 name; `cq_type` was the v1/v2 name.
  cb$cq <- cb$subclassification %||% cb$cq_type
  if (is.null(cb$cq)) stop("codebook has no `subclassification` section", call. = FALSE)

  cb$primary_labels <- vapply(cb$primary$codes, function(x) x$label, character(1))
  cb$cq_labels      <- vapply(cb$cq$factors,    function(x) x$label, character(1))

  if (anyDuplicated(cb$primary_labels) || anyDuplicated(cb$cq_labels)) {
    stop("duplicate label in codebook", call. = FALSE)
  }

  # Which stage-1 codes trigger the subclassification. The pilot used
  # "Positive"; v3 uses "Self awareness". Validated here so a typo cannot
  # silently disable stage 2 for the whole run.
  cb$cq_applies_to <- cb$cq$applies_to %||% "Positive"
  unknown <- setdiff(cb$cq_applies_to, cb$primary_labels)
  if (length(unknown)) {
    stop("subclassification.applies_to names code(s) not in the scheme: ",
         paste(unknown, collapse = ", "),
         "\n  primary codes are: ", paste(cb$primary_labels, collapse = ", "),
         call. = FALSE)
  }

  # --- Stage-2 granularity: items nested in factors -----------------------
  # `level: item` (v4) means the model chooses one of the 20 questionnaire
  # items and the factor is DERIVED from it, so item and factor can never
  # contradict each other. `level: factor` is the v1-v3 behaviour.
  cb$cq_level <- cb$cq$level %||% "factor"
  if (!cb$cq_level %in% c("item", "factor")) {
    stop("subclassification.level must be 'item' or 'factor', not: ", cb$cq_level,
         call. = FALSE)
  }

  cb$cq_items <- NULL
  if (length(cb$cq$items)) {
    it <- cb$cq$items
    cb$cq_items <- data.frame(
      code        = vapply(it, function(x) as.character(x$code %||% NA), character(1)),
      factor      = vapply(it, function(x) as.character(x$factor %||% NA), character(1)),
      text        = vapply(it, function(x) squish(x$text %||% ""), character(1)),
      course_item = vapply(it, function(x) isTRUE(x$course_item), logical(1)),
      stringsAsFactors = FALSE)

    # ITEM WORDING IS HELD OUTSIDE THIS REPOSITORY. The Cultural Intelligence
    # Scale is copyrighted, so config/codebook.yml carries only the item CODES,
    # their factor and whether the course administered them -- structural facts
    # about the instrument -- while the item TEXT is read from a local file named
    # by `items_text_file` and excluded from version control.
    #
    # The prompts need the wording, so a missing file is a hard error rather than
    # a silent degradation: coding against item codes alone would produce
    # plausible-looking output from a model that had never seen the items.
    if (any(!nzchar(cb$cq_items$text)) && !is.null(cb$cq$items_text_file)) {
      cb$cq_items <- merge_item_text(cb$cq_items, cb$cq$items_text_file)
    }

    if (anyNA(cb$cq_items$code) || !all(nzchar(cb$cq_items$code))) {
      stop("every subclassification item needs a `code`", call. = FALSE)
    }
    if (anyDuplicated(cb$cq_items$code)) {
      stop("duplicate subclassification item code: ",
           paste(unique(cb$cq_items$code[duplicated(cb$cq_items$code)]), collapse = ", "),
           call. = FALSE)
    }
    if (!all(nzchar(cb$cq_items$text))) {
      stop("subclassification item(s) with no `text`: ",
           paste(cb$cq_items$code[!nzchar(cb$cq_items$text)], collapse = ", "), call. = FALSE)
    }
    # An item whose factor is misspelt would roll up to NA and silently empty
    # the factor column, so it is a hard error rather than a warning.
    bad <- setdiff(cb$cq_items$factor, cb$cq_labels)
    if (length(bad)) {
      stop("subclassification item(s) name a factor not in the scheme: ",
           paste(bad, collapse = ", "),
           "\n  factors are: ", paste(cb$cq_labels, collapse = ", "), call. = FALSE)
    }
  }
  if (identical(cb$cq_level, "item") && is.null(cb$cq_items)) {
    stop("subclassification.level is 'item' but no `items:` are defined", call. = FALSE)
  }

  # What stage 2 actually asks the model to choose from.
  cb$cq_target_labels <- if (identical(cb$cq_level, "item")) {
    cb$cq_items$code
  } else cb$cq_labels

  # Read the block out before overwriting the name with the boolean.
  ml <- cb$multi_label
  cb$multi_label     <- isTRUE(ml$enabled)
  cb$max_codes       <- as.integer(ml$max_codes %||% length(cb$primary_labels))
  if (is.na(cb$max_codes) || cb$max_codes < 1L) cb$max_codes <- length(cb$primary_labels)
  cb$exclusive_codes <- unlist(ml$exclusive %||% list())

  cb
}

#' Enforce the codebook's structural constraints on a set of codes.
#'
#' Returns the cleaned set, or NA when the answer cannot be repaired. Exclusive
#' codes ("Not marked" means either "not about intercultural experience" or "I
#' cannot decide") must
#' never appear beside a substantive code, so a model returning both is
#' contradicting itself and the pairing is dropped rather than guessed at.
enforce_code_set <- function(codes, cb) {
  codes <- unique(codes[!is.na(codes) & nzchar(codes)])
  if (!length(codes)) return(NA_character_)

  if (length(cb$exclusive_codes)) {
    excl <- intersect(codes, cb$exclusive_codes)
    if (length(excl) && length(codes) > length(excl)) {
      # substantive codes win: the model did assert something concrete
      codes <- setdiff(codes, cb$exclusive_codes)
      attr(codes, "repaired") <- sprintf("dropped exclusive code(s) %s",
                                         paste(excl, collapse = ", "))
    } else if (length(excl) > 1L) {
      codes <- excl[1]
    }
  }
  if (length(codes) > cb$max_codes) {
    codes <- codes[seq_len(cb$max_codes)]
    attr(codes, "repaired") <- "truncated to max_codes"
  }
  # canonical order follows the codebook, so a set has one string form
  codes[order(match(codes, cb$primary_labels))]
}

#' Canonical string form of a code set, for joins and for the ratings matrix
code_set_string <- function(codes, sep = " | ") {
  if (all(is.na(codes)) || !length(codes)) return(NA_character_)
  paste(codes, collapse = sep)
}

load_run_config <- function(path = "config/run.yml") {
  if (!file.exists(path)) stop("run config not found: ", path, call. = FALSE)
  cfg <- yaml::read_yaml(path)

  cfg$active_models <- Filter(function(m) isTRUE(m$enabled), cfg$models)
  if (!length(cfg$active_models)) stop("no enabled models in ", path, call. = FALSE)

  ids <- vapply(cfg$active_models, function(m) m$id, character(1))
  if (anyDuplicated(ids)) stop("model listed twice: ", paste(ids[duplicated(ids)]), call. = FALSE)

  # unit_context is the sentence-level default: the sentence plus its own
  # paragraph, with no journal question involved.
  valid_modes <- c("unit", "unit_context", "unit_question", "unit_question_context")
  if (!cfg$context_mode %in% valid_modes) {
    stop("context_mode must be one of: ", paste(valid_modes, collapse = ", "), call. = FALSE)
  }
  cfg
}


# --- Prompt fragments -----------------------------------------------------

#' Primary codes as a numbered block, in the given order
codes_block <- function(cb, order = cb$primary_labels) {
  codes <- cb$primary$codes[match(order, cb$primary_labels)]
  paste(vapply(seq_along(codes), function(i) {
    sprintf("%d. %s\n   %s", i, codes[[i]]$label, squish(codes[[i]]$definition))
  }, character(1)), collapse = "\n\n")
}

#' Roll an item code up to its factor.
#'
#' The factor is never asked for separately: it is looked up here, so the two
#' levels of the scheme cannot disagree. Unknown codes give NA rather than a
#' guess, so a bad answer surfaces as missing.
item_factor <- function(codes, cb) {
  if (is.null(cb$cq_items)) return(rep(NA_character_, length(codes)))
  cb$cq_items$factor[match(as.character(codes), cb$cq_items$code)]
}

#' Item codes in the order implied by a factor ordering.
#'
#' Rotation for the anti-position-bias profiles rotates the four FACTOR blocks,
#' keeping each factor's items in scale order -- scrambling items inside a factor
#' would make the block harder to read without adding anything.
cq_item_order <- function(cb, factor_order = cb$cq_labels) {
  if (is.null(cb$cq_items)) return(character(0))
  unlist(lapply(factor_order, function(f) cb$cq_items$code[cb$cq_items$factor == f]),
         use.names = FALSE)
}

#' The stage-2 choice set, in the order implied by a factor ordering
cq_choice_order <- function(cb, factor_order = cb$cq_labels) {
  if (identical(cb$cq_level, "item")) cq_item_order(cb, factor_order) else factor_order
}

#' CQ factors with their questionnaire items, in the given factor order.
#'
#' At `level: item` each item is listed with the code the model must return; at
#' `level: factor` the items appear as anchors illustrating the factor.
factors_block <- function(cb, order = cb$cq_labels) {
  fs <- cb$cq$factors[match(order, cb$cq_labels)]
  by_item <- identical(cb$cq_level, "item")

  paste(vapply(seq_along(fs), function(i) {
    lab <- fs[[i]]$label
    its <- if (is.null(cb$cq_items)) NULL else cb$cq_items[cb$cq_items$factor == lab, ]
    body <- if (!is.null(its) && nrow(its)) {
      lines <- sprintf("     %s  %s", format(its$code, width = 5), its$text)
      paste0(if (by_item) "\n   Items:\n" else "\n   Anchor items:\n",
             paste(lines, collapse = "\n"))
    } else ""
    sprintf("%s %s\n   %s%s", paste0(LETTERS[i], "."), lab,
            squish(fs[[i]]$definition), body)
  }, character(1)), collapse = "\n\n")
}

rules_block <- function(rules) {
  paste0("- ", vapply(rules, squish, character(1)), collapse = "\n")
}

#' Rotate a label vector by k positions.
#' Used only when a profile sets rotate_labels, to keep the position of a label
#' in the prompt from acting as a cue. Rotation is deterministic in k so a run
#' stays reproducible.
rotate <- function(x, k) {
  if (k %% length(x) == 0L) return(x)
  k <- k %% length(x)
  c(x[(k + 1L):length(x)], x[1L:k])
}


# --- Label normalisation --------------------------------------------------

#' Fold a model's answer onto a canonical label. Returns NA for anything the
#' codebook does not know, so a bad value surfaces as missing rather than as a
#' silently invented category.
canonical_label <- function(x, allowed, cb) {
  if (is.null(x) || !length(x) || is.na(x)) return(NA_character_)
  x <- trimws(as.character(x)[1])
  if (x %in% allowed) return(x)

  hit <- cb$aliases[[tolower(x)]]
  if (!is.null(hit) && hit %in% allowed) return(hit)

  # last resort: case-insensitive match against the allowed set
  ci <- allowed[tolower(allowed) == tolower(x)]
  if (length(ci) == 1L) return(ci)

  NA_character_
}


# --- Prompt assembly ------------------------------------------------------

#' Split a prompt template into its SYSTEM and USER parts
read_prompt_template <- function(path) {
  if (!file.exists(path)) stop("prompt template not found: ", path, call. = FALSE)
  lines <- readLines(path, warn = FALSE)

  i_sys <- grep("^### SYSTEM\\s*$", lines)
  i_usr <- grep("^### USER\\s*$", lines)
  if (length(i_sys) != 1L || length(i_usr) != 1L || i_usr < i_sys) {
    stop("template ", path, " needs exactly one '### SYSTEM' then one '### USER' marker",
         call. = FALSE)
  }
  list(system = paste(lines[(i_sys + 1L):(i_usr - 1L)], collapse = "\n"),
       user   = paste(lines[(i_usr + 1L):length(lines)], collapse = "\n"))
}

#' Fill {{placeholders}}. Errors on any placeholder left unfilled, so a typo in
#' a template surfaces immediately instead of reaching the model verbatim.
render_template <- function(text, values) {
  for (k in names(values)) {
    text <- gsub(paste0("{{", k, "}}"), values[[k]], text, fixed = TRUE)
  }
  left <- regmatches(text, gregexpr("\\{\\{[a-z_]+\\}\\}", text))[[1]]
  if (length(left)) {
    stop("unfilled placeholder(s) in prompt: ", paste(unique(left), collapse = ", "),
         call. = FALSE)
  }
  trimws(text)
}

#' The context shown above the unit, per context_mode.
#'
#' Returns "" for mode "unit", so the unit is judged in isolation. At sentence
#' level that is often too little -- "It was great." cannot be coded alone -- so
#' `unit_context` prepends the sentence's own paragraph, labelled clearly as
#' context that must not itself be coded.
build_context_block <- function(unit, mode) {
  if (mode == "unit") return("")

  parts <- character(0)
  if (mode %in% c("unit_question", "unit_question_context")) {
    q <- unit$question %||% NA
    if (!is.na(q) && nzchar(trimws(q))) {
      parts <- c(parts, paste0("The passage answers this journal question:\n\"\"\"\n",
                               trimws(q), "\n\"\"\"\n"))
    }
  }
  if (mode %in% c("unit_context", "unit_question_context")) {
    ctx <- unit$context %||% NA
    if (!is.na(ctx) && nzchar(trimws(ctx))) {
      parts <- c(parts, paste0(
        "The paragraph the sentence comes from, for reference only. Do NOT code ",
        "the paragraph; code only the single sentence given below it.\n\"\"\"\n",
        trimws(ctx), "\n\"\"\"\n"))
    }
  }
  if (!length(parts)) return("")
  paste0(paste(parts, collapse = "\n"), "\n")
}
