# ---------------------------------------------------------------------------
# rate.R -- the two-stage rating runner
#
# Stage 1  assign the primary code(s). Multi-label when the codebook enables it,
#          so a mixed passage can carry both Positive and Self awareness rather
#          than forcing a lossy single choice.
# Stage 2  for passages whose stage-1 codes include a code listed under
#          `subclassification.applies_to` (v4: "Positive"), assign the cultural-
#          intelligence subclassification. At codebook `level: item` the model
#          returns one of the 20 CQS item codes and the four-level FACTOR IS
#          DERIVED from it here, so `cq_type` stays a 4-category column for every
#          downstream analysis while `cq_item` carries the finer resolution.
#
# Two calls rather than one because the probe showed that when a single schema
# offered a "None" factor option, two of four models assigned a substantive
# primary code and then declined to name a factor -- a protocol violation.
# Splitting the stages and giving stage 2 a closed enum makes that impossible.
# ---------------------------------------------------------------------------

CODE_SEP <- " | "

#' The instruction that tells the model how many codes to return
multi_block_text <- function(cb) {
  if (!isTRUE(cb$multi_label)) {
    return("Assign exactly one code.")
  }
  paste0("More than one code may apply to the same passage. Assign every code ",
         "that genuinely applies, and only those - at most ", cb$max_codes,
         ". Do not pad the answer: if only one code applies, return only that one.",
         if (length(cb$exclusive_codes))
           paste0(" The following code(s) must never be combined with any other: ",
                  paste0("\"", cb$exclusive_codes, "\"", collapse = ", "), ".")
         else "")
}

#' The stage-2 answering instruction, which differs by subclassification level
cq_answer_block <- function(cb) {
  if (identical(cb$cq_level, "item")) {
    paste0("How to answer:\n",
           "- Return the CODE of exactly one item (for example MOT1), not the ",
           "item text and not a factor name.\n",
           "- First settle which factor the passage is about, then pick the ",
           "best-matching item inside that factor.\n",
           "- Exactly one item must be chosen, even if no item fits well. Put ",
           "your doubt in the confidence score, not in a refusal.")
  } else {
    paste0("How to answer:\n",
           "- Return exactly one factor name. A passage may carry more than ",
           "one; pick the one the statement is mostly about.")
  }
}

#' Build the system/user pair for one unit at one stage
build_messages <- function(stage, cb, cfg, unit, tmpl, label_order,
                           assigned = NULL, trigger = NULL) {
  ctx <- build_context_block(unit, cfg$context_mode)

  vals <- if (stage == "primary") {
    list(codes_block   = codes_block(cb, label_order),
         code_list     = paste(label_order, collapse = ", "),
         rules_block   = rules_block(cb$primary$decision_rules),
         multi_block   = multi_block_text(cb),
         context_block = ctx,
         unit_text     = unit$text)
  } else {
    # `label_order` is the FACTOR order; the choice set follows from it, so the
    # prompt and the response schema can never offer different options.
    list(factors_block   = factors_block(cb, label_order),
         choice_list     = paste(cq_choice_order(cb, label_order), collapse = ", "),
         answer_block    = cq_answer_block(cb),
         rules_block     = rules_block(cb$cq$decision_rules),
         trigger_code    = trigger,
         assigned_codes  = paste(assigned, collapse = " + "),
         context_block   = ctx,
         unit_text       = unit$text)
  }
  list(system = render_template(tmpl$system, vals),
       user   = render_template(tmpl$user, vals))
}

#' Rate every unit with every enabled model
#'
#' @return long data.frame, one row per unit x model x replicate. `codes` is the
#'   canonical " | "-joined code set; `is_<code>` columns give the binary form
#'   the per-code reliability analysis needs.
rate_units <- function(units, cb, cfg, profile = "main", refresh = FALSE,
                       progress = TRUE) {

  prof <- cfg$profiles[[profile]]
  if (is.null(prof)) stop("no such profile in run.yml: ", profile, call. = FALSE)

  tmpl_primary <- read_prompt_template(cfg$paths$prompts_primary)
  tmpl_cq      <- read_prompt_template(cfg$paths$prompts_cq)

  models <- cfg$active_models
  n_rep <- prof$n_replicates
  total <- nrow(units) * length(models) * n_rep
  done <- 0L
  t_start <- Sys.time()
  rows <- vector("list", total)

  for (m in models) {
    for (rep in seq_len(n_rep)) {

      # Label order is fixed unless the profile asks for rotation, in which
      # case it rotates deterministically with the replicate index.
      k <- if (isTRUE(prof$rotate_labels)) rep - 1L else 0L
      order_primary <- rotate(cb$primary_labels, k)
      order_cq      <- rotate(cb$cq_labels, k)

      opts <- c(cfg$options,
                list(temperature = prof$temperature,
                     seed = as.integer(cfg$base_seed + rep - 1L)))

      schema1 <- if (isTRUE(cb$multi_label)) {
        multi_label_schema("codes", order_primary, cb$max_codes)
      } else {
        label_schema("codes", order_primary)
      }
      # Stage 2 offers item codes at `level: item`, factor names otherwise. The
      # JSON field is named after what is actually being asked for, so a cached
      # answer or a rationale is self-describing.
      choices_cq <- cq_choice_order(cb, order_cq)
      cq_field   <- if (identical(cb$cq_level, "item")) "cq_item" else "cq_type"
      schema2    <- label_schema(cq_field, choices_cq)

      for (i in seq_len(nrow(units))) {
        unit <- units[i, ]

        msg <- build_messages("primary", cb, cfg, unit, tmpl_primary, order_primary)
        r1 <- llm_label(cfg, m, msg$system, msg$user, schema1, "codes",
                        opts, multi = isTRUE(cb$multi_label),
                        cache_dir = cfg$paths$cache, refresh = refresh)

        codes <- NA_character_; repaired <- NA_character_
        if (isTRUE(r1$ok)) {
          raw <- vapply(r1$value, canonical_label, character(1),
                        allowed = cb$primary_labels, cb = cb, USE.NAMES = FALSE)
          codes <- enforce_code_set(raw, cb)
          repaired <- attr(codes, "repaired") %||% NA_character_
        }

        # Stage 2 fires when any assigned code triggers the subclassification.
        trigger <- if (!all(is.na(codes))) intersect(cb$cq_applies_to, codes) else character(0)
        cq <- NA_character_; cq_item <- NA_character_; r2 <- NULL
        if (length(trigger)) {
          msg2 <- build_messages("cq", cb, cfg, unit, tmpl_cq, order_cq,
                                 assigned = codes, trigger = trigger[1])
          r2 <- llm_label(cfg, m, msg2$system, msg2$user, schema2, cq_field,
                          opts, multi = FALSE,
                          cache_dir = cfg$paths$cache, refresh = refresh)
          if (isTRUE(r2$ok)) {
            if (identical(cb$cq_level, "item")) {
              # The factor is rolled up from the item, never asked for, so the
              # two levels of the scheme cannot disagree.
              cq_item <- canonical_label(r2$value, cb$cq_items$code, cb)
              cq <- if (is.na(cq_item)) NA_character_ else item_factor(cq_item, cb)
            } else {
              cq <- canonical_label(r2$value, cb$cq_labels, cb)
            }
          }
        }

        done <- done + 1L
        row <- data.frame(
          unit_id = unit$unit_id, student_id = unit$student_id, week = unit$week,
          text_sha1 = unit$text_sha1,
          provider = m$provider %||% "local_ollama",
          model = m$id, tier = m$tier %||% NA_character_,
          replicate = rep, profile = profile,
          rater_id = if (n_rep > 1L) sprintf("llm:%s#r%d", m$id, rep) else paste0("llm:", m$id),
          codes = code_set_string(codes, CODE_SEP),
          n_codes = if (all(is.na(codes))) NA_integer_ else length(codes),
          # cq_type is the 4-level factor throughout, derived from cq_item when
          # the codebook works at item level. Analyses that want the coarse
          # level therefore need no changes.
          cq_type = cq,
          cq_item = cq_item,
          conf_primary = if (isTRUE(r1$ok)) r1$confidence else NA_real_,
          conf_cq = if (!is.null(r2) && isTRUE(r2$ok)) r2$confidence else NA_real_,
          # Whether the answer was grammar-constrained ("schema") or merely
          # requested in words and validated afterwards ("prompt"). Not all
          # servers enforce schemas, and the difference is a real asymmetry
          # between raters that belongs in the results, not just in a comment.
          mode_primary = if (isTRUE(r1$ok)) (r1$output_mode %||% NA_character_) else NA_character_,
          repaired = repaired,
          secs = (r1$secs %||% 0) + (r2$secs %||% 0),
          cached = isTRUE(r1$cached) && (is.null(r2) || isTRUE(r2$cached)),
          ok = isTRUE(r1$ok) && (is.null(r2) || isTRUE(r2$ok)),
          error = paste(stats::na.omit(c(if (!isTRUE(r1$ok)) r1$error,
                                        if (!is.null(r2) && !isTRUE(r2$ok)) r2$error)),
                        collapse = " | "),
          rationale_primary = if (isTRUE(r1$ok)) r1$rationale else NA_character_,
          rationale_cq = if (!is.null(r2) && isTRUE(r2$ok)) r2$rationale else NA_character_,
          stringsAsFactors = FALSE)

        # One binary column per code: multi-label reliability is computed per
        # code, because a single nominal coefficient cannot express set overlap.
        for (lab in cb$primary_labels) {
          row[[paste0("is_", gsub("[^A-Za-z]+", "_", tolower(lab)))]] <-
            if (all(is.na(codes))) NA else lab %in% codes
        }

        rows[[done]] <- row
        if (progress) report_progress(done, total, t_start, m$id, rep, unit$unit_id,
                                      row$codes, row$cached)
      }
    }

    # Free the model before moving to the next one. Without this, a panel whose
    # models do not all fit in RAM silently loses the later ones: Ollama answers
    # HTTP 200 with an empty body instead of reporting that it could not load.
    prov <- tryCatch(get_provider(cfg, m$provider %||% "local_ollama"),
                     error = function(e) NULL)
    if (!is.null(prov) && identical(prov$type, "ollama")) {
      if (progress) cat(sprintf("\n  unloading %s\n", m$id))
      ollama_unload(prov$base_url, m$id)
    }
  }
  if (progress) cat("\n")
  do.call(rbind, rows)
}

report_progress <- function(done, total, t_start, model, rep, unit_id, codes, cached) {
  elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
  eta <- if (done > 0) (elapsed / done) * (total - done) else NA_real_
  cat(sprintf("\r[%d/%d] %-16s r%d %-14s -> %-30s %s eta %s      ",
              done, total, substr(model, 1, 16), rep, substr(unit_id, 1, 14),
              substr(codes %||% "FAILED", 1, 30),
              if (isTRUE(cached)) "(cached)" else "        ", fmt_dur(eta)))
  utils::flush.console()
}

fmt_dur <- function(s) {
  if (is.na(s)) return("?")
  if (s < 90) return(sprintf("%.0fs", s))
  if (s < 5400) return(sprintf("%.0fm", s / 60))
  sprintf("%.1fh", s / 3600)
}


# --- Outputs --------------------------------------------------------------

#' Split the run into a shareable table and a private one.
#'
#' Rationales and reasoning traces can quote the passage, so they never go into
#' the committable output. `ratings_<profile>.csv` carries ids and labels only.
#' Derive a run label from the units file name, so separate datasets cannot
#' overwrite each other's results. "data/units_synth5.csv" -> "synth5".
run_label <- function(units_path) {
  lab <- tools::file_path_sans_ext(basename(units_path))
  lab <- sub("^units_?", "", lab)
  if (!nzchar(lab)) lab <- "run"
  gsub("[^A-Za-z0-9._-]+", "_", lab)
}

#' Merge a new run into an existing ratings file rather than replacing it.
#'
#' Rating a subset -- `--models qwen3:14b` to retry one model, or `--limit` for a
#' quick check -- used to overwrite the whole file with just that subset, which
#' silently destroyed the rest of a long run. Rows are now keyed on
#' (unit_id, model, replicate); matching keys are replaced and everything else
#' is kept. Pass a different `--label` if you genuinely want a separate file.
merge_ratings <- function(new, path) {
  if (!file.exists(path)) return(new)
  old <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE,
                                  na.strings = c("NA", "")),
                  error = function(e) NULL)
  if (is.null(old) || !nrow(old)) return(new)

  if (!setequal(names(old), names(new))) {
    warning("existing ", basename(path), " has different columns (likely an older ",
            "codebook version); it is being replaced rather than merged.",
            call. = FALSE)
    return(new)
  }
  keyof <- function(d) paste(d$unit_id, d$model, d$replicate, sep = "\r")
  kept <- old[!keyof(old) %in% keyof(new), , drop = FALSE]
  if (nrow(kept)) {
    message(sprintf("merged into %s: %d new row(s), %d existing row(s) kept",
                    basename(path), nrow(new), nrow(kept)))
  }
  out <- rbind(kept, new[, names(old), drop = FALSE])
  out[order(out$model, out$replicate, out$unit_id), , drop = FALSE]
}

write_ratings <- function(ratings, cfg, profile, label = "run") {
  dir.create(cfg$paths$results, showWarnings = FALSE, recursive = TRUE)
  stem <- sprintf("%s_%s", label, profile)

  drop <- c("rationale_primary", "rationale_cq", "text_sha1")
  safe <- ratings[, setdiff(names(ratings), drop), drop = FALSE]
  f_safe <- file.path(cfg$paths$results, sprintf("ratings_%s.csv", stem))
  utils::write.csv(merge_ratings(safe, f_safe), f_safe, row.names = FALSE, na = "")

  # Rationales are genuinely useful for error analysis, so keep them -- but in
  # the gitignored cache directory, not in results/.
  f_full <- file.path(cfg$paths$cache, sprintf("ratings_%s_with_rationales.csv", stem))
  utils::write.csv(merge_ratings(ratings, f_full), f_full, row.names = FALSE, na = "")

  list(safe = f_safe, full = f_full, stem = stem)
}

#' Provenance record, so a results directory alone reconstructs the methods
write_manifest <- function(ratings, cb, cfg, profile, units_path, label = "run") {
  prof <- cfg$profiles[[profile]]
  provs <- unique(ratings$provider)
  man <- list(
    generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    profile = profile,
    codebook_version = cb$version,
    multi_label = isTRUE(cb$multi_label),
    max_codes = cb$max_codes,
    exclusive_codes = cb$exclusive_codes,
    subclassification_applies_to = cb$cq_applies_to,
    subclassification_level = cb$cq_level,
    subclassification_n_choices = length(cb$cq_target_labels),
    context_mode = cfg$context_mode,
    units_file = units_path,
    n_units = length(unique(ratings$unit_id)),
    temperature = prof$temperature,
    n_replicates = prof$n_replicates,
    rotate_labels = isTRUE(prof$rotate_labels),
    base_seed = cfg$base_seed,
    options = cfg$options,
    providers = lapply(provs, function(p) {
      pp <- tryCatch(get_provider(cfg, p), error = function(e) NULL)
      list(name = p, type = pp$type %||% NA, base_url = pp$base_url %||% NA,
           structured_output = pp$structured_output %||% "schema",
           ollama_version = if (identical(pp$type, "ollama")) ollama_version(pp$base_url) else NA)
    }),
    models = lapply(cfg$active_models, function(m)
      list(id = m$id, provider = m$provider %||% "local_ollama",
           tier = m$tier %||% NA, think = m$think %||% NA)),
    prompt_sha1 = list(
      primary = digest::digest(file = cfg$paths$prompts_primary, algo = "sha1"),
      cq_type = digest::digest(file = cfg$paths$prompts_cq, algo = "sha1"),
      codebook = digest::digest(file = cfg$paths$codebook, algo = "sha1")),
    n_calls_failed = sum(!ratings$ok),
    n_repaired = sum(!is.na(ratings$repaired)),
    r_version = R.version.string
  )
  man$label <- label
  f <- file.path(cfg$paths$results,
                 sprintf("manifest_%s_%s.json", label, profile))
  jsonlite::write_json(man, f, auto_unbox = TRUE, pretty = TRUE, null = "null")
  f
}
