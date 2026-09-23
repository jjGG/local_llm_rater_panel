#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# 02_rate.R -- run every enabled local model over every unit
#
#   Rscript scripts/02_rate.R data/units_sub01.csv
#   Rscript scripts/02_rate.R data/units_sub01.csv --profile consistency
#   Rscript scripts/02_rate.R data/units_synthetic.csv --dry-run
#
# Options:
#   --profile NAME   profile from config/run.yml (default: main)
#   --models a,b     only these model ids, overriding run.yml
#   --replicates N   override the profile's replicate count, for a model too
#                    slow to afford the full number. Replicates 1..N are the
#                    same requests either way, so 3 now and 5 later costs only
#                    the two extra ones
#   --limit N        first N units only, for a quick check
#   --refresh        ignore the cache and re-ask the models
#   --dry-run        print the assembled prompt for unit 1 and stop
#   --allow-unreachable  warn instead of aborting when a provider is offline;
#                    cached answers still serve, uncached calls are recorded as failed
#
# Safe to interrupt: completed calls are cached, so re-running resumes.
# ---------------------------------------------------------------------------

repo <- normalizePath(file.path(dirname(sub("^--file=", "",
        commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])), ".."))
setwd(repo)
source("R/codebook.R"); source("R/units.R"); source("R/llm.R"); source("R/rate.R")

a <- commandArgs(trailingOnly = TRUE)
getopt <- function(flag, default = NULL) {
  i <- match(flag, a); if (is.na(i) || i == length(a)) default else a[i + 1L]
}
has <- function(flag) flag %in% a

positional <- a[!grepl("^--", a)]
drop_vals <- unlist(lapply(c("--profile", "--models", "--limit", "--label",
                             "--replicates"), function(f) {
  i <- match(f, a); if (!is.na(i) && i < length(a)) a[i + 1L] else NULL }))
positional <- setdiff(positional, drop_vals)
if (!length(positional)) stop("usage: Rscript scripts/02_rate.R <units.csv> [options]", call. = FALSE)
units_path <- positional[1]

profile <- getopt("--profile", "main")
cb  <- load_codebook("config/codebook.yml")
cfg <- load_run_config("config/run.yml")

only <- getopt("--models")
if (!is.null(only)) {
  want <- trimws(strsplit(only, ",")[[1]])
  cfg$active_models <- Filter(function(m) m$id %in% want, cfg$models)
  if (!length(cfg$active_models)) stop("no model in run.yml matches: ", only, call. = FALSE)
}

# --replicates overrides the profile, for when one model is too slow to afford
# the full number. Written back into the profile itself so that rate_units AND
# write_manifest both see the effective value -- a manifest claiming 5 replicates
# for a 3-replicate run would be a provenance lie, and the manifest is meant to
# be enough to reconstruct the methods section on its own.
#
# Replicate k always uses seed base_seed+k-1 and label rotation k-1, so
# replicates 1..3 of a 3-replicate run are byte-identical requests to
# replicates 1..3 of a 5-replicate one. Running 3 now and 5 later therefore
# re-uses the cache and only costs the two extra replicates.
nrep <- suppressWarnings(as.integer(getopt("--replicates", NA)))
if (!is.na(nrep)) {
  if (nrep < 1L) stop("--replicates must be at least 1", call. = FALSE)
  if (is.null(cfg$profiles[[profile]])) {
    stop("no such profile in run.yml: ", profile, call. = FALSE)
  }
  was <- cfg$profiles[[profile]]$n_replicates
  cfg$profiles[[profile]]$n_replicates <- nrep
  if (!identical(as.integer(was), nrep)) {
    message(sprintf("NOTE  profile '%s' defines %s replicate(s); running %d instead.",
                    profile, was, nrep))
    message("      Recorded in the manifest. Comparing a 3-replicate self-consistency")
    message("      figure with a 5-replicate one is valid, but its interval is wider.")
  }
}

units <- read_units(units_path)
check_context_available(units, cfg$context_mode)
lim <- suppressWarnings(as.integer(getopt("--limit", NA)))
if (!is.na(lim)) units <- units[seq_len(min(lim, nrow(units))), , drop = FALSE]

# --- dry run: show exactly what the model will receive --------------------
if (has("--dry-run")) {
  tmpl <- read_prompt_template(cfg$paths$prompts_primary)
  msg <- build_messages("primary", cb, cfg, units[1, ], tmpl, cb$primary_labels)
  cat("========== SYSTEM ==========\n", msg$system,
      "\n\n========== USER ==========\n", msg$user, "\n", sep = "")
  cat("\n========== SCHEMA ==========\n")
  sch <- if (isTRUE(cb$multi_label)) {
    multi_label_schema("codes", cb$primary_labels, cb$max_codes)
  } else {
    label_schema("codes", cb$primary_labels)
  }
  cat(jsonlite::toJSON(sch, auto_unbox = TRUE, pretty = TRUE), "\n")
  cat(sprintf("\nstage 2 fires when the code set includes: %s\n",
              paste(cb$cq_applies_to, collapse = ", ")))
  cat(sprintf("stage 2 level: %s (%d choice(s)%s)\n", cb$cq_level,
              length(cb$cq_target_labels),
              if (identical(cb$cq_level, "item")) ", factor derived by roll-up" else ""))

  # Stage 2 is where the v4 changes live, so show that prompt too -- on an
  # invented passage, since unit 1 may not even trigger it.
  tmpl2 <- read_prompt_template(cfg$paths$prompts_cq)
  demo <- units[1, ]; demo$text <- "I really enjoyed working with the other group."
  msg2 <- build_messages("cq", cb, cfg, demo, tmpl2, cb$cq_labels,
                         assigned = cb$cq_applies_to[1], trigger = cb$cq_applies_to[1])
  cat("\n========== STAGE 2 SYSTEM ==========\n", msg2$system,
      "\n\n========== STAGE 2 USER ==========\n", msg2$user, "\n", sep = "")
  cat("\n========== STAGE 2 SCHEMA ==========\n")
  cat(jsonlite::toJSON(label_schema(
        if (identical(cb$cq_level, "item")) "cq_item" else "cq_type",
        cq_choice_order(cb)), auto_unbox = TRUE, pretty = TRUE), "\n")
  quit(status = 0L)
}

preflight(cfg, strict = !has("--allow-unreachable"))
prof <- cfg$profiles[[profile]]
cat(sprintf("\nprofile '%s': temperature %s, %d replicate(s), rotate_labels=%s\n",
            profile, prof$temperature, prof$n_replicates, isTRUE(prof$rotate_labels)))
cat(sprintf("%d unit(s) x %d model(s) x %d replicate(s) = %d stage-1 call(s)\n\n",
            nrow(units), length(cfg$active_models), prof$n_replicates,
            nrow(units) * length(cfg$active_models) * prof$n_replicates))

ratings <- rate_units(units, cb, cfg, profile = profile, refresh = has("--refresh"))

label <- getopt("--label", run_label(units_path))
files <- write_ratings(ratings, cfg, profile, label)
manifest <- write_manifest(ratings, cb, cfg, profile, units_path, label)

cat("\n== code-set distribution per model ==\n")
print(table(model = ratings$model, codes = ratings$codes, useNA = "ifany"))

cat("\n== per-code usage (a passage may carry several codes) ==\n")
is_cols <- grep("^is_", names(ratings), value = TRUE)
per_code <- do.call(rbind, lapply(unique(ratings$model), function(mid) {
  r <- ratings[ratings$model == mid, ]
  out <- as.data.frame(lapply(r[is_cols], function(x) sum(x, na.rm = TRUE)))
  cbind(model = mid, out, mean_n_codes = round(mean(r$n_codes, na.rm = TRUE), 2))
}))
print(per_code, row.names = FALSE)

sub <- ratings[!is.na(ratings$cq_type), ]
if (nrow(sub)) {
  cat(sprintf("\n== CQ factor per model (fires on: %s) ==\n",
              paste(cb$cq_applies_to, collapse = ", ")))
  print(table(model = sub$model, cq_type = sub$cq_type, useNA = "ifany"))
}

if ("cq_item" %in% names(ratings) && any(!is.na(ratings$cq_item))) {
  cat("\n== CQ item usage (all 20 shown; unused items are the interesting part) ==\n")
  used <- factor(ratings$cq_item[!is.na(ratings$cq_item)], levels = cb$cq_items$code)
  tab <- as.data.frame(table(item = used), stringsAsFactors = FALSE)
  tab$factor <- item_factor(tab$item, cb)
  tab$course_item <- cb$cq_items$course_item[match(tab$item, cb$cq_items$code)]
  print(tab[, c("item", "factor", "course_item", "Freq")], row.names = FALSE)
  cat(sprintf("%d of %d items were never used.\n",
              sum(tab$Freq == 0), nrow(tab)))
}

rep_rows <- ratings[!is.na(ratings$repaired), ]
if (nrow(rep_rows)) {
  cat(sprintf("\n== %d answer(s) repaired to satisfy the codebook constraints ==\n",
              nrow(rep_rows)))
  print(table(model = rep_rows$model, repaired = rep_rows$repaired))
}

bad_conf <- ratings[ratings$ok & is.na(ratings$conf_primary), ]
if (nrow(bad_conf)) {
  cat(sprintf("\nNOTE: %d call(s) returned a confidence outside [0,1]; stored as NA.\n",
              nrow(bad_conf)))
  print(table(model = bad_conf$model))
  cat("Do not use confidence from these models for triage or abstention.\n")
}

failed <- ratings[!ratings$ok, ]
if (nrow(failed)) {
  cat(sprintf("\n%d call(s) FAILED -- re-run to retry (successes are cached):\n", nrow(failed)))
  print(unique(failed[, c("model", "unit_id", "error")]), row.names = FALSE)
}

cat(sprintf("\nwall clock %.1f min (%d cached)\n", sum(ratings$secs) / 60, sum(ratings$cached)))
cat(sprintf("labels only  -> %s\n", files$safe))
cat(sprintf("+ rationales -> %s  (gitignored: may quote passages)\n", files$full))
cat(sprintf("manifest     -> %s\n", manifest))
