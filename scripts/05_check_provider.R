#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# 05_check_provider.R -- verify a model server before committing to a long run
#
#   Rscript scripts/05_check_provider.R fgcz_vllm
#   Rscript scripts/05_check_provider.R fgcz_vllm --model DeepSeek-V4-Flash-DSpark
#   Rscript scripts/05_check_provider.R local_ollama
#
# Checks, in order:
#   1. the base_url resolves from the environment
#   2. the server answers and lists models
#   3. the requested model id is actually served (exact, case-sensitive match)
#   4. a real schema-constrained call returns a valid label, and WHICH output
#      mode produced it -- native schema enforcement or the prompt fallback
#
# The test passage is invented, so this sends no student data anywhere.
# ---------------------------------------------------------------------------

repo <- normalizePath(file.path(dirname(sub("^--file=", "",
        commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])), ".."))
setwd(repo)
source("R/codebook.R"); source("R/llm.R"); source("R/units.R"); source("R/rate.R")

a <- commandArgs(trailingOnly = TRUE)
getopt <- function(f, d = NULL) { i <- match(f, a); if (is.na(i) || i == length(a)) d else a[i + 1L] }
pos <- setdiff(a[!grepl("^--", a)], getopt("--model", ""))
if (!length(pos)) stop("usage: Rscript scripts/05_check_provider.R <provider> [--model ID]",
                       call. = FALSE)
pname <- pos[1]

cb  <- load_codebook("config/codebook.yml")
cfg <- load_run_config("config/run.yml")

cat(sprintf("provider: %s\n", pname))
p <- tryCatch(get_provider(cfg, pname), error = function(e) {
  cat("FAIL  ", conditionMessage(e), "\n", sep = ""); quit(status = 1L) })
cat(sprintf("  type      %s\n  base_url  %s\n  api key   %s\n  structured %s\n",
            p$type, p$base_url,
            if (nzchar(p$api_key)) sprintf("set (%s, %d chars)", p$api_key_env, nchar(p$api_key))
            else if (is.null(p$api_key_env)) "not required"
            # Not an error on its own: the FGCZ internal endpoint accepts
            # unauthenticated requests. Step 2 below is the real test.
            else sprintf("not set (%s) -- fine if the endpoint needs none", p$api_key_env),
            p$structured_output %||% "schema"))

# --- 2. does it answer? ---------------------------------------------------
avail <- provider_probe(p)
if (is.null(avail)) {
  cat("\nFAIL  no model list from ", p$base_url, "\n", sep = "")
  cat("  - on the right network? this endpoint is FGCZ-internal\n")
  cat("  - correct path? Open WebUI serves its API under /api, not /v1\n")
  cat("  - token needed? create one in the UI under Settings > Account > API keys\n")
  quit(status = 1L)
}
cat(sprintf("\nOK    server lists %d model(s)\n", length(avail)))

want <- getopt("--model")
if (is.null(want)) {
  ms <- Filter(function(m) (m$provider %||% "local_ollama") == pname, cfg$models)
  want <- if (length(ms)) ms[[1]]$id else avail[1]
}

# --- 3. is the model served? --------------------------------------------
if (!want %in% avail) {
  cat(sprintf("\nFAIL  '%s' is not in the served list. Ids are case-sensitive.\n", want))
  near <- avail[grepl(substr(want, 1, 8), avail, ignore.case = TRUE)]
  if (length(near)) cat("  close matches: ", paste(near, collapse = ", "), "\n", sep = "")
  cat("  served: ", paste(utils::head(avail, 30), collapse = ", "), "\n", sep = "")
  quit(status = 1L)
}
cat(sprintf("OK    '%s' is served\n", want))

# --- 4. a real constrained call -----------------------------------------
# One invented sentence, since the unit of analysis is a sentence. Should come
# back Positive, but the point of this check is that a constrained call works at
# all, not that the label is right.
UNIT <- "I am really looking forward to working with the students from the partner university."
tmpl <- read_prompt_template(cfg$paths$prompts_primary)
unit <- data.frame(text = UNIT, question = NA_character_, context = NA_character_,
                   stringsAsFactors = FALSE)
msg <- build_messages("primary", cb, cfg, unit, tmpl, cb$primary_labels)
schema <- if (isTRUE(cb$multi_label)) {
  multi_label_schema("codes", cb$primary_labels, cb$max_codes)
} else {
  label_schema("codes", cb$primary_labels)
}

model_cfg <- list(id = want, provider = pname, think = NULL)
cat("\n      sending one invented test passage ...\n")
r <- llm_label(cfg, model_cfg, msg$system, msg$user, schema, "codes",
               options = c(cfg$options, list(temperature = 0, seed = cfg$base_seed)),
               multi = isTRUE(cb$multi_label),
               cache_dir = cfg$paths$cache, refresh = TRUE)

if (!isTRUE(r$ok)) {
  cat("\nFAIL  the call did not return a usable answer:\n  ", r$error, "\n", sep = "")
  quit(status = 1L)
}

codes <- enforce_code_set(
  vapply(r$value, canonical_label, character(1),
         allowed = cb$primary_labels, cb = cb, USE.NAMES = FALSE), cb)

cat(sprintf("\nOK    %.1fs | output mode: %s\n", r$secs, r$output_mode %||% "schema"))
cat(sprintf("      codes      %s\n", code_set_string(codes)))
cat(sprintf("      confidence %s%s\n",
            ifelse(is.na(r$confidence), "NA", format(r$confidence)),
            if (isTRUE(r$confidence_out_of_range)) "  (OUT OF RANGE, stored as NA)" else ""))
cat(sprintf("      rationale  %s\n", substr(r$rationale, 1, 100)))

if (!identical(r$output_mode %||% "schema", "schema")) {
  cat("\nNOTE  this answer was NOT grammar-constrained: JSON was requested in the\n")
  cat("      prompt and validated afterwards. That is weaker than constrained\n")
  cat("      decoding -- an unparseable reply is recorded as a failed call rather\n")
  cat("      than guessed at -- and it means this model sees a slightly different\n")
  cat("      user message than schema-mode models do. Disclose that asymmetry.\n")
  cat("      If the server was upgraded, retry with structured_output: schema.\n")
}
cat("\nprovider looks usable. Set enabled: true for this model in config/run.yml.\n")
