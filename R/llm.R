# ---------------------------------------------------------------------------
# llm.R -- schema-constrained chat completions against local model servers
#
# Two provider types, both on-premises:
#   ollama   local daemon on this machine, /api/chat with `format`
#   openai   any OpenAI-compatible server, /chat/completions with
#            response_format = json_schema. This is how the FGCZ vLLM
#            deployment is reached (DeepSeek and other open-weights models).
#
# No cloud provider is supported on purpose. Journal passages are confidential
# personal data, so inference must stay inside the local machine or the FGCZ
# network -- see README "Data protection".
#
# Every call is keyed by everything that could change its answer (provider,
# model, prompt text, schema, sampling options, seed). A cache hit means the
# request is byte-identical to one already answered, so runs are interruptible.
# The cache holds reasoning traces that can quote passages: it is gitignored.
# ---------------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

#' Expand ${VAR} / $VAR references against the environment.
#' Endpoints differ per site and drift over time, so they belong in the
#' environment rather than in a committed config file.
expand_env <- function(x) {
  if (is.null(x) || !is.character(x) || !length(x)) return(x)
  m <- gregexpr("\\$\\{?[A-Za-z_][A-Za-z0-9_]*\\}?", x)
  regmatches(x, m) <- lapply(regmatches(x, m), function(v) {
    nm <- gsub("[${}]", "", v)
    val <- Sys.getenv(nm, unset = NA_character_)
    ifelse(is.na(val), v, val)
  })
  x
}

#' Resolve a provider definition from run.yml, expanding env references
get_provider <- function(cfg, name) {
  p <- cfg$providers[[name]]
  if (is.null(p)) {
    stop("model refers to unknown provider '", name, "'; defined providers: ",
         paste(names(cfg$providers), collapse = ", "), call. = FALSE)
  }
  p$name <- name
  p$type <- p$type %||% "ollama"
  # A literal base_url keeps the repo working out of the box; `base_url_env`
  # lets a site override it without editing the config, which matters because
  # the FGCZ policy warns these endpoints drift.
  if (!is.null(p$base_url_env)) {
    override <- Sys.getenv(p$base_url_env, "")
    if (nzchar(override)) p$base_url <- override
  }
  p$base_url <- sub("/+$", "", expand_env(p$base_url %||% ""))
  if (!nzchar(p$base_url) || grepl("^\\$", p$base_url)) {
    stop("provider '", name, "' has no usable base_url (got '", p$base_url, "').\n",
         "  Set the environment variable it refers to, in ~/.Renviron (no quotes,\n",
         "  no `export`), then restart R. The path depends on what is serving:\n",
         "    Open WebUI front end : FGCZ_LLM_BASE_URL=http://<vllm-host>:8080/api\n",
         "    raw vLLM             : FGCZ_LLM_BASE_URL=http://<host>:<port>/v1\n",
         "  Open WebUI also needs a token: Settings > Account > API keys, then\n",
         "    FGCZ_LLM_API_KEY=<token>", call. = FALSE)
  }
  p$timeout_s <- p$timeout_s %||% 900
  p$max_tries <- p$max_tries %||% 3
  p$api_key <- if (!is.null(p$api_key_env)) Sys.getenv(p$api_key_env, "") else ""
  p
}


# --- Reachability ---------------------------------------------------------

provider_probe <- function(p) {
  url <- if (identical(p$type, "ollama")) paste0(p$base_url, "/api/tags")
         else paste0(p$base_url, "/models")
  tryCatch({
    req <- httr2::request(url) |> httr2::req_timeout(15)
    if (nzchar(p$api_key)) req <- httr2::req_auth_bearer_token(req, p$api_key)
    j <- httr2::resp_body_json(httr2::req_perform(req))
    if (identical(p$type, "ollama")) {
      vapply(j$models, function(m) m$name, character(1))
    } else {
      vapply(j$data, function(m) m$id, character(1))
    }
  }, error = function(e) NULL)
}

ollama_version <- function(host) {
  tryCatch(httr2::resp_body_json(httr2::req_perform(
    httr2::req_timeout(httr2::request(paste0(sub("/+$", "", host), "/api/version")), 5)))$version,
    error = function(e) NULL)
}

#' Fail early and clearly if a server or a model is missing
#'
#' @param strict TRUE (default) aborts on an unreachable provider, so a typo or a
#'   stopped daemon is caught before a multi-hour run rather than after it. Set
#'   FALSE to warn instead: useful when a provider's answers are already cached
#'   and you are off its network -- cached calls need no server, and any call
#'   that genuinely needs one is recorded as failed rather than guessed at.
preflight <- function(cfg, strict = TRUE) {
  used <- unique(vapply(cfg$active_models,
                        function(m) m$provider %||% "local_ollama", character(1)))
  unreachable <- character(0)
  for (pn in used) {
    p <- get_provider(cfg, pn)
    avail <- provider_probe(p)
    if (is.null(avail)) {
      msg <- paste0("cannot reach provider '", pn, "' at ", p$base_url,
                    if (identical(p$type, "ollama")) "\n  start it with:  ollama serve"
                    else "\n  check you are on the network that serves this endpoint")
      if (isTRUE(strict)) {
        stop(msg, "\n  (--allow-unreachable proceeds anyway, serving whatever is cached)",
             call. = FALSE)
      }
      warning(msg, call. = FALSE)
      message(sprintf("provider %-14s UNREACHABLE -- only cached calls will succeed", pn))
      unreachable <- c(unreachable, pn)
      next
    }
    wanted <- vapply(Filter(function(m) (m$provider %||% "local_ollama") == pn,
                            cfg$active_models), function(m) m$id, character(1))
    absent <- setdiff(wanted, avail)
    if (length(absent)) {
      stop("provider '", pn, "' does not serve: ", paste(absent, collapse = ", "),
           "\n  it serves: ", paste(utils::head(avail, 20), collapse = ", "),
           if (identical(p$type, "ollama"))
             paste0("\n  pull with:  ", paste0("ollama pull ", absent, collapse = " ; ")) else "",
           call. = FALSE)
    }
    message(sprintf("provider %-14s %-6s %-40s ok (%s)", pn, p$type, p$base_url,
                    paste(wanted, collapse = ", ")))
  }
  invisible(list(ok = length(unreachable) == 0L, unreachable = unreachable))
}


# --- Response schemas -----------------------------------------------------

# Property order matters: constrained decoding emits fields in schema order, so
# `rationale` comes first and the model reasons before committing to a label.
# Enums contain ONLY valid labels -- no "None", no "Unsure" -- which makes an
# off-scheme answer unrepresentable rather than merely discouraged.
#
# `minimum`/`maximum` on confidence are declared because a bare
# {"type":"number"} is unconstrained: gemma4:e4b returned 5.0 on every call.
# Servers may not enforce the bounds, so llm_label() range-checks as well.

conf_prop <- function() {
  list(type = "number", minimum = 0, maximum = 1,
       description = "Confidence, a number between 0 and 1.")
}

#' Render a schema as plain instructions, for servers that do not enforce one
#'
#' Needed because some OpenAI-compatible front ends (Open WebUI, for one) proxy
#' requests and may drop `response_format` before it reaches the model. Then the
#' only way to get JSON is to ask for it and validate afterwards -- strictly
#' weaker than constrained decoding, so it is a fallback, never the default.
#' Which mode produced an answer is recorded per call.
json_instructions <- function(schema) {
  props <- schema$properties
  lines <- vapply(names(props), function(k) {
    p <- props[[k]]
    if (identical(p$type, "array")) {
      sprintf('  "%s": an array of 1 to %s distinct strings, each exactly one of [%s]',
              k, p$maxItems %||% "N",
              paste0('"', p$items$enum, '"', collapse = ", "))
    } else if (!is.null(p$enum)) {
      sprintf('  "%s": exactly one of [%s]', k, paste0('"', p$enum, '"', collapse = ", "))
    } else if (identical(p$type, "number")) {
      sprintf('  "%s": a number between %s and %s', k, p$minimum %||% 0, p$maximum %||% 1)
    } else {
      sprintf('  "%s": a string', k)
    }
  }, character(1))

  paste0("\nReply with a single JSON object and nothing else. No prose before or ",
         "after it, no markdown code fences. The object must have exactly these keys:\n",
         paste(lines, collapse = "\n"),
         "\nUse the label spellings exactly as given.")
}

#' Single-label schema (the subclassification stage)
label_schema <- function(field, labels) {
  props <- list(rationale = list(type = "string",
                  description = "One or two sentences justifying the choice."))
  props[[field]] <- list(type = "string", enum = labels)
  props$confidence <- conf_prop()
  list(type = "object", properties = props,
       required = c("rationale", field, "confidence"),
       additionalProperties = FALSE)
}

#' Multi-label schema (stage 1): an array of distinct labels
multi_label_schema <- function(field, labels, max_codes = 3L) {
  props <- list(
    rationale = list(type = "string",
                     description = "One or two sentences justifying the choice."))
  # NOTE: no `uniqueItems`. vLLM's grammar backend rejects it outright
  # ("Grammar error: Unimplemented keys: [\"uniqueItems\"]"), and duplicates are
  # removed by enforce_code_set() anyway. Keeping the key would force a
  # different schema per provider, which would mean the models were not all
  # answering the same constrained question -- a confound, not a convenience.
  props[[field]] <- list(
    type = "array",
    items = list(type = "string", enum = labels),
    minItems = 1L, maxItems = as.integer(min(max_codes, length(labels))),
    description = paste("Every code that applies to the passage, and only those.",
                        "Do not repeat a code."))
  props$confidence <- conf_prop()
  list(type = "object", properties = props,
       required = c("rationale", field, "confidence"),
       additionalProperties = FALSE)
}


# --- Cache ----------------------------------------------------------------

slug <- function(x) gsub("[^A-Za-z0-9._-]+", "_", x)

#' Parse a model's JSON answer, tolerating the wrappers that appear when the
#' schema is not enforced server-side: markdown fences, or leading prose.
parse_json_answer <- function(content) {
  if (!nzchar(content)) return(NULL)
  direct <- tryCatch(jsonlite::fromJSON(content), error = function(e) NULL)
  if (!is.null(direct) && is.list(direct)) return(direct)

  txt <- gsub("```[a-zA-Z]*", "", content, perl = TRUE)
  txt <- gsub("```", "", txt, fixed = TRUE)
  fenced <- tryCatch(jsonlite::fromJSON(trimws(txt)), error = function(e) NULL)
  if (!is.null(fenced) && is.list(fenced)) return(fenced)

  # Last resort: try every '{' as a candidate start and take the first
  # balanced object that parses. A single outermost-brace span is not enough --
  # reasoning models prepend prose, and the vLLM build on <vllm-host> prepends a
  # literal '{"' to an otherwise valid object, so the outermost span is invalid
  # while an inner one is fine.
  starts <- gregexpr("\\{", txt)[[1]]
  if (starts[1] == -1) return(NULL)
  chars <- strsplit(txt, "", fixed = TRUE)[[1]]

  for (s in starts) {
    depth <- 0L; in_str <- FALSE; esc <- FALSE; end <- NA_integer_
    for (i in seq(s, length(chars))) {
      ch <- chars[i]
      if (in_str) {
        if (esc) esc <- FALSE
        else if (ch == "\\") esc <- TRUE
        else if (ch == '"') in_str <- FALSE
      } else if (ch == '"') in_str <- TRUE
      else if (ch == "{") depth <- depth + 1L
      else if (ch == "}") {
        depth <- depth - 1L
        if (depth == 0L) { end <- i; break }
      }
    }
    if (!is.na(end)) {
      got <- tryCatch(jsonlite::fromJSON(paste(chars[s:end], collapse = "")),
                      error = function(e) NULL)
      if (!is.null(got) && is.list(got) && length(names(got))) return(got)
    }
  }
  NULL
}

#' Ask Ollama to unload a model from memory.
#'
#' Ollama keeps a model resident for five minutes after the last request. With a
#' multi-model panel that is actively harmful: the models in this panel are 9,
#' 13 and 22 GB, so on a 36 GB machine the third one cannot be loaded while the
#' first two are still warm, and Ollama signals that by answering HTTP 200 with
#' an empty body rather than by failing. Unloading each model when the run
#' finishes with it makes the panel independent of how much RAM the machine has.
#'
#' `keep_alive: 0` is Ollama's documented way to evict. Best-effort: a failure
#' here must never stop a run.
ollama_unload <- function(base_url, model) {
  tryCatch({
    httr2::req_perform(
      httr2::req_timeout(
        httr2::req_body_raw(
          httr2::request(paste0(sub("/+$", "", base_url), "/api/chat")),
          jsonlite::toJSON(list(model = model, keep_alive = 0, messages = list()),
                           auto_unbox = TRUE),
          "application/json"), 30))
    TRUE
  }, error = function(e) FALSE)
}

cache_key <- function(provider, model, system, user, schema, options, think) {
  digest::digest(list(provider = provider, model = model, system = system,
                      user = user, schema = schema, options = options,
                      think = think), algo = "sha1")
}


# --- The call ------------------------------------------------------------

#' One schema-constrained completion, cached, from either provider type
#'
#' @param field name of the answer property ("codes" or "cq_type")
#' @param multi TRUE when the answer is an array
#' @return list(ok, value, rationale, confidence, thinking, secs, cached, error)
llm_label <- function(cfg, model_cfg, system, user, schema, field,
                      options, multi = FALSE, cache_dir = "cache",
                      refresh = FALSE) {

  pname <- model_cfg$provider %||% "local_ollama"
  p <- get_provider(cfg, pname)
  model <- model_cfg$id
  think <- model_cfg$think

  key <- cache_key(pname, model, system, user, schema, options, think)
  cf <- file.path(cache_dir, slug(pname), slug(model), paste0(key, ".json"))

  if (!refresh && file.exists(cf)) {
    hit <- jsonlite::fromJSON(cf, simplifyVector = FALSE)
    hit$cached <- TRUE
    hit$value <- unlist(hit$value)
    return(hit)
  }

  # `structured_output`: "schema" trusts the server to enforce the schema,
  # "prompt" asks for JSON in words and validates here, "auto" tries the schema
  # first and falls back. Proxying front ends may silently drop response_format.
  mode_cfg <- p$structured_output %||% "schema"

  #' One HTTP attempt in a given mode. `strict` chooses native schema enforcement.
  attempt <- function(strict) {
    u <- user
    if (!strict) u <- paste0(user, "\n", json_instructions(schema))
    msgs <- list(list(role = "system", content = system),
                 list(role = "user", content = u))

    if (identical(p$type, "ollama")) {
      url <- paste0(p$base_url, "/api/chat")
      body <- list(model = model, stream = FALSE, options = options, messages = msgs)
      if (strict) body$format <- schema
      if (!is.null(think) && !identical(think, "")) body$think <- think
    } else {
      # OpenAI-compatible. Sampling params are top-level here, not nested in
      # `options`, and the schema goes in response_format.
      url <- paste0(p$base_url, "/chat/completions")
      body <- list(model = model, stream = FALSE, messages = msgs,
                   temperature = options$temperature %||% 0,
                   top_p = options$top_p %||% 1,
                   max_tokens = p$max_tokens %||% 1024L)
      # Not every proxy accepts `seed`; omit it rather than risk a 400.
      if (!isTRUE(p$omit_seed)) body$seed <- options$seed
      # Reasoning models served by vLLM accept an effort level. Left unset by
      # default -- it changes both latency and answers, so it belongs in the
      # manifest when used.
      eff <- model_cfg$reasoning_effort %||% p$reasoning_effort
      if (!is.null(eff) && nzchar(eff)) body$reasoning_effort <- eff
      if (strict) {
        body$response_format <- list(
          type = "json_schema",
          json_schema = list(name = "coding_decision", strict = TRUE, schema = schema))
      }
    }

    tryCatch({
      req <- httr2::request(url) |>
        httr2::req_body_raw(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
                            "application/json") |>
        httr2::req_timeout(p$timeout_s) |>
        httr2::req_retry(max_tries = p$max_tries,
                         is_transient = function(r)
                           httr2::resp_status(r) %in% c(408, 429, 500, 502, 503, 504)) |>
        httr2::req_error(is_error = function(r) FALSE)
      if (nzchar(p$api_key)) req <- httr2::req_auth_bearer_token(req, p$api_key)

      resp <- httr2::req_perform(req)
      if (httr2::resp_status(resp) != 200L) {
        return(list(ok = FALSE, kind = "http",
                    error = paste0("HTTP ", httr2::resp_status(resp), ": ",
                                   substr(httr2::resp_body_string(resp), 1, 300))))
      }
      j <- httr2::resp_body_json(resp, simplifyVector = FALSE)
      content <- if (identical(p$type, "ollama")) j$message$content %||% ""
                 else j$choices[[1]]$message$content %||% ""
      thinking <- if (identical(p$type, "ollama")) j$message$thinking %||% ""
                  else j$choices[[1]]$message$reasoning_content %||% ""

      # An EMPTY body on HTTP 200 is a different failure from a malformed one,
      # and conflating them cost a whole model's worth of data once: Ollama
      # answers 200 with no content when it cannot load a model, and 107 calls
      # were logged as "unparseable" when the real cause was that three local
      # models (9 + 13 + 22 GB) do not fit in 36 GB at once.
      if (!nzchar(trimws(content))) {
        return(list(ok = FALSE, kind = "semantic",
                    error = paste0("EMPTY response body (HTTP 200). For a local ",
                                   "model this usually means it could not be loaded -- ",
                                   "check `ollama ps` and whether the models in the ",
                                   "panel fit in RAM together.")))
      }

      parsed <- parse_json_answer(content)
      got <- if (!is.null(parsed)) parsed[[field]] else NULL
      if (is.null(got) || !length(got)) {
        return(list(ok = FALSE, kind = "semantic",
                    error = paste0("unparseable or schema-violating response: ",
                                   substr(content, 1, 300))))
      }
      conf <- suppressWarnings(as.numeric(parsed$confidence %||% NA))[1]
      bad <- !is.na(conf) && (conf < 0 || conf > 1)
      list(ok = TRUE,
           value = as.character(unlist(got)),
           rationale = as.character(parsed$rationale %||% "")[1],
           confidence = if (isTRUE(bad)) NA_real_ else conf,
           confidence_out_of_range = isTRUE(bad),
           thinking = as.character(thinking)[1],
           output_mode = if (strict) "schema" else "prompt")
    }, error = function(e) list(ok = FALSE, kind = "transport",
                                error = conditionMessage(e)))
  }

  # req_retry only covers HTTP-level transients. An empty or unparseable body
  # comes back as HTTP 200, and we saw exactly that intermittently from the FGCZ
  # vLLM -- the identical request succeeded on the next attempt. So retry
  # semantic failures too, which matters most in prompt mode where nothing
  # guarantees well-formed output.
  #
  # BUT NOT A TIMEOUT. Retrying a request that already ran out its full timeout
  # is the one case where retrying cannot help: a generation that hung will hang
  # again, and each retry costs another full timeout. Measured on qwen3:14b with
  # timeout_s 300 and 3 tries, a single hung unit consumed 900 s while the median
  # unit took 15 s -- one stall cost as much as sixty normal units.
  #
  # A timeout is recognised by how long the attempt took, not by the wording of
  # the curl message, so it keeps working if that wording changes. Failed calls
  # are never cached, so a stalled unit is simply picked up by the next run.
  timed_out <- function(res, secs) {
    identical(res$kind, "transport") && secs >= 0.9 * p$timeout_s
  }

  try_mode <- function(strict, tries) {
    last <- NULL
    for (k in seq_len(max(1L, tries))) {
      t0 <- Sys.time()
      last <- attempt(strict)
      secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      if (isTRUE(last$ok)) {
        last$attempts <- k
        return(last)
      }
      if (timed_out(last, secs)) {
        last$attempts <- k
        last$error <- paste0(last$error,
          sprintf(" [gave up after %.0f s; a timeout is not retried]", secs))
        return(last)
      }
    }
    last$attempts <- max(1L, tries)
    last
  }
  n_parse_tries <- as.integer(p$parse_retries %||% 3L)

  t0 <- Sys.time()
  res <- if (identical(mode_cfg, "prompt")) {
    try_mode(FALSE, n_parse_tries)
  } else {
    first <- try_mode(TRUE, if (identical(mode_cfg, "auto")) 1L else n_parse_tries)
    if (isTRUE(first$ok) || !identical(mode_cfg, "auto")) {
      first
    } else {
      second <- try_mode(FALSE, n_parse_tries)
      if (isTRUE(second$ok)) second else
        list(ok = FALSE, error = paste0("schema mode: ", first$error,
                                        " || prompt mode: ", second$error))
    }
  }

  res$secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
  res$cached <- FALSE
  res$provider <- pname
  res$model <- model

  # Only successful calls are cached: a transient failure must not be frozen in.
  if (isTRUE(res$ok)) {
    dir.create(dirname(cf), showWarnings = FALSE, recursive = TRUE)
    jsonlite::write_json(res, cf, auto_unbox = TRUE, null = "null")
  }
  res
}
