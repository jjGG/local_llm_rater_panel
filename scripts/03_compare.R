#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# 03_compare.R -- how do the local models compare with the human coders?
#
# DEFERRED, NOT PART OF THE CURRENT WORKFLOW (decided 2026-08-20). This round is
# LLM-only: use scripts/07_alpha.R, which needs no human labels. This script is
# kept working for when human data is generated again -- see RUNBOOK step 3b for
# what the repository preserves so that stays possible. Against the 2026-07-28
# sheet it will stop with "no shared unit_id", because that sheet is
# paragraph-level and current runs are sentence-level. That is correct.
#
#   Rscript scripts/03_compare.R                        # profile main
#   Rscript scripts/03_compare.R --profile consistency
#   Rscript scripts/03_compare.R --metric jaccard       # default: masi
#
# Reuses the validated coefficients in test_for_consistency/R/agreement.R --
# each model enters the analysis as one more rater.
#
# MULTI-LABEL. Since codebook v3 a passage may carry several codes, so a rating
# is a SET. Agreement is therefore reported two ways, because neither alone is
# enough:
#   set-valued alpha   one overall number, crediting partial overlap between
#                      sets via the MASI (default) or Jaccard difference
#   per-code alpha     one present/absent reliability per code, which says
#                      WHICH code the raters actually disagree about
# ---------------------------------------------------------------------------

repo <- normalizePath(file.path(dirname(sub("^--file=", "",
        commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])), ".."))
setwd(repo)
source("R/codebook.R")
source("test_for_consistency/R/agreement.R")
source("test_for_consistency/R/io.R")

a <- commandArgs(trailingOnly = TRUE)
getopt <- function(f, d = NULL) { i <- match(f, a); if (is.na(i) || i == length(a)) d else a[i + 1L] }
profile <- getopt("--profile", "main")
metric  <- match.arg(getopt("--metric", "masi"), c("masi", "jaccard"))
B_BOOT  <- as.integer(getopt("--boot", "2000"))
label   <- getopt("--label")   # matches 02_rate.R's --label / units-file stem

cb  <- load_codebook("config/codebook.yml")
cfg <- load_run_config("config/run.yml")
out_dir <- cfg$paths$results
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

stopifnot(setequal(cb$primary_labels, PRIMARY_LEVELS),
          setequal(cb$cq_labels, CQ_LEVELS))

SEP <- " | "
fmt <- function(x, d = 3) ifelse(is.na(x), "  -  ", sprintf(paste0("%.", d, "f"), x))
write_tbl <- function(df, name) {
  utils::write.csv(df, file.path(out_dir, name), row.names = FALSE, na = "")
  invisible(df)
}
canon <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    s <- trimws(strsplit(v, "|", fixed = TRUE)[[1]]); s <- unique(s[nzchar(s)])
    if (!length(s)) return(NA_character_)
    paste(s[order(match(s, cb$primary_labels))], collapse = SEP)
  }, character(1), USE.NAMES = FALSE)
}


# --- Load both sides ------------------------------------------------------

# 02_rate.R writes ratings_<label>_<profile>.csv, the label coming from the
# units-file stem. With one run present we can infer it; with several, the user
# has to say which, because silently picking one would compare the wrong data.
available <- list.files(out_dir, pattern = sprintf("^ratings_.*_%s\\.csv$", profile))
if (is.null(label)) {
  if (length(available) == 1L) {
    label <- sub(sprintf("^ratings_(.*)_%s\\.csv$", profile), "\\1", available)
    message("using the only run found for profile '", profile, "': label '", label, "'")
  } else if (length(available) > 1L) {
    stop("several runs exist for profile '", profile, "'. Pass --label <name>:\n  ",
         paste(sub(sprintf("^ratings_(.*)_%s\\.csv$", profile), "\\1", available),
               collapse = "\n  "), call. = FALSE)
  } else {
    stop("no LLM ratings for profile '", profile, "': run scripts/02_rate.R first",
         call. = FALSE)
  }
}
stem <- sprintf("%s_%s", label, profile)
llm_path <- file.path(out_dir, sprintf("ratings_%s.csv", stem))
if (!file.exists(llm_path)) {
  stop("not found: ", llm_path,
       if (length(available)) paste0("\n  available label(s) for this profile: ",
         paste(sub(sprintf("^ratings_(.*)_%s\\.csv$", profile), "\\1", available),
               collapse = ", ")) else "",
       call. = FALSE)
}
llm <- utils::read.csv(llm_path, stringsAsFactors = FALSE, na.strings = c("NA", ""))
if (!"codes" %in% names(llm)) {
  stop(llm_path, " has no `codes` column -- it predates codebook v3.\n",
       "  Re-run scripts/02_rate.R to regenerate it.", call. = FALSE)
}

human_path <- cfg$paths$human_long
if (!file.exists(human_path)) {
  stop("human ratings not found: ", human_path,
       "\n  run: cd test_for_consistency && Rscript reliability.R", call. = FALSE)
}
human <- utils::read.csv(human_path, stringsAsFactors = FALSE, na.strings = c("NA", ""))

human_raters <- sort(unique(human$rater))
llm_raters   <- sort(unique(llm$rater_id))

# --- Alignment check ------------------------------------------------------
common <- intersect(unique(human$unit_id), unique(llm$unit_id))
only_h <- setdiff(unique(human$unit_id), unique(llm$unit_id))
only_l <- setdiff(unique(llm$unit_id), unique(human$unit_id))

cat(sprintf("human units %d | llm units %d | shared %d\n",
            length(unique(human$unit_id)), length(unique(llm$unit_id)), length(common)))
if (!length(common)) {
  stop("no shared unit_id between the human sheet and the LLM ratings.\n\n",
       "  This script compares models against HUMAN labels, so it needs units that\n",
       "  both sides coded. The human sheet covers student(s): ",
       paste(sort(unique(human$student_id)), collapse = ", "), "\n",
       "  These ratings cover student(s): ",
       paste(sort(unique(llm$student_id)), collapse = ", "), "\n\n",
       "  If you meant to measure agreement AMONG THE MODELS (no humans needed),\n",
       "  which is what a `consistency` profile run is for, use:\n",
       "      Rscript scripts/07_alpha.R --label ", label,
       " --profile ", profile, "\n\n",
       "  If you did mean human comparison: both sides derive unit ids from row\n",
       "  order within (student_id, week), so the units file must list the same\n",
       "  passages in the same order as the alignment sheet.",
       call. = FALSE)
}
if (length(only_h) || length(only_l)) {
  warning("comparing on the ", length(common), " shared unit(s) only", call. = FALSE)
}


# --- Combined long table, both sides as code SETS -------------------------
# The human sheet is single-label, so its sets are singletons. A set-valued
# difference handles that correctly: {Positive} against {Positive, Self
# awareness} is partial agreement, not a flat miss.

combined <- rbind(
  data.frame(unit_id = human$unit_id, rater = human$rater, kind = "human",
             codes = canon(human$primary), cq_type = human$cq_type,
             # The humans coded at factor level only, so they have no item.
             cq_item = NA_character_,
             stringsAsFactors = FALSE),
  data.frame(unit_id = llm$unit_id, rater = llm$rater_id, kind = "llm",
             codes = canon(llm$codes), cq_type = llm$cq_type,
             cq_item = if ("cq_item" %in% names(llm)) llm$cq_item else NA_character_,
             stringsAsFactors = FALSE))
combined <- combined[combined$unit_id %in% common, ]
write_tbl(combined, sprintf("combined_long_%s.csv", stem))

mat <- function(raters = NULL, value = "codes") {
  d <- if (is.null(raters)) combined else combined[combined$rater %in% raters, ]
  ratings_matrix(d, value, units = common)
}
m_h <- mat(human_raters); m_l <- mat(llm_raters); m_a <- mat()

# Levels and the difference matrix are built ONCE from every set observed
# anywhere, so the measurement instrument is identical across all analyses and
# across bootstrap replicates.
all_sets <- sort(unique(stats::na.omit(as.character(m_a))))
delta <- if (metric == "masi") delta_masi(all_sets, SEP) else delta_jaccard(all_sets, SEP)
cat(sprintf("multi-label: %d distinct code set(s) observed | difference = %s\n",
            length(all_sets), metric))
n_multi <- sum(grepl("|", stats::na.omit(as.character(m_a)), fixed = TRUE))
cat(sprintf("%d of %d ratings carry more than one code\n",
            n_multi, sum(!is.na(as.character(m_a)))))


# --- Group-level, set-valued ---------------------------------------------

grp <- rbind(
  reliability_summary(m_h, all_sets, "humans only",     B_BOOT, delta = delta),
  reliability_summary(m_l, all_sets, "models only",     B_BOOT, delta = delta),
  reliability_summary(m_a, all_sets, "humans + models", B_BOOT, delta = delta))
grp$difference_metric <- metric
write_tbl(grp, sprintf("compare_group_%s.csv", stem))


# --- Per-code, the interpretable breakdown -------------------------------

per_code <- rbind(
  cbind(group = "humans only",     per_code_reliability(m_h, cb$primary_labels, SEP, B_BOOT)),
  cbind(group = "models only",     per_code_reliability(m_l, cb$primary_labels, SEP, B_BOOT)),
  cbind(group = "humans + models", per_code_reliability(m_a, cb$primary_labels, SEP, B_BOOT)))
write_tbl(per_code, sprintf("compare_per_code_%s.csv", stem))


# --- Each model against the human consensus ------------------------------
# Per code, so "agrees with the humans" is a claim about a specific code.

h_consensus <- do.call(cbind, lapply(cb$primary_labels, function(cd) {
  present <- vapply(seq_along(common), function(i) {
    v <- m_h[i, ]; v <- v[!is.na(v)]
    if (!length(v)) return(NA)
    sets <- lapply(strsplit(v, SEP, fixed = TRUE), trimws)
    mean(vapply(sets, function(s) cd %in% s, logical(1))) > 0.5
  }, logical(1))
  present
}))
colnames(h_consensus) <- cb$primary_labels

vs_human <- do.call(rbind, lapply(c(llm_raters, human_raters), function(r) {
  own <- m_a[, r]
  do.call(rbind, lapply(cb$primary_labels, function(cd) {
    mine <- vapply(own, function(v) {
      if (is.na(v)) return(NA)
      cd %in% trimws(strsplit(v, SEP, fixed = TRUE)[[1]])
    }, logical(1))
    ref <- h_consensus[, cd]
    k <- cohens_kappa(ifelse(mine, "present", "absent"),
                      ifelse(ref, "present", "absent"), c("present", "absent"))
    data.frame(rater = r, kind = if (r %in% human_raters) "human" else "llm",
               code = cd, n = k$n,
               percent_agreement = percent_agreement(
                 ifelse(mine, "present", "absent"), ifelse(ref, "present", "absent")),
               cohens_kappa = k$kappa, stringsAsFactors = FALSE)
  }))
}))
write_tbl(vs_human, sprintf("compare_vs_human_consensus_%s.csv", stem))


# --- Label usage ---------------------------------------------------------

usage <- do.call(rbind, lapply(colnames(m_a), function(r) {
  v <- m_a[, r]; v <- v[!is.na(v)]
  sets <- lapply(strsplit(v, SEP, fixed = TRUE), trimws)
  do.call(rbind, lapply(cb$primary_labels, function(cd) {
    data.frame(rater = r, kind = if (r %in% human_raters) "human" else "llm",
               code = cd, n = sum(vapply(sets, function(s) cd %in% s, logical(1))),
               proportion = mean(vapply(sets, function(s) cd %in% s, logical(1))),
               mean_codes_per_unit = mean(lengths(sets)),
               stringsAsFactors = FALSE)
  }))
}))
write_tbl(usage, sprintf("compare_label_usage_%s.csv", stem))


# --- The CQ subclassification: comparable or not? ------------------------
# The pilot assigned a factor on POSITIVE passages; the codebook assigns it on
# the codes in `subclassification.applies_to`. If those differ, the two sets of
# factor labels describe different passages and must not be pooled into one
# reliability figure -- so refuse rather than produce a meaningless number.
#
# Under codebook v4 the trigger is "Positive" again, matching the pilot, so this
# guard passes and the joint figure IS computed. It stays in place because the
# trigger is one editable line: flip it and the comparison must stop again.
#
# Comparison is at FACTOR level in both cases. The models also choose a
# questionnaire ITEM (24 labels in total = 20 items in 4 factors), which the
# humans never did, so item-level agreement is reported for the models alone.

PILOT_TRIGGER <- "Positive"
h_cq_units <- human$unit_id[!is.na(human$cq_type)]
h_trigger_ok <- all(vapply(h_cq_units, function(u) {
  any(grepl(PILOT_TRIGGER, human$primary[human$unit_id == u & !is.na(human$cq_type)], fixed = TRUE))
}, logical(1)))

cq_note <- if (!identical(sort(cb$cq_applies_to), sort(PILOT_TRIGGER)) && h_trigger_ok) {
  sprintf(paste0("NOT COMPARED. The human factor labels were assigned on '%s' passages ",
                 "(pilot protocol); codebook v%s assigns them on '%s'. The two describe ",
                 "different passages, so a joint reliability figure would be meaningless. ",
                 "Re-code the human side under v%s, or set subclassification.applies_to ",
                 "back to '%s', before comparing."),
          PILOT_TRIGGER, cb$version, paste(cb$cq_applies_to, collapse = "/"),
          cb$version, PILOT_TRIGGER)
} else NA_character_

cq_out <- NULL
if (is.na(cq_note)) {
  m_h_cq <- mat(human_raters, "cq_type"); m_l_cq <- mat(llm_raters, "cq_type")
  m_a_cq <- mat(NULL, "cq_type")
  cq_out <- rbind(
    reliability_summary(m_h_cq, CQ_LEVELS, "cq: humans only",     B_BOOT),
    reliability_summary(m_l_cq, CQ_LEVELS, "cq: models only",     B_BOOT),
    reliability_summary(m_a_cq, CQ_LEVELS, "cq: humans + models", B_BOOT))
  write_tbl(cq_out, sprintf("compare_cq_%s.csv", stem))
} else {
  write_tbl(data.frame(status = "not compared", reason = cq_note),
            sprintf("compare_cq_%s.csv", stem))
}

# The models' own factor agreement is still meaningful on its own terms.
m_l_cq <- mat(llm_raters, "cq_type")
if (any(!is.na(m_l_cq))) {
  cq_models <- reliability_summary(m_l_cq, CQ_LEVELS, "cq: models only", B_BOOT)
  write_tbl(cq_models, sprintf("compare_cq_models_%s.csv", stem))
} else cq_models <- NULL

# Item level: models only, since the humans never assigned items. Expect a much
# lower figure than at factor level -- 20 categories over a few dozen passages
# means most cells are empty, and two raters can agree on the factor while
# picking different items inside it. That gap is the point of reporting both.
cq_items <- NULL
if (!is.null(cb$cq_items) && any(!is.na(combined$cq_item))) {
  m_l_item <- mat(llm_raters, "cq_item")
  if (any(!is.na(m_l_item))) {
    cq_items <- reliability_summary(m_l_item, cb$cq_items$code,
                                    "cq item: models only", B_BOOT)
    write_tbl(cq_items, sprintf("compare_cq_items_%s.csv", stem))

    item_use <- do.call(rbind, lapply(colnames(m_l_item), function(r) {
      v <- factor(m_l_item[, r], levels = cb$cq_items$code)
      data.frame(rater = r, item = levels(v), factor = item_factor(levels(v), cb),
                 n = as.integer(table(v)), stringsAsFactors = FALSE)
    }))
    write_tbl(item_use, sprintf("compare_cq_item_usage_%s.csv", stem))
  }
}


# --- Console ------------------------------------------------------------

cat("\n================ SET-VALUED RELIABILITY (primary codes) ================\n")
print(grp[, c("analysis", "n_units_pairable", "n_raters", "n_categories",
              "prop_unanimous", "krippendorff_alpha", "alpha_ci_lower",
              "alpha_ci_upper")], row.names = FALSE, digits = 3)
cat(sprintf("difference function: %s (MASI is stricter than Jaccard)\n", metric))

cat("\n================ PER-CODE RELIABILITY ================\n")
print(per_code[, c("group", "code", "prevalence", "krippendorff_alpha",
                   "alpha_ci_lower", "alpha_ci_upper", "gwet_ac1")],
      row.names = FALSE, digits = 3)

cat("\n================ EACH RATER VS HUMAN CONSENSUS (kappa per code) ================\n")
print(stats::reshape(vs_human[, c("rater", "code", "cohens_kappa")],
                     idvar = "rater", timevar = "code", direction = "wide"),
      row.names = FALSE, digits = 3)

cat("\n================ CODE USAGE (proportion of units) ================\n")
print(stats::reshape(usage[, c("rater", "code", "proportion")],
                     idvar = "rater", timevar = "code", direction = "wide"),
      row.names = FALSE, digits = 2)
cat("\nmean codes per unit:\n")
print(unique(usage[, c("rater", "mean_codes_per_unit")]), row.names = FALSE, digits = 3)

cat("\n================ CQ SUBCLASSIFICATION ================\n")
if (!is.na(cq_note)) {
  cat(strwrap(cq_note, width = 78), sep = "\n")
  if (!is.null(cq_models)) {
    cat("\nModel-only factor agreement (valid on its own terms):\n")
    print(cq_models[, c("analysis", "n_units_pairable", "krippendorff_alpha",
                        "alpha_ci_lower", "alpha_ci_upper")],
          row.names = FALSE, digits = 3)
  }
} else {
  cat(sprintf("factor level, %d categories (trigger '%s' matches the pilot)\n",
              length(CQ_LEVELS), paste(cb$cq_applies_to, collapse = "/")))
  print(cq_out[, c("analysis", "n_units_pairable", "krippendorff_alpha",
                   "alpha_ci_lower", "alpha_ci_upper", "gwet_ac1")],
        row.names = FALSE, digits = 3)
}

if (!is.null(cq_items)) {
  cat(sprintf("\nitem level, %d categories (models only -- the humans coded factors):\n",
              nrow(cb$cq_items)))
  print(cq_items[, c("analysis", "n_units_pairable", "n_categories",
                     "prop_unanimous", "krippendorff_alpha",
                     "alpha_ci_lower", "alpha_ci_upper")],
        row.names = FALSE, digits = 3)
  n_used <- length(unique(stats::na.omit(combined$cq_item)))
  cat(sprintf("%d of %d items were ever used. Item-level alpha rests on thin\n",
              n_used, nrow(cb$cq_items)))
  cat("cells -- read the interval, not the point estimate.\n")
}

cat(sprintf("\nwrote tables to %s (profile '%s')\n", out_dir, profile))
cat("Reminder: alpha and AC1 diverge sharply when one category dominates.\n")
