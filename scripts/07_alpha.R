#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# 07_alpha.R -- Krippendorff's alpha across the LLM raters, no humans needed
#
#   Rscript scripts/07_alpha.R --ratings results/bak/ratings_synth5_main.csv
#   Rscript scripts/07_alpha.R --label synth5
#   Rscript scripts/07_alpha.R --label synth5 --metric jaccard
#   Rscript scripts/07_alpha.R --label synth5 --by rater_id     # replicates as raters
#
# Use 03_compare.R instead when the units also have human labels; this script is
# for "how well do the models agree with each other", which is a question you
# can ask of any run.
#
# The coefficients come from test_for_consistency/R/agreement.R, validated by
# validate_agreement.R (36 checks, cross-checked against irrCAC). Nothing is
# reimplemented here -- this script only assembles the matrices and reports.
#
# MULTI-LABEL. A rating is a SET of codes, so a plain nominal alpha scores
# {Positive} against {Positive, Self awareness} as a total miss. Three numbers
# are therefore reported:
#   nominal   exact-set match only -- the strict floor
#   MASI      credits partial overlap, ranking nested sets closer than crossing
#   Jaccard   credits partial overlap, more forgiving than MASI
# MASI is the default headline. Jaccard will always be >= MASI, so pick one in
# advance and report it consistently.
#
# Options:
#   --ratings PATH   ratings csv to analyse (overrides --label)
#   --label NAME     results/ratings_<NAME>_<profile>.csv
#   --profile NAME   default main
#   --metric masi|jaccard   headline difference function (default masi)
#   --by model|rater_id     what counts as a rater (default model)
#   --boot N         bootstrap replicates for the headline CIs (default 2000)
#   --only a,b       analyse ONLY these raters
#   --exclude a,b    analyse everything except these raters
#   --reps 1,2,3     analyse only these replicate numbers (for like-for-like
#                    comparison when models were run with different counts)
#
# A ratings file keeps every model that was ever run against those units, which
# is right -- the data is the data. But agreement is a property of a PANEL, so
# when the panel changes, the analysis has to be restricted to its members or
# the reported alpha belongs to a panel nobody is using. Hence --only/--exclude
# rather than deleting rows from the ratings file.
# ---------------------------------------------------------------------------

repo <- normalizePath(file.path(dirname(sub("^--file=", "",
        commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])), ".."))
setwd(repo)
source("R/codebook.R")
source("test_for_consistency/R/agreement.R")

a <- commandArgs(trailingOnly = TRUE)
getopt <- function(f, d = NULL) { i <- match(f, a); if (is.na(i) || i == length(a)) d else a[i + 1L] }

profile   <- getopt("--profile", "main")
metric    <- match.arg(getopt("--metric", "masi"), c("masi", "jaccard"))
by_col    <- match.arg(getopt("--by", "model"), c("model", "rater_id"))
B_BOOT    <- as.integer(getopt("--boot", "2000"))
ratings_p <- getopt("--ratings")
label     <- getopt("--label")

cb  <- load_codebook("config/codebook.yml")
cfg <- load_run_config("config/run.yml")
out_dir <- cfg$paths$results
SEP <- " | "

# --- Locate the ratings ---------------------------------------------------
if (is.null(ratings_p)) {
  avail <- list.files(out_dir, pattern = sprintf("^ratings_.*_%s\\.csv$", profile))
  if (is.null(label)) {
    if (length(avail) == 1L) {
      label <- sub(sprintf("^ratings_(.*)_%s\\.csv$", profile), "\\1", avail)
    } else {
      stop(if (length(avail))
             paste0("pass --label <name> or --ratings <path>. Labels found: ",
                    paste(sub(sprintf("^ratings_(.*)_%s\\.csv$", profile), "\\1", avail),
                          collapse = ", "))
           else "no ratings in results/ -- pass --ratings <path>", call. = FALSE)
    }
  }
  ratings_p <- file.path(out_dir, sprintf("ratings_%s_%s.csv", label, profile))
}
if (!file.exists(ratings_p)) stop("not found: ", ratings_p, call. = FALSE)
if (is.null(label)) {
  label <- sub("^ratings_", "", tools::file_path_sans_ext(basename(ratings_p)))
  label <- sub(sprintf("_%s$", profile), "", label)
}
stem <- sprintf("%s_%s_%s", label, profile, metric)

r <- utils::read.csv(ratings_p, stringsAsFactors = FALSE, na.strings = c("NA", ""))
need <- c("unit_id", "codes", by_col)
if (!all(need %in% names(r))) {
  stop(ratings_p, " is missing column(s): ", paste(setdiff(need, names(r)), collapse = ", "),
       "\n  a pre-v3 file has `primary` instead of `codes` -- re-run 02_rate.R",
       call. = FALSE)
}

# Restrict to the panel actually under analysis, before anything is computed.
split_csv <- function(x) if (is.null(x)) NULL else trimws(strsplit(x, ",")[[1]])
only <- split_csv(getopt("--only")); excl <- split_csv(getopt("--exclude"))
if (!is.null(only) || !is.null(excl)) {
  present <- unique(r[[by_col]])
  unknown <- setdiff(c(only, excl), present)
  if (length(unknown)) {
    stop("no such rater in ", basename(ratings_p), ": ", paste(unknown, collapse = ", "),
         "\n  present: ", paste(present, collapse = ", "), call. = FALSE)
  }
  if (!is.null(only)) r <- r[r[[by_col]] %in% only, , drop = FALSE]
  if (!is.null(excl)) r <- r[!r[[by_col]] %in% excl, , drop = FALSE]
  if (length(unique(r[[by_col]])) < 2L) {
    stop("agreement needs at least two raters; the filter left ",
         length(unique(r[[by_col]])), call. = FALSE)
  }
  cat(sprintf("PANEL    restricted to %d rater(s): %s\n",
              length(unique(r[[by_col]])), paste(sort(unique(r[[by_col]])), collapse = ", ")))
  # The filter belongs in the output file names, so a restricted analysis can
  # never be mistaken for the full one.
  stem <- sprintf("%s_p%d", stem, length(unique(r[[by_col]])))
}

# --reps restricts to specific replicate numbers. Needed because a model run
# with fewer replicates than the others is NOT directly comparable on
# prop_units_stable: with fewer runs there are fewer chances to disagree, so the
# "identical every time" proportion is biased upward. Krippendorff's alpha is
# designed to be unbiased in the number of observers, so alpha IS comparable --
# but only its interval tells you how much less precise the smaller one is.
keep_reps <- getopt("--reps")
if (!is.null(keep_reps) && "replicate" %in% names(r)) {
  want <- suppressWarnings(as.integer(trimws(strsplit(keep_reps, ",")[[1]])))
  if (anyNA(want)) stop("--reps takes integers, e.g. --reps 1,2,3", call. = FALSE)
  missing_r <- setdiff(want, unique(r$replicate))
  if (length(missing_r)) {
    stop("replicate(s) not in the data: ", paste(missing_r, collapse = ", "),
         "\n  present: ", paste(sort(unique(r$replicate)), collapse = ", "),
         call. = FALSE)
  }
  r <- r[r$replicate %in% want, , drop = FALSE]
  cat(sprintf("REPS     restricted to %s\n", paste(sort(want), collapse = ", ")))
  stem <- sprintf("%s_r%s", stem, paste(sort(want), collapse = ""))
}

cat(sprintf("source   %s\n", ratings_p))
cat(sprintf("raters   %d (%s)\n", length(unique(r[[by_col]])), by_col))
if ("replicate" %in% names(r) && "model" %in% names(r)) {
  per_model_reps <- tapply(r$replicate, r$model, function(x) length(unique(x)))
  if (length(unique(per_model_reps)) > 1L) {
    cat("NOTE     models have UNEQUAL replicate counts: ",
        paste(sprintf("%s=%d", names(per_model_reps), per_model_reps), collapse = ", "),
        "\n", sep = "")
    cat("         Self-consistency alpha is still comparable (alpha is unbiased in\n")
    cat("         the number of observers) but `prop_units_stable` is NOT -- fewer\n")
    cat("         replicates means fewer chances to disagree. For a like-for-like\n")
    cat(sprintf("         reading, re-run with --reps 1,..,%d\n", min(per_model_reps)))
  }
}
cat(sprintf("units    %d\n", length(unique(r$unit_id))))
n_fail <- sum(!is.na(r$ok) & !r$ok)
if (n_fail) cat(sprintf("WARNING  %d failed call(s) enter as missing values\n", n_fail))

write_tbl <- function(df, name) {
  utils::write.csv(df, file.path(out_dir, name), row.names = FALSE, na = "")
  invisible(df)
}

# --- Build the units x raters matrices ------------------------------------
canon <- function(x) vapply(x, function(v) {
  if (is.na(v)) return(NA_character_)
  s <- unique(trimws(strsplit(v, "|", fixed = TRUE)[[1]])); s <- s[nzchar(s)]
  if (!length(s)) return(NA_character_)
  paste(s[order(match(s, cb$primary_labels))], collapse = SEP)
}, character(1), USE.NAMES = FALSE)

r$codes <- canon(r$codes)
units  <- unique(r$unit_id)
raters <- sort(unique(r[[by_col]]))

#' Modal value, ties broken by first occurrence; NA when nothing is present.
modal_chr <- function(v) {
  v <- v[!is.na(v)]
  if (!length(v)) return(NA_character_)
  tb <- sort(table(v), decreasing = TRUE)
  names(tb)[1]
}

#' Fill a units x raters matrix.
#'
#' With --by model and several replicates there are multiple rows per
#' (unit, rater). Assigning them all to one cell would silently keep whichever
#' happened to be last, so replicates are collapsed to the model's MODAL answer
#' and the collapse is reported. Pass --by rater_id to keep them separate.
mk <- function(col, data = r, rcol = by_col, rset = raters, uset = units) {
  m <- matrix(NA_character_, length(uset), length(rset), dimnames = list(uset, rset))
  key <- paste(data$unit_id, data[[rcol]], sep = "\r")
  vals <- as.character(data[[col]])
  if (anyDuplicated(key)) {
    agg <- tapply(vals, key, modal_chr)
    parts <- do.call(rbind, strsplit(names(agg), "\r", fixed = TRUE))
    m[cbind(match(parts[, 1], uset), match(parts[, 2], rset))] <- unname(agg)
  } else {
    m[cbind(match(data$unit_id, uset), match(data[[rcol]], rset))] <- vals
  }
  m
}

n_rep <- if ("replicate" %in% names(r)) length(unique(r$replicate)) else 1L
if (n_rep > 1L && by_col == "model") {
  cat(sprintf("NOTE     %d replicates per model collapsed to each model's MODAL code set.\n",
              n_rep))
  cat("         Between-model agreement below is therefore computed on each model's\n")
  cat("         consensus answer, which removes its own run-to-run noise.\n")
  cat("         Pass --by rater_id to treat every replicate as a separate rater.\n")
}

M <- mk("codes")
Q <- if ("cq_type" %in% names(r)) mk("cq_type") else NULL
# Item level (codebook v4): 20 questionnaire items rolling up into the 4 factors
# in Q. Present only in runs made after the switch to `level: item`.
I <- if ("cq_item" %in% names(r) && any(!is.na(r$cq_item))) mk("cq_item") else NULL

# Levels and difference matrices are built ONCE from every set observed, so the
# measurement instrument is identical across every analysis below and across
# bootstrap replicates.
sets <- sort(unique(stats::na.omit(as.character(M))))
D <- list(nominal = delta_nominal(sets),
          masi    = delta_masi(sets, SEP),
          jaccard = delta_jaccard(sets, SEP))
d_head <- D[[metric]]

n_multi <- sum(grepl("|", stats::na.omit(as.character(M)), fixed = TRUE))
cat(sprintf("code sets observed: %d | %d of %d ratings carry >1 code (%.0f%%)\n\n",
            length(sets), n_multi, sum(!is.na(M)), 100 * n_multi / sum(!is.na(M))))


# --- 1. Headline: overall alpha under all three difference functions ------
overall <- do.call(rbind, lapply(names(D), function(nm) {
  ka <- krippendorff_alpha(M, sets, D[[nm]])
  ci <- bootstrap_ci(M, stat_alpha_delta(D[[nm]]), sets, B = B_BOOT)
  data.frame(difference = nm, n_units = ka$n_units, n_raters = ncol(M),
             n_pairable_values = ka$n_pairable_values,
             krippendorff_alpha = ka$alpha,
             ci_lower = ci$lower, ci_upper = ci$upper, boot_se = ci$se,
             interpretation = interpret_coefficient(ka$alpha),
             stringsAsFactors = FALSE)
}))
un <- unanimity(M)
write_tbl(overall, sprintf("alpha_overall_%s.csv", stem))


# --- 2. Per-code: which code do they disagree about? ---------------------
per_code <- per_code_reliability(M, cb$primary_labels, SEP, B = B_BOOT)
write_tbl(per_code, sprintf("alpha_per_code_%s.csv", stem))


# --- 3. Pairwise between raters (point estimates only) -------------------
# CIs are omitted here on purpose: with k raters there are k(k-1)/2 pairs, and
# bootstrapping each one invites reading noise as signal.
pairs <- utils::combn(raters, 2L, simplify = FALSE)
pairwise <- do.call(rbind, lapply(pairs, function(p) {
  mm <- M[, p, drop = FALSE]
  data.frame(rater_a = p[1], rater_b = p[2],
             n_joint = sum(stats::complete.cases(mm)),
             exact_set_agreement = percent_agreement(mm[, 1], mm[, 2]),
             alpha_nominal = krippendorff_alpha(mm, sets, D$nominal)$alpha,
             alpha_headline = krippendorff_alpha(mm, sets, d_head)$alpha,
             stringsAsFactors = FALSE)
}))
pairwise <- pairwise[order(-pairwise$alpha_headline), ]
write_tbl(pairwise, sprintf("alpha_pairwise_%s.csv", stem))


# --- 4. Leave-one-rater-out: who drags agreement down? ------------------
loo <- do.call(rbind, lapply(raters, function(x) {
  keep <- setdiff(raters, x)
  ka <- krippendorff_alpha(M[, keep, drop = FALSE], sets, d_head)
  data.frame(dropped = x, n_remaining = length(keep),
             alpha_without = ka$alpha, stringsAsFactors = FALSE)
}))
alpha_all <- krippendorff_alpha(M, sets, d_head)$alpha
loo$delta_vs_all <- loo$alpha_without - alpha_all
loo <- loo[order(-loo$delta_vs_all), ]
write_tbl(loo, sprintf("alpha_leave_one_out_%s.csv", stem))


# --- 5. Breakdowns, descriptive only -----------------------------------
breakdown <- function(group_col) {
  if (!group_col %in% names(r)) return(NULL)
  key <- unique(r[, c("unit_id", group_col)])
  key <- key[match(units, key$unit_id), ]
  do.call(rbind, lapply(sort(unique(key[[group_col]])), function(g) {
    idx <- which(key[[group_col]] == g)
    mm <- M[idx, , drop = FALSE]
    ka <- krippendorff_alpha(mm, sets, d_head)
    data.frame(group = group_col, value = as.character(g), n_units = nrow(mm),
               prop_unanimous = unanimity(mm)$proportion,
               krippendorff_alpha = ka$alpha,
               underpowered = nrow(mm) < 15L, stringsAsFactors = FALSE)
  }))
}
groups <- rbind(breakdown("student_id"), breakdown("week"))
if (!is.null(groups)) write_tbl(groups, sprintf("alpha_by_group_%s.csv", stem))


# --- 6. The CQ subclassification ---------------------------------------
cq <- NULL
if (!is.null(Q) && any(!is.na(Q))) {
  kq <- krippendorff_alpha(Q, cb$cq_labels)
  ciq <- bootstrap_ci(Q, stat_alpha, cb$cq_labels, B = B_BOOT)
  cq <- data.frame(n_units_pairable = kq$n_units, n_raters = ncol(Q),
                   krippendorff_alpha = kq$alpha,
                   ci_lower = ciq$lower, ci_upper = ciq$upper,
                   gwet_ac1 = gwet_ac1(Q, cb$cq_labels)$ac1,
                   prop_unanimous = unanimity(Q)$proportion,
                   stringsAsFactors = FALSE)
  write_tbl(cq, sprintf("alpha_cq_%s.csv", stem))
  cq_marg <- marginal_table(Q, cb$cq_labels)
  write_tbl(cq_marg, sprintf("alpha_cq_marginals_%s.csv", stem))
}

# Item level. Reported next to the factor level on purpose: the gap between the
# two is the share of disagreement that is merely about WHICH item inside an
# agreed factor, which a factor-only figure hides and an item-only figure
# overstates as total disagreement.
cq_item <- NULL
if (!is.null(I) && !is.null(cb$cq_items)) {
  lv <- cb$cq_items$code
  ki <- krippendorff_alpha(I, lv)
  cii <- bootstrap_ci(I, stat_alpha, lv, B = B_BOOT)
  cq_item <- data.frame(level = "item", n_categories = length(lv),
                        n_items_used = length(unique(stats::na.omit(as.character(I)))),
                        n_units_pairable = ki$n_units, n_raters = ncol(I),
                        krippendorff_alpha = ki$alpha,
                        ci_lower = cii$lower, ci_upper = cii$upper,
                        prop_unanimous = unanimity(I)$proportion,
                        stringsAsFactors = FALSE)
  write_tbl(cq_item, sprintf("alpha_cq_item_%s.csv", stem))

  item_marg <- do.call(rbind, lapply(colnames(I), function(rr) {
    v <- factor(I[, rr], levels = lv)
    data.frame(rater = rr, item = lv, factor = item_factor(lv, cb),
               n = as.integer(table(v)),
               proportion = as.numeric(table(v)) / max(1L, sum(!is.na(I[, rr]))),
               stringsAsFactors = FALSE)
  }))
  write_tbl(item_marg, sprintf("alpha_cq_item_marginals_%s.csv", stem))

  # Which items nobody ever reaches for. A category the coders never use is not
  # evidence about the coders; it is evidence the category does not apply here.
  never <- setdiff(lv, unique(stats::na.omit(as.character(I))))
  if (length(never)) {
    write_tbl(data.frame(item = never, factor = item_factor(never, cb),
                         course_item = cb$cq_items$course_item[match(never, cb$cq_items$code)]),
              sprintf("alpha_cq_items_unused_%s.csv", stem))
  }
}


# --- 7. Self-consistency: does each model agree with ITSELF? ------------
# Only meaningful when a profile ran several replicates. Each model's own
# replicates become the raters, so this measures run-to-run stability and is
# entirely separate from whether the models agree with each other.
selfcon <- NULL
if (n_rep > 1L && "replicate" %in% names(r) && "model" %in% names(r)) {
  reps <- sort(unique(r$replicate))
  selfcon <- do.call(rbind, lapply(sort(unique(r$model)), function(mid) {
    d <- r[r$model == mid, ]
    # Only this model's OWN replicates. A model run with 3 replicates in a file
    # whose other models have 5 would otherwise be credited with 5, two of them
    # entirely missing -- the alpha would still be right (missing cells are
    # handled natively) but the reported rater count would be a fiction.
    own <- sort(unique(d$replicate))
    mm <- mk("codes", data = d, rcol = "replicate", rset = as.character(own))
    ka_h <- krippendorff_alpha(mm, sets, d_head)
    ci <- bootstrap_ci(mm, stat_alpha_delta(d_head), sets, B = B_BOOT)
    n_distinct_sets <- apply(mm, 1L, function(v) length(unique(v[!is.na(v)])))
    data.frame(model = mid, n_replicates = length(own), n_units = ka_h$n_units,
               prop_units_stable = unanimity(mm)$proportion,
               mean_distinct_sets = mean(n_distinct_sets),
               alpha_nominal = krippendorff_alpha(mm, sets, D$nominal)$alpha,
               alpha_headline = ka_h$alpha,
               ci_lower = ci$lower, ci_upper = ci$upper,
               n_failed = sum(!is.na(d$ok) & !d$ok),
               stringsAsFactors = FALSE)
  }))
  selfcon <- selfcon[order(-selfcon$alpha_headline), ]
  write_tbl(selfcon, sprintf("alpha_self_consistency_%s.csv", stem))

  # Same for the CQ factor, where instability was expected to be worse.
  if (!is.null(Q)) {
    selfcon_cq <- do.call(rbind, lapply(sort(unique(r$model)), function(mid) {
      d <- r[r$model == mid, ]
      own <- sort(unique(d$replicate))
      qq <- mk("cq_type", data = d, rcol = "replicate", rset = as.character(own))
      if (all(is.na(qq))) return(NULL)
      ka <- krippendorff_alpha(qq, cb$cq_labels)
      data.frame(model = mid, n_replicates = length(own),
                 n_units_pairable = ka$n_units,
                 prop_units_stable = unanimity(qq)$proportion,
                 krippendorff_alpha = ka$alpha, stringsAsFactors = FALSE)
    }))
    if (!is.null(selfcon_cq)) {
      write_tbl(selfcon_cq, sprintf("alpha_self_consistency_cq_%s.csv", stem))
    }
  } else selfcon_cq <- NULL
} else selfcon_cq <- NULL


# --- Console ----------------------------------------------------------
cat("================ OVERALL (primary code sets) ================\n")
print(overall[, c("difference", "n_units", "n_raters", "krippendorff_alpha",
                  "ci_lower", "ci_upper", "interpretation")],
      row.names = FALSE, digits = 3)
cat(sprintf("\nall %d raters identical on %d/%d units (%.0f%%)\n",
            ncol(M), un$n_unanimous, un$n_units, 100 * un$proportion))
cat(sprintf("headline = %s, bootstrap B = %d over coding units\n", metric, B_BOOT))

cat("\n================ PER CODE ================\n")
print(per_code, row.names = FALSE, digits = 3)
cat("AC1 sits beside alpha because a skewed code inflates alpha's chance term.\n")

cat("\n================ PAIRWISE (best to worst) ================\n")
print(pairwise, row.names = FALSE, digits = 3)

cat("\n================ LEAVE ONE OUT ================\n")
cat(sprintf("alpha with all %d raters = %.3f\n", ncol(M), alpha_all))
print(loo, row.names = FALSE, digits = 3)
cat("A large positive delta_vs_all means agreement improves without that rater.\n")

if (!is.null(groups)) {
  cat("\n================ BY STUDENT / WEEK (descriptive) ================\n")
  print(groups, row.names = FALSE, digits = 3)
  cat("Alpha on a handful of units is noise; treat `underpowered` rows as such.\n")
}

if (!is.null(cq)) {
  cat("\n================ CQ FACTOR ================\n")
  print(cq, row.names = FALSE, digits = 3)
  cat("\nfactor share per rater:\n")
  print(stats::reshape(cq_marg[, c("rater", "label", "proportion")],
                       idvar = "rater", timevar = "label", direction = "wide"),
        row.names = FALSE, digits = 2)
  cat("\nIf one factor dominates every rater, a high alpha can reflect a shared\n")
  cat("default rather than genuine discrimination -- read it with the shares.\n")
}

if (!is.null(cq_item)) {
  cat("\n================ CQ ITEM (20 categories, rolling up into the 4 factors) ================\n")
  print(cq_item, row.names = FALSE, digits = 3)
  if (!is.null(cq)) {
    cat(sprintf("\nfactor alpha %.3f vs item alpha %.3f: the gap is disagreement about\n",
                cq$krippendorff_alpha, cq_item$krippendorff_alpha))
    cat("WHICH item inside an agreed factor.\n")
  }
  cat(sprintf("\n%d of %d items ever used. Most-used:\n",
              cq_item$n_items_used, nrow(cb$cq_items)))
  tot <- stats::aggregate(n ~ item + factor, data = item_marg, FUN = sum)
  print(head(tot[order(-tot$n), ], 8), row.names = FALSE)
  cat("\nWith 20 categories over this many passages most cells are empty, so the\n")
  cat("interval matters far more than the point estimate.\n")
}

if (!is.null(selfcon)) {
  cat("\n================ SELF-CONSISTENCY (each model vs its own replicates) ================\n")
  print(selfcon, row.names = FALSE, digits = 3)
  cat("\nThis is run-to-run stability, NOT agreement with other models. A model\n")
  cat("cannot agree with anyone more reliably than it agrees with itself, so these\n")
  cat("values are the ceiling on the between-model numbers above.\n")
  if (!is.null(selfcon_cq)) {
    cat("\nSame thing for the CQ factor:\n")
    print(selfcon_cq, row.names = FALSE, digits = 3)
  }
}

cat(sprintf("\nwrote alpha_*_%s.csv to %s\n", stem, out_dir))
