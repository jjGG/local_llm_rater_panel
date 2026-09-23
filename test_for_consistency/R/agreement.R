# ---------------------------------------------------------------------------
# agreement.R -- inter-rater reliability coefficients, base R only
#
# Deliberately dependency-free so collaborators can source this file and
# reproduce every number without installing anything. Cross-validated against
# irr / irrCAC and against Krippendorff's published worked example in
# validate_agreement.R.
#
# Rating input convention throughout: a data.frame or matrix with one row per
# coding unit and one column per rater. Cell values are character labels drawn
# from `levels`, or NA when that rater assigned no value to that unit (e.g. a
# CQ type is only assigned when the primary code is "Positive").
# ---------------------------------------------------------------------------


#' Coerce a units x raters table to a character matrix
as_ratings <- function(ratings) {
  m <- as.matrix(ratings)
  mode(m) <- "character"
  m
}

#' Labels actually observed anywhere in the table, sorted
observed_levels <- function(ratings) {
  v <- as.character(as_ratings(ratings))
  sort(unique(v[!is.na(v)]))
}


# --- Krippendorff's alpha --------------------------------------------------

#' Coincidence matrix (Krippendorff 2011)
#'
#' Counts every ordered pair of distinct raters within a unit, weighting each
#' unit by 1/(m_u - 1) where m_u is the number of raters who actually rated
#' that unit. Units with fewer than 2 values carry no pairable information and
#' are skipped -- this is what lets alpha handle missing data natively.
coincidence_matrix <- function(ratings, levels) {
  r <- as_ratings(ratings)
  L <- length(levels)
  o <- matrix(0, L, L, dimnames = list(levels, levels))

  for (u in seq_len(nrow(r))) {
    v <- r[u, ]
    v <- v[!is.na(v)]
    m <- length(v)
    if (m < 2L) next
    idx <- match(v, levels)
    if (anyNA(idx)) {
      stop("unit ", u, " contains a label absent from `levels`: ",
           paste(unique(v[is.na(idx)]), collapse = ", "))
    }
    nk <- tabulate(idx, nbins = L)
    # ordered pairs (i != j) with values (c, k): n_c * n_k - delta_ck * n_c
    o <- o + (outer(nk, nk) - diag(nk, nrow = L)) / (m - 1)
  }
  o
}

#' Krippendorff's alpha
#'
#' NOTE ON REFERENCE IMPLEMENTATIONS: do not check these numbers against
#' `irr::kripp.alpha` when there are 3+ raters and no missing cells -- it
#' weights units by 1 instead of (raters - 1) in that branch and returns a
#' slightly low alpha. `irrCAC::krippen.alpha.raw` agrees with this function
#' exactly. See section 5 of validate_agreement.R.
#'
#' alpha = 1 - D_o / D_e with
#'   D_o = sum_ck o_ck d_ck / n
#'   D_e = sum_ck n_c n_k d_ck / (n (n - 1))
#'
#' @param delta optional L x L difference matrix over `levels`, zero on the
#'   diagonal and symmetric. Defaults to the nominal function (0 if equal,
#'   else 1). Pass `delta_jaccard()` or `delta_masi()` when each "level" is a
#'   SET of codes, so that partially overlapping answers count as partial
#'   agreement instead of as a flat miss -- which is what multi-label coding
#'   needs.
#' @return list(alpha, n_pairable_values, n_units, D_o, D_e, coincidence)
krippendorff_alpha <- function(ratings, levels = NULL, delta = NULL) {
  if (is.null(levels)) levels <- observed_levels(ratings)
  o <- coincidence_matrix(ratings, levels)

  n <- sum(o)                 # total pairable values
  nc <- rowSums(o)            # marginal frequencies
  d2 <- if (is.null(delta)) delta_nominal(levels) else validate_delta(delta, levels)

  if (n < 2 || sum(nc^2) == n^2) {
    # no pairable data, or every value identical -> alpha undefined
    return(list(alpha = NA_real_, n_pairable_values = n,
                n_units = n_pairable_units(ratings), D_o = NA_real_,
                D_e = NA_real_, coincidence = o))
  }

  D_o <- sum(o * d2) / n
  D_e <- sum(outer(nc, nc) * d2) / (n * (n - 1))

  list(alpha = 1 - D_o / D_e, n_pairable_values = n,
       n_units = n_pairable_units(ratings), D_o = D_o, D_e = D_e,
       coincidence = o)
}

#' Number of units carrying at least two ratings
n_pairable_units <- function(ratings) {
  sum(rowSums(!is.na(as_ratings(ratings))) >= 2L)
}


# --- Difference functions -------------------------------------------------
#
# For single-label data the nominal function is right: two answers either match
# or they do not. For MULTI-LABEL data each answer is a set, and treating
# {Positive} versus {Positive, Self awareness} as a total miss throws away the
# fact that the raters agree about Positive. Jaccard and MASI both express that
# partial agreement, so both give a higher alpha than nominal on overlapping data.
#
# The two are not interchangeable. MASI similarity is J * M with M <= 1, so MASI
# distance is always >= Jaccard distance: MASI is the STRICTER choice. What its
# monotonicity term M buys is a ranking among non-identical pairs -- nested sets
# (one rater simply saw an extra code) count as closer than crossing sets (the
# raters overlap on one code but each has one the other lacks).
#
# Krippendorff's framework allows any difference function, so alpha stays
# interpretable on the same scale. Report which one you used -- the value
# depends on it, and Jaccard will flatter the result relative to MASI.

CODE_SET_SEP <- " | "

delta_nominal <- function(levels) {
  d <- 1 - diag(length(levels))
  dimnames(d) <- list(levels, levels)
  d
}

#' Interval difference: squared distance between the level POSITIONS.
#'
#' Provided for DIAGNOSIS, not for use on this scheme. Our four codes are
#' unordered names, so an interval alpha on them is not interpretable -- but it is
#' the single most common way to get an inflated Krippendorff's alpha by
#' accident. If the labels are recoded as 1..4 and a tool's measurement level is
#' left at scale/interval, it will treat "Positive vs Negative" as a smaller
#' disagreement than "Positive vs Not marked" purely because of the order the
#' categories happen to be listed in, and the coefficient rises.
#'
#' Use it to test whether a competing figure was computed this way.
delta_interval <- function(levels, values = seq_along(levels)) {
  if (length(values) != length(levels)) {
    stop("`values` must give one number per level", call. = FALSE)
  }
  d <- outer(values, values, function(a, b) (a - b)^2)
  dimnames(d) <- list(levels, levels)
  d
}

#' Ordinal difference (Krippendorff): depends on the observed marginals.
#'
#' delta^2(c,k) = ( sum of n_g for g between c and k, minus (n_c + n_k)/2 )^2,
#' so unlike the nominal and interval cases this cannot be built from the level
#' names alone -- it needs the data. Also supplied for diagnosis only.
delta_ordinal <- function(ratings, levels) {
  o <- coincidence_matrix(ratings, levels)
  n <- rowSums(o)
  L <- length(levels)
  d <- matrix(0, L, L, dimnames = list(levels, levels))
  for (i in seq_len(L)) for (j in seq_len(L)) {
    if (i == j) next
    lo <- min(i, j); hi <- max(i, j)
    d[i, j] <- (sum(n[lo:hi]) - (n[i] + n[j]) / 2)^2
  }
  d
}

#' Split canonical " | "-joined set strings back into code vectors
parse_code_sets <- function(levels, sep = CODE_SET_SEP) {
  lapply(strsplit(levels, sep, fixed = TRUE),
         function(v) unique(trimws(v[nzchar(trimws(v))])))
}

#' Jaccard distance between code sets: 1 - |A and B| / |A or B|
delta_jaccard <- function(levels, sep = CODE_SET_SEP) {
  S <- parse_code_sets(levels, sep)
  L <- length(S)
  d <- matrix(0, L, L, dimnames = list(levels, levels))
  for (i in seq_len(L)) for (j in seq_len(L)) {
    if (i == j) next
    u <- length(union(S[[i]], S[[j]]))
    d[i, j] <- if (u == 0L) 0 else 1 - length(intersect(S[[i]], S[[j]])) / u
  }
  d
}

#' MASI distance (Passonneau): 1 - J * M, where the monotonicity term M is
#' 1 for identical sets, 2/3 when one is a subset of the other, 1/3 when they
#' intersect without nesting, and 0 when disjoint.
delta_masi <- function(levels, sep = CODE_SET_SEP) {
  S <- parse_code_sets(levels, sep)
  L <- length(S)
  d <- matrix(0, L, L, dimnames = list(levels, levels))
  for (i in seq_len(L)) for (j in seq_len(L)) {
    if (i == j) next
    A <- S[[i]]; B <- S[[j]]
    inter <- length(intersect(A, B)); uni <- length(union(A, B))
    J <- if (uni == 0L) 1 else inter / uni
    M <- if (setequal(A, B)) 1
         else if (inter > 0L && (inter == length(A) || inter == length(B))) 2 / 3
         else if (inter > 0L) 1 / 3
         else 0
    d[i, j] <- 1 - J * M
  }
  d
}

validate_delta <- function(delta, levels) {
  L <- length(levels)
  if (!is.matrix(delta) || any(dim(delta) != c(L, L))) {
    stop("delta must be a ", L, " x ", L, " matrix matching `levels`", call. = FALSE)
  }
  if (any(abs(diag(delta)) > 1e-12)) stop("delta must be zero on the diagonal", call. = FALSE)
  if (max(abs(delta - t(delta))) > 1e-12) stop("delta must be symmetric", call. = FALSE)
  delta
}


# --- Cohen's kappa (two raters) -------------------------------------------

#' Cohen's kappa on the units both raters rated
cohens_kappa <- function(x, y, levels = NULL) {
  x <- as.character(x); y <- as.character(y)
  if (is.null(levels)) levels <- sort(unique(c(x, y)[!is.na(c(x, y))]))
  ok <- !is.na(x) & !is.na(y)
  n <- sum(ok)
  if (n == 0) return(list(n = 0L, p_o = NA_real_, p_e = NA_real_, kappa = NA_real_))

  tab <- table(factor(x[ok], levels), factor(y[ok], levels))
  p_o <- sum(diag(tab)) / n
  p_e <- sum(rowSums(tab) * colSums(tab)) / n^2
  kappa <- if (isTRUE(all.equal(p_e, 1))) NA_real_ else (p_o - p_e) / (1 - p_e)

  list(n = n, p_o = p_o, p_e = p_e, kappa = kappa)
}

#' Simple pairwise percent agreement on jointly rated units
percent_agreement <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  if (!any(ok)) return(NA_real_)
  mean(as.character(x)[ok] == as.character(y)[ok])
}


# --- Fleiss' kappa and Gwet's AC1 (complete cases only) -------------------

#' Per-unit label counts for units rated by every rater
#' @return list(counts = N x L matrix, m = raters per unit, n_units = N)
complete_case_counts <- function(ratings, levels) {
  r <- as_ratings(ratings)
  keep <- stats::complete.cases(r)
  r <- r[keep, , drop = FALSE]
  L <- length(levels)
  counts <- t(apply(r, 1L, function(v) tabulate(match(v, levels), nbins = L)))
  if (nrow(r) == 1L) counts <- matrix(counts, nrow = 1L)
  list(counts = counts, m = ncol(r), n_units = nrow(r))
}

#' Fleiss' kappa. Requires the same number of raters per unit, so it is
#' computed on complete cases only; alpha is preferable when data are missing.
fleiss_kappa <- function(ratings, levels = NULL) {
  if (is.null(levels)) levels <- observed_levels(ratings)
  cc <- complete_case_counts(ratings, levels)
  N <- cc$n_units; m <- cc$m
  if (N < 2 || m < 2) return(list(n_units = N, kappa = NA_real_))

  p_j <- colSums(cc$counts) / (N * m)
  P_i <- (rowSums(cc$counts^2) - m) / (m * (m - 1))
  P_bar <- mean(P_i)
  P_e <- sum(p_j^2)
  kappa <- if (isTRUE(all.equal(P_e, 1))) NA_real_ else (P_bar - P_e) / (1 - P_e)

  list(n_units = N, p_o = P_bar, p_e = P_e, kappa = kappa)
}

#' Gwet's AC1. Chance agreement is estimated from the propensity to code at
#' random rather than from the marginals, which makes it robust to the skewed
#' category prevalence that deflates kappa and alpha (the "kappa paradox").
#'
#' GOTCHA: p_e = sum_k pi_k (1 - pi_k) / (q - 1) depends on q, the number of
#' declared categories -- so AC1 changes if a codebook category happens to go
#' unused in the sample. Always pass the FULL codebook as `levels`, because
#' that is what the raters chose from. (irrCAC defaults to the observed
#' categories instead; pass its `categ.labels` argument to match.) Krippendorff's
#' alpha and Cohen's/Fleiss' kappa are unaffected by unused categories.
gwet_ac1 <- function(ratings, levels = NULL) {
  if (is.null(levels)) levels <- observed_levels(ratings)
  cc <- complete_case_counts(ratings, levels)
  N <- cc$n_units; m <- cc$m; L <- length(levels)
  if (N < 2 || m < 2 || L < 2) return(list(n_units = N, ac1 = NA_real_))

  p_a <- mean(rowSums(cc$counts * (cc$counts - 1)) / (m * (m - 1)))
  pi_k <- colSums(cc$counts) / (N * m)
  p_e <- sum(pi_k * (1 - pi_k)) / (L - 1)
  ac1 <- if (isTRUE(all.equal(p_e, 1))) NA_real_ else (p_a - p_e) / (1 - p_e)

  list(n_units = N, p_o = p_a, p_e = p_e, ac1 = ac1)
}


# --- Uncertainty ----------------------------------------------------------

#' Percentile bootstrap CI, resampling coding units with replacement
#'
#' Units are the sampling level because they are the independent observations;
#' raters are fixed by design. Krippendorff's own bootstrap resamples pairable
#' values instead -- report which one you used.
bootstrap_ci <- function(ratings, stat_fn, levels = NULL, B = 2000L,
                         conf = 0.95, seed = 20260730L) {
  if (is.null(levels)) levels <- observed_levels(ratings)
  r <- as_ratings(ratings)
  N <- nrow(r)
  set.seed(seed)

  est <- vapply(seq_len(B), function(b) {
    idx <- sample.int(N, N, replace = TRUE)
    out <- tryCatch(stat_fn(r[idx, , drop = FALSE], levels), error = function(e) NA_real_)
    as.numeric(out)
  }, numeric(1))

  a <- (1 - conf) / 2
  q <- stats::quantile(est, c(a, 1 - a), na.rm = TRUE, names = FALSE)
  list(lower = q[1], upper = q[2], se = stats::sd(est, na.rm = TRUE),
       n_valid = sum(!is.na(est)), B = B, replicates = est)
}

# Thin wrappers with the (ratings, levels) signature bootstrap_ci expects
stat_alpha  <- function(ratings, levels) krippendorff_alpha(ratings, levels)$alpha
stat_ac1    <- function(ratings, levels) gwet_ac1(ratings, levels)$ac1
stat_fleiss <- function(ratings, levels) fleiss_kappa(ratings, levels)$kappa

#' Bootstrap-ready alpha for a fixed difference matrix.
#' The delta is captured from the FULL level set so it stays constant across
#' bootstrap replicates; recomputing it per replicate would change the
#' measurement instrument along with the sample.
stat_alpha_delta <- function(delta) {
  function(ratings, levels) krippendorff_alpha(ratings, levels, delta)$alpha
}


# --- Descriptives ---------------------------------------------------------

#' All pairwise Cohen's kappa and percent agreement
pairwise_table <- function(ratings, levels = NULL) {
  r <- as_ratings(ratings)
  if (is.null(levels)) levels <- observed_levels(r)
  raters <- colnames(r)
  combos <- utils::combn(seq_along(raters), 2L, simplify = FALSE)

  do.call(rbind, lapply(combos, function(ij) {
    i <- ij[1]; j <- ij[2]
    k <- cohens_kappa(r[, i], r[, j], levels)
    # Two-rater alpha is reported alongside kappa on purpose: the two should
    # land close together, which is a cheap sanity check on both.
    a <- krippendorff_alpha(r[, c(i, j), drop = FALSE], levels)$alpha
    data.frame(rater_a = raters[i], rater_b = raters[j], n_joint = k$n,
               percent_agreement = percent_agreement(r[, i], r[, j]),
               p_expected = k$p_e, cohens_kappa = k$kappa,
               krippendorff_alpha = a,
               stringsAsFactors = FALSE)
  }))
}

#' Per-rater label usage. Systematic differences here explain a lot of the
#' disagreement that the coefficients then report.
marginal_table <- function(ratings, levels = NULL) {
  r <- as_ratings(ratings)
  if (is.null(levels)) levels <- observed_levels(r)

  do.call(rbind, lapply(colnames(r), function(rt) {
    v <- factor(r[, rt], levels = levels)
    cnt <- table(v)
    data.frame(rater = rt, label = names(cnt), n = as.integer(cnt),
               proportion = as.numeric(cnt) / max(1L, sum(cnt)),
               n_missing = sum(is.na(r[, rt])),
               stringsAsFactors = FALSE)
  }))
}

#' Long-format confusion matrix for one rater pair
confusion_long <- function(ratings, rater_a, rater_b, levels = NULL) {
  r <- as_ratings(ratings)
  if (is.null(levels)) levels <- observed_levels(r)
  tab <- table(factor(r[, rater_a], levels), factor(r[, rater_b], levels))
  out <- as.data.frame(tab, stringsAsFactors = FALSE)
  names(out) <- c("label_a", "label_b", "n")
  cbind(rater_a = rater_a, rater_b = rater_b, out)
}

#' How many units did all raters agree on, exactly?
unanimity <- function(ratings) {
  r <- as_ratings(ratings)
  pairable <- rowSums(!is.na(r)) >= 2L
  agree <- apply(r[pairable, , drop = FALSE], 1L, function(v) {
    v <- v[!is.na(v)]
    length(unique(v)) == 1L
  })
  list(n_units = sum(pairable), n_unanimous = sum(agree),
       proportion = if (any(pairable)) mean(agree) else NA_real_)
}

#' Majority label per unit; ties resolved to NA (an honest "no majority")
majority_label <- function(ratings) {
  r <- as_ratings(ratings)
  apply(r, 1L, function(v) {
    v <- v[!is.na(v)]
    if (!length(v)) return(NA_character_)
    cnt <- table(v)
    top <- names(cnt)[cnt == max(cnt)]
    if (length(top) == 1L) top else NA_character_
  })
}


# --- Reporting helper -----------------------------------------------------

#' Landis & Koch benchmark. A convention, not a law -- quote it as such.
interpret_coefficient <- function(x) {
  ifelse(is.na(x), "undefined",
  ifelse(x < 0.00, "less than chance",
  ifelse(x < 0.21, "slight",
  ifelse(x < 0.41, "fair",
  ifelse(x < 0.61, "moderate",
  ifelse(x < 0.81, "substantial", "almost perfect"))))))
}

#' Per-code reliability for multi-label data
#'
#' Each code becomes its own present/absent variable. This is the interpretable
#' companion to a set-valued alpha: it says WHICH code the raters disagree
#' about, which a single overall coefficient cannot. Prevalence is reported
#' alongside because a rare code can show a low alpha and a high AC1 purely
#' from its skew.
#'
#' @param ratings units x raters matrix of canonical " | "-joined code sets
#' @param codes the full codebook code list
per_code_reliability <- function(ratings, codes, sep = CODE_SET_SEP,
                                 B = 2000L, seed = 20260730L) {
  r <- as_ratings(ratings)
  lv <- c("present", "absent")

  do.call(rbind, lapply(codes, function(cd) {
    bm <- matrix(NA_character_, nrow(r), ncol(r), dimnames = dimnames(r))
    for (i in seq_len(nrow(r))) for (j in seq_len(ncol(r))) {
      v <- r[i, j]
      if (!is.na(v)) {
        has <- cd %in% trimws(strsplit(v, sep, fixed = TRUE)[[1]])
        bm[i, j] <- if (has) "present" else "absent"
      }
    }
    ka <- krippendorff_alpha(bm, lv)
    ci <- bootstrap_ci(bm, stat_alpha, lv, B = B, seed = seed)
    data.frame(code = cd,
               prevalence = mean(bm == "present", na.rm = TRUE),
               n_units_pairable = ka$n_units,
               krippendorff_alpha = ka$alpha,
               alpha_ci_lower = ci$lower, alpha_ci_upper = ci$upper,
               gwet_ac1 = gwet_ac1(bm, lv)$ac1,
               stringsAsFactors = FALSE)
  }))
}

#' One-row summary for a rating table: alpha, AC1, Fleiss, with bootstrap CIs
#'
#' @param delta optional difference matrix (see krippendorff_alpha). When set,
#'   Fleiss' kappa and AC1 are omitted: both are defined for nominal categories
#'   only and would silently treat overlapping code sets as unrelated.
reliability_summary <- function(ratings, levels = NULL, label = "",
                                B = 2000L, seed = 20260730L, delta = NULL) {
  if (is.null(levels)) levels <- observed_levels(ratings)
  ka <- krippendorff_alpha(ratings, levels, delta)
  set_valued <- !is.null(delta)
  ac <- if (set_valued) list(ac1 = NA_real_) else gwet_ac1(ratings, levels)
  fk <- if (set_valued) list(kappa = NA_real_) else fleiss_kappa(ratings, levels)
  un <- unanimity(ratings)

  ci_a <- bootstrap_ci(ratings, if (set_valued) stat_alpha_delta(delta) else stat_alpha,
                       levels, B = B, seed = seed)
  ci_c <- if (set_valued) list(lower = NA_real_, upper = NA_real_) else
          bootstrap_ci(ratings, stat_ac1, levels, B = B, seed = seed)

  data.frame(
    analysis            = label,
    n_units_total       = nrow(ratings),
    n_units_pairable    = ka$n_units,
    n_raters            = ncol(ratings),
    n_categories        = length(levels),
    prop_unanimous      = un$proportion,
    krippendorff_alpha  = ka$alpha,
    alpha_ci_lower      = ci_a$lower,
    alpha_ci_upper      = ci_a$upper,
    gwet_ac1            = ac$ac1,
    ac1_ci_lower        = ci_c$lower,
    ac1_ci_upper        = ci_c$upper,
    fleiss_kappa        = fk$kappa,
    alpha_interpretation = interpret_coefficient(ka$alpha),
    stringsAsFactors = FALSE
  )
}
