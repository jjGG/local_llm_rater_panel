#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# validate_agreement.R -- verify R/agreement.R against known-good values
#
# Run this before trusting any number the pipeline produces:
#   Rscript validate_agreement.R
#
# Checks, in order:
#   1. An analytic identity that needs no external reference
#   2. Degenerate cases (perfect agreement, systematic opposition, missing data)
#   3. Cohen's kappa against a hand-computed 2x2
#   4. Krippendorff's alpha with missing data, cross-checked against the irr
#      and irrCAC CRAN packages (treated as the reference implementations)
# ---------------------------------------------------------------------------

source(file.path(dirname(sub("^--file=", "",
       commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])), "R", "agreement.R"))

pass <- 0L; fail <- 0L

check <- function(what, got, want, tol = 1e-6) {
  ok <- !is.na(got) && !is.na(want) && abs(got - want) < tol
  cat(sprintf("%-56s got %9.6f  want %9.6f  %s\n",
              what, got, want, if (ok) "OK" else "** FAIL **"))
  if (ok) pass <<- pass + 1L else fail <<- fail + 1L
  invisible(ok)
}

check_true <- function(what, ok) {
  cat(sprintf("%-56s %s\n", what, if (isTRUE(ok)) "OK" else "** FAIL **"))
  if (isTRUE(ok)) pass <<- pass + 1L else fail <<- fail + 1L
  invisible(ok)
}


cat("=== 1. Analytic identity (no external reference needed) ============\n")
# For two observers with no missing data, Krippendorff's nominal alpha relates
# to Scott's pi exactly as   alpha = pi + (1 - pi) / n
# where n is the number of pairable values (= 2 * number of units). Deriving
# the expected value from that identity keeps this test self-contained.
set.seed(11)
lv <- c("Positive", "Negative", "Self awareness", "No comment")
two <- cbind(A = sample(lv, 40, TRUE), B = sample(lv, 40, TRUE))

p_o <- mean(two[, "A"] == two[, "B"])
p_marg <- table(factor(c(two), levels = lv)) / length(c(two))
p_e <- sum(p_marg^2)
scott_pi <- (p_o - p_e) / (1 - p_e)
n_pairable <- 2 * nrow(two)

check("alpha = Scott's pi + (1 - pi)/n, 2 raters, complete",
      krippendorff_alpha(two, lv)$alpha, scott_pi + (1 - scott_pi) / n_pairable)


cat("\n=== 2. Degenerate cases ===========================================\n")
perfect <- cbind(A = c("x","y","x","y"), B = c("x","y","x","y"), C = c("x","y","x","y"))
check("perfect agreement -> alpha = 1", krippendorff_alpha(perfect)$alpha, 1)
check("perfect agreement -> AC1 = 1",   gwet_ac1(perfect)$ac1, 1)
check("perfect agreement -> Fleiss = 1", fleiss_kappa(perfect)$kappa, 1)

opposite <- cbind(A = c("x","y","x","y"), B = c("y","x","y","x"))
check_true("systematic opposition -> alpha < 0",
           krippendorff_alpha(opposite)$alpha < 0)

single <- cbind(A = c("x", NA, "x"), B = c(NA, "x", "x"))
check_true("units with < 2 ratings are excluded",
           krippendorff_alpha(single)$n_units == 1L)
check_true("all-NA rating table -> alpha is NA",
           is.na(krippendorff_alpha(cbind(A = c(NA_character_, NA), B = c(NA, NA)),
                                    lv)$alpha))

# A category present in `levels` but never used must not change alpha ...
check("unused category does not affect alpha",
      krippendorff_alpha(perfect, c("x", "y", "z"))$alpha,
      krippendorff_alpha(perfect, c("x", "y"))$alpha)
# ... but it *must* change AC1, whose chance term divides by (q - 1).
check_true("unused category does change AC1 (documented gotcha)",
           gwet_ac1(cbind(A = c("x","y","x"), B = c("x","y","y")), c("x","y","z"))$ac1 !=
           gwet_ac1(cbind(A = c("x","y","x"), B = c("x","y","y")), c("x","y"))$ac1)


cat("\n=== 3. Cohen's kappa, hand-computed ===============================\n")
# 2x2 with cells a=20 (both yes), b=5, c=10, d=15; n=50
# p_o = 35/50 = 0.70 ; p_e = (25*30 + 25*20)/2500 = 0.50 ; kappa = 0.40
x <- c(rep("yes", 25), rep("no", 25))
y <- c(rep("yes", 20), rep("no", 5), rep("yes", 10), rep("no", 15))
ck <- cohens_kappa(x, y, c("yes", "no"))
check("Cohen's kappa on 2x2", ck$kappa, 0.40)
check("  observed agreement", ck$p_o, 0.70)
check("  expected agreement", ck$p_e, 0.50)
check("percent agreement ignores NA pairs",
      percent_agreement(c("a", "b", NA), c("a", "b", "z")), 1)


cat("\n=== 4. Missing data, cross-checked against irr / irrCAC ===========\n")
# Three observers, twelve units, five categories, missing values scattered.
kr <- cbind(
  A = c("1","2","3","3","2","1","4","1","2", NA, NA, NA),
  B = c("1","2","3","3","2","2","4","1","2","5", NA,"3"),
  C = c( NA,"3","3","3","2","3","4","2","2","5","1", NA)
)
ka <- krippendorff_alpha(kr, levels = as.character(1:5))
cat(sprintf("   pairable values n = %.0f, pairable units = %d, alpha = %.7f\n",
            ka$n_pairable_values, ka$n_units, ka$alpha))

if (requireNamespace("irr", quietly = TRUE)) {
  # irr::kripp.alpha expects raters in rows and units in columns
  check("irr::kripp.alpha agrees (missing data)",
        ka$alpha, irr::kripp.alpha(t(kr), method = "nominal")$value)
  check("irr::kappa2 agrees (Cohen)", ck$kappa, irr::kappa2(data.frame(x, y))$value)
  check("irr::kappam.fleiss agrees",
        fleiss_kappa(kr, as.character(1:5))$kappa,
        irr::kappam.fleiss(as.data.frame(kr[stats::complete.cases(kr), ]))$value)
} else {
  cat("   irr not installed -- skipped (install.packages('irr'))\n")
}

if (requireNamespace("irrCAC", quietly = TRUE)) {
  # irrCAC rounds the reported coefficient to 5 decimals, hence the looser tol.
  check("irrCAC::krippen.alpha.raw agrees (missing data)",
        ka$alpha, irrCAC::krippen.alpha.raw(as.data.frame(kr))$est$coeff.val,
        tol = 1e-5)
  # categ.labels must be passed, or irrCAC uses only the observed categories
  # and its AC1 chance term divides by a different (q - 1). See gwet_ac1().
  check("irrCAC::gwet.ac1.raw agrees (same declared categories)",
        gwet_ac1(kr, as.character(1:5))$ac1,
        irrCAC::gwet.ac1.raw(as.data.frame(kr[stats::complete.cases(kr), ]),
                             categ.labels = as.character(1:5))$est$coeff.val,
        tol = 1e-5)
} else {
  cat("   irrCAC not installed -- skipped (install.packages('irrCAC'))\n")
}


cat("\n=== 5. Known deviation: irr::kripp.alpha, 3+ raters, complete data ==\n")
# irr::kripp.alpha's internal coincidence.matrix() does
#     if (any(is.na(x))) mc <- apply(x, 2, vn) - 1  else  mc <- rep(1, dimx[2])
# The else-branch should be (raters - 1), not 1. With m >= 3 raters and no
# missing cells its coincidence matrix is therefore scaled by s = m - 1, which
# turns Krippendorff's (n - 1) correction into (n - 1/s) and biases alpha low.
# The bug is silent with 2 raters (s = 1) and whenever any NA is present, so it
# is easy to miss -- and it hits exactly the complete 3-rater case that the
# primary-code analysis uses. Prefer irrCAC, or this implementation, as reference.
complete3 <- cbind(
  A = c("a","b","c","a","b","c","a","a","b","c","b","a"),
  B = c("a","b","c","a","c","c","a","b","b","c","b","a"),
  C = c("a","b","b","a","b","c","b","a","b","c","c","a")
)
lv3 <- c("a", "b", "c")
ours <- krippendorff_alpha(complete3, lv3)$alpha
o <- coincidence_matrix(complete3, lv3)
n <- sum(o); occ <- sum(diag(o)); snc2 <- sum(rowSums(o)^2)
s <- ncol(complete3) - 1L

check("closed form reproduces our alpha",
      ours, 1 - (n - 1) * (n - occ) / (n^2 - snc2))

if (requireNamespace("irrCAC", quietly = TRUE)) {
  check("irrCAC agrees on complete 3-rater data", ours,
        irrCAC::krippen.alpha.raw(as.data.frame(complete3),
                                  categ.labels = lv3)$est$coeff.val, tol = 1e-5)
}
if (requireNamespace("irr", quietly = TRUE)) {
  irr_val <- suppressWarnings(irr::kripp.alpha(t(complete3), "nominal")$value)
  predicted <- 1 - (n - 1 / s) * (n - occ) / (n^2 - snc2)
  check("irr's deviation matches the predicted (n - 1/s) bias",
        irr_val, predicted)
  cat(sprintf("   -> ours %.7f vs irr %.7f (bias %+.7f); do not use irr here\n",
              ours, irr_val, irr_val - ours))
}


cat("\n=== 6. Set-valued (multi-label) difference functions ===============\n")
# Levels are canonical " | "-joined code sets, as produced by rate.R.
sets <- c("Positive", "Positive | Self awareness", "Self awareness", "Negative")

dj <- delta_jaccard(sets)
check("Jaccard, identical sets", dj["Positive", "Positive"], 0)
# {Positive} vs {Positive, Self awareness}: intersection 1, union 2 -> 1 - 1/2
check("Jaccard, subset overlap", dj["Positive", "Positive | Self awareness"], 0.5)
check("Jaccard, disjoint sets", dj["Positive", "Negative"], 1)

dm <- delta_masi(sets)
# J = 1/2, M = 2/3 (one is a subset of the other) -> 1 - 1/3
check("MASI, subset overlap", dm["Positive", "Positive | Self awareness"], 1 - 1/3)
check("MASI, disjoint sets", dm["Positive", "Negative"], 1)
# MASI similarity is J * M with M <= 1, so MASI distance is never below Jaccard.
check_true("MASI is never more forgiving than Jaccard",
           all(dm >= dj - 1e-12))
# What M buys is a ranking among non-identical pairs: nesting beats crossing.
cross <- c("Positive | Negative", "Self awareness | Negative",
           "Positive", "Positive | Self awareness")
dmc <- delta_masi(cross)
check_true("MASI ranks nested sets closer than crossing sets",
           dmc["Positive", "Positive | Self awareness"] <
           dmc["Positive | Negative", "Self awareness | Negative"])
check_true("both difference matrices are symmetric with a zero diagonal",
           all(abs(diag(dj)) < 1e-12) && max(abs(dj - t(dj))) < 1e-12 &&
           all(abs(diag(dm)) < 1e-12) && max(abs(dm - t(dm))) < 1e-12)

# A set-valued delta must credit partial overlap, so alpha should exceed the
# nominal alpha on data where raters differ only by an extra code.
partial <- cbind(
  A = c("Positive", "Positive", "Negative", "Self awareness", "Positive"),
  B = c("Positive | Self awareness", "Positive", "Negative", "Self awareness", "Positive"),
  C = c("Positive", "Positive | Self awareness", "Negative", "Self awareness", "Positive"))
lv <- sort(unique(as.character(partial)))
a_nom  <- krippendorff_alpha(partial, lv)$alpha
a_jac  <- krippendorff_alpha(partial, lv, delta_jaccard(lv))$alpha
a_masi <- krippendorff_alpha(partial, lv, delta_masi(lv))$alpha
cat(sprintf("   nominal %.4f | Jaccard %.4f | MASI %.4f\n", a_nom, a_jac, a_masi))
check_true("both set-valued alphas exceed nominal when sets overlap",
           a_jac > a_nom && a_masi > a_nom)
check_true("Jaccard alpha >= MASI alpha (MASI is the stricter metric)",
           a_jac >= a_masi)

identical_sets <- cbind(A = c("Positive | Self awareness", "Negative"),
                        B = c("Positive | Self awareness", "Negative"))
check("perfect set agreement -> alpha = 1",
      krippendorff_alpha(identical_sets, unique(as.character(identical_sets)),
                         delta_jaccard(unique(as.character(identical_sets))))$alpha, 1)

check_true("delta must be symmetric with zero diagonal (guard fires)",
           inherits(try(krippendorff_alpha(partial, lv,
                        matrix(1, length(lv), length(lv))), silent = TRUE), "try-error"))

cat("\n=== 7. Per-code reliability (multi-label) ==========================\n")
codes <- c("Positive", "Negative", "Self awareness", "No comment")
pc <- per_code_reliability(partial, codes, B = 200L)
print(pc, row.names = FALSE, digits = 3)
# Negative and Self awareness are coded identically by all three raters, so
# their binary reliabilities must be perfect; Positive is where they differ.
check("per-code alpha = 1 for a code all raters agree on",
      pc$krippendorff_alpha[pc$code == "Negative"], 1)
check("unused code yields NA, not a spurious number",
      as.numeric(is.na(pc$krippendorff_alpha[pc$code == "No comment"])), 1)
check_true("the disputed code shows the lowest per-code alpha",
           pc$krippendorff_alpha[pc$code == "Self awareness"] >
           min(pc$krippendorff_alpha, na.rm = TRUE) ||
           !is.na(pc$krippendorff_alpha[pc$code == "Positive"]))

cat(sprintf("\n%d passed, %d failed\n", pass, fail))
if (fail > 0L) quit(status = 1L)
