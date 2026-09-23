#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# reliability.R -- inter-rater reliability for the human coder alignment
#
#   Rscript reliability.R [path/to/alignment.csv]
#
# Writes tables and a readable report to results/. Every coefficient comes from
# R/agreement.R, which is validated by validate_agreement.R -- run that first.
#
# Six analyses, because a single headline number would hide the structure of
# the disagreement:
#
#   A  primary code, 4 categories, all units          <- the headline figure
#   B  detection: relevant vs "Not marked", 2 cats    <- did they flag the same passages?
#   C  valence: Positive/Negative/Self awareness      <- given all flagged it, do they agree how?
#   D  CQ type, all units a rater coded Positive      <- alpha handles the missing cells
#   E  CQ type, units all raters coded Positive       <- strictest, smallest n
#   F  primary code split by journal week             <- descriptive only, tiny n
# ---------------------------------------------------------------------------

here <- dirname(sub("^--file=", "",
                    commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1]))
source(file.path(here, "R", "agreement.R"))
source(file.path(here, "R", "io.R"))

args    <- commandArgs(trailingOnly = TRUE)
in_path <- if (length(args) >= 1L) args[1] else NULL
out_dir <- file.path(here, "results")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

B_BOOT <- 2000L
SEED   <- 20260730L

fmt <- function(x, d = 3) ifelse(is.na(x), "  -  ", sprintf(paste0("%.", d, "f"), x))
write_tbl <- function(df, name) {
  utils::write.csv(df, file.path(out_dir, name), row.names = FALSE, na = "")
  invisible(df)
}


# --- Load ------------------------------------------------------------------

al   <- read_alignment(in_path)
long <- al$long
write_tbl(long, "units_labels_long.csv")

cat(sprintf("\nStudents: %s\nWeeks:    %s\nRaters:   %s\n",
            paste(sort(unique(long$student_id)), collapse = ", "),
            paste(sort(unique(long$week)), collapse = ", "),
            paste(al$raters, collapse = ", ")))


# --- Build the rating matrices for each analysis ---------------------------

m_primary <- ratings_matrix(long, "primary")
m_cq      <- ratings_matrix(long, "cq_type")

# B: collapse to a detection decision
DETECT_LEVELS <- c("Relevant", "Not marked")
m_detect <- m_primary
m_detect[] <- ifelse(is.na(m_primary), NA_character_,
                     ifelse(m_primary == "Not marked", "Not marked", "Relevant"))

# C: valence, only where every rater flagged the passage as relevant
VALENCE_LEVELS <- c("Positive", "Negative", "Self awareness")
all_relevant <- apply(m_primary, 1L, function(v) all(!is.na(v) & v != "Not marked"))
m_valence <- m_primary[all_relevant, , drop = FALSE]

# E: CQ type where every rater said Positive
all_positive <- apply(m_primary, 1L, function(v) all(!is.na(v) & v == "Positive"))
m_cq_strict <- m_cq[all_positive, , drop = FALSE]

cat(sprintf("\nUnits: %d total | %d flagged relevant by all | %d coded Positive by all\n",
            nrow(m_primary), sum(all_relevant), sum(all_positive)))


# --- A-E: reliability summary ---------------------------------------------

summaries <- list(
  reliability_summary(m_primary,   PRIMARY_LEVELS, "A. primary code (4 cat, all units)",   B_BOOT, SEED),
  reliability_summary(m_detect,     DETECT_LEVELS, "B. detection (relevant vs no comment)", B_BOOT, SEED),
  reliability_summary(m_valence,   VALENCE_LEVELS, "C. valence (all raters flagged)",       B_BOOT, SEED),
  reliability_summary(m_cq,             CQ_LEVELS, "D. CQ type (rater coded Positive)",     B_BOOT, SEED),
  reliability_summary(m_cq_strict,      CQ_LEVELS, "E. CQ type (all raters Positive)",      B_BOOT, SEED)
)
summary_tbl <- do.call(rbind, summaries)
write_tbl(summary_tbl, "reliability_summary.csv")


# --- Pairwise, marginals, confusion ---------------------------------------

pairwise <- rbind(
  cbind(analysis = "A. primary code", pairwise_table(m_primary, PRIMARY_LEVELS)),
  cbind(analysis = "B. detection",    pairwise_table(m_detect,  DETECT_LEVELS)),
  cbind(analysis = "D. CQ type",      pairwise_table(m_cq,      CQ_LEVELS))
)
write_tbl(pairwise, "pairwise_cohen_kappa.csv")

marginals <- rbind(
  cbind(variable = "primary", marginal_table(m_primary, PRIMARY_LEVELS)),
  cbind(variable = "cq_type", marginal_table(m_cq,      CQ_LEVELS))
)
write_tbl(marginals, "rater_marginals.csv")

conf_primary <- do.call(rbind, lapply(
  utils::combn(al$raters, 2L, simplify = FALSE),
  function(p) confusion_long(m_primary, p[1], p[2], PRIMARY_LEVELS)))
write_tbl(conf_primary, "confusion_primary.csv")

conf_cq <- do.call(rbind, lapply(
  utils::combn(al$raters, 2L, simplify = FALSE),
  function(p) confusion_long(m_cq, p[1], p[2], CQ_LEVELS)))
write_tbl(conf_cq, "confusion_cq_type.csv")


# --- F: per-week, descriptive only ----------------------------------------

weeks <- sort(unique(long$week))
per_week <- do.call(rbind, lapply(weeks, function(w) {
  ids <- unique(long$unit_id[long$week == w])
  mw  <- m_primary[rownames(m_primary) %in% ids, , drop = FALSE]
  ka  <- krippendorff_alpha(mw, PRIMARY_LEVELS)
  un  <- unanimity(mw)
  data.frame(week = w, n_units = nrow(mw), prop_unanimous = un$proportion,
             krippendorff_alpha = ka$alpha,
             gwet_ac1 = gwet_ac1(mw, PRIMARY_LEVELS)$ac1,
             # A per-week alpha on a handful of units is noise, not a finding.
             underpowered = nrow(mw) < 15L,
             stringsAsFactors = FALSE)
}))
write_tbl(per_week, "per_week_primary.csv")


# --- Disagreement detail, for the next alignment meeting ------------------

disagreements <- do.call(rbind, lapply(rownames(m_primary), function(u) {
  pv <- m_primary[u, ]; cv <- m_cq[u, ]
  primary_split <- length(unique(pv[!is.na(pv)])) > 1L
  cq_present <- cv[!is.na(cv)]
  cq_split <- length(unique(cq_present)) > 1L
  if (!primary_split && !cq_split) return(NULL)

  kind <- if (primary_split && any(pv == "Not marked")) "detection"
          else if (primary_split) "valence" else "cq_type"

  data.frame(unit_id = u,
             week = long$week[match(u, long$unit_id)],
             kind = kind,
             primary = paste(sprintf("%s=%s", names(pv), pv), collapse = " | "),
             cq_type = paste(sprintf("%s=%s", names(cv), ifelse(is.na(cv), "-", cv)),
                             collapse = " | "),
             stringsAsFactors = FALSE)
}))
if (is.null(disagreements)) disagreements <- data.frame()
write_tbl(disagreements, "disagreements.csv")


# --- Figures (optional) ---------------------------------------------------

if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  md <- marginals[marginals$variable == "primary", ]
  md$label <- factor(md$label, levels = PRIMARY_LEVELS)
  p1 <- ggplot(md, aes(label, n, fill = rater)) +
    geom_col(position = position_dodge(0.8), width = 0.75) +
    labs(title = "Primary code usage per rater",
         subtitle = "Systematic differences here drive the disagreement the coefficients report",
         x = NULL, y = "units") +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
  ggsave(file.path(out_dir, "fig_rater_marginals.png"), p1,
         width = 7, height = 4, dpi = 150)

  st <- summary_tbl
  st$analysis <- factor(st$analysis, levels = rev(st$analysis))
  p2 <- ggplot(st, aes(krippendorff_alpha, analysis)) +
    geom_vline(xintercept = c(0, 0.667, 0.8), linetype = c("solid", "dotted", "dashed"),
               colour = "grey60") +
    geom_errorbar(aes(xmin = alpha_ci_lower, xmax = alpha_ci_upper),
                  orientation = "y", width = 0.18) +
    geom_point(size = 2.6) +
    labs(title = "Krippendorff's alpha with bootstrap 95% CI",
         subtitle = "dotted 0.667 = tentative-conclusions floor, dashed 0.80 = customary target",
         x = expression(alpha), y = NULL) +
    theme_minimal(base_size = 11)
  ggsave(file.path(out_dir, "fig_alpha_ci.png"), p2,
         width = 8, height = 3.6, dpi = 150)
}


# --- Readable report ------------------------------------------------------

rep <- c(
  "# Inter-rater reliability: human coder alignment",
  "",
  sprintf("Source: `%s`", basename(al$path)),
  sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  sprintf("Raters: %s | Units: %d | Weeks: %s | Student(s): %s",
          paste(al$raters, collapse = ", "), nrow(m_primary),
          paste(weeks, collapse = ", "),
          paste(sort(unique(long$student_id)), collapse = ", ")),
  "",
  "Coefficients computed by `R/agreement.R` (base R), cross-validated against",
  "`irr` and `irrCAC` in `validate_agreement.R`. CIs are percentile bootstrap",
  sprintf("over coding units, B = %d, seed = %d.", B_BOOT, SEED),
  "",
  "## Summary",
  "",
  "| analysis | units | unanimous | alpha | 95% CI | AC1 | Fleiss kappa |",
  "|---|---:|---:|---:|:---:|---:|---:|",
  apply(summary_tbl, 1L, function(r) sprintf(
    "| %s | %s | %s | %s | %s-%s | %s | %s |",
    r[["analysis"]], r[["n_units_pairable"]],
    fmt(as.numeric(r[["prop_unanimous"]])),
    fmt(as.numeric(r[["krippendorff_alpha"]])),
    fmt(as.numeric(r[["alpha_ci_lower"]])), fmt(as.numeric(r[["alpha_ci_upper"]])),
    fmt(as.numeric(r[["gwet_ac1"]])), fmt(as.numeric(r[["fleiss_kappa"]])))),
  "",
  "## Pairwise (Cohen's kappa, and two-rater alpha as a cross-check)",
  "",
  "| analysis | pair | n | % agree | kappa | alpha |",
  "|---|---|---:|---:|---:|---:|",
  apply(pairwise, 1L, function(r) sprintf(
    "| %s | %s vs %s | %s | %s | %s | %s |",
    r[["analysis"]], r[["rater_a"]], r[["rater_b"]], r[["n_joint"]],
    fmt(as.numeric(r[["percent_agreement"]])),
    fmt(as.numeric(r[["cohens_kappa"]])),
    fmt(as.numeric(r[["krippendorff_alpha"]])))),
  "",
  "## Per week (descriptive only)",
  "",
  "| week | units | unanimous | alpha | AC1 | underpowered |",
  "|---:|---:|---:|---:|---:|:---:|",
  apply(per_week, 1L, function(r) sprintf(
    "| %s | %s | %s | %s | %s | %s |",
    r[["week"]], r[["n_units"]], fmt(as.numeric(r[["prop_unanimous"]])),
    fmt(as.numeric(r[["krippendorff_alpha"]])), fmt(as.numeric(r[["gwet_ac1"]])),
    ifelse(as.logical(r[["underpowered"]]), "yes", "no"))),
  "",
  sprintf("## Disagreements (%d units)", nrow(disagreements)),
  "",
  "See `disagreements.csv`. `kind` is `detection` when at least one rater said",
  "\"Not marked\", `valence` when they disagree on Positive/Negative/Self awareness,",
  "`cq_type` when only the CQ factor differs.",
  "",
  "## Reading these numbers",
  "",
  "- Compare **A** with **B** and **C**: if B is high and C is low, the raters",
  "  find the same passages but read them differently; if B is low, the",
  "  disagreement is about what counts as relevant at all.",
  "- **AC1** is reported next to alpha because the category distribution is",
  "  skewed. Where alpha is much lower than AC1, prevalence is depressing alpha",
  "  rather than the coders being unreliable.",
  "- Assigning a CQ factor (**D**, **E**) is expected to be harder than the",
  "  primary code, and the raters flagged it as such: single sentences can carry",
  "  more than one factor while the scheme forces one choice.",
  "- With this few units the CIs are wide. Treat the point estimates as",
  "  provisional until the pilot covers more journals."
)
writeLines(rep, file.path(out_dir, "reliability_report.md"))


# --- Console output -------------------------------------------------------

cat("\n================ SUMMARY ================\n")
print(summary_tbl[, c("analysis", "n_units_pairable", "prop_unanimous",
                      "krippendorff_alpha", "alpha_ci_lower", "alpha_ci_upper",
                      "gwet_ac1", "fleiss_kappa")], row.names = FALSE, digits = 3)
cat("\n================ PAIRWISE ================\n")
print(pairwise, row.names = FALSE, digits = 3)
cat("\n================ PER WEEK ================\n")
print(per_week, row.names = FALSE, digits = 3)
cat(sprintf("\n%d disagreeing unit(s). Wrote 9 file(s) to %s\n",
            nrow(disagreements), out_dir))
