#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# 18_trajectory.R -- how the cohort's reflection changes across the four weeks
#
#   Rscript scripts/18_trajectory.R                     # consensus of all raters
#   Rscript scripts/18_trajectory.R --rater "jonas"     # one rater, for phase 2
#
# THE QUESTION is about the students, not the raters: across weeks 1, 4, 7 and
# 10, do they write more positively about intercultural teamwork, more
# negatively, more self-awarely -- and does the KIND of cultural intelligence
# they show shift? Every rater who saw a sentence is collapsed into one consensus
# label first, so the trajectory is a property of the cohort rather than of any
# one reader. Whether the human consensus differs from the model consensus is a
# separate question for later; --rater keeps that door open.
#
# RATES ARE PER SENTENCE WRITTEN. The denominator for every code is all the
# sentences that student wrote that week, so the measure does not depend on how
# much of the entry happened to be about culture. The composition among coded
# sentences is shown separately, because it answers a different question.
#
# WEEK IS CONFOUNDED WITH THE PROMPT. Each week asks different questions, so a
# change from week 1 to week 10 cannot be separated from a change in what was
# ASKED. No design here can break that; it is printed on every figure.
#
# READS NO JOURNAL TEXT.
# ---------------------------------------------------------------------------

repo <- normalizePath(file.path(dirname(sub("^--file=", "",
        commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])), ".."))
setwd(repo)
suppressPackageStartupMessages(library(ggplot2))

a <- commandArgs(trailingOnly = TRUE)
getopt <- function(f, d = NULL) { i <- match(f, a); if (is.na(i) || i == length(a)) d else a[i + 1L] }
IN    <- getopt("--pooled", "results/pooled_units.csv")
RATER <- getopt("--rater", "consensus")
NPERM <- as.integer(getopt("--perm", "20000"))
TAG   <- getopt("--tag", if (RATER == "consensus") "" else paste0("_", gsub("[^A-Za-z0-9]", "", RATER)))

set.seed(20260911)
p <- utils::read.csv(IN, stringsAsFactors = FALSE, na.strings = c("NA", ""))
p <- p[p$rater == RATER, , drop = FALSE]
stopifnot(nrow(p) > 0)
p <- p[order(p$student, p$week), ]

n_nocons <- sum(is.na(p$code))
p <- p[!is.na(p$code), , drop = FALSE]

CODES <- c("Positive", "Negative", "Self awareness")
INK <- "#0b0b0b"; INK2 <- "#52514e"; INK3 <- "#8a8880"; SURFACE <- "#fcfcfb"
CODE_FILL <- c("Positive" = "#1baf7a", "Negative" = "#eb6834",
               "Self awareness" = "#2a78d6", "Not marked" = "#e9e7e2")
FAC_FILL <- c(Metacognitive = "#3f7cac", Cognitive = "#6f4e9c",
              Motivational = "#1baf7a", Behavioural = "#c98a1b")
hr <- function(s) cat("\n", strrep("=", 74), "\n", s, "\n", strrep("=", 74), "\n", sep = "")

weeks <- sort(unique(p$week))
studs <- unique(p$student)
si <- match(p$student, studs); wi <- match(p$week, weeks)

hr(sprintf("COHORT -- consensus label, %d students, %d sentences", length(studs), nrow(p)))
tb <- table(student = p$student, week = p$week)
print(tb)
cat(sprintf("\nsmallest student-week cell: %d sentences\n", min(tb)))
if (n_nocons > 0L)
  cat(sprintf("%d sentences had no majority among their raters and are excluded (%.1f%%)\n",
              n_nocons, 100 * n_nocons / (n_nocons + nrow(p))))
if ("n_raters" %in% names(p))
  cat(sprintf("panel behind each consensus label: %s raters\n",
              paste(sort(unique(p$n_raters)), collapse = " or ")))

# --- cells ---------------------------------------------------------------
N <- table(factor(si, levels = seq_along(studs)), factor(wi, levels = seq_along(weeks)))
N <- matrix(as.integer(N), length(studs), length(weeks),
            dimnames = list(studs, paste0("w", weeks)))

cell_counts <- function(x) {
  m <- matrix(0L, length(studs), length(weeks), dimnames = dimnames(N))
  t2 <- tapply(x, list(si, wi), sum)
  m[] <- ifelse(is.na(t2), 0L, t2)
  m
}

# --- the statistic: mean over students of the per-student OLS slope ------
# Every student has all four weeks and the cell sizes are fixed, so the slope is
# a FIXED linear functional of the cell counts. Precomputing its weights turns
# each permutation into one vector lookup and a sum.
wbar <- mean(weeks); Sxx <- sum((weeks - wbar)^2)
CF <- matrix(rep((weeks - wbar) / Sxx, each = length(studs)),
             length(studs), length(weeks))
WT <- CF / N / length(studs)          # statistic = sum over sentences of WT[student, week]
WT[!is.finite(WT)] <- 0

idx <- split(seq_along(si), si)
perm_test <- function(x, B = NPERM) {
  obs <- sum(WT[cbind(si, wi)] * x)
  null <- numeric(B)
  for (b in seq_len(B)) {
    w <- wi
    for (g in idx) w[g] <- sample(w[g])   # weeks shuffled WITHIN each student
    null[b] <- sum(WT[cbind(si, w)] * x)
  }
  list(obs = obs, p = (1 + sum(abs(null) >= abs(obs))) / (B + 1))
}

hr(sprintf("TREND ACROSS THE FOUR WEEKS (%d within-student permutations)", NPERM))
cat("rate = share of the sentences that student wrote that week\n\n")
cat(sprintf("  %-16s %8s %8s %8s %8s   %9s %8s\n",
            "code", "w1", "w4", "w7", "w10", "slope/wk", "perm p"))
trend <- list(); series <- list()
for (cd in c(CODES, "any code")) {
  x <- if (cd == "any code") as.integer(p$code != "Not marked") else as.integer(p$code == cd)
  K <- cell_counts(x)
  rate <- K / N
  wk_mean <- colMeans(rate)                     # mean of per-student rates
  tt <- perm_test(x)
  cat(sprintf("  %-16s %7.1f%% %7.1f%% %7.1f%% %7.1f%%   %+8.4f %8.4f\n",
              cd, 100 * wk_mean[1], 100 * wk_mean[2], 100 * wk_mean[3],
              100 * wk_mean[4], tt$obs, tt$p))
  trend[[length(trend) + 1L]] <- data.frame(
    code = cd, slope_per_week = tt$obs, perm_p = tt$p,
    w1 = wk_mean[1], w4 = wk_mean[2], w7 = wk_mean[3], w10 = wk_mean[4],
    delta = wk_mean[4] - wk_mean[1], stringsAsFactors = FALSE)
  series[[length(series) + 1L]] <- data.frame(
    code = cd, student = rep(studs, length(weeks)),
    week = rep(weeks, each = length(studs)),
    rate = as.vector(rate), n = as.vector(N), k = as.vector(K),
    stringsAsFactors = FALSE)
}
trend <- do.call(rbind, trend); series <- do.call(rbind, series)
cat("\n  slope is rate-points per week. The permutation shuffles week labels within\n")
cat("  each student, so it holds every student's overall rate and weekly sentence\n")
cat("  counts fixed and tests only the ordering in time.\n")

# --- pooled counts per week, with Wilson intervals -----------------------
wilson <- function(x, n, z = 1.96) {
  if (n == 0) return(c(NA, NA))
  ph <- x / n; d <- 1 + z^2 / n
  c((ph + z^2 / (2 * n) - z * sqrt(ph * (1 - ph) / n + z^2 / (4 * n^2))) / d,
    (ph + z^2 / (2 * n) + z * sqrt(ph * (1 - ph) / n + z^2 / (4 * n^2))) / d)
}
hr("WEEK BY WEEK, SENTENCES POOLED ACROSS STUDENTS")
cat(sprintf("  %-6s %9s %10s %10s %10s %10s\n", "week", "sentences",
            "Positive", "Negative", "Self aware", "any code"))
pool <- list()
for (j in seq_along(weeks)) {
  d <- p[p$week == weeks[j], ]
  nn <- nrow(d)
  f <- function(lab) {
    k <- if (lab == "any code") sum(d$code != "Not marked") else sum(d$code == lab)
    ci <- wilson(k, nn)
    list(k = k, r = k / nn, lo = ci[1], hi = ci[2])
  }
  vs <- lapply(c(CODES, "any code"), f)
  cat(sprintf("  w%-5d %9d %10s %10s %10s %10s\n", weeks[j], nn,
              sprintf("%d (%.0f%%)", vs[[1]]$k, 100 * vs[[1]]$r),
              sprintf("%d (%.0f%%)", vs[[2]]$k, 100 * vs[[2]]$r),
              sprintf("%d (%.0f%%)", vs[[3]]$k, 100 * vs[[3]]$r),
              sprintf("%d (%.0f%%)", vs[[4]]$k, 100 * vs[[4]]$r)))
  for (i in seq_along(vs)) pool[[length(pool) + 1L]] <- data.frame(
    week = weeks[j], code = c(CODES, "any code")[i], n = nn, k = vs[[i]]$k,
    rate = vs[[i]]$r, lo = vs[[i]]$lo, hi = vs[[i]]$hi, stringsAsFactors = FALSE)
}
pool <- do.call(rbind, pool)
cat(sprintf("\n  95%% Wilson intervals are in results/trajectory_pooled%s.csv\n", TAG))

# --- composition among coded sentences ----------------------------------
hr("AMONG THE CODED SENTENCES ONLY -- what kind of comment")
cat(sprintf("  %-6s %7s %9s %9s %9s\n", "week", "coded", "Positive", "Negative", "Self aware"))
comp <- list()
for (j in seq_along(weeks)) {
  d <- p[p$week == weeks[j] & p$code != "Not marked", ]
  nn <- nrow(d)
  ks <- vapply(CODES, function(cd) sum(d$code == cd), integer(1))
  cat(sprintf("  w%-5d %7d %8.0f%% %8.0f%% %8.0f%%\n", weeks[j], nn,
              100 * ks[1] / nn, 100 * ks[2] / nn, 100 * ks[3] / nn))
  for (i in seq_along(CODES)) comp[[length(comp) + 1L]] <- data.frame(
    week = weeks[j], code = CODES[i], n = nn, k = ks[i], share = ks[i] / nn,
    stringsAsFactors = FALSE)
}
comp <- do.call(rbind, comp)

# --- CQ factor among Positive -------------------------------------------
hr("WHICH CULTURAL-INTELLIGENCE FACTOR, AMONG THE POSITIVE SENTENCES")
pf <- p[!is.na(p$cq_factor), ]
fac <- NULL
if (nrow(pf) > 0L) {
  cat(sprintf("  %-6s %9s %s\n", "week", "positive",
              paste(sprintf("%14s", names(FAC_FILL)), collapse = "")))
  fac <- list()
  for (j in seq_along(weeks)) {
    d <- pf[pf$week == weeks[j], ]; nn <- nrow(d)
    ks <- vapply(names(FAC_FILL), function(z) sum(d$cq_factor == z), integer(1))
    cat(sprintf("  w%-5d %9d %s\n", weeks[j], nn,
        paste(sprintf("%13s ", sprintf("%d (%.0f%%)", ks,
              if (nn > 0) 100 * ks / nn else rep(0, length(ks)))), collapse = "")))
    for (i in seq_along(ks)) fac[[length(fac) + 1L]] <- data.frame(
      week = weeks[j], factor_ = names(ks)[i], n = nn, k = ks[i],
      share = if (nn > 0) ks[i] / nn else NA_real_, stringsAsFactors = FALSE)
  }
  fac <- do.call(rbind, fac)
  cat(sprintf("\n  %d positive sentences carry a consensus factor, out of %d positive overall.\n",
              nrow(pf), sum(p$code == "Positive")))
  cat("  Cells this small cannot support a test; the shape is a hypothesis for the\n")
  cat("  full cohort, not a finding.\n")
}

# --- per student ---------------------------------------------------------
hr("EVERY STUDENT, WEEK BY WEEK")
cat("  share of that week's sentences, as Positive / Negative / Self-aware\n\n")
cat(sprintf("  %-8s %s\n", "student",
            paste(sprintf("%-22s", paste0("week ", weeks)), collapse = "")))
for (st in studs) {
  cells <- vapply(weeks, function(w) {
    d <- p[p$student == st & p$week == w, ]
    sprintf("%2.0f/%2.0f/%2.0f  (n=%d)", 100 * mean(d$code == "Positive"),
            100 * mean(d$code == "Negative"), 100 * mean(d$code == "Self awareness"),
            nrow(d))
  }, character(1))
  cat(sprintf("  %-8s %s\n", st, paste(sprintf("%-22s", cells), collapse = "")))
}

dir.create("results", showWarnings = FALSE)
utils::write.csv(trend,  sprintf("results/trajectory_trend%s.csv", TAG), row.names = FALSE)
utils::write.csv(series, sprintf("results/trajectory_cells%s.csv", TAG), row.names = FALSE)
utils::write.csv(pool,   sprintf("results/trajectory_pooled%s.csv", TAG), row.names = FALSE)
if (!is.null(fac)) utils::write.csv(fac, sprintf("results/trajectory_cq%s.csv", TAG), row.names = FALSE)
cat(sprintf("\nwrote results/trajectory_{trend,cells,pooled,cq}%s.csv\n", TAG))

# ===========================================================================
# FIGURES
# ===========================================================================
dir.create("reports", showWarnings = FALSE)
CAP <- paste0(
  "Week is confounded with the prompt: each week asks different questions, so a change over time cannot be separated from a change in what was asked.\n",
  sprintf("%d students, %d sentences, consensus of %s raters per sentence; the smallest student-week cell holds %d.\nExploratory -- read the spread, not the p-value.",
          length(studs), nrow(p),
          if ("n_raters" %in% names(p)) paste(sort(unique(p$n_raters)), collapse = " or ") else "all",
          min(tb)))

base_theme <- theme_minimal(base_size = 9) +
  theme(plot.background = element_rect(fill = SURFACE, colour = NA),
        panel.background = element_rect(fill = SURFACE, colour = NA),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#ecebe6", linewidth = 0.4),
        strip.text = element_text(face = "bold", size = 9, colour = INK),
        plot.title = element_text(face = "bold", size = 12.5, colour = INK),
        plot.subtitle = element_text(size = 8.3, colour = INK2, margin = margin(b = 9)),
        plot.caption = element_text(size = 6.5, colour = INK3, hjust = 0, margin = margin(t = 10)),
        legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 8, colour = INK2),
        axis.title = element_text(size = 8, colour = INK2),
        plot.margin = margin(12, 14, 10, 14))

# --- FIGURE 1: the three codes over time --------------------------------
s3 <- series[series$code %in% CODES, ]
s3$code <- factor(s3$code, levels = CODES)
mn <- do.call(rbind, lapply(split(s3, list(s3$code, s3$week), drop = TRUE), function(d)
  data.frame(code = d$code[1], week = d$week[1], rate = mean(d$rate),
             se = stats::sd(d$rate) / sqrt(nrow(d)), stringsAsFactors = FALSE)))
lab <- merge(trend[trend$code %in% CODES, c("code", "slope_per_week", "perm_p")],
             data.frame(code = CODES, stringsAsFactors = FALSE))
lab$code <- factor(lab$code, levels = CODES)
lab$txt <- sprintf("%+.2f pp/week   p = %.3f", 100 * lab$slope_per_week, lab$perm_p)

p1 <- ggplot(s3, aes(week, rate)) +
  geom_line(aes(group = student), colour = "#cbc8c1", linewidth = 0.4) +
  geom_point(aes(size = n), colour = "#cbc8c1") +
  geom_ribbon(data = mn, aes(ymin = pmax(0, rate - se), ymax = rate + se, fill = code),
              alpha = 0.18, colour = NA) +
  geom_line(data = mn, aes(colour = code), linewidth = 1.2) +
  geom_point(data = mn, aes(colour = code), size = 2.2) +
  geom_text(data = lab, aes(x = min(weeks), y = Inf, label = txt), hjust = 0, vjust = 1.6,
            size = 2.6, colour = INK2, inherit.aes = FALSE) +
  facet_wrap(~ code, nrow = 1) +
  scale_x_continuous(breaks = weeks, labels = paste0("w", weeks)) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"),
                     expand = expansion(mult = c(0.02, 0.20))) +
  scale_colour_manual(values = CODE_FILL, guide = "none") +
  scale_fill_manual(values = CODE_FILL, guide = "none") +
  scale_size_area(max_size = 2.4, name = "sentences that week", breaks = c(6, 20, 40)) +
  labs(title = "How the cohort writes about intercultural teamwork, week by week",
       subtitle = sprintf(paste0("Each code as a share of everything the student wrote that week. ",
         "Grey lines are the %d students, coloured\nline the cohort mean, ribbon +/- 1 SE. ",
         "Slope and permutation p are for the cohort mean."), length(studs)),
       x = NULL, y = "share of that week's sentences", caption = CAP) +
  base_theme
ggsave(sprintf("reports/fig_trajectory%s.png", TAG), p1,
       width = 9.4, height = 4.9, dpi = 220, bg = SURFACE)
cat(sprintf("wrote reports/fig_trajectory%s.png\n", TAG))

# --- FIGURE 2: composition among coded ----------------------------------
comp$code <- factor(comp$code, levels = rev(CODES))
p2 <- ggplot(comp, aes(factor(week, levels = weeks, labels = paste0("w", weeks)),
                       share, fill = code)) +
  geom_col(width = 0.68, colour = SURFACE, linewidth = 0.5) +
  geom_text(aes(label = ifelse(share > 0.07, sprintf("%.0f%%", 100 * share), "")),
            position = position_stack(vjust = 0.5), size = 2.7,
            colour = "#0d2018", fontface = "bold") +
  geom_text(data = unique(comp[, c("week", "n")]), aes(y = 1.03, label = paste0("n=", n)),
            inherit.aes = FALSE, size = 2.4, colour = INK3,
            x = factor(unique(comp$week), levels = weeks, labels = paste0("w", weeks))) +
  scale_fill_manual(values = CODE_FILL, breaks = CODES) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"), limits = c(0, 1.07)) +
  labs(title = "When they do comment on culture, what kind of comment is it?",
       subtitle = sprintf(paste0("Composition of the coded sentences only, so this is free of how much ",
         "of each week's entry was about\nculture at all. Sentences pooled across the %d students."),
         length(studs)),
       x = NULL, y = NULL, caption = CAP) +
  base_theme
ggsave(sprintf("reports/fig_composition%s.png", TAG), p2,
       width = 6.6, height = 5.0, dpi = 220, bg = SURFACE)
cat(sprintf("wrote reports/fig_composition%s.png\n", TAG))

# --- FIGURE 3: CQ factor drift ------------------------------------------
if (!is.null(fac) && sum(fac$k) > 0L) {
  fac$factor_ <- factor(fac$factor_, levels = rev(names(FAC_FILL)))
  p3 <- ggplot(fac[!is.na(fac$share), ],
               aes(factor(week, levels = weeks, labels = paste0("w", weeks)), share, fill = factor_)) +
    geom_col(width = 0.68, colour = SURFACE, linewidth = 0.5) +
    geom_text(aes(label = ifelse(share > 0.08, sprintf("%.0f%%", 100 * share), "")),
              position = position_stack(vjust = 0.5), size = 2.7,
              colour = "#ffffff", fontface = "bold") +
    geom_text(data = unique(fac[, c("week", "n")]), aes(y = 1.03, label = paste0("n=", n)),
              inherit.aes = FALSE, size = 2.4, colour = INK3,
              x = factor(unique(fac$week), levels = weeks, labels = paste0("w", weeks))) +
    scale_fill_manual(values = FAC_FILL, breaks = names(FAC_FILL)) +
    scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"), limits = c(0, 1.07)) +
    labs(title = "Which kind of cultural intelligence shows up, and when",
         subtitle = paste0("Ang & Van Dyne factor of the POSITIVE sentences. A drift from Motivational ",
           "towards Metacognitive or\nBehavioural would be the signature of learning rather than mood. ",
           "The counts are small -- this is a hypothesis to\ntest on the full cohort, not a result.\n",
           "BEHAVIOURAL IS IN THE KEY BUT NEVER IN THE DATA: two of the three models never assign it, so a\n",
           "majority for it cannot form. Its absence is a property of the panel, not of the students."),
         x = NULL, y = NULL, caption = CAP) +
    base_theme
  ggsave(sprintf("reports/fig_cq_drift%s.png", TAG), p3,
         width = 7.4, height = 5.5, dpi = 220, bg = SURFACE)
  cat(sprintf("wrote reports/fig_cq_drift%s.png\n", TAG))
}

# --- FIGURE 4: per-student grid -----------------------------------------
g <- series[series$code == "any code", ]
g$pos <- series$rate[series$code == "Positive"]
g$neg <- series$rate[series$code == "Negative"]
g$sel <- series$rate[series$code == "Self awareness"]
gl <- do.call(rbind, lapply(CODES, function(cd) data.frame(
  student = g$student, week = g$week, n = g$n, code = cd,
  rate = switch(cd, Positive = g$pos, Negative = g$neg, `Self awareness` = g$sel),
  stringsAsFactors = FALSE)))
gl$code <- factor(gl$code, levels = CODES)
gl$student <- factor(gl$student, levels = studs)

p4 <- ggplot(gl, aes(factor(week, levels = weeks, labels = paste0("w", weeks)), student)) +
  geom_tile(aes(fill = rate), colour = SURFACE, linewidth = 1.1) +
  geom_text(aes(label = sprintf("%.0f", 100 * rate),
                colour = rate > 0.28), size = 2.6, fontface = "bold") +
  facet_wrap(~ code, nrow = 1) +
  # A NEUTRAL ramp on purpose. One fill scale has to serve all three facets, and
  # a green one would paint a high Negative rate in the colour the rest of this
  # report uses for Positive. Here colour means intensity only; the facet title
  # carries the valence.
  scale_fill_gradientn(colours = c("#f7f6f3", "#dcdae3", "#adaac0", "#726e91", "#403c63"),
                       limits = c(0, max(gl$rate)), name = "share of that week's sentences",
                       labels = function(x) paste0(round(100 * x), "%"),
                       guide = guide_colourbar(barheight = unit(4, "pt"),
                                               barwidth = unit(90, "pt"), title.position = "top")) +
  scale_colour_manual(values = c(`TRUE` = "#ffffff", `FALSE` = "#4a4845"), guide = "none") +
  labs(title = "Every student, every week, as a percentage",
       subtitle = paste0("The same numbers as the trajectory figure, laid out so one student's ",
         "path can be followed across the row.\nShading is intensity only -- the column heading says which code."),
       x = NULL, y = NULL, caption = CAP) +
  base_theme + theme(panel.grid = element_blank())
ggsave(sprintf("reports/fig_student_grid%s.png", TAG), p4,
       width = 9.0, height = 4.3, dpi = 220, bg = SURFACE)
cat(sprintf("wrote reports/fig_student_grid%s.png\n", TAG))
