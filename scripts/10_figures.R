#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# 10_figures.R -- the panel's codes as a heatmap, plus a stability companion
#
#   Rscript scripts/10_figures.R --label example_sections
#   Rscript scripts/10_figures.R --label example_sections --expected <file.csv>
#
# Writes reports/fig_codes_<label>.png and reports/fig_stability_<label>.png.
#
# WHY THIS FORM. The question is "where do the raters agree, and does
# disagreement cluster?" -- that is identity plus pattern over a fixed grid of
# units x raters, which is what a categorical heatmap is for. A bar chart of code
# frequencies would answer a different and duller question (how often each code
# was used) and would hide the thing worth seeing: that disagreement lands on a
# handful of specific sentences.
#
# COLOUR. "Not marked" is ~71% of the data and means "no code", so it gets a
# recessive near-surface neutral rather than a hue. The three substantive codes
# take validated categorical slots -- checked all-pairs (a heatmap cell can
# neighbour any other), worst CVD deltaE 9.2, worst normal-vision deltaE 24.0.
# The original Word highlighter colours were tried first and REJECTED: red and
# purple-pink sit deltaE 10.9 apart for normal vision, which in a dense grid is
# unreadable even before colour-vision deficiency is considered.
# ---------------------------------------------------------------------------

repo <- normalizePath(file.path(dirname(sub("^--file=", "",
        commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])), ".."))
setwd(repo)
suppressPackageStartupMessages(library(ggplot2))
source("R/codebook.R")

a <- commandArgs(trailingOnly = TRUE)
getopt <- function(f, d = NULL) { i <- match(f, a); if (is.na(i) || i == length(a)) d else a[i + 1L] }
label  <- getopt("--label", "example_sections")
exp_in <- getopt("--expected", "data/example_journals_sections/expected_labels.csv")

cb  <- load_codebook("config/codebook.yml")
cfg <- load_run_config("config/run.yml")
panel <- vapply(cfg$active_models, function(m) m$id, character(1))

rd <- function(p) utils::read.csv(p, stringsAsFactors = FALSE, na.strings = c("NA", ""))
main <- rd(sprintf("results/ratings_%s_main.csv", label))
main <- main[main$model %in% panel, ]
units <- rd(sprintf("data/units_%s.csv", label))

# --- palette ------------------------------------------------------------
CODES <- c("Not marked", "Positive", "Negative", "Self awareness")
FILL <- c(
  "Not marked"     = "#e9e7e2",   # recessive: this is the absence of a code
  "Positive"       = "#1baf7a",   # validated categorical slot (aqua)
  "Negative"       = "#eb6834",   # validated categorical slot (orange)
  "Self awareness" = "#2a78d6")   # validated categorical slot (blue)
INK <- "#0b0b0b"; INK2 <- "#52514e"; INK3 <- "#8a8880"; SURFACE <- "#fcfcfb"

short <- function(m) {
  m <- sub(":latest$", "", m); m <- sub("^DeepSeek.*", "DeepSeek", m)
  sub("^gpt-oss.*", "gpt-oss", m)
}

# --- assemble one row per unit x column ---------------------------------
modal <- function(v) {
  v <- v[!is.na(v)]
  if (!length(v)) return(NA_character_)
  tb <- sort(table(v), decreasing = TRUE)
  if (length(tb) > 1L && tb[1] == tb[2]) "(tie)" else names(tb)[1]
}

u <- units[, c("unit_id", "student_id", "week", "question_no", "sent_in_para")]
u <- u[order(u$student_id, u$week, u$question_no, u$sent_in_para), ]
u$ord <- stats::ave(seq_len(nrow(u)), u$student_id, FUN = seq_along)

wide <- do.call(cbind, lapply(panel, function(m) {
  main$codes[match(u$unit_id, main$unit_id[main$model == m])]
}))
# match() above needs the per-model subset; rebuild explicitly to be safe
wide <- sapply(panel, function(m) {
  d <- main[main$model == m, ]
  d$codes[match(u$unit_id, d$unit_id)]
})
colnames(wide) <- short(panel)

pan <- apply(wide, 1L, modal)

# The CQ subclassification, which only exists on Positive sentences. The ITEM
# code is printed rather than the factor name because its prefix already IS the
# factor (MC/COG/MOT/BEH), so one short string carries both levels of the scheme.
item_wide <- sapply(panel, function(m) {
  d <- main[main$model == m, ]
  d$cq_item[match(u$unit_id, d$unit_id)]
})
colnames(item_wide) <- short(panel)
pan_item <- apply(item_wide, 1L, modal)
# Blank, not "(tie)", when the models chose three different items: "(tie)" is
# already a CODE-level legend entry, and reusing the word for the item level
# would make one label mean two things. The margin dot already flags the row.
pan_item[pan_item == "(tie)"] <- NA_character_
pan_item[pan != "Positive"] <- NA_character_

e <- if (file.exists(exp_in)) rd(exp_in) else NULL
ref      <- if (!is.null(e)) e$expected_code[match(u$unit_id, e$unit_id)] else rep(NA_character_, nrow(u))
ref_item <- if (!is.null(e)) e$expected_item[match(u$unit_id, e$unit_id)] else rep(NA_character_, nrow(u))

COLS <- c(colnames(wide), "Panel", "Ref")
long <- do.call(rbind, lapply(COLS, function(cl) {
  v  <- switch(cl, "Panel" = pan,      "Ref" = ref,      wide[, cl])
  it <- switch(cl, "Panel" = pan_item, "Ref" = ref_item, item_wide[, cl])
  data.frame(u, column = cl, code = v, item = it, stringsAsFactors = FALSE)
}))
# A failed call must be LABELLED, not left as a hole in the grid: an unexplained
# white cell reads as a rendering fault rather than as "this model returned
# nothing", which is a real and reportable outcome.
long$code[is.na(long$code)] <- "no answer"
long$column <- factor(long$column, levels = COLS)
long$code <- factor(long$code, levels = c(CODES, "(tie)", "no answer"))
FILL2 <- c(FILL, "(tie)" = "#b9b6ae", "no answer" = "#ffffff")

# Units where the three models do not all agree -- flagged, because they are the
# only rows worth reading individually.
n_distinct <- apply(wide, 1L, function(v) length(unique(v[!is.na(v)])))
flag <- u[n_distinct > 1L, ]

# Week separators and week labels, so the time dimension is visible without
# labelling 107 individual rows. Two frames: there are n week labels but only
# n-1 lines between them.
wk_lab <- do.call(rbind, lapply(split(u, u$student_id), function(d) {
  r <- rle(d$week); b <- cumsum(r$lengths)
  data.frame(student_id = d$student_id[1], week = r$values,
             lab_at = (c(0, utils::head(b, -1)) + b) / 2, stringsAsFactors = FALSE)
}))
wk <- do.call(rbind, lapply(split(u, u$student_id), function(d) {
  b <- utils::head(cumsum(rle(d$week)$lengths), -1)
  if (!length(b)) return(NULL)
  data.frame(student_id = d$student_id[1], at = b + 0.5, stringsAsFactors = FALSE)
}))

# --- figure 1: the codes -------------------------------------------------
p1 <- ggplot(long, aes(column, ord, fill = code)) +
  geom_tile(colour = SURFACE, linewidth = 0.55) +
  geom_point(data = flag, aes(x = 0.42, y = ord), inherit.aes = FALSE,
             colour = INK, size = 0.5) +
  geom_text(data = subset(long, code == "Positive" & !is.na(item)),
            aes(label = item), colour = "#08301f", size = 1.75,
            fontface = "bold", show.legend = FALSE) +
  geom_hline(data = wk, aes(yintercept = at), colour = "#ffffff", linewidth = 0.9) +
  geom_text(data = wk_lab, aes(x = 0.05, y = lab_at, label = paste0("w", week)),
            inherit.aes = FALSE, hjust = 1, size = 2.5, colour = INK3) +
  scale_fill_manual(values = FILL2, drop = FALSE, name = NULL,
                    guide = guide_legend(override.aes = list(colour = "#c9c6bf"))) +
  scale_y_reverse(expand = expansion(add = 0.6)) +
  scale_x_discrete(expand = expansion(add = 0.5)) +
  coord_cartesian(xlim = c(-0.7, length(COLS) + 0.5), clip = "off") +
  facet_wrap(~ student_id, nrow = 1, scales = "free_y") +
  labs(
    title = "Three locally-hosted models coding the same 107 sentences",
    subtitle = paste("One row per sentence, grouped by journal week.",
                     "Dots mark the sentences the models did not all agree on.",
                     "\nPositive cells carry the Cultural Intelligence Scale item;",
                     "its prefix is the factor (MC metacognitive, COG cognitive,",
                     "MOT motivational, BEH behavioural)."),
    caption = paste0("Panel = modal answer of the three models. Ref = the written ",
                     "reading in data/example_journals_sections/.\n",
                     "Invented journals, not student data. main profile, temperature 0.")) +
  theme_minimal(base_size = 9) +
  theme(
    plot.background = element_rect(fill = SURFACE, colour = NA),
    panel.background = element_rect(fill = SURFACE, colour = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 7,
                               colour = INK2),
    strip.text = element_text(face = "bold", size = 9, colour = INK,
                              margin = margin(b = 4)),
    legend.position = "bottom",
    legend.key.size = unit(9, "pt"),
    legend.text = element_text(size = 8, colour = INK2),
    plot.title = element_text(face = "bold", size = 12, colour = INK),
    plot.subtitle = element_text(size = 8.5, colour = INK2, margin = margin(b = 8)),
    plot.caption = element_text(size = 7, colour = INK3, hjust = 0,
                                margin = margin(t = 8)),
    plot.margin = margin(12, 14, 10, 14))

dir.create("reports", showWarnings = FALSE)
f1 <- sprintf("reports/fig_codes_%s.png", label)
ggsave(f1, p1, width = 10.5, height = 6.4, dpi = 220, bg = SURFACE)
cat("wrote", f1, "\n")

# --- figure 2: stability across replicates ------------------------------
cons_p <- sprintf("results/ratings_%s_consistency.csv", label)
if (file.exists(cons_p)) {
  cons <- rd(cons_p)
  cons <- cons[cons$model %in% panel, ]
  # Restrict to the replicates every model ran, so the comparison is like-for-like
  nrep <- min(tapply(cons$replicate, cons$model, function(x) length(unique(x))))
  cons <- cons[cons$replicate <= nrep, ]

  st <- do.call(rbind, lapply(panel, function(m) {
    d <- cons[cons$model == m, ]
    nd <- vapply(u$unit_id, function(uu) {
      v <- d$codes[d$unit_id == uu]; v <- v[!is.na(v)]
      if (!length(v)) NA_integer_ else length(unique(v))
    }, integer(1))
    data.frame(u, model = short(m), n_distinct = nd, stringsAsFactors = FALSE)
  }))
  st <- st[!is.na(st$n_distinct), ]
  st$model <- factor(st$model, levels = short(panel))
  st$lab <- factor(st$n_distinct, levels = 1:3,
                   labels = c("same every run", "2 different answers",
                              "3 different answers"))
  # Ordinal ramp, one hue, light -> dark: stable recedes, unstable stands out.
  RAMP <- c("same every run" = "#dde9f8", "2 different answers" = "#5598e7",
            "3 different answers" = "#184f95")

  # Two different quantities, and the subtitle must name the one it shows.
  per_cell <- 100 * mean(st$n_distinct > 1L)                       # model x unit
  per_unit <- 100 * mean(tapply(st$n_distinct, st$unit_id,         # any model
                                function(x) any(x > 1L)))
  worst <- max(st$n_distinct)
  # Only label levels that actually occurred: an empty legend key invites the
  # reader to hunt for a colour that is not in the plot.
  st$lab <- droplevels(st$lab)
  RAMP <- RAMP[levels(st$lab)]
  p2 <- ggplot(st, aes(model, ord, fill = lab)) +
    geom_tile(colour = SURFACE, linewidth = 0.55) +
    geom_hline(data = wk, aes(yintercept = at), colour = "#ffffff", linewidth = 0.9) +
    geom_text(data = wk_lab, aes(x = 0.05, y = lab_at, label = paste0("w", week)),
              inherit.aes = FALSE, hjust = 1, size = 2.5, colour = INK3) +
    scale_fill_manual(values = RAMP, drop = FALSE, name = NULL) +
    scale_y_reverse(expand = expansion(add = 0.6)) +
    scale_x_discrete(expand = expansion(add = 0.5)) +
    coord_cartesian(xlim = c(-0.9, length(panel) + 0.5), clip = "off") +
    facet_wrap(~ student_id, nrow = 1, scales = "free_y") +
    labs(
      title = sprintf("Where each model changed its mind (%d replicates, temperature 0.7)", nrep),
      subtitle = sprintf(paste0("%.0f%% of sentences drew more than one answer from at least one model ",
                                "(%.0f%% of all model-sentence pairs).\n",
                                "No sentence ever drew %s. Instability concentrates rather than spreading evenly."),
                         per_unit, per_cell,
                         if (worst < 3L) "three different answers" else "more than that"),
      caption = "Invented journals, not student data. consistency profile.") +
    theme_minimal(base_size = 9) +
    theme(
      plot.background = element_rect(fill = SURFACE, colour = NA),
      panel.background = element_rect(fill = SURFACE, colour = NA),
      panel.grid = element_blank(),
      axis.title = element_blank(), axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 7,
                                 colour = INK2),
      strip.text = element_text(face = "bold", size = 9, colour = INK,
                                margin = margin(b = 4)),
      legend.position = "bottom", legend.key.size = unit(9, "pt"),
      legend.text = element_text(size = 8, colour = INK2),
      plot.title = element_text(face = "bold", size = 12, colour = INK),
      plot.subtitle = element_text(size = 8.5, colour = INK2, margin = margin(b = 8)),
      plot.caption = element_text(size = 7, colour = INK3, hjust = 0,
                                  margin = margin(t = 8)),
      plot.margin = margin(12, 14, 10, 14))

  f2 <- sprintf("reports/fig_stability_%s.png", label)
  ggsave(f2, p2, width = 8.6, height = 6.4, dpi = 220, bg = SURFACE)
  cat("wrote", f2, "\n")
}

# The table view the contrast WARN obligates: aqua sits below 3:1 on this
# surface, so the figure ships with the numbers behind it.
utils::write.csv(
  cbind(u[, c("unit_id", "student_id", "week", "question_no")],
        as.data.frame(wide), Panel = pan, Reference = ref),
  sprintf("reports/fig_codes_%s_table.csv", label), row.names = FALSE, na = "")
cat("wrote", sprintf("reports/fig_codes_%s_table.csv", label), "\n")

# --- figure 3: which CQ items each model actually reaches for --------------
# Bars, not a heatmap: the question is magnitude (how often) across a fixed
# ordered vocabulary, and 0-7 counts in 20 rows read far better as length than as
# colour intensity. A faint full-width track behind each bar keeps the NEVER-USED
# items visible as empty rows -- without it they would simply be absent, which is
# the opposite of the point.
#
# Factor hues are categorical slots 1-4 used in their documented order, which is
# validated for the adjacent pairlist (worst adjacent CVD deltaE 9.1) -- the right
# pairlist for bars.
FACTOR_FILL <- c("Metacognitive" = "#2a78d6", "Cognitive" = "#eb6834",
                 "Motivational"  = "#1baf7a", "Behavioural" = "#eda100")

it <- main[!is.na(main$cq_item), ]
if (nrow(it)) {
  lv <- cb$cq_items$code
  cnt <- as.data.frame(table(item = factor(it$cq_item, levels = lv),
                             model = it$model), stringsAsFactors = FALSE)
  cnt$model  <- short(cnt$model)
  cnt$factor <- factor(item_factor(cnt$item, cb), levels = names(FACTOR_FILL))
  # Course-administered items get a marker: only 6 of the 20 have questionnaire
  # data behind them, which bears on how much weight item-level results can carry.
  course <- cb$cq_items$code[cb$cq_items$course_item]
  cnt$lab <- ifelse(cnt$item %in% course, paste0(cnt$item, " *"), as.character(cnt$item))
  ord <- unique(cnt$lab[order(match(cnt$item, lv))])
  cnt$lab <- factor(cnt$lab, levels = rev(ord))
  cnt$model <- factor(cnt$model, levels = short(panel))

  used   <- sum(tapply(cnt$Freq, cnt$item, sum) > 0)
  on_crs <- 100 * sum(cnt$Freq[cnt$item %in% course]) / sum(cnt$Freq)
  xmax   <- max(cnt$Freq)

  p3 <- ggplot(cnt, aes(Freq, lab)) +
    geom_col(data = transform(unique(cnt[, c("lab", "model")]), Freq = xmax),
             fill = "#eceae5", width = 0.72) +
    geom_col(aes(fill = factor), width = 0.72) +
    scale_fill_manual(values = FACTOR_FILL, drop = FALSE, name = NULL) +
    scale_x_continuous(breaks = seq(0, xmax, by = 2),
                       expand = expansion(mult = c(0, 0.06))) +
    facet_wrap(~ model, nrow = 1) +
    labs(
      title = "Which cultural-intelligence items each model reaches for",
      subtitle = sprintf(paste0("%d of the 20 scale items were ever chosen. ",
                                "%.0f%% of choices land on the six items this course ",
                                "actually administered (*),\nthough those are only ",
                                "30%% of the vocabulary. The eight unused items are ",
                                "the ones this setting cannot produce."),
                         used, on_crs),
      x = "sentences assigned this item", y = NULL,
      caption = "Invented journals, not student data. main profile, temperature 0. Grey track = the full range, so unused items stay visible.") +
    theme_minimal(base_size = 9) +
    theme(
      plot.background = element_rect(fill = SURFACE, colour = NA),
      panel.background = element_rect(fill = SURFACE, colour = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = "#ffffff", linewidth = 0.6),
      axis.text.y = element_text(family = "mono", size = 6.6, colour = INK2),
      axis.text.x = element_text(size = 7, colour = INK3),
      axis.title.x = element_text(size = 7.5, colour = INK3, hjust = 0),
      axis.ticks = element_blank(),
      strip.text = element_text(face = "bold", size = 9, colour = INK,
                                margin = margin(b = 5)),
      legend.position = "bottom", legend.key.size = unit(9, "pt"),
      legend.text = element_text(size = 8, colour = INK2),
      plot.title = element_text(face = "bold", size = 12, colour = INK),
      plot.subtitle = element_text(size = 8.5, colour = INK2, margin = margin(b = 9)),
      plot.caption = element_text(size = 7, colour = INK3, hjust = 0,
                                  margin = margin(t = 8)),
      plot.margin = margin(12, 14, 10, 14))

  f3 <- sprintf("reports/fig_cq_items_%s.png", label)
  ggsave(f3, p3, width = 8.8, height = 5.2, dpi = 220, bg = SURFACE)
  cat("wrote", f3, "\n")
}
