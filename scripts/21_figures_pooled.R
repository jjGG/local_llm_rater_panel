#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# 21_figures_pooled.R -- the coding surface and the rater pairs, for the WHOLE
#                        cohort in one figure each
#
#   Rscript scripts/21_figures_pooled.R \
#     --pooled results/pooled_units_all7studs.csv --tag _all7studs
#
# scripts/13_figures_9n99.R draws one (human files, model file) pair at a time,
# so a pooled report ended up showing one heat map per batch. The batch split is
# an artefact of how the rating sheets arrive, not a property of the cohort, and
# it has no counterpart in any of the statistics -- those are computed across all
# sentences at once. These two figures take the same view: every student in one
# panel, every rater pair from one table.
#
# READS NO JOURNAL TEXT. Everything comes from the pooled label table.
# ---------------------------------------------------------------------------

repo <- normalizePath(file.path(dirname(sub("^--file=", "",
        commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])), ".."))
setwd(repo)
suppressPackageStartupMessages(library(ggplot2))
source("test_for_consistency/R/agreement.R")
source("R/six_rater.R")

a <- commandArgs(trailingOnly = TRUE)
getopt <- function(f, d = NULL) { i <- match(f, a); if (is.na(i) || i == length(a)) d else a[i + 1L] }
IN  <- getopt("--pooled", "results/pooled_units_all7studs.csv")
TAG <- getopt("--tag", "_all7studs")
DPI <- as.numeric(getopt("--dpi", "170"))

p <- utils::read.csv(IN, stringsAsFactors = FALSE, na.strings = c("NA", ""))

INK <- "#0b0b0b"; INK2 <- "#52514e"; INK3 <- "#8a8880"; SURFACE <- "#fcfcfb"
FILL <- c("Not marked" = "#e9e7e2", "Positive" = "#1baf7a",
          "Negative" = "#eb6834", "Self awareness" = "#2a78d6",
          "(tie)" = "#b9b6ae", "no answer" = "#ffffff")

hum_r <- sort(unique(p$rater[p$rater_kind == "human"]))
mod_r <- sort(unique(p$rater[p$rater_kind == "model"]))
NH <- length(hum_r); NM <- length(mod_r)

# --- unit order ----------------------------------------------------------
u <- unique(p[, c("unit_id", "student", "week")])
k <- regmatches(u$unit_id, regexec("^(.+)_w[0-9]+_q([0-9]+)_s([0-9]+)$", u$unit_id))
u$question <- as.integer(vapply(k, `[`, character(1), 3))
u$sentence <- as.integer(vapply(k, `[`, character(1), 4))
snum <- suppressWarnings(as.numeric(u$student))
u$sord <- if (anyNA(snum)) match(u$student, sort(unique(u$student))) else snum
u <- u[order(u$sord, u$week, u$question, u$sentence), ]
studs <- unique(u$student)
u$panel_lab <- factor(paste("student", u$student),
                      levels = paste("student", studs))

# Each week gets a band as tall as its busiest student, so the weeks line up
# across all facets and a week reads straight across the whole cohort.
weeks <- sort(unique(u$week))
wk_n <- tapply(u$unit_id, list(u$week, u$student), length)
band_h <- apply(wk_n, 1L, max, na.rm = TRUE)[as.character(weeks)]
band_start <- c(0, cumsum(utils::head(band_h, -1)))
names(band_start) <- as.character(weeks)
u$ord <- band_start[as.character(u$week)] +
         stats::ave(seq_len(nrow(u)), u$student, u$week, FUN = seq_along)
Y_MAX <- sum(band_h)

grab <- function(who, col) p[[col]][match(paste(u$unit_id, who),
                                          paste(p$unit_id, p$rater))]

wide <- function(rs, col = "code") vapply(rs, function(r) grab(r, col),
                                          character(nrow(u)))
Mh <- wide(hum_r); Mm <- wide(mod_r)
# A majority column earns its place only when there is more than one rater to
# take a majority OF. With a single reference coder it would just repeat that
# column, reading as corroboration the data does not contain.
SHOW_H <- NH > 1L; SHOW_M <- NM > 1L
# The pooled table stores each side's majority with ties already collapsed to
# NA, which is right for counting but wrong for drawing: a tied panel would be
# painted the same white as a rater who gave no answer at all. Recompute the
# DISPLAY majority from the individual columns so a split shows as "(tie)", and
# keep the NA version for the agreement counts below.
disp_h <- if (SHOW_H) panel_modal(Mh, tie = "(tie)") else Mh[, 1]
disp_m <- panel_modal(Mm, tie = "(tie)")
h_pref <- apply(wide(hum_r, "cq_factor"), 2L, function(v) unname(FACTOR_PREFIX[v]))
dim(h_pref) <- dim(Mh); dimnames(h_pref) <- dimnames(Mh)
disp_h_it <- panel_modal(h_pref, tie = NA_character_)
disp_m_it <- panel_modal(wide(mod_r, "cq_item"), tie = NA_character_)
disp_h_it[disp_h != "Positive"] <- NA_character_
disp_m_it[disp_m != "Positive"] <- NA_character_

# --- long form -----------------------------------------------------------
COLS <- c(hum_r, if (SHOW_H) "humans", mod_r, if (SHOW_M) "models")
KIND <- c(rep("human", NH), if (SHOW_H) "human", rep("model", NM), if (SHOW_M) "model")
SRC  <- c(hum_r, if (SHOW_H) "human panel", mod_r, if (SHOW_M) "model panel")
long <- do.call(rbind, lapply(seq_along(COLS), function(i) {
  src <- SRC[i]
  if (identical(COLS[i], "humans"))      { code <- disp_h; it <- disp_h_it }
  else if (identical(COLS[i], "models")) { code <- disp_m; it <- disp_m_it }
  else {
    code <- grab(src, "code")
    # people carry the FACTOR, shown as its prefix; models the ITEM, shown whole
    it <- if (KIND[i] == "human") unname(FACTOR_PREFIX[grab(src, "cq_factor")])
          else grab(src, "cq_item")
  }
  data.frame(u, column = COLS[i], kind = KIND[i], code = code, item = it,
             stringsAsFactors = FALSE)
}))
long$code[is.na(long$code)] <- "no answer"
long$column <- factor(long$column, levels = COLS)
lv <- intersect(c(PRIMARY_LEVELS, "(tie)", "no answer"), unique(long$code))
long$code <- factor(long$code, levels = lv)
long$item[long$code != "Positive"] <- NA_character_

# With a single human rater there is no "human panel" row to read; that rater's
# own column IS the human verdict. Reading the absent row would make every
# comparison below rest on zero units and report NaN.
hmaj <- if (SHOW_H) grab("human panel", "code") else Mh[, 1]
mmaj <- if (SHOW_M) grab("model panel", "code") else Mm[, 1]
comparable <- sum(!is.na(hmaj) & !is.na(mmaj))
disagree <- u[!is.na(hmaj) & !is.na(mmaj) & hmaj != mmaj, ]
agree_n <- comparable - nrow(disagree)
n_tie <- nrow(u) - comparable

wk_lab <- data.frame(week = weeks, lab_at = band_start + band_h / 2 + 0.5)
wk <- data.frame(at = utils::head(cumsum(band_h), -1) + 0.5)
gap <- NH + as.integer(SHOW_H) + 0.5

kh <- if (NH > 1L) krippendorff_alpha(Mh, PRIMARY_LEVELS)$alpha else NA_real_
kl <- krippendorff_alpha(Mm, PRIMARY_LEVELS)$alpha

# ===========================================================================
# FIGURE 1 -- the coding surface, every student
# ===========================================================================
pA <- ggplot(long, aes(column, ord, fill = code)) +
  geom_tile(colour = SURFACE, linewidth = 0.3) +
  geom_point(data = disagree, aes(x = 0.42, y = ord), inherit.aes = FALSE,
             colour = "#a8392c", size = 0.6) +
  geom_text(data = subset(long, code == "Positive" & !is.na(item)),
            aes(label = item, fontface = ifelse(kind == "human", "italic", "bold")),
            colour = "#08301f", size = 1.25, show.legend = FALSE) +
  geom_hline(data = wk, aes(yintercept = at), colour = "#ffffff", linewidth = 0.7) +
  geom_vline(xintercept = gap, colour = INK3, linewidth = 0.4, linetype = "22") +
  geom_text(data = wk_lab, aes(x = 0.06, y = lab_at, label = paste0("w", week)),
            inherit.aes = FALSE, hjust = 1, size = 2.1, colour = INK3) +
  scale_fill_manual(values = FILL, drop = FALSE, name = NULL,
                    guide = guide_legend(nrow = 1,
                      override.aes = list(colour = "#c9c6bf"))) +
  scale_y_reverse(limits = c(Y_MAX + 0.6, 0.4), expand = expansion(0)) +
  scale_x_discrete(expand = expansion(add = 0.5)) +
  coord_cartesian(xlim = c(-0.9, length(COLS) + 0.5), clip = "off") +
  facet_wrap(~ panel_lab, nrow = 1) +
  labs(
    title = sprintf("%d human rater%s and %d local model%s on the same %d sentences, across %d student%s",
                    NH, if (NH == 1L) "" else "s", NM, if (NM == 1L) "" else "s",
                    nrow(u), length(studs), if (length(studs) == 1L) "" else "s"),
    subtitle = sprintf(paste0(
      sprintf("Left of the dashed line the %s, right of it the machines%s.\n",
              if (NH == 1L) "reference coding" else "people",
              if (SHOW_M) "; `models` is the model block's majority vote" else ""),
      "%s Krippendorff's alpha %.3f within the model panel. ",
      "The two sides agree on %d of the %d sentences\nwhere both reached a verdict (%.0f%%); ",
      "red dots mark the %d where they do not.%s"),
      if (NH > 1L) sprintf("Alpha %.3f within the human panel,", kh) else
        sprintf("`%s` is one written coding, so it has no internal alpha;", hum_r[1]),
      kl, agree_n, comparable, 100 * agree_n / comparable, nrow(disagree),
      if (n_tie > 0L) sprintf(" On %d further sentences a panel split with no majority.", n_tie) else ""),
    caption = paste0(
      "Positive cells carry the cultural-intelligence subclassification. The people coded the FACTOR only, shown as its prefix in italics (MC, COG, MOT, BEH);\n",
      "the models coded the ITEM, shown in bold with its number (MC2, COG3, MOT1). The prefixes are the same alphabet, so the columns line up.\n",
      "Each week occupies a band as tall as its busiest student, so the weeks line up across every panel; a band's blank tail means that student wrote fewer\n",
      if (length(unique(p$batch)) > 1L)
        sprintf("sentences that week. All students are drawn from one pooled table -- the rating sheets arrived in %d batches, but nothing here is split by batch.\n",
                length(unique(p$batch)))
      else "sentences that week. All raters are drawn from one pooled table.\n",
      "Labels only -- no journal text is read.")) +
  theme_minimal(base_size = 9) +
  theme(
    plot.background = element_rect(fill = SURFACE, colour = NA),
    panel.background = element_rect(fill = SURFACE, colour = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(), axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 6.8, colour = INK2),
    strip.text = element_text(face = "bold", size = 9, colour = INK, margin = margin(b = 4)),
    legend.position = "bottom", legend.key.size = unit(9, "pt"),
    legend.text = element_text(size = 8, colour = INK2),
    plot.title = element_text(face = "bold", size = 12.5, colour = INK),
    plot.subtitle = element_text(size = 8, colour = INK2, margin = margin(b = 9)),
    plot.caption = element_text(size = 6.4, colour = INK3, hjust = 0, margin = margin(t = 9)),
    plot.margin = margin(12, 14, 10, 16))

dir.create("reports", showWarnings = FALSE)
f1 <- sprintf("reports/fig_surface%s.png", TAG)
w1 <- max(9, 1.2 + length(studs) * (0.40 + 0.26 * length(COLS)))
h1 <- 3.0 + min(13, max(4, Y_MAX * 0.100))
ggsave(f1, pA, width = w1, height = h1, dpi = DPI, bg = SURFACE, limitsize = FALSE)
cat(sprintf("wrote %s  (%.1f x %.1f in, %d students, %d cols, %d rows, %.2f MB)\n",
            f1, w1, h1, length(studs), length(COLS), Y_MAX, file.size(f1) / 1e6))

# ===========================================================================
# FIGURE 2 -- every rater pair, on every sentence
# ===========================================================================
M <- cbind(Mh, Mm); K <- c(rep("human", NH), rep("model", NM)); ord <- colnames(M)
cells <- do.call(rbind, lapply(utils::combn(ord, 2, simplify = FALSE), function(q) {
  x <- M[, q[1]]; y <- M[, q[2]]; ok <- !is.na(x) & !is.na(y)
  ta <- K[match(q[1], ord)]; tb <- K[match(q[2], ord)]
  data.frame(a = q[1], b = q[2],
             alpha = krippendorff_alpha(M[, q], PRIMARY_LEVELS)$alpha,
             pct = percent_agreement(x[ok], y[ok]),
             type = if (ta == tb) ta else "mixed", stringsAsFactors = FALSE)
}))
cells$row <- factor(cells$b, levels = ord); cells$col <- factor(cells$a, levels = ord)
BORDER <- c(human = "#1b5fa8", model = "#b4600d", mixed = "#9a9790")
TYPE_LAB <- c(human = "human-human", model = "model-model", mixed = "human-model")
byt <- tapply(cells$alpha, cells$type, mean)
present <- names(TYPE_LAB)[names(TYPE_LAB) %in% names(byt)]
xlv <- ord[-length(ord)]; ylv <- rev(ord[-1])
note <- data.frame(x = xlv[min(2L, length(xlv))], y = ylv[length(ylv)],
  txt = paste0("mean alpha by pair type\n\n", paste(vapply(present, function(t)
    sprintf("%-12s %.3f   (%d pairs)", TYPE_LAB[[t]], byt[[t]], sum(cells$type == t)),
    character(1)), collapse = "\n")), stringsAsFactors = FALSE)
lab_col <- unname(c(human = "#1b5fa8", model = "#b4600d")[K]); names(lab_col) <- ord
pair_lab <- function(r) sprintf("%s+%s (%.3f)", r$a, r$b, r$alpha)
top <- cells[order(-cells$alpha), ]
best_line <- paste0("Strongest pair of each kind: ",
  paste(vapply(present, function(t) sprintf("%s %s", TYPE_LAB[[t]],
    pair_lab(top[top$type == t, ][1, ])), character(1)), collapse = ", "), ".\n",
  if ("human" %in% present && "mixed" %in% present) {
    wh <- top[top$type == "human", ][sum(top$type == "human"), ]
    if (top$alpha[top$type == "mixed"][1] > wh$alpha) sprintf(
      "The best human-model pair beats the weakest human-human pair, %s, so the split is not simply people against machines.",
      pair_lab(wh)) else "Every within-type pair outranks every cross-type pair."
  } else "")

pB <- ggplot(cells, aes(col, row)) +
  geom_tile(aes(fill = alpha), colour = SURFACE, linewidth = 1.6) +
  geom_tile(aes(colour = type), fill = NA, linewidth = 0.7) +
  geom_text(aes(label = sprintf("%.3f", alpha),
                colour = ifelse(alpha > 0.80, "hi", "lo")),
            size = 2.9, fontface = "bold", show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.0f%%", 100 * pct)), nudge_y = -0.27,
            size = 2.1, colour = "#4a4845", show.legend = FALSE) +
  geom_text(data = note, aes(x = x, y = y, label = txt), inherit.aes = FALSE,
            hjust = 0, vjust = 0.4, size = 2.6, colour = INK2,
            lineheight = 1.3, family = "mono") +
  scale_fill_gradientn(
    colours = c("#f7eeda", "#e8dcb8", "#bcd3a8", "#7fbb8e", "#3f9b76", "#1c7a63"),
    limits = c(0.5, 0.92), name = "Krippendorff's alpha",
    guide = guide_colourbar(barheight = unit(4, "pt"), barwidth = unit(78, "pt"),
                            title.position = "top")) +
  scale_colour_manual(values = c(BORDER, hi = "#ffffff", lo = "#141414"),
                      breaks = names(BORDER), labels = TYPE_LAB, name = NULL,
                      guide = guide_legend(override.aes = list(
                        fill = NA, linewidth = 1.1, label = FALSE), order = 2)) +
  scale_x_discrete(limits = xlv, position = "top") +
  scale_y_discrete(limits = ylv) +
  coord_fixed(clip = "off") +
  labs(title = "Which raters agree with which",
       subtitle = sprintf(paste0(
         "All %d pairs from the %d-rater panel, on the same %d sentences from %d students. ",
         "Large number is pairwise\nKrippendorff's alpha, small number below it raw percent agreement. ",
         "Border colour marks the kind of pair."), nrow(cells), NH + NM, nrow(u), length(studs)),
       caption = paste0(best_line, "\n",
         sprintf("Pairwise alpha on two raters is noisier than the panel figure and these %d pairs are not independent of one another;\n", nrow(cells)),
         "read the ordering, not the third decimal.")) +
  theme_minimal(base_size = 9) +
  theme(
    plot.background = element_rect(fill = SURFACE, colour = NA),
    panel.background = element_rect(fill = SURFACE, colour = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(), axis.ticks = element_blank(),
    axis.text.y = element_text(size = 8.5, face = "bold", colour = unname(lab_col[ylv])),
    axis.text.x.top = element_text(size = 8.5, face = "bold", angle = 30,
                                   hjust = 0, vjust = 0, colour = unname(lab_col[xlv])),
    legend.position = "bottom", legend.box = "horizontal",
    legend.title = element_text(size = 7.5, colour = INK2),
    legend.text = element_text(size = 7.5, colour = INK2),
    legend.key.size = unit(10, "pt"),
    plot.title = element_text(face = "bold", size = 12.5, colour = INK),
    plot.subtitle = element_text(size = 8.3, colour = INK2, margin = margin(b = 12)),
    plot.caption = element_text(size = 6.6, colour = INK3, hjust = 0, margin = margin(t = 12)),
    plot.margin = margin(12, 22, 10, 14))

f2 <- sprintf("reports/fig_pairs%s.png", TAG)
ggsave(f2, pB, width = 7.4, height = 7.6, dpi = 220, bg = SURFACE)
cat("wrote", f2, "\n")
