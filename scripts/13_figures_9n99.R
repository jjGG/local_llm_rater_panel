#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# 13_figures_9n99.R -- the six-rater panel, drawn two ways
#
#   Rscript scripts/13_figures_9n99.R \
#     --human results/ratings_9n99_Human.tsv \
#     --llm   results/ratings_9n99_main.csv --label 9n99
#
#   figure A  every sentence x every rater, the raw coding surface
#   figure B  the 15 rater pairs as an agreement matrix
#
# TWO GRANULARITIES, SHOWN HONESTLY. The humans coded the CQ subclassification
# at FACTOR level; the models coded it at ITEM level. Rather than flatten the
# models or invent items for the humans, both print in the same alphabet:
# humans get the factor prefix in italics (MC, COG, MOT, BEH), models the full
# item code in bold (MC2, COG3, MOT1). The prefixes line up, so the columns
# compare directly, and the missing digit is exactly where the humans did not
# code that level.
#
# READS NO JOURNAL TEXT. Row order is parsed from the unit_id strings.
# ---------------------------------------------------------------------------

repo <- normalizePath(file.path(dirname(sub("^--file=", "",
        commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])), ".."))
setwd(repo)
suppressPackageStartupMessages(library(ggplot2))
source("test_for_consistency/R/agreement.R")
source("R/six_rater.R")

a <- commandArgs(trailingOnly = TRUE)
getopt <- function(f, d = NULL) { i <- match(f, a); if (is.na(i) || i == length(a)) d else a[i + 1L] }
H_IN <- getopt("--human", "results/ratings_9n99_Human.tsv")
L_IN <- getopt("--llm",   "results/ratings_9n99_main.csv")
LAB  <- getopt("--label", "9n99")
OMIT <- getopt("--omit",  "results/Omit_ids.txt")
RMAP <- getopt("--remap", "results/remap_9n99.csv")
if (identical(OMIT, "none")) OMIT <- NULL

if (identical(RMAP, "none") || !file.exists(RMAP)) RMAP <- NULL
al <- align_raters(H_IN, L_IN, omit_file = OMIT, remap_file = RMAP)
n_omit <- length(intersect(al$omit$human, al$omit$model))
u <- al$units; n <- nrow(u)

# --- week-aligned rows ---------------------------------------------------
# Each week gets a band as tall as its busiest student, so w1/w4/w7/w10 begin at
# the same height in every facet and a week can be read straight across the
# figure. A student with fewer sentences in a week leaves that band's tail blank,
# which is also the honest way to show that they wrote less that week.
weeks <- sort(unique(u$week))
wk_n <- tapply(u$unit_id, list(u$week, u$student), length)
band_h <- apply(wk_n, 1L, max, na.rm = TRUE)[as.character(weeks)]
band_start <- c(0, cumsum(utils::head(band_h, -1)))
names(band_start) <- as.character(weeks)

u <- u[order(u$student, u$week, u$question, u$sentence), ]
u$ord <- band_start[as.character(u$week)] +
         stats::ave(seq_len(nrow(u)), u$student, u$week, FUN = seq_along)
Y_MAX <- sum(band_h)

FILL <- c("Not marked" = "#e9e7e2", "Positive" = "#1baf7a",
          "Negative" = "#eb6834", "Self awareness" = "#2a78d6",
          "(tie)" = "#b9b6ae", "no answer" = "#ffffff")
INK <- "#0b0b0b"; INK2 <- "#52514e"; INK3 <- "#8a8880"; SURFACE <- "#fcfcfb"

h_mod <- panel_modal(al$h_code); l_mod <- panel_modal(al$l_code)
h_lab <- apply(al$h_cq, 2L, function(v) unname(FACTOR_PREFIX[v]))
dim(h_lab) <- dim(al$h_cq); dimnames(h_lab) <- dimnames(al$h_cq)
h_mod_cq <- panel_modal(h_lab, tie = NA_character_)
l_mod_it <- panel_modal(al$l_item, tie = NA_character_)
h_mod_cq[h_mod != "Positive"] <- NA_character_
l_mod_it[l_mod != "Positive"] <- NA_character_

# ===========================================================================
# FIGURE A -- the coding surface
# ===========================================================================
# A majority column earns its place only when there is something to take a
# majority OF. With a single rater on a side it would just repeat that rater's
# column, which reads as corroboration the data does not contain.
SHOW_HMAJ <- length(al$humans) > 1L
SHOW_LMAJ <- length(al$models) > 1L
COLS <- c(al$humans, if (SHOW_HMAJ) "humans", al$models, if (SHOW_LMAJ) "models")
KIND <- c(rep("human", length(al$humans)), if (SHOW_HMAJ) "human",
          rep("model", length(al$models)), if (SHOW_LMAJ) "model")
long <- do.call(rbind, lapply(seq_along(COLS), function(i) {
  cl <- COLS[i]
  if (cl == "humans")          { v <- h_mod; it <- h_mod_cq }
  else if (cl == "models")     { v <- l_mod; it <- l_mod_it }
  else if (KIND[i] == "human") { v <- al$h_code[, cl]; it <- h_lab[, cl] }
  else                         { v <- al$l_code[, cl]; it <- al$l_item[, cl] }
  data.frame(u, column = cl, kind = KIND[i], code = v, item = it,
             stringsAsFactors = FALSE)
}))
long$code[is.na(long$code)] <- "no answer"
long$column <- factor(long$column, levels = COLS)
# Keep only the levels that actually occur: an empty legend swatch invites the
# reader to hunt the figure for a colour that is not in it.
lv <- intersect(c(PRIMARY_LEVELS, "(tie)", "no answer"), unique(long$code))
long$code <- factor(long$code, levels = lv)
long$item[long$code != "Positive"] <- NA_character_

# h_mod/l_mod carry "(tie)" so the majority columns can SHOW a split panel.
# For counting, a tie is not a verdict, so it is excluded rather than compared
# as if it were a fifth label -- otherwise a tie against any code would read as
# a disagreement and the figure would contradict scripts/12_six_rater.R.
h_maj <- panel_modal(al$h_code, tie = NA_character_)
l_maj <- panel_modal(al$l_code, tie = NA_character_)
comparable <- sum(!is.na(h_maj) & !is.na(l_maj))
disagree <- u[!is.na(h_maj) & !is.na(l_maj) & h_maj != l_maj, ]
agree_n  <- comparable - nrow(disagree)
n_tie    <- nrow(u) - comparable

# No panel_lab: the bands are identical in every facet now, so one set of
# separators and labels is drawn across all of them.
wk_lab <- data.frame(week = weeks, lab_at = band_start + band_h / 2 + 0.5)
wk <- data.frame(at = utils::head(cumsum(band_h), -1) + 0.5)
# The divider sits after the last human column, whether or not a human majority
# column was drawn.
gap <- length(al$humans) + as.integer(SHOW_HMAJ) + 0.5

NH <- length(al$humans); NM <- length(al$models)
kh <- if (NH > 1L) krippendorff_alpha(al$h_code, PRIMARY_LEVELS)$alpha else NA_real_
kl <- if (NM > 1L) krippendorff_alpha(al$l_code, PRIMARY_LEVELS)$alpha else NA_real_
plural <- function(k, s) sprintf("%d %s%s", k, s, if (k == 1L) "" else "s")

side_lab <- if (NH > 1L && NM > 1L)
  "`humans` and `models` are each block's majority vote" else if (NM > 1L)
  "`models` is the model block's majority vote" else "one rater a side"
alpha_lab <- {
  if (NH > 1L) {
    sprintf("Krippendorff's alpha %.3f within the human panel, %.3f within the model panel.", kh, kl)
  } else {
    sprintf(paste0("A single human rater, so there is no human-panel alpha to report; ",
                   "within the model panel alpha is %.3f."), kl)
  }
}
agree_lab <- {
  if (NH > 1L) {
    sprintf(paste0("The two majorities agree on %d of the %d sentences where both panels ",
                   "reached one (%.0f%%);\nred dots mark the %d where they do not."),
            agree_n, comparable, 100 * agree_n / comparable, nrow(disagree))
  } else {
    sprintf(paste0("%s agrees with the model majority on %d of %d sentences (%.0f%%); ",
                   "red dots mark the %d where they do not."),
            al$humans[1], agree_n, comparable, 100 * agree_n / comparable, nrow(disagree))
  }
}
tie_lab <- if (n_tie > 0L)
  sprintf(" On %d further sentence%s a panel split with no majority.",
          n_tie, if (n_tie == 1L) "" else "s") else ""

# Braces matter here: at top level `x <- if (a) f()` followed by `else` on the
# next line is a parse error, because the assignment already closed.
prov_lab <- {
  if (length(al$divergent$blocks)) {
    sprintf("Student %s week %s is absent: the two files segmented it differently, so its sentence ids are not comparable. ",
            sub(" .*", "", al$divergent$blocks[1]), sub(".* ", "", al$divergent$blocks[1]))
  } else if (al$n_remapped > 0L) {
    sprintf("Student 9 week 1 is realigned by the remap table, pairing the %d units the two files numbered differently. ",
            al$n_remapped)
  } else ""
}
omit_lab <- {
  if (n_omit > 0L) {
    sprintf("%d units are excluded as question prompts that survived segmentation rather than student prose. ", n_omit)
  } else "The two files agree on every sentence id; nothing is excluded. "
}

pA <- ggplot(long, aes(column, ord, fill = code)) +
  geom_tile(colour = SURFACE, linewidth = 0.35) +
  geom_point(data = disagree, aes(x = 0.42, y = ord), inherit.aes = FALSE,
             colour = "#a8392c", size = 0.7) +
  geom_text(data = subset(long, code == "Positive" & !is.na(item)),
            aes(label = item, fontface = ifelse(kind == "human", "italic", "bold")),
            colour = "#08301f", size = 1.45, show.legend = FALSE) +
  geom_hline(data = wk, aes(yintercept = at), colour = "#ffffff", linewidth = 0.8) +
  geom_vline(xintercept = gap, colour = INK3, linewidth = 0.45, linetype = "22") +
  geom_text(data = wk_lab, aes(x = 0.06, y = lab_at, label = paste0("w", week)),
            inherit.aes = FALSE, hjust = 1, size = 2.3, colour = INK3) +
  scale_fill_manual(values = FILL, drop = FALSE, name = NULL,
                    guide = guide_legend(nrow = 1,
                      override.aes = list(colour = "#c9c6bf"))) +
  # A SHARED y range on purpose. With free scales the student with fewer
  # sentences gets physically taller rows, which makes a side-by-side read of
  # "how much got marked" wrong.
  scale_y_reverse(limits = c(Y_MAX + 0.6, 0.4), expand = expansion(0)) +
  scale_x_discrete(expand = expansion(add = 0.5)) +
  coord_cartesian(xlim = c(-0.9, length(COLS) + 0.5), clip = "off") +
  facet_wrap(~ panel_lab, nrow = 1) +
  labs(
    title = sprintf("%s and %s on the same %d sentences, across %s",
                    plural(NH, "human rater"), plural(NM, "local model"), n,
                    plural(length(unique(u$student)), "student")),
    subtitle = sprintf("Left of the dashed line the people, right of it the machines; %s.\n%s\n%s%s",
                       side_lab, alpha_lab, agree_lab, tie_lab),
    caption = paste0(
      sprintf("Positive cells carry the cultural-intelligence subclassification. The human%s coded the FACTOR only, shown as its prefix in italics (MC, COG, MOT, BEH);\n",
              if (NH == 1L) "" else "s"),
      "the models coded the ITEM, shown in bold with its number (MC2, COG3, MOT1). The prefixes are the same alphabet, so the columns line up.\n",
      "Each week occupies a band as tall as its busiest student, so the weeks line up across every panel; a band's blank tail means that student wrote\n",
      "fewer sentences that week. ", prov_lab, omit_lab, "\n",
      "Labels only -- no journal text is read.")) +
  theme_minimal(base_size = 9) +
  theme(
    plot.background = element_rect(fill = SURFACE, colour = NA),
    panel.background = element_rect(fill = SURFACE, colour = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(), axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 7.5, colour = INK2),
    strip.text = element_text(face = "bold", size = 9.5, colour = INK, margin = margin(b = 4)),
    legend.position = "bottom", legend.key.size = unit(9, "pt"),
    legend.text = element_text(size = 8, colour = INK2),
    plot.title = element_text(face = "bold", size = 12.5, colour = INK),
    plot.subtitle = element_text(size = 8.3, colour = INK2, margin = margin(b = 9)),
    plot.caption = element_text(size = 6.6, colour = INK3, hjust = 0, margin = margin(t = 9)),
    plot.margin = margin(12, 14, 10, 16))

dir.create("reports", showWarnings = FALSE)
fA <- sprintf("reports/fig_combined_%s.png", LAB)
# Size from the content: one facet per student, one column per rater, one row
# per sentence in the tallest week stack. A fixed size silently squashes the
# cells as soon as another student or rater joins.
n_fac <- length(unique(u$student))
wA <- max(7, 1.1 + n_fac * (0.42 + 0.30 * length(COLS)))
# Cap the panel height. A batch with few students but long journals would
# otherwise render as a metre-tall sliver that no one can print; past ~13in the
# rows simply get shorter instead.
hA <- 2.9 + min(13, max(4, Y_MAX * 0.105))
ggsave(fA, pA, width = wA, height = hA, dpi = 200, bg = SURFACE, limitsize = FALSE)
cat(sprintf("wrote %s  (%.1f x %.1f in, %d facets, %d cols, %d rows)\n",
            fA, wA, hA, n_fac, length(COLS), Y_MAX))

# ===========================================================================
# FIGURE B -- the 15 rater pairs
# ===========================================================================
m_all <- cbind(al$h_code, al$l_code)
K <- c(rep("human", ncol(al$h_code)), rep("model", ncol(al$l_code)))
ord <- colnames(m_all)

cells <- do.call(rbind, lapply(utils::combn(ord, 2, simplify = FALSE), function(p) {
  x <- m_all[, p[1]]; y <- m_all[, p[2]]; ok <- !is.na(x) & !is.na(y)
  ka <- krippendorff_alpha(m_all[, p], PRIMARY_LEVELS)$alpha
  ta <- K[match(p[1], ord)]; tb <- K[match(p[2], ord)]
  data.frame(a = p[1], b = p[2], alpha = ka,
             pct = percent_agreement(x[ok], y[ok]),
             type = if (ta == tb) ta else "mixed", stringsAsFactors = FALSE)
}))
# fill the lower triangle: row = later rater, col = earlier rater
cells$row <- factor(cells$b, levels = ord)
cells$col <- factor(cells$a, levels = ord)

BORDER <- c(human = "#1b5fa8", model = "#b4600d", mixed = "#9a9790")

# Summary for the empty upper triangle, which otherwise does no work.
byt <- tapply(cells$alpha, cells$type, mean)
xlv <- ord[-length(ord)]; ylv <- rev(ord[-1])
# Built from the pair types that actually occur. With a single rater on one
# side there are no within-type pairs for it at all, and a hard-coded row would
# either crash or print an empty mean.
TYPE_LAB <- c(human = "human-human", model = "model-model", mixed = "human-model")
present <- names(TYPE_LAB)[names(TYPE_LAB) %in% names(byt)]
note <- data.frame(
  # a discrete y scale is built bottom-up, so the last level is the TOP row
  x = xlv[min(2L, length(xlv))], y = ylv[length(ylv)],
  txt = paste0("mean alpha by pair type\n\n",
    paste(vapply(present, function(t) sprintf("%-12s %.3f   (%d pair%s)",
        TYPE_LAB[[t]], byt[[t]], sum(cells$type == t),
        if (sum(cells$type == t) == 1L) "" else "s"), character(1)),
      collapse = "\n")),
  stringsAsFactors = FALSE)

# Rater names carry their own type, so the axis does the job the stray
# diagonal markers were doing badly.
lab_col <- unname(c(human = "#1b5fa8", model = "#b4600d")[K])
names(lab_col) <- ord

pair_lab <- function(r) sprintf("%s+%s (%.3f)", r$a, r$b, r$alpha)
top <- cells[order(-cells$alpha), ]
best_line <- paste0(
  "Strongest pair of each kind: ",
  paste(vapply(present, function(t)
    sprintf("%s %s", TYPE_LAB[[t]], pair_lab(top[top$type == t, ][1, ])),
    character(1)), collapse = ", "), ".\n",
  # The people-against-machines claim is only meaningful when there IS more than
  # one human to compare against.
  if ("human" %in% present && "mixed" %in% present) {
    wh <- top[top$type == "human", ][sum(top$type == "human"), ]
    if (top$alpha[top$type == "mixed"][1] > wh$alpha) sprintf(
      "The best human-model pair beats the weakest human-human pair, %s, so the split is not simply people against machines.",
      pair_lab(wh)) else "Every within-type pair outranks every cross-type pair."
  } else sprintf(
    "With a single human rater there are no human-human pairs, so nothing here speaks to how %s compares with another person.",
    al$humans[1]))

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
                      breaks = names(BORDER),
                      labels = c(human = "human-human", model = "model-model",
                                 mixed = "human-model"),
                      name = NULL,
                      guide = guide_legend(override.aes = list(
                        fill = NA, linewidth = 1.1, label = FALSE), order = 2)) +
  scale_x_discrete(limits = xlv, position = "top") +
  scale_y_discrete(limits = ylv) +
  coord_fixed(clip = "off") +
  labs(
    title = "Which raters agree with which",
    subtitle = sprintf(paste0(
      "All %d pairs from the %d-rater panel, on the same %d sentences. Large ",
      "number is pairwise Krippendorff's alpha,\nsmall number below it is raw ",
      "percent agreement. Border colour marks the kind of pair."),
      nrow(cells), NH + NM, n),
    # Computed, not written by hand: a caption asserting which pair wins would
    # quietly become false the next time a unit enters or leaves the set.
    caption = paste0(best_line, "\n",
      sprintf("Pairwise alpha on two raters is noisier than the panel figure and these %d pairs are not independent of one another;\n", nrow(cells)),
      "read the ordering, not the third decimal.")) +
  theme_minimal(base_size = 9) +
  theme(
    plot.background = element_rect(fill = SURFACE, colour = NA),
    panel.background = element_rect(fill = SURFACE, colour = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(), axis.ticks = element_blank(),
    axis.text.y = element_text(size = 8.5, face = "bold",
                               colour = unname(lab_col[ylv])),
    axis.text.x.top = element_text(size = 8.5, face = "bold", angle = 30,
                                   hjust = 0, vjust = 0,
                                   colour = unname(lab_col[xlv])),
    legend.position = "bottom", legend.box = "horizontal",
    legend.title = element_text(size = 7.5, colour = INK2),
    legend.text = element_text(size = 7.5, colour = INK2),
    legend.key.size = unit(10, "pt"),
    plot.title = element_text(face = "bold", size = 12.5, colour = INK),
    plot.subtitle = element_text(size = 8.3, colour = INK2, margin = margin(b = 12)),
    plot.caption = element_text(size = 6.6, colour = INK3, hjust = 0, margin = margin(t = 12)),
    plot.margin = margin(12, 22, 10, 14))

fB <- sprintf("reports/fig_pairwise_%s.png", LAB)
ggsave(fB, pB, width = 7.4, height = 7.6, dpi = 220, bg = SURFACE)
cat("wrote", fB, "\n")
