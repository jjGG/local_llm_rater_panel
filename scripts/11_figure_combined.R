#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# 11_figure_combined.R -- the six-rater panel on one heat map
#
#   Rscript scripts/11_figure_combined.R \
#     --human results/ratings_test2_main_STJ_real.csv \
#     --llm   results/ratings_test2_main_real.csv \
#     --out   reports/fig_combined_test2.png
#
# Writes the figure plus a companion label table.
#
# TWO GRANULARITIES, SHOWN HONESTLY. The human raters coded the CQ
# subclassification at FACTOR level only; the models coded it at ITEM level.
# Rather than flatten the models down or invent items for the humans, both are
# printed in the same alphabet: humans get the factor prefix (MC, COG, MOT, BEH),
# models get the full item code (MC2, COG3, MOT1). The prefixes line up, so the
# columns are directly comparable, and the missing digit is exactly where the
# humans did not code that level. Nothing is hidden and nothing is fabricated.
#
# READS NO JOURNAL TEXT. Row ordering is parsed out of the unit_id strings
# (<student>_w<week>_q<question>_s<sentence>), so the units file -- which does
# hold the sentences -- is never opened.
# ---------------------------------------------------------------------------

repo <- normalizePath(file.path(dirname(sub("^--file=", "",
        commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])), ".."))
setwd(repo)
suppressPackageStartupMessages(library(ggplot2))

a <- commandArgs(trailingOnly = TRUE)
getopt <- function(f, d = NULL) { i <- match(f, a); if (is.na(i) || i == length(a)) d else a[i + 1L] }
H_IN  <- getopt("--human", "results/ratings_test2_main_STJ_real.csv")
L_IN  <- getopt("--llm",   "results/ratings_test2_main_real.csv")
OUT   <- getopt("--out",   "reports/fig_combined_test2.png")

rd <- function(p) utils::read.csv(p, stringsAsFactors = FALSE, na.strings = c("NA", ""))
hum <- rd(H_IN); llm <- rd(L_IN)

# --- vocabulary ---------------------------------------------------------
CODES <- c("Not marked", "Positive", "Negative", "Self awareness")
FILL <- c("Not marked" = "#e9e7e2", "Positive" = "#1baf7a",
          "Negative" = "#eb6834", "Self awareness" = "#2a78d6",
          "(tie)" = "#b9b6ae", "not coded" = "#ffffff")
INK <- "#0b0b0b"; INK2 <- "#52514e"; INK3 <- "#8a8880"; SURFACE <- "#fcfcfb"

# The humans wrote "Behavioral"; the codebook spells it "Behavioural". Folded
# here rather than edited in their file, which stays as they delivered it.
FACTOR_PREFIX <- c(Metacognitive = "MC", Cognitive = "COG",
                   Motivational = "MOT", Behavioural = "BEH",
                   Behavioral = "BEH")

modal <- function(v) {
  v <- v[!is.na(v)]
  if (!length(v)) return(NA_character_)
  tb <- sort(table(v), decreasing = TRUE)
  if (length(tb) > 1L && tb[1] == tb[2]) "(tie)" else names(tb)[1]
}
short <- function(m) {
  m <- sub(":latest$", "", m); m <- sub("^DeepSeek.*", "DeepSeek", m)
  sub("^gpt-oss.*", "gpt-oss", m)
}

# --- unit order, parsed from the ids (never from the text file) ----------
ids <- sort(unique(c(hum$unit_id, llm$unit_id)))
key <- regmatches(ids, regexec("^(.+)_w([0-9]+)_q([0-9]+)_s([0-9]+)$", ids))
stopifnot(all(lengths(key) == 5L))
u <- data.frame(
  unit_id  = ids,
  student  = vapply(key, `[`, character(1), 2),
  week     = as.integer(vapply(key, `[`, character(1), 3)),
  question = as.integer(vapply(key, `[`, character(1), 4)),
  sentence = as.integer(vapply(key, `[`, character(1), 5)),
  stringsAsFactors = FALSE)
u <- u[order(u$student, u$week, u$question, u$sentence), ]
u$ord <- stats::ave(seq_len(nrow(u)), u$student, FUN = seq_along)
u$panel_lab <- paste("student", u$student)

H <- sort(unique(hum$model)); L <- unique(llm$model)

pick <- function(d, who, col) d[[col]][match(u$unit_id, d$unit_id[d$model == who])]
grab <- function(d, who, col) { s <- d[d$model == who, ]; s[[col]][match(u$unit_id, s$unit_id)] }

h_code <- sapply(H, function(x) grab(hum, x, "codes"))
h_cq   <- sapply(H, function(x) grab(hum, x, "cq_type"))
l_code <- sapply(L, function(x) grab(llm, x, "codes"))
l_item <- sapply(L, function(x) grab(llm, x, "cq_item"))
colnames(l_code) <- colnames(l_item) <- short(L)

h_mod <- apply(h_code, 1L, modal); l_mod <- apply(l_code, 1L, modal)

# Human CQ is a factor; render it as the shared prefix so it lines up with the
# models' item codes in the same column of glyphs.
h_lab <- apply(h_cq, 2L, function(v) unname(FACTOR_PREFIX[v]))
h_mod_cq <- apply(h_lab, 1L, modal); h_mod_cq[h_mod_cq == "(tie)"] <- NA_character_
l_mod_it <- apply(l_item, 1L, modal); l_mod_it[l_mod_it == "(tie)"] <- NA_character_
h_mod_cq[h_mod != "Positive"] <- NA_character_
l_mod_it[l_mod != "Positive"] <- NA_character_

# --- long form ----------------------------------------------------------
COLS <- c(H, "humans", colnames(l_code), "models")
KIND <- c(rep("human", length(H)), "human", rep("model", length(L)), "model")
long <- do.call(rbind, lapply(seq_along(COLS), function(i) {
  cl <- COLS[i]
  if (cl == "humans")      { v <- h_mod; it <- h_mod_cq }
  else if (cl == "models") { v <- l_mod; it <- l_mod_it }
  else if (KIND[i] == "human") { v <- h_code[, cl]; it <- h_lab[, cl] }
  else                     { v <- l_code[, cl]; it <- l_item[, cl] }
  data.frame(u, column = cl, kind = KIND[i], code = v, item = it,
             stringsAsFactors = FALSE)
}))
long$code[is.na(long$code)] <- "not coded"
long$column <- factor(long$column, levels = COLS)
long$code <- factor(long$code, levels = c(CODES, "(tie)", "not coded"))
long$item[long$code != "Positive"] <- NA_character_

# The study's primary question is between rater TYPES, so that is what the
# margin flags -- not every within-panel wobble, which the columns already show.
disagree <- u[h_mod != l_mod & !is.na(h_mod) & !is.na(l_mod), ]
agree_n  <- sum(h_mod == l_mod, na.rm = TRUE)

wk_lab <- do.call(rbind, lapply(split(u, u$student), function(d) {
  r <- rle(d$week); b <- cumsum(r$lengths)
  data.frame(panel_lab = d$panel_lab[1], week = r$values,
             lab_at = (c(0, utils::head(b, -1)) + b) / 2, stringsAsFactors = FALSE)
}))
wk <- do.call(rbind, lapply(split(u, u$student), function(d) {
  b <- utils::head(cumsum(rle(d$week)$lengths), -1)
  if (!length(b)) return(NULL)
  data.frame(panel_lab = d$panel_lab[1], at = b + 0.5, stringsAsFactors = FALSE)
}))

# --- plot ---------------------------------------------------------------
gap <- length(H) + 1.5   # between the human block and the model block

p <- ggplot(long, aes(column, ord, fill = code)) +
  geom_tile(colour = SURFACE, linewidth = 0.5) +
  geom_point(data = disagree, aes(x = 0.40, y = ord), inherit.aes = FALSE,
             colour = "#a8392c", size = 0.85) +
  geom_text(data = subset(long, code == "Positive" & !is.na(item)),
            aes(label = item, fontface = ifelse(kind == "human", "italic", "bold")),
            colour = "#08301f", size = 1.7, show.legend = FALSE) +
  geom_hline(data = wk, aes(yintercept = at), colour = "#ffffff", linewidth = 0.9) +
  geom_vline(xintercept = gap, colour = INK3, linewidth = 0.45, linetype = "22") +
  geom_text(data = wk_lab, aes(x = 0.02, y = lab_at, label = paste0("w", week)),
            inherit.aes = FALSE, hjust = 1, size = 2.4, colour = INK3) +
  scale_fill_manual(values = FILL, drop = FALSE, name = NULL,
                    guide = guide_legend(override.aes = list(colour = "#c9c6bf"))) +
  scale_y_reverse(expand = expansion(add = 0.6)) +
  scale_x_discrete(expand = expansion(add = 0.5)) +
  coord_cartesian(xlim = c(-0.85, length(COLS) + 0.5), clip = "off") +
  facet_wrap(~ panel_lab, nrow = 1, scales = "free_y") +
  labs(
    title = "Three human raters and three local models on the same 99 sentences",
    subtitle = sprintf(paste0("First real journals. Left of the dashed line the ",
      "people, right of it the machines; `humans` and `models` are each block's ",
      "majority.\nThe two panels agree on %d of 99 sentences (%.0f%%). Red dots ",
      "mark the %d where they do not."),
      agree_n, 100 * agree_n / nrow(u), nrow(disagree)),
    caption = paste0(
      "Positive cells carry the cultural-intelligence subclassification. The humans coded the FACTOR only, shown as its prefix in italics (MC, COG, MOT, BEH);\n",
      "the models coded the ITEM, shown in bold with its number (MC2, COG3, MOT1). The prefixes are the same alphabet, so the columns line up.\n",
      "Pseudonymous ids and labels only -- no journal text is read by this script.")) +
  theme_minimal(base_size = 9) +
  theme(
    plot.background = element_rect(fill = SURFACE, colour = NA),
    panel.background = element_rect(fill = SURFACE, colour = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(), axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 7.5,
                               colour = INK2),
    strip.text = element_text(face = "bold", size = 9.5, colour = INK,
                              margin = margin(b = 4)),
    legend.position = "bottom", legend.key.size = unit(9, "pt"),
    legend.text = element_text(size = 8, colour = INK2),
    plot.title = element_text(face = "bold", size = 12.5, colour = INK),
    plot.subtitle = element_text(size = 8.5, colour = INK2, margin = margin(b = 9)),
    plot.caption = element_text(size = 6.8, colour = INK3, hjust = 0,
                                margin = margin(t = 9)),
    plot.margin = margin(12, 14, 10, 16))

dir.create("reports", showWarnings = FALSE)
ggsave(OUT, p, width = 9.6, height = 7.4, dpi = 220, bg = SURFACE)
cat("wrote", OUT, "\n")

tab <- cbind(u[, c("unit_id", "student", "week", "question", "sentence")],
             as.data.frame(h_code), humans = h_mod,
             as.data.frame(l_code), models = l_mod,
             human_factor = h_mod_cq, model_item = l_mod_it)
tbf <- sub("\\.png$", "_table.csv", OUT)
utils::write.csv(tab, tbf, row.names = FALSE, na = "")
cat("wrote", tbf, "\n")
cat(sprintf("\nhuman panel and model panel agree on %d of %d sentences (%.0f%%)\n",
            agree_n, nrow(u), 100 * agree_n / nrow(u)))
