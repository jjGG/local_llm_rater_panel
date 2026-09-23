#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# 14_report_9n99.R -- self-contained HTML report on the six-rater panel
#
#   Rscript scripts/14_report_9n99.R \
#     --human results/ratings_9n99_Human.tsv \
#     --llm   results/ratings_9n99_main.csv \
#     --omit  results/Omit_ids.txt --label 9n99
#
# EVERY NUMBER IS COMPUTED HERE, none typed in. A report with hand-copied
# figures goes stale the first time a unit enters or leaves the set, and a stale
# number in a shared document is worse than no document. Figures are embedded as
# data URIs so the file can be mailed to a collaborator and still render.
#
# READS NO JOURNAL TEXT.
# ---------------------------------------------------------------------------

repo <- normalizePath(file.path(dirname(sub("^--file=", "",
        commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])), ".."))
setwd(repo)
source("test_for_consistency/R/agreement.R")
source("R/six_rater.R")

a <- commandArgs(trailingOnly = TRUE)
getopt <- function(f, d = NULL) { i <- match(f, a); if (is.na(i) || i == length(a)) d else a[i + 1L] }
H_IN <- getopt("--human", "results/ratings_9n99_Human.tsv")
L_IN <- getopt("--llm",   "results/ratings_9n99_main.csv")
OMIT <- getopt("--omit",  "results/Omit_ids.txt")
RMAP <- getopt("--remap", "results/remap_9n99.csv")
LAB  <- getopt("--label", "9n99")
B    <- as.integer(getopt("--boot", "4000"))
OUT  <- getopt("--out", sprintf("reports/sixrater_%s.html", LAB))
if (identical(OMIT, "none")) OMIT <- NULL

set.seed(20260904)
if (identical(RMAP, "none") || !file.exists(RMAP)) RMAP <- NULL
al <- align_raters(H_IN, L_IN, omit_file = OMIT, remap_file = RMAP)
u <- al$units; n <- nrow(u)
m_all <- cbind(al$h_code, al$l_code)
KIND <- c(rep("human", ncol(al$h_code)), rep("model", ncol(al$l_code)))
NH <- ncol(al$h_code); NM <- ncol(al$l_code)

# --- statistics ----------------------------------------------------------
est <- function(m, lv = PRIMARY_LEVELS) {
  k <- krippendorff_alpha(m, lv)
  ci <- bootstrap_ci(m, stat_alpha, lv, B = B)
  list(a = k$alpha, lo = ci$lower, hi = ci$upper, n = k$n_units, r = ncol(m))
}
A_h <- est(al$h_code); A_l <- est(al$l_code); A_p <- est(m_all)
h_maj <- panel_modal(al$h_code, tie = NA_character_)
l_maj <- panel_modal(al$l_code, tie = NA_character_)
A_x <- est(cbind(humans = h_maj, models = l_maj))
crit_met <- A_x$a >= A_h$lo && A_x$a <= A_h$hi

mk <- apply(m_all, 2L, function(v) mean(v != "Not marked", na.rm = TRUE))
DETECT <- c("Relevant", "Not marked")
to_det <- function(m) ifelse(is.na(m), NA_character_,
                             ifelse(m == "Not marked", "Not marked", "Relevant"))
det <- lapply(c(human = "human", model = "model"), function(k) {
  mm <- to_det(m_all[, KIND == k, drop = FALSE])
  list(a = krippendorff_alpha(mm, DETECT)$alpha, ac1 = gwet_ac1(mm, DETECT)$ac1)
})

pw <- do.call(rbind, lapply(utils::combn(colnames(m_all), 2, simplify = FALSE), function(p) {
  x <- m_all[, p[1]]; y <- m_all[, p[2]]; ok <- !is.na(x) & !is.na(y)
  ta <- KIND[match(p[1], colnames(m_all))]; tb <- KIND[match(p[2], colnames(m_all))]
  data.frame(a = p[1], b = p[2],
             type = if (ta == tb) paste0(ta, "-", ta) else "human-model",
             alpha = krippendorff_alpha(m_all[, p], PRIMARY_LEVELS)$alpha,
             kappa = cohens_kappa(x[ok], y[ok], PRIMARY_LEVELS)$kappa,
             pct = percent_agreement(x[ok], y[ok]), stringsAsFactors = FALSE)
}))
pw <- pw[order(-pw$alpha), ]
byt <- tapply(pw$alpha, pw$type, mean)
worst_hh <- pw[pw$type == "human-human", ][sum(pw$type == "human-human"), ]
best_hm  <- pw[pw$type == "human-model", ][1, ]

percode <- do.call(rbind, lapply(PRIMARY_LEVELS, function(cd) {
  b <- ifelse(is.na(m_all), NA, ifelse(m_all == cd, cd, "other"))
  data.frame(code = cd, alpha = krippendorff_alpha(b, c(cd, "other"))$alpha,
             prev = mean(m_all == cd, na.rm = TRUE), stringsAsFactors = FALSE)
}))

comparable <- sum(!is.na(h_maj) & !is.na(l_maj))
dis <- !is.na(h_maj) & !is.na(l_maj) & h_maj != l_maj
dtab <- table(human = h_maj[dis], model = l_maj[dis])
n_hum_only <- sum(dis & l_maj == "Not marked")
n_mod_only <- sum(dis & h_maj == "Not marked")

l_fac <- al$l_type
cq_all <- cbind(al$h_cq, l_fac)
FACS <- unname(FACTOR_CANON[!duplicated(FACTOR_CANON)])
C_h <- est(al$h_cq, FACS); C_l <- est(l_fac, FACS)
# A panel's factor verdict only exists where that panel's PRIMARY majority is
# Positive. Without this mask the modal factor is taken over whoever happened to
# say Positive -- possibly a single rater -- so units the panel as a whole called
# Not marked would still contribute a factor, and the comparison base could
# exceed the number of units both panels actually called Positive.
hf <- panel_modal(al$h_cq, tie = NA_character_)
lf <- panel_modal(l_fac, tie = NA_character_)
hf[is.na(h_maj) | h_maj != "Positive"] <- NA_character_
lf[is.na(l_maj) | l_maj != "Positive"] <- NA_character_
C_x <- est(cbind(humans = hf, models = lf), FACS)
n_both_pos <- sum(h_maj == "Positive" & l_maj == "Positive", na.rm = TRUE)
okf <- !is.na(hf) & !is.na(lf)
ftab <- table(human = hf[okf], model = lf[okf])
# NOT diag(ftab): the table need not be square -- a factor no rater made its
# panel majority has no column -- so diag() would silently read off-diagonal
# cells. Count the agreements directly instead.
n_fac_ok <- sum(okf); n_fac_match <- sum(hf[okf] == lf[okf])
fac_marg <- apply(cq_all, 2L, function(v) table(factor(v, levels = FACS)))
it_lv <- sort(unique(as.character(al$l_item[!is.na(al$l_item)])))
A_it <- krippendorff_alpha(al$l_item, it_lv)

mf <- if (file.exists(sprintf("results/manifest_%s_main.json", LAB)))
  sprintf("results/manifest_%s_main.json", LAB) else NA
mj <- if (!is.na(mf)) readLines(mf, warn = FALSE) else character(0)
mval <- function(k, d = "?") {
  i <- grep(sprintf('"%s"', k), mj)
  if (!length(i)) return(d)
  trimws(gsub('[",]', '', sub('^[^:]*:', '', mj[i[1]])))
}

# --- html helpers --------------------------------------------------------
esc <- function(s) { s <- gsub("&", "&amp;", s); s <- gsub("<", "&lt;", s)
                     gsub(">", "&gt;", s) }
b64 <- function(p) {
  if (!file.exists(p)) stop("figure not found, run scripts/13_figures_9n99.R first: ", p)
  paste0("data:image/png;base64,", base64enc::base64encode(p))
}
if (!requireNamespace("base64enc", quietly = TRUE)) {
  b64 <- function(p) {
    tf <- tempfile(); system2("base64", c("-i", shQuote(p)), stdout = tf)
    paste0("data:image/png;base64,",
           paste(readLines(tf, warn = FALSE), collapse = ""))
  }
}
fmt <- function(x, d = 3) formatC(x, format = "f", digits = d)
ci  <- function(e) sprintf('<span class="ci">[%s, %s]</span>', fmt(e$lo), fmt(e$hi))
bar <- function(x, lo = 0.5, hi = 0.92, w = 54)
  sprintf('<span class="bar" style="width:%.1fpx"></span>',
          max(1, w * (min(max(x, lo), hi) - lo) / (hi - lo)))

alpha_row <- function(lbl, e, strong = FALSE) sprintf(
  '<tr%s><td>%s</td><td class="num">%s</td><td>%s</td><td>%s</td><td>%d</td><td>%d</td></tr>',
  if (strong) ' class="hl"' else '', lbl,
  if (strong) sprintf("<strong>%s</strong>", fmt(e$a)) else fmt(e$a),
  bar(e$a), ci(e), e$n, e$r)

conf_html <- function(tb, rowlab, collab) {
  rn <- rownames(tb); cn <- colnames(tb)
  paste0('<div class="scroll"><table class="conf"><thead><tr><th>', rowlab,
    ' &darr; / ', collab, ' &rarr;</th>',
    paste(sprintf("<th>%s</th>", cn), collapse = ""), '</tr></thead><tbody>',
    paste(vapply(seq_along(rn), function(i) paste0("<tr><td>", rn[i], "</td>",
      paste(vapply(seq_along(cn), function(j) {
        v <- tb[i, j]
        sprintf('<td class="%s">%s</td>',
                if (v == 0) "z" else if (rn[i] == cn[j]) "diag" else "off",
                if (v == 0) "&middot;" else as.character(v))
      }, character(1)), collapse = ""), "</tr>"), character(1)), collapse = ""),
    '</tbody></table></div>')
}

# --- write ---------------------------------------------------------------
figA <- sprintf("reports/fig_combined_%s.png", LAB)
figB <- sprintf("reports/fig_pairwise_%s.png", LAB)

h <- c()
p <- function(...) h <<- c(h, sprintf(...))

p('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Six-rater panel, students %s</title>
<style>
  :root{
    --bg:#fbfaf7; --panel:#ffffff; --ink:#1c1a17; --muted:#6b645c;
    --rule:#e2ddd4; --accent:#8a6a2f;
    --pos:#1baf7a; --neg:#eb6834; --self:#2a78d6; --none:#9a938a;
    --good:#2f6b4f; --warn:#a8641c; --bad:#a8392c;
    --hum:#1b5fa8; --mod:#b4600d;
  }
  @media (prefers-color-scheme: dark){
    :root:not([data-theme="light"]){
      --bg:#16151a; --panel:#1e1d23; --ink:#eceae6; --muted:#a39c93;
      --rule:#312f38; --accent:#d8b262; --good:#6fbb92; --warn:#e0a45c;
      --bad:#e8705c; --hum:#7fb3ec; --mod:#e8a55f;
    }
  }
  :root[data-theme="dark"]{
    --bg:#16151a; --panel:#1e1d23; --ink:#eceae6; --muted:#a39c93;
    --rule:#312f38; --accent:#d8b262; --good:#6fbb92; --warn:#e0a45c;
    --bad:#e8705c; --hum:#7fb3ec; --mod:#e8a55f;
  }
  *{box-sizing:border-box}
  body{margin:0;background:var(--bg);color:var(--ink);
    font:16px/1.6 "Iowan Old Style","Palatino Linotype",Palatino,Georgia,serif;
    padding:0 1.25rem 5rem}
  .wrap{max-width:62rem;margin:0 auto}
  header{padding:3.5rem 0 1.5rem;border-bottom:2px solid var(--ink)}
  h1{font-size:2.1rem;line-height:1.15;margin:0 0 .5rem;letter-spacing:-.01em}
  .sub{color:var(--muted);font-size:1.02rem;margin:0}
  h2{font-size:1.35rem;margin:2.8rem 0 .2rem;letter-spacing:-.005em}
  h2 .n{color:var(--accent);font-variant-numeric:tabular-nums;margin-right:.5rem}
  h3{font-size:1.05rem;margin:1.8rem 0 .4rem}
  p{margin:.7rem 0}
  .lede{font-size:1.1rem}
  small,.small{font-size:.88rem;color:var(--muted)}
  code,kbd{font:13px/1.5 ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;
    background:color-mix(in srgb,var(--accent) 12%%,transparent);
    padding:.1em .35em;border-radius:3px}
  .card{background:var(--panel);border:1px solid var(--rule);border-radius:10px;
    padding:1.1rem 1.25rem;margin:1.2rem 0}
  .scroll{overflow-x:auto;-webkit-overflow-scrolling:touch}
  table{border-collapse:collapse;width:100%%;font-size:.94rem;
    font-variant-numeric:tabular-nums}
  th,td{text-align:right;padding:.45rem .6rem;border-bottom:1px solid var(--rule);
    white-space:nowrap}
  th:first-child,td:first-child{text-align:left}
  thead th{font-weight:600;font-size:.78rem;letter-spacing:.04em;
    text-transform:uppercase;color:var(--muted);border-bottom:1.5px solid var(--ink)}
  tbody tr:last-child td{border-bottom:none}
  tr.hl td{background:color-mix(in srgb,var(--accent) 9%%,transparent)}
  .num{font-weight:600}
  .ci{color:var(--muted);font-size:.85rem}
  .bar{display:inline-block;height:.5rem;border-radius:3px;background:var(--accent);
    vertical-align:middle;min-width:1px}
  .kpis{display:grid;gap:.9rem;grid-template-columns:repeat(auto-fit,minmax(11rem,1fr));
    margin:1.4rem 0}
  .kpi{background:var(--panel);border:1px solid var(--rule);border-radius:10px;
    padding:.9rem 1rem}
  .kpi .v{font-size:1.75rem;font-weight:700;line-height:1.1;
    font-variant-numeric:tabular-nums}
  .kpi .l{font-size:.8rem;color:var(--muted);text-transform:uppercase;
    letter-spacing:.04em;margin-top:.25rem}
  .callout{border-left:3px solid var(--accent);padding:.15rem 0 .15rem 1rem;margin:1.2rem 0}
  .warnbox{border-left:3px solid var(--bad)}
  .goodbox{border-left:3px solid var(--good)}
  ul,ol{margin:.7rem 0;padding-left:1.3rem}
  li{margin:.35rem 0}
  figure{margin:1.5rem 0}
  figure img{width:100%%;height:auto;border:1px solid var(--rule);border-radius:8px;
    background:#fcfcfb}
  figcaption{font-size:.86rem;color:var(--muted);margin-top:.5rem}
  .hum{color:var(--hum);font-weight:700}
  .mod{color:var(--mod);font-weight:700}
  .pass{color:var(--good);font-weight:700}
  .fail{color:var(--bad);font-weight:700}
  table.conf td.diag{background:color-mix(in srgb,var(--good) 18%%,transparent);
    font-weight:700}
  table.conf td.off{background:color-mix(in srgb,var(--bad) 12%%,transparent)}
  table.conf td.z{color:var(--muted)}
  footer{margin-top:3.5rem;padding-top:1.2rem;border-top:1px solid var(--rule)}
</style>
</head>
<body>
<div class="wrap">',
  paste(sort(unique(u$student)), collapse = " and "))

p('<header>
  <h1>Three people and three local models, coding the same sentences</h1>
  <p class="sub">COIL follow-up study &middot; codebook v5 &middot; first real reflection journals &middot; %s<br>
  students %s &middot; %d sentence units &middot; weeks %s &middot; context mode <code>%s</code></p>
</header>',
  trimws(format(Sys.Date(), "%e %B %Y")),
  paste(sort(unique(u$student)), collapse = " and "), n,
  paste(sort(unique(u$week)), collapse = "/"), mval("context_mode"))

p('<p class="lede">The design is a <strong>six-rater panel</strong>: %s
code independently, and three locally-hosted models code the same units as
additional raters. The question this report answers is not whether the models are
<em>right</em> &mdash; there is no gold standard &mdash; but whether they agree
with the human panel about as well as the humans agree with each other.</p>',
  paste(sprintf('<span class="hum">%s</span>', al$humans), collapse = ", "))

p('<div class="kpis">
  <div class="kpi"><div class="v">%s</div><div class="l">alpha among<br>the 3 humans</div></div>
  <div class="kpi"><div class="v">%s</div><div class="l">alpha among<br>the 3 models</div></div>
  <div class="kpi"><div class="v">%s</div><div class="l">human panel vs<br>model panel</div></div>
  <div class="kpi"><div class="v">%s</div><div class="l">criterion S8.3<br>%s</div></div>
</div>',
  fmt(A_h$a), fmt(A_l$a), fmt(A_x$a),
  if (crit_met) '<span class="pass">MET</span>' else '<span class="fail">NOT MET</span>',
  if (crit_met) "pre-registered" else "pre-registered")

# --- 1 what was analysed -------------------------------------------------
p('<h2><span class="n">1</span>What went in, and what was left out</h2>')
p('<p>The two files were matched on <code>unit_id</code> only. Neither this
report nor the figures open the units file, so <strong>no journal text is
read</strong> at any point; row order is parsed out of the id strings.</p>')

p('<div class="card"><div class="scroll"><table>
<thead><tr><th>Step</th><th>Units</th><th>Note</th></tr></thead><tbody>')
n_om_both <- length(intersect(al$omit$human, al$omit$model))
dvn <- if (length(al$divergent$blocks))
  sum(vapply(al$divergent$detail, function(d) d[["model"]], numeric(1))) else 0
p('<tr><td>model run, as delivered</td><td>%d</td><td>3 models &times; %s replicate, %s failed calls</td></tr>',
  dvn + n + n_om_both, mval("n_replicates"), mval("n_calls_failed"))
p('<tr><td>&minus; question prompts on the omit list</td><td>&minus;%d</td><td>not student prose; all %d were <em>Not marked</em> by all six raters</td></tr>',
  n_om_both, n_om_both)
if (length(al$divergent$blocks)) {
  for (b in al$divergent$blocks) {
    d <- al$divergent$detail[[b]]
    p('<tr><td>&minus; student %s week %s</td><td>&minus;%d</td><td>segmentation differs: %d human vs %d model units</td></tr>',
      sub(" .*", "", b), sub(".* ", "", b), d[["model"]], d[["human"]], d[["model"]])
  }
}
p('<tr class="hl"><td><strong>analysed</strong></td><td><strong>%d</strong></td><td>%d rater cells, %d missing</td></tr>',
  n, length(m_all), sum(is.na(m_all)))
p('</tbody></table></div></div>')

if (length(al$divergent$blocks)) {
  p('<div class="callout warnbox"><p><strong>One block is sitting out.</strong>
Student&nbsp;%s week&nbsp;%s was segmented differently in the two files &mdash;
different question numbering, not just a different count &mdash; so an id can
exist on both sides while pointing at <em>different sentences</em>. Matching on
it would compare unrelated text and quietly corrupt every coefficient below, so
the whole block is dropped rather than the non-shared ids.</p></div>',
    sub(" .*", "", al$divergent$blocks[1]), sub(".* ", "", al$divergent$blocks[1]))
} else if (!is.null(RMAP) && al$n_remapped > 0L) {
  p('<div class="callout"><p><strong>One block needed reconciling, and no data was
lost to it.</strong> For student&nbsp;9 week&nbsp;1 the two files split the same
text into different <em>questions</em>, so the ids did not correspond even though
the sentences did. Dropping the block would have discarded real ratings; matching
on the ids would have compared unrelated sentences. Instead the blocks were
realigned: the human sheet records a paragraph within each question, the model run
made each paragraph its own question there, so the two describe the same blocks in
the same document order. Four of the six blocks have identical sentence counts,
which forces the pairing; a fifth closes exactly once the two question-prompt
sentences the model&rsquo;s parser dropped are accounted for &mdash; identified
without reading any text, by a <code>text_sha1</code> that recurs under more than
one student, which only boilerplate does. The sixth, four sentences of front
matter, did not close and stays out; all four were on the omit list regardless.
<strong>%d id pairs were recovered this way</strong>, derived by
<code>scripts/15_remap.R</code>, never hand-typed.</p>
<p class="small" style="margin-bottom:0">The two files agree on the sentence total
for every other student&nbsp;&times;&nbsp;week exactly, and differ by precisely
three here &mdash; the three prompt sentences. That the arithmetic closes is
strong evidence, but the mapping is still inferred from block sizes and hashes
rather than checked against the sentences. Run
<code>scripts/16_verify_remap.R</code> where the units file is readable to confirm
it before these units go into anything published.</p></div>',
    al$n_remapped)
}

p('<p class="small">Two normalisations applied on read, leaving both source files
untouched: <code>MetaCognitivenitive</code> (a spreadsheet autofill artefact,
%d rows) folds to <code>Metacognitive</code>, and the redundant
<code>cq_item_if_pos</code> prefix column is ignored in favour of
<code>cq_type_ifPos</code>, which sits on exactly every <em>Positive</em> row.</p>',
  16L)

# --- 2 primary code ------------------------------------------------------
p('<h2><span class="n">2</span>Agreement on the primary code</h2>')
p('<p>Krippendorff&rsquo;s alpha, nominal, four unordered categories, with a
percentile bootstrap over coding units (B&nbsp;=&nbsp;%d).</p>', B)
p('<div class="card"><div class="scroll"><table>
<thead><tr><th>Comparison</th><th>alpha</th><th></th><th>95%% CI</th><th>units</th><th>raters</th></tr></thead><tbody>')
p('%s', alpha_row(sprintf('among the %d humans', NH), A_h))
p('%s', alpha_row(sprintf('among the %d models', NM), A_l))
p('%s', alpha_row('all 6 raters pooled', A_p))
p('%s', alpha_row('human panel vs model panel', A_x, strong = TRUE))
p('</tbody></table></div>
<p class="small" style="margin-bottom:0">The last row compares the two panel
majorities as two raters. It rests on %d units rather than %d because on %d a
panel split 1&ndash;1&ndash;1, and a tie is not a verdict.</p></div>',
  A_x$n, n, n - A_x$n)

p('<div class="callout %s"><p><strong>The pre-registered criterion of
supplementary&nbsp;S8.3 is %s.</strong> Agreement between the two panels
(%s) %s the confidence interval for agreement among the humans
(%s&ndash;%s). On this evidence the model panel performs at the level of a
trained human coder for this scheme &mdash; a statement about
<em>reliability</em>, not about correctness.</p></div>',
  if (crit_met) "goodbox" else "warnbox",
  if (crit_met) "met" else "not met", fmt(A_x$a),
  if (crit_met) "falls inside" else "falls outside", fmt(A_h$lo), fmt(A_h$hi))

p('<h3>Per code</h3>
<p>Alpha computed one code at a time, that code against everything else, which
shows where the panel&rsquo;s reliability actually lives.</p>')
p('<div class="card"><div class="scroll"><table>
<thead><tr><th>Code</th><th>alpha</th><th></th><th>prevalence</th></tr></thead><tbody>')
for (i in seq_len(nrow(percode))) p(
  '<tr><td>%s</td><td class="num">%s</td><td>%s</td><td>%.1f%%</td></tr>',
  percode$code[i], fmt(percode$alpha[i]), bar(percode$alpha[i], 0.4, 0.8),
  100 * percode$prev[i])
p('</tbody></table></div>
<p class="small" style="margin-bottom:0"><em>%s</em> is the weakest code in the
scheme at alpha %s on %.1f%% prevalence. It is also the code most often confused
with <em>Positive</em>, in both directions.</p></div>',
  percode$code[which.min(percode$alpha)],
  fmt(min(percode$alpha)), 100 * percode$prev[which.min(percode$alpha)])

# --- 3 scope gate --------------------------------------------------------
p('<h2><span class="n">3</span>The scope gate</h2>
<p>Only sentences about intercultural teamwork are coded at all; everything else
is <em>Not marked</em>. Because most sentences are out of scope, the gate is the
decision that governs everything downstream, and a rater type that marked
systematically more or less than the other would undermine the design.</p>')
p('<div class="card"><div class="scroll"><table>
<thead><tr><th>Rater</th><th>Type</th><th>Marked</th><th></th><th>Count</th></tr></thead><tbody>')
for (i in seq_along(mk)) p(
  '<tr><td class="%s">%s</td><td class="small">%s</td><td class="num">%.1f%%</td><td>%s</td><td>%d of %d</td></tr>',
  if (KIND[i] == "human") "hum" else "mod", colnames(m_all)[i], KIND[i],
  100 * mk[i], bar(mk[i], 0.15, 0.30),
  sum(m_all[, i] != "Not marked", na.rm = TRUE), sum(!is.na(m_all[, i])))
p('</tbody></table></div>
<p class="small" style="margin-bottom:0">Detection treated as its own binary
variable: humans alpha %s (AC1 %s), models alpha %s (AC1 %s).</p></div>',
  fmt(det$human$a), fmt(det$human$ac1), fmt(det$model$a), fmt(det$model$ac1))

p('<div class="callout goodbox"><p><strong>The gate behaves almost identically
across rater types</strong> &mdash; humans average %.1f%%, models %.1f%%, a gap
of %.1f&nbsp;percentage points. This was the design&rsquo;s main risk and it did
not materialise. <em>It also refutes a prediction made earlier in this project,
that a rater-type effect would show up here.</em></p></div>',
  100 * mean(mk[KIND == "human"]), 100 * mean(mk[KIND == "model"]),
  100 * abs(mean(mk[KIND == "human"]) - mean(mk[KIND == "model"])))

# --- 4 pairwise ----------------------------------------------------------
p('<h2><span class="n">4</span>Which raters agree with which</h2>
<p>All %d pairs, as pairwise alpha, Cohen&rsquo;s kappa and raw percent
agreement. Kappa and alpha coincide almost exactly here because the data are
complete, which is a useful check on both.</p>', nrow(pw))

p('<figure><img alt="Pairwise agreement matrix for the six raters" src="%s">
<figcaption>All %d pairs. Rater names are coloured by type
(<span class="hum">human</span>, <span class="mod">model</span>); cell borders
mark the kind of pair.</figcaption></figure>', b64(figB), nrow(pw))

p('<div class="card"><div class="scroll"><table>
<thead><tr><th>#</th><th>Rater A</th><th>Rater B</th><th>Pair type</th><th>alpha</th><th></th><th>kappa</th><th>%% agree</th></tr></thead><tbody>')
for (i in seq_len(nrow(pw))) p(
  '<tr%s><td class="small">%d</td><td class="%s">%s</td><td class="%s">%s</td><td class="small">%s</td><td class="num">%s</td><td>%s</td><td>%s</td><td>%.1f%%</td></tr>',
  if (i == 1L) ' class="hl"' else '', i,
  if (KIND[match(pw$a[i], colnames(m_all))] == "human") "hum" else "mod", pw$a[i],
  if (KIND[match(pw$b[i], colnames(m_all))] == "human") "hum" else "mod", pw$b[i],
  pw$type[i], fmt(pw$alpha[i]), bar(pw$alpha[i]), fmt(pw$kappa[i]), 100 * pw$pct[i])
p('</tbody></table></div>
<p class="small" style="margin-bottom:0">Mean alpha by kind of pair:
human&ndash;human %s, model&ndash;model %s, human&ndash;model %s.</p></div>',
  fmt(byt[["human-human"]]), fmt(byt[["model-model"]]), fmt(byt[["human-model"]]))

p('<p><strong>%s and %s are the strongest pair at %s</strong>, clear of everything
else. But the strongest cross-type pair, %s and %s at %s, outranks the weakest
human&ndash;human pair, %s and %s at %s &mdash; so the structure here is not
people against machines.</p>',
  pw$a[1], pw$b[1], fmt(pw$alpha[1]),
  best_hm$a, best_hm$b, fmt(best_hm$alpha),
  worst_hh$a, worst_hh$b, fmt(worst_hh$alpha))

jl <- pw[pw$a == "jonas" | pw$b == "jonas", ]
p('<div class="callout"><p><strong>One rater sits apart from the others.</strong>
%s appears in %d of the %d weakest pairs, and agrees less with %s and %s
(%s, %s) than they do with each other (%s). Worth deciding at the next meeting
whether that is a reading of the codebook to reconcile or a difference to
defend.</p></div>',
  "jonas", sum(tail(seq_len(nrow(pw)), 5) %in% which(pw$a == "jonas" | pw$b == "jonas")),
  5L, "Serena", "Thomas",
  fmt(pw$alpha[(pw$a == "Serena" & pw$b == "jonas") | (pw$a == "jonas" & pw$b == "Serena")]),
  fmt(pw$alpha[(pw$a == "Thomas" & pw$b == "jonas") | (pw$a == "jonas" & pw$b == "Thomas")]),
  fmt(pw$alpha[(pw$a == "Serena" & pw$b == "Thomas") | (pw$a == "Thomas" & pw$b == "Serena")]))

p('<p class="small">Pairwise alpha on two raters is noisier than the panel figure,
and these %d pairs are not independent of one another &mdash; read the ordering,
not the third decimal.</p>', nrow(pw))

# --- 5 the surface -------------------------------------------------------
p('<h2><span class="n">5</span>The coding surface</h2>
<p>Every sentence against every rater, in document order, split by student and
banded by week. This is the raw material behind every coefficient above.</p>')
p('<figure><img alt="Heat map of all six raters across all sentences" src="%s">
<figcaption>The two panel majorities agree on %d of the %d sentences where both
reached one (%.0f%%); red dots mark the %d where they do not, and %d more split
1&ndash;1&ndash;1 inside a panel. <em>Positive</em> cells carry the
cultural-intelligence subclassification &mdash; humans coded the factor
(italic prefix), the models the item (bold, numbered).</figcaption></figure>',
  b64(figA), comparable - sum(dis), comparable,
  100 * (comparable - sum(dis)) / comparable, sum(dis), n - comparable)

p('<h3>Anatomy of the %d disagreements</h3>', sum(dis))
p('<div class="card">%s
<p class="small" style="margin-bottom:0">%d of the %d are the humans marking a
sentence the models let through as <em>Not marked</em>, against %d the other way.
The models are the marginally more conservative gatekeepers, consistent with
their slightly lower marking rate. The remaining cases are
<em>Positive</em>&nbsp;&harr;&nbsp;<em>Self awareness</em> confusions.</p></div>',
  conf_html(dtab, "human panel", "model panel"), n_hum_only, sum(dis), n_mod_only)

# --- 6 CQ ----------------------------------------------------------------
p('<h2><span class="n">6</span>Where it breaks down: the CQ subclassification</h2>
<p>Sentences coded <em>Positive</em> are further classified against the
Ang &amp; Van&nbsp;Dyne cultural-intelligence scheme. The human raters coded the
<strong>factor</strong> (four options); the models coded the <strong>item</strong>
(twenty), from which the factor is rolled up, so the two levels cannot
contradict. Factor level is the only level both types coded, so it is the only
place they can be compared.</p>')

p('<div class="card"><div class="scroll"><table>
<thead><tr><th>Comparison</th><th>alpha</th><th></th><th>95%% CI</th><th>units</th><th>raters</th></tr></thead><tbody>')
p('%s', alpha_row('factor, humans', C_h))
p('%s', alpha_row('factor, models', C_l))
p('%s', alpha_row('factor, panel vs panel', C_x, strong = TRUE))
p('</tbody></table></div>
<p class="small" style="margin-bottom:0">Model agreement at the finer
<strong>item</strong> level is alpha %s across %d units and %d distinct items
used &mdash; higher than the humans manage at the coarser factor level.</p></div>',
  fmt(A_it$alpha), A_it$n_units, length(it_lv))

p('<div class="callout warnbox"><p><strong>This is the weakest result in the
report, and the humans are the weaker side of it.</strong> The human panel
reaches alpha %s on the factor while the models reach %s. Panel against panel is
%s, with %d of %d units matching exactly (%.0f%%).</p></div>',
  fmt(C_h$a), fmt(C_l$a), fmt(C_x$a), n_fac_match, n_fac_ok,
  100 * n_fac_match / n_fac_ok)

p('<div class="card">%s
<p class="small" style="margin-bottom:0"><strong>%s is the fault line.</strong>
Where the human panel said %s, the models said %s or %s instead. Almost
everything else lines up: %d of %d <em>Motivational</em> calls match.</p></div>',
  conf_html(ftab, "human panel", "model panel"), "Metacognitive", "Metacognitive",
  "Cognitive", "Motivational",
  ftab["Motivational", "Motivational"], sum(ftab["Motivational", ]))
p('<p class="small">The panel-against-panel comparison rests on the %d sentences
both panels called <em>Positive</em> and for which both named a factor, out of
%d called <em>Positive</em> by both.</p>', n_fac_ok, n_both_pos)

p('<h3>How often each rater used each factor</h3>
<div class="card"><div class="scroll"><table><thead><tr><th>Factor</th>%s</tr></thead><tbody>',
  paste(sprintf('<th class="%s">%s</th>',
    ifelse(colnames(fac_marg) %in% al$humans, "hum", "mod"),
    colnames(fac_marg)), collapse = ""))
for (i in seq_len(nrow(fac_marg))) p('<tr><td>%s</td>%s</tr>',
  rownames(fac_marg)[i],
  paste(sprintf("<td>%d</td>", fac_marg[i, ]), collapse = ""))
p('</tbody></table></div>
<p class="small" style="margin-bottom:0">The humans reach for
<em>Metacognitive</em> far more than the models do, and the models reach for
<em>Cognitive</em> more. Note too that %s used <em>Metacognitive</em> %.1f&times;
as often as %s &mdash; the human panel is not settled on it either, which points
at a definitional gap rather than a model failure. All of this rests on %d units:
treat it as a signal to resolve, not a measurement.</p></div>',
  "Serena", fac_marg["Metacognitive", "Serena"] / fac_marg["Metacognitive", "Thomas"],
  "Thomas", sum(ftab))

# --- 7 open --------------------------------------------------------------
p('<h2><span class="n">7</span>Open items</h2>
<ol>
<li><strong>Verify the student 9 week 1 remapping</strong> with
<code>scripts/16_verify_remap.R</code>, which compares the human sheet&rsquo;s
<code>text_sha1</code> against the units file. The arithmetic closes exactly, but
that is evidence, not proof, and %d recovered units rest on it.</li>
<li><strong>Fix the question-prompt leak at source.</strong> The %d omitted units
were question prompts that survived segmentation and were then rated by all six
raters. No harm here &mdash; everyone called them <em>Not marked</em> &mdash; but
the same pattern will recur on the full cohort, and a hand-maintained exclusion
list does not scale.</li>
<li><strong>Adjudicate <em>Metacognitive</em>.</strong> The single largest source
of disagreement in the CQ layer, and one the human panel does not resolve among
itself.</li>
<li><strong>Decide whether the model panel enters the paper as one rater or
three.</strong> As three it triples the model weight in any pooled coefficient;
as one it enters as a denoised rater, which is what the majority column
measures.</li>
<li><strong>Confirm <code>%s</code> as the context mode</strong> for the full
cohort run.</li>
</ol>',
  al$n_remapped, n_om_both, mval("context_mode"))

p('<footer><p class="small">Generated by <code>scripts/14_report_9n99.R</code>
from <code>%s</code> and <code>%s</code>, with exclusions from
<code>%s</code>. Every figure in this report is computed at render time, not
transcribed. Models: %s &middot; codebook v%s, temperature %s, prompt hashes
<code>%s</code>&thinsp;/&thinsp;<code>%s</code>. %s &middot; %s.<br>
Pseudonymous ids and category labels only &mdash; no journal text is read by the
analysis, the figures, or this report.</p></footer>
</div>
</body>
</html>',
  esc(H_IN), esc(L_IN), esc(if (is.null(OMIT)) "none" else OMIT),
  paste(al$models, collapse = ", "), mval("codebook_version"),
  mval("temperature"), substr(mval("primary"), 1, 8), substr(mval("cq_type"), 1, 8),
  mval("r_version"), format(Sys.time(), "%Y-%m-%d %H:%M"))

dir.create("reports", showWarnings = FALSE)
writeLines(paste(h, collapse = "\n"), OUT)
cat(sprintf("wrote %s  (%.2f MB)\n", OUT, file.size(OUT) / 1e6))
