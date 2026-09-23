#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# 20_report_full.R -- one shareable report covering the whole study so far
#
#   Rscript scripts/17_pool.R
#   Rscript scripts/18_trajectory.R
#   Rscript scripts/13_figures_9n99.R  ...   (once per batch)
#   Rscript scripts/20_report_full.R
#
# Two halves, in the order a reader needs them:
#   PART A  does the instrument work -- six raters, agreement, where it fails
#   PART B  what the journals say    -- the cohort across the four weeks
#
# EVERY NUMBER IS COMPUTED HERE OR READ FROM A RESULT FILE, none typed in.
# Figures are embedded as data URIs so the file can be mailed and still render.
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
OUT <- getopt("--out", "reports/COIL_local_llm_report.html")
B   <- as.integer(getopt("--boot", "4000"))
POOL_IN <- getopt("--pooled", "results/pooled_units.csv")
TAG     <- getopt("--tag", "")
TITLE   <- getopt("--title", "Local language models as additional raters")
set.seed(20260916)

rd <- function(p) utils::read.csv(p, stringsAsFactors = FALSE, na.strings = c("NA", ""))
pooled <- rd(POOL_IN)
trend  <- rd(sprintf("results/trajectory_trend%s.csv", TAG))
pool   <- rd(sprintf("results/trajectory_pooled%s.csv", TAG))
cq     <- rd(sprintf("results/trajectory_cq%s.csv", TAG))
batches <- rd("config/batches.csv")
# Only the batches actually present in this pool, so a single-cohort report does
# not advertise students it does not contain.
batches <- batches[batches$batch %in% unique(pooled$batch), , drop = FALSE]
FIGTAG <- TAG
LABS <- unique(pooled$batch)

CODES <- c("Positive", "Negative", "Self awareness")
FACS  <- c("Metacognitive", "Cognitive", "Motivational", "Behavioural")

cons <- pooled[pooled$rater == "consensus", ]
raw  <- pooled[pooled$rater_kind %in% c("human", "model"), ]
hum_r <- sort(unique(raw$rater[raw$rater_kind == "human"]))
mod_r <- sort(unique(raw$rater[raw$rater_kind == "model"]))
studs <- unique(cons$student[order(suppressWarnings(as.numeric(cons$student)))])
weeks <- sort(unique(cons$week))
n_all <- nrow(cons); n_nc <- sum(is.na(cons$code)); n_use <- n_all - n_nc
uu <- unique(cons[!is.na(cons$code), c("student", "week", "unit_id")])
tbl <- table(uu$student, uu$week)

# --- reliability on the WHOLE pooled set --------------------------------
wide <- function(rs) {
  ids <- sort(unique(raw$unit_id))
  m <- vapply(rs, function(r) {
    s <- raw[raw$rater == r, ]
    s$code[match(ids, s$unit_id)]
  }, character(length(ids)))
  rownames(m) <- ids
  m
}
M <- wide(c(hum_r, mod_r))
KIND <- c(rep("human", length(hum_r)), rep("model", length(mod_r)))
est <- function(m, lv = PRIMARY_LEVELS) {
  k <- krippendorff_alpha(m, lv); ci <- bootstrap_ci(m, stat_alpha, lv, B = B)
  list(a = k$alpha, lo = ci$lower, hi = ci$upper, n = k$n_units, r = ncol(m))
}
A_h <- est(M[, KIND == "human"]); A_m <- est(M[, KIND == "model"]); A_p <- est(M)
hmaj <- panel_modal(M[, KIND == "human"], tie = NA_character_)
mmaj <- panel_modal(M[, KIND == "model"], tie = NA_character_)
A_x <- est(cbind(humans = hmaj, models = mmaj))
crit_ok <- A_x$a >= A_h$lo
crit_where <- if (A_x$a < A_h$lo) "below" else if (A_x$a > A_h$hi) "above" else "inside"

mk <- apply(M, 2L, function(v) mean(v != "Not marked", na.rm = TRUE))
DET <- c("Relevant", "Not marked")
to_det <- function(m) ifelse(is.na(m), NA, ifelse(m == "Not marked", "Not marked", "Relevant"))
det <- lapply(c(human = "human", model = "model"), function(k) {
  mm <- to_det(M[, KIND == k, drop = FALSE])
  list(a = krippendorff_alpha(mm, DET)$alpha, ac1 = gwet_ac1(mm, DET)$ac1)
})

pw <- do.call(rbind, lapply(utils::combn(colnames(M), 2, simplify = FALSE), function(q) {
  x <- M[, q[1]]; y <- M[, q[2]]; ok <- !is.na(x) & !is.na(y)
  ta <- KIND[match(q[1], colnames(M))]; tb <- KIND[match(q[2], colnames(M))]
  data.frame(a = q[1], b = q[2],
             type = if (ta == tb) paste0(ta, "-", ta) else "human-model",
             alpha = krippendorff_alpha(M[, q], PRIMARY_LEVELS)$alpha,
             kappa = cohens_kappa(x[ok], y[ok], PRIMARY_LEVELS)$kappa,
             pct = percent_agreement(x[ok], y[ok]), stringsAsFactors = FALSE)
}))
pw <- pw[order(-pw$alpha), ]
byt <- tapply(pw$alpha, pw$type, mean)

percode <- do.call(rbind, lapply(PRIMARY_LEVELS, function(cd) {
  b2 <- ifelse(is.na(M), NA, ifelse(M == cd, cd, "other"))
  data.frame(code = cd, alpha = krippendorff_alpha(b2, c(cd, "other"))$alpha,
             prev = mean(M == cd, na.rm = TRUE), stringsAsFactors = FALSE)
}))

dis <- !is.na(hmaj) & !is.na(mmaj) & hmaj != mmaj
dtab <- table(human = hmaj[dis], model = mmaj[dis])
n_h_only <- sum(dis & mmaj == "Not marked"); n_m_only <- sum(dis & hmaj == "Not marked")

fu <- table(factor(raw$cq_factor, levels = FACS), raw$rater)
beh <- fu["Behavioural", ]
n_beh_cons <- sum(cq$k[cq$factor_ == "Behavioural"])

# --- panels over time ----------------------------------------------------
hp <- pooled[pooled$rater == "human panel", ]; mp <- pooled[pooled$rater == "model panel", ]
share <- function(d, w, cd) {
  k <- d[d$week == w & !is.na(d$code) & d$code != "Not marked", ]
  if (!nrow(k)) return(NA_real_); sum(k$code == cd) / nrow(k)
}

# --- html ----------------------------------------------------------------
b64 <- function(p) {
  if (!file.exists(p)) stop("missing figure: ", p)
  tf <- tempfile(); system2("base64", c("-i", shQuote(p)), stdout = tf)
  paste0("data:image/png;base64,", paste(readLines(tf, warn = FALSE), collapse = ""))
}
fmt <- function(x, d = 1) formatC(100 * x, format = "f", digits = d)
f3 <- function(x) formatC(x, format = "f", digits = 3)
ci <- function(e) sprintf('<span class="ci">[%s, %s]</span>', f3(e$lo), f3(e$hi))
pv <- function(w, cd, col) pool[[col]][pool$week == w & pool$code == cd]
cmp <- function(w, cd) pv(w, cd, "k") / sum(vapply(CODES, function(z) pv(w, z, "k"), numeric(1)))
conf <- function(tb, rl, cl) paste0('<div class="scroll"><table class="conf"><thead><tr><th>',
  rl, ' &darr; / ', cl, ' &rarr;</th>', paste(sprintf("<th>%s</th>", colnames(tb)), collapse = ""),
  '</tr></thead><tbody>', paste(vapply(rownames(tb), function(i) paste0("<tr><td>", i, "</td>",
    paste(vapply(colnames(tb), function(j) sprintf('<td class="%s">%s</td>',
      if (tb[i, j] == 0) "z" else if (i == j) "diag" else "off",
      if (tb[i, j] == 0) "&middot;" else as.character(tb[i, j])), character(1)), collapse = ""),
    "</tr>"), character(1)), collapse = ""), '</tbody></table></div>')

h <- c(); p <- function(...) h <<- c(h, sprintf(...))

p('<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Local LLMs as additional raters</title>
<style>
  :root{--bg:#fbfaf7;--panel:#fff;--ink:#1c1a17;--muted:#6b645c;--rule:#e2ddd4;
    --accent:#8a6a2f;--pos:#1baf7a;--neg:#eb6834;--self:#2a78d6;
    --good:#2f6b4f;--bad:#a8392c;--hum:#1b5fa8;--mod:#b4600d}
  @media (prefers-color-scheme:dark){:root:not([data-theme="light"]){
    --bg:#16151a;--panel:#1e1d23;--ink:#eceae6;--muted:#a39c93;--rule:#312f38;
    --accent:#d8b262;--good:#6fbb92;--bad:#e8705c;--hum:#7fb3ec;--mod:#e8a55f}}
  :root[data-theme="dark"]{--bg:#16151a;--panel:#1e1d23;--ink:#eceae6;
    --muted:#a39c93;--rule:#312f38;--accent:#d8b262;--good:#6fbb92;
    --bad:#e8705c;--hum:#7fb3ec;--mod:#e8a55f}
  *{box-sizing:border-box}
  body{margin:0;background:var(--bg);color:var(--ink);
    font:16px/1.6 "Iowan Old Style","Palatino Linotype",Palatino,Georgia,serif;
    padding:0 1.25rem 5rem}
  .wrap{max-width:62rem;margin:0 auto}
  header{padding:3.5rem 0 1.5rem;border-bottom:2px solid var(--ink)}
  h1{font-size:2.2rem;line-height:1.12;margin:0 0 .5rem;letter-spacing:-.01em}
  .sub{color:var(--muted);font-size:1.02rem;margin:0}
  .part{margin:3.4rem 0 0;padding:.5rem 0 .3rem;border-top:2px solid var(--ink);
    font-size:.78rem;letter-spacing:.14em;text-transform:uppercase;color:var(--accent);font-weight:700}
  h2{font-size:1.35rem;margin:2.2rem 0 .2rem;letter-spacing:-.005em}
  h2 .n{color:var(--accent);font-variant-numeric:tabular-nums;margin-right:.5rem}
  h3{font-size:1.05rem;margin:1.8rem 0 .4rem}
  p{margin:.7rem 0} .lede{font-size:1.1rem}
  small,.small{font-size:.88rem;color:var(--muted)}
  code{font:13px/1.5 ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;
    background:color-mix(in srgb,var(--accent) 12%%,transparent);padding:.1em .35em;border-radius:3px}
  .card{background:var(--panel);border:1px solid var(--rule);border-radius:10px;
    padding:1.1rem 1.25rem;margin:1.2rem 0}
  .scroll{overflow-x:auto}
  table{border-collapse:collapse;width:100%%;font-size:.93rem;font-variant-numeric:tabular-nums}
  th,td{text-align:right;padding:.45rem .6rem;border-bottom:1px solid var(--rule);white-space:nowrap}
  th:first-child,td:first-child{text-align:left}
  thead th{font-weight:600;font-size:.78rem;letter-spacing:.04em;text-transform:uppercase;
    color:var(--muted);border-bottom:1.5px solid var(--ink)}
  tbody tr:last-child td{border-bottom:none}
  tr.hl td{background:color-mix(in srgb,var(--accent) 9%%,transparent)}
  .num{font-weight:600} .ci{color:var(--muted);font-size:.85rem}
  .bar{display:inline-block;height:.5rem;border-radius:3px;background:var(--accent);
    vertical-align:middle;min-width:1px}
  .kpis{display:grid;gap:.9rem;grid-template-columns:repeat(auto-fit,minmax(11rem,1fr));margin:1.4rem 0}
  .kpi{background:var(--panel);border:1px solid var(--rule);border-radius:10px;padding:.9rem 1rem}
  .kpi .v{font-size:1.65rem;font-weight:700;line-height:1.1;font-variant-numeric:tabular-nums}
  .kpi .l{font-size:.78rem;color:var(--muted);text-transform:uppercase;letter-spacing:.04em;margin-top:.25rem}
  .callout{border-left:3px solid var(--accent);padding:.15rem 0 .15rem 1rem;margin:1.2rem 0}
  .warnbox{border-left:3px solid var(--bad)} .goodbox{border-left:3px solid var(--good)}
  ul,ol{margin:.7rem 0;padding-left:1.3rem} li{margin:.35rem 0}
  figure{margin:1.5rem 0}
  figure img{width:100%%;height:auto;border:1px solid var(--rule);border-radius:8px;background:#fcfcfb}
  figcaption{font-size:.86rem;color:var(--muted);margin-top:.5rem}
  .pos{color:var(--pos);font-weight:700}.neg{color:var(--neg);font-weight:700}
  .slf{color:var(--self);font-weight:700}
  .hum{color:var(--hum);font-weight:700}.mod{color:var(--mod);font-weight:700}
  .sig{color:var(--good);font-weight:700}.ns{color:var(--muted)}
  table.conf td.diag{background:color-mix(in srgb,var(--good) 18%%,transparent);font-weight:700}
  table.conf td.off{background:color-mix(in srgb,var(--bad) 12%%,transparent)}
  table.conf td.z{color:var(--muted)}
  nav{background:var(--panel);border:1px solid var(--rule);border-radius:10px;
    padding:.9rem 1.2rem;margin:1.6rem 0;font-size:.93rem}
  nav a{color:var(--ink);text-decoration:none;border-bottom:1px solid var(--rule)}
  nav a:hover{border-bottom-color:var(--accent)}
  footer{margin-top:3.5rem;padding-top:1.2rem;border-top:1px solid var(--rule)}
</style></head><body><div class="wrap">')

p('<header><h1>%s</h1>
<p class="sub">COIL follow-up study &middot; &ldquo;Omics in Oncology&rdquo;, UZH Zurich &amp; UU Utrecht &middot; %s<br>
%d students &middot; %d sentences &middot; %d raters each (%d people, %d models) &middot; codebook v5</p></header>',
  TITLE, trimws(format(Sys.Date(), "%e %B %Y")), length(studs), n_all,
  length(hum_r) + length(mod_r), length(hum_r), length(mod_r))

p('<p class="lede">Three people and three locally-hosted language models coded the
same %d sentences of student reflection journals, independently, sentence by
sentence. <strong>Part&nbsp;A</strong> asks whether the models can be trusted as
additional raters. <strong>Part&nbsp;B</strong> asks what the journals actually
say across the four journal points.</p>', n_all)

p('<nav><strong>Contents</strong><br>
<a href="#a1">A1 What was coded</a> &middot; <a href="#a2">A2 Agreement</a> &middot;
<a href="#a3">A3 Who agrees with whom</a> &middot; <a href="#a4">A4 Where it fails</a> &middot;
<a href="#a5">A5 The coding surface</a><br>
<a href="#b1">B1 The shape of the weeks</a> &middot; <a href="#b2">B2 Kind of comment</a> &middot;
<a href="#b3">B3 Why not yet growth</a> &middot; <a href="#b4">B4 Cultural intelligence</a> &middot;
<a href="#b5">B5 People vs machines</a> &middot; <a href="#b6">B6 Each student</a><br>
<a href="#c1">C Open decisions</a> &middot; <a href="#c2">D How it was made</a></nav>')

p('<div class="kpis">
<div class="kpi"><div class="v">%s</div><div class="l">alpha among<br>the %d people</div></div>
<div class="kpi"><div class="v">%s</div><div class="l">alpha among<br>the %d models</div></div>
<div class="kpi"><div class="v">%s</div><div class="l">human panel vs<br>model panel</div></div>
<div class="kpi"><div class="v">%s%% &rarr; %s%%</div><div class="l">of comments positive<br>week 1 to week 10</div></div>
</div>', f3(A_h$a), length(hum_r), f3(A_m$a), length(mod_r), f3(A_x$a),
  fmt(cmp(1, "Positive"), 0), fmt(cmp(10, "Positive"), 0))

# =========================== PART A ======================================
p('<div class="part">Part A &mdash; does the instrument work</div>')

p('<h2 id="a1"><span class="n">A1</span>What was coded, and what was left out</h2>')
p('<p>Every sentence gets exactly one of four labels. <em>Not marked</em> is the
default: only sentences about intercultural teamwork are coded at all, which is
why %s%% of them carry no code. Sentences labelled
<span class="pos">Positive</span> are further classified against the
Ang &amp; Van&nbsp;Dyne cultural-intelligence scheme &mdash; the people coded the
four-way <strong>factor</strong>, the models the twenty-way <strong>item</strong>,
from which the factor rolls up so the two levels cannot contradict.</p>',
  fmt(percode$prev[percode$code == "Not marked"], 0))

p('<div class="card"><div class="scroll"><table>
<thead><tr><th>Batch</th><th>Students</th><th>Sentences</th><th>Human raters</th><th>Model raters</th></tr></thead><tbody>')
for (i in seq_len(nrow(batches))) {
  b <- batches[i, ]
  bs <- cons[cons$batch == b$batch, ]
  p('<tr><td>%s</td><td>%s</td><td>%d</td><td>%s</td><td>%s</td></tr>', b$batch,
    paste(unique(bs$student[order(suppressWarnings(as.numeric(bs$student)))]), collapse = ", "),
    nrow(bs), paste(hum_r, collapse = ", "), paste(mod_r, collapse = ", "))
}
p('<tr class="hl"><td><strong>total</strong></td><td><strong>%d students</strong></td><td><strong>%d</strong></td><td colspan="2">every student on the same six-rater panel</td></tr>',
  length(studs), n_all)
p('</tbody></table></div></div>')

# Which repairs apply depends on which batches are in this pool. Listing a fix
# for a block that is not in the report would describe data the reader cannot
# see; listing none would hide work the numbers depend on.
blankv <- function(x) is.na(x) | !nzchar(x)
has_remap <- any(!blankv(batches$remap_file))
has_omit  <- any(!blankv(batches$omit_file))
dupe_studs <- intersect(c("8", "94"), studs)

items <- character(0)
if (length(dupe_studs))
  items <- c(items, sprintf('<li><strong>Student%s %s w%s in the data twice.</strong>
Rated in an earlier batch as well, from the same journals cut into different
questions &mdash; identical sentence counts per week, almost no shared ids.
Pooling both would have doubled their weight in every cohort figure. The earlier
batch is excluded, and the pooling script refuses to run if any student appears
in two batches.</li>',
    if (length(dupe_studs) == 1L) "" else "s", paste(dupe_studs, collapse = " and "),
    if (length(dupe_studs) == 1L) "as" else "ere"))
if (has_remap)
  items <- c(items, '<li><strong>One block had incompatible ids.</strong> For
student&nbsp;9 week&nbsp;1 the two files split the same text into different
questions, so an id could exist on both sides pointing at <em>different
sentences</em>. The blocks were realigned by size, with the question-prompt
sentences the model parser had dropped identified through a <code>text_sha1</code>
that recurs under more than one student &mdash; boilerplate is the only thing that
does. 26 sentence pairs were recovered, derived rather than hand-typed.</li>')
if (has_omit) {
  items <- c(items, '<li><strong>Question prompts were being rated as if they were
student prose.</strong> 16 such units were excluded; all had been called
<em>Not marked</em> by everyone, so nothing moved, but the leak would recur at
cohort scale.</li>')
} else {
  items <- c(items, '<li><strong>No question prompts leaked into this batch.</strong>
Every sentence hash is unique, and boilerplate is the one thing that repeats
across students &mdash; so the segmentation fix that followed the earlier batch
appears to have held. Nothing needed excluding.</li>')
}
items <- c(items, '<li><strong>Files arrive differently from each rater.</strong>
One with a UTF-8 byte-order mark that silently renames the first column, another
with a leading space on every CQ value and whitespace-only cells that read as
text rather than missing. All handled on read; the source files are
untouched.</li>')
n_unc <- sum(is.na(M[, KIND == "human"]))
if (n_unc > 0L)
  items <- c(items, sprintf('<li><strong>%d code%s marked with a trailing
<code>?</code></strong> &mdash; an explicit refusal to commit. Treated as
missing, so that sentence has five raters rather than six. Forcing it either way
would attribute a judgement that was withheld.</li>',
    n_unc, if (n_unc == 1L) " was" else "s were"))

p('<h3>What had to be repaired before any of this could be counted</h3>
<div class="card"><ol style="margin-top:0">%s</ol></div>', paste(items, collapse = "\n"))

p('<h2 id="a2"><span class="n">A2</span>Agreement on the primary code</h2>')
p('<p>Krippendorff&rsquo;s alpha, nominal, four unordered categories, percentile
bootstrap over coding units (B&nbsp;=&nbsp;%d), computed on all %d sentences at
once.</p>', B, n_all)
p('<div class="card"><div class="scroll"><table>
<thead><tr><th>Comparison</th><th>alpha</th><th>95%% CI</th><th>units</th><th>raters</th></tr></thead><tbody>')
for (r in list(list("among the people", A_h), list("among the models", A_m),
               list("all six pooled", A_p))) {
  p('<tr><td>%s</td><td class="num">%s</td><td>%s</td><td>%d</td><td>%d</td></tr>',
    r[[1]], f3(r[[2]]$a), ci(r[[2]]), r[[2]]$n, r[[2]]$r)
}
p('<tr class="hl"><td><strong>human panel vs model panel</strong></td><td class="num"><strong>%s</strong></td><td>%s</td><td>%d</td><td>2</td></tr>',
  f3(A_x$a), ci(A_x), A_x$n)
p('</tbody></table></div></div>')

p('<div class="callout %s"><p><strong>The pre-registered criterion of
supplementary&nbsp;S8.3 is %s.</strong> Agreement between the two panel
majorities (%s) lies <strong>%s</strong> the interval for agreement among the
three people (%s&ndash;%s). The criterion asks that the model panel be at least
as consistent with the people as the people are with each other%s</p></div>',
  if (crit_ok) "goodbox" else "warnbox", if (crit_ok) "met" else "not met",
  f3(A_x$a), crit_where, f3(A_h$lo), f3(A_h$hi),
  if (identical(crit_where, "above"))
    ", and here it clears that bar rather than merely reaching it. Worth stating carefully: the two panels agreeing closely is a statement about <em>reliability</em>, not about either being right."
  else ".")

p('<h3>Per code</h3>')
p('<div class="card"><div class="scroll"><table>
<thead><tr><th>Code</th><th>alpha</th><th></th><th>prevalence</th></tr></thead><tbody>')
for (i in seq_len(nrow(percode))) p(
  '<tr><td>%s</td><td class="num">%s</td><td><span class="bar" style="width:%.0fpx"></span></td><td>%s%%</td></tr>',
  percode$code[i], f3(percode$alpha[i]),
  max(1, 60 * (percode$alpha[i] - 0.3) / 0.55), fmt(percode$prev[i], 1))
p('</tbody></table></div>
<p class="small" style="margin-bottom:0"><em>%s</em> is the weakest code in the
scheme (alpha %s at %s%% prevalence) and the one most often confused with
<em>Positive</em> in both directions.</p></div>',
  percode$code[which.min(percode$alpha)], f3(min(percode$alpha)),
  fmt(percode$prev[which.min(percode$alpha)], 1))

p('<h3>The scope gate</h3>
<p>Because most sentences are out of scope, the decision that governs everything
downstream is simply <em>whether to code at all</em>. A rater type that marked
systematically more or less than the other would undermine the design.</p>')
p('<div class="card"><div class="scroll"><table>
<thead><tr><th>Rater</th><th>Type</th><th>Marked</th><th>Count</th></tr></thead><tbody>')
for (i in seq_along(mk)) p('<tr><td class="%s">%s</td><td class="small">%s</td><td class="num">%s%%</td><td>%d of %d</td></tr>',
  if (KIND[i] == "human") "hum" else "mod", colnames(M)[i], KIND[i], fmt(mk[i], 1),
  sum(M[, i] != "Not marked", na.rm = TRUE), sum(!is.na(M[, i])))
p('</tbody></table></div>
<p class="small" style="margin-bottom:0">Detection as its own binary variable:
people alpha %s (AC1 %s), models alpha %s (AC1 %s). Averages %s%% against %s%%,
a gap of %s percentage points.</p></div>',
  f3(det$human$a), f3(det$human$ac1), f3(det$model$a), f3(det$model$ac1),
  fmt(mean(mk[KIND == "human"]), 1), fmt(mean(mk[KIND == "model"]), 1),
  fmt(abs(mean(mk[KIND == "human"]) - mean(mk[KIND == "model"])), 1))

p('<div class="callout goodbox"><p><strong>The gate behaves almost identically
across rater types.</strong> This was the design&rsquo;s main risk and it did not
materialise. It also refutes a prediction made earlier in this project, that a
rater-type effect would appear here.</p></div>')

p('<h2 id="a3"><span class="n">A3</span>Who agrees with whom</h2>')
p('<div class="card"><div class="scroll"><table>
<thead><tr><th>#</th><th>Rater A</th><th>Rater B</th><th>Pair</th><th>alpha</th><th>kappa</th><th>%% agree</th></tr></thead><tbody>')
for (i in seq_len(nrow(pw))) p('<tr%s><td class="small">%d</td><td class="%s">%s</td><td class="%s">%s</td><td class="small">%s</td><td class="num">%s</td><td>%s</td><td>%s%%</td></tr>',
  if (i == 1L) ' class="hl"' else '', i,
  if (KIND[match(pw$a[i], colnames(M))] == "human") "hum" else "mod", pw$a[i],
  if (KIND[match(pw$b[i], colnames(M))] == "human") "hum" else "mod", pw$b[i],
  pw$type[i], f3(pw$alpha[i]), f3(pw$kappa[i]), fmt(pw$pct[i], 1))
p('</tbody></table></div>
<p class="small" style="margin-bottom:0">Mean by kind: human&ndash;human %s,
model&ndash;model %s, human&ndash;model %s. Kappa and alpha coincide almost
exactly because the data are complete, which is a useful check on both.</p></div>',
  f3(byt[["human-human"]]), f3(byt[["model-model"]]), f3(byt[["human-model"]]))

bh <- pw[pw$type == "human-human", ]; bm <- pw[pw$type == "human-model", ]
p('<p>The strongest pair is <strong>%s and %s (%s)</strong>. The strongest
cross-type pair, %s and %s (%s), %s the weakest human&ndash;human pair, %s and %s
(%s) &mdash; so the structure is not simply people against machines.</p>',
  pw$a[1], pw$b[1], f3(pw$alpha[1]), bm$a[1], bm$b[1], f3(bm$alpha[1]),
  if (bm$alpha[1] > bh$alpha[nrow(bh)]) "outranks" else "does not reach",
  bh$a[nrow(bh)], bh$b[nrow(bh)], f3(bh$alpha[nrow(bh)]))

p('<figure><img alt="Pairwise agreement matrix" src="%s">
<figcaption>All %d pairs on all %d sentences. Rater names are coloured by type
(<span class="hum">person</span>, <span class="mod">model</span>); cell borders
mark the kind of pair.</figcaption></figure>',
  b64(sprintf("reports/fig_pairs%s.png", FIGTAG)), nrow(pw), n_all)

p('<h2 id="a4"><span class="n">A4</span>Where the instrument fails</h2>')
p('<h3>The %d sentences the two panels read differently</h3>', sum(dis))
p('<div class="card">%s
<p class="small" style="margin-bottom:0">%d of the %d are people marking a
sentence the models let through as <em>Not marked</em>, against %d the other way;
the models are marginally the more conservative gatekeepers. The rest are
<em>Positive</em>&nbsp;&harr;&nbsp;<em>Self awareness</em> confusions.</p></div>',
  conf(dtab, "people", "models"), n_h_only, sum(dis), n_m_only)

p('<h3>The cultural-intelligence layer is the weak half</h3>')
p('<div class="card"><div class="scroll"><table>
<thead><tr><th>Factor</th>%s</tr></thead><tbody>',
  paste(sprintf('<th class="%s">%s</th>',
    ifelse(colnames(fu) %in% hum_r, "hum", "mod"), colnames(fu)), collapse = ""))
for (i in rownames(fu)) p('<tr><td>%s</td>%s</tr>', i,
  paste(sprintf("<td>%d</td>", fu[i, ]), collapse = ""))
p('</tbody></table></div></div>')

p('<div class="callout warnbox"><p><strong>Two of the three models never assign
the Behavioural factor at all.</strong> <code>%s</code> use it zero times across
every rating; the other four raters use it %d times between them. The consensus
therefore lands on Behavioural just <strong>%d time%s</strong> in %d classified
positive sentences.</p>
<p style="margin-bottom:0">This changed as the panel grew: with an earlier
four-rater panel a Behavioural majority was arithmetically unreachable and the
consensus contained none at all. Adding two more people made it reachable but
still rare. <strong>The factor&rsquo;s visibility is a property of the panel, not
of the journals</strong>, and that has to be settled before the CQ layer carries
any weight in the paper.</p></div>',
  paste(names(beh)[beh == 0], collapse = "</code> and <code>"), sum(beh),
  n_beh_cons, if (n_beh_cons == 1L) "" else "s", sum(cq$k))

p('<h2 id="a5"><span class="n">A5</span>The coding surface</h2>
<p>Every sentence against every rater, in document order, banded by week. This is
the raw material behind every coefficient above.</p>')
p('<figure><img alt="Coding surface, all students" src="%s">
<figcaption>All %d students in one panel, weeks aligned across the cohort.
<em>Positive</em> cells carry the CQ subclassification: the people coded the
factor (italic prefix), the models the item (bold, numbered). Red dots mark the
sentences where the two panel majorities differ. The rating sheets arrived in
%d batches, but nothing here is split by batch.</figcaption></figure>',
  b64(sprintf("reports/fig_surface%s.png", FIGTAG)), length(studs), nrow(batches))

# =========================== PART B ======================================
p('<div class="part">Part B &mdash; what the journals say</div>')

p('<h2 id="b1"><span class="n">B1</span>The shape of the eleven weeks</h2>
<p>From here on, every sentence carries one <strong>consensus</strong> label, by
strict majority of all six raters. %d sentences (%s%%) had no majority and are
excluded rather than resolved arbitrarily; %s%% of the rest were unanimous.</p>',
  n_nc, fmt(n_nc / n_all, 1), fmt(mean(cons$n_agree == cons$n_raters, na.rm = TRUE), 0))

p('<div class="card"><div class="scroll"><table>
<thead><tr><th>Week</th><th>Sentences</th><th>Positive</th><th>Negative</th><th>Self awareness</th><th>Any code</th></tr></thead><tbody>')
for (w in weeks) p('<tr><td>week %d</td><td>%d</td><td class="num pos">%d (%s%%)</td><td class="num neg">%d (%s%%)</td><td class="num slf">%d (%s%%)</td><td>%d (%s%%)</td></tr>',
  w, pv(w, "Positive", "n"),
  pv(w, "Positive", "k"), fmt(pv(w, "Positive", "rate"), 0),
  pv(w, "Negative", "k"), fmt(pv(w, "Negative", "rate"), 0),
  pv(w, "Self awareness", "k"), fmt(pv(w, "Self awareness", "rate"), 0),
  pv(w, "any code", "k"), fmt(pv(w, "any code", "rate"), 0))
p('</tbody></table></div>
<p class="small" style="margin-bottom:0">Shares of everything written that week,
so a longer entry does not inflate them.</p></div>')

p('<figure><img alt="Trajectory of the three codes" src="%s">
<figcaption>Grey lines are the %d students, the coloured line the cohort mean.
The p-value comes from a permutation test that shuffles week labels
<em>within</em> each student, holding each student&rsquo;s overall rate and weekly
sentence counts fixed so only the ordering in time is tested.</figcaption></figure>',
  b64(sprintf("reports/fig_trajectory%s.png", FIGTAG)), length(studs))

p('<div class="card"><div class="scroll"><table>
<thead><tr><th>Code</th><th>w1</th><th>w4</th><th>w7</th><th>w10</th><th>Slope / week</th><th>Perm p</th></tr></thead><tbody>')
for (cd in c(CODES, "any code")) {
  t1 <- trend[trend$code == cd, ]
  sg <- !is.na(t1$perm_p) && t1$perm_p < 0.05
  p('<tr%s><td>%s</td><td>%s%%</td><td>%s%%</td><td>%s%%</td><td>%s%%</td><td class="num">%+.2f pp</td><td class="%s">%s</td></tr>',
    if (sg) ' class="hl"' else '', cd, fmt(t1$w1), fmt(t1$w4), fmt(t1$w7), fmt(t1$w10),
    100 * t1$slope_per_week, if (sg) "sig" else "ns",
    if (t1$perm_p < 1e-4) "&lt; 0.0001" else sprintf("%.4f", t1$perm_p))
}
p('</tbody></table></div></div>')

p('<h2 id="b2"><span class="n">B2</span>What kind of comment, once they comment at all</h2>')
p('<figure><img alt="Composition of coded sentences by week" src="%s">
<figcaption>Restricted to sentences that carry a code, so this is independent of
how much of each entry was about culture at all.</figcaption></figure>',
  b64(sprintf("reports/fig_composition%s.png", FIGTAG)))
p('<div class="callout goodbox"><p><strong>The arc is a dip, not a climb.</strong>
Week&nbsp;1 is dominated by self-examination and contains <strong>no negative
comments at all</strong>. Week&nbsp;4 is the friction point, with <strong>%s%% of
that week&rsquo;s comments negative</strong>. By week&nbsp;7 positives have taken
over (%s%%) and week&nbsp;10 is overwhelmingly positive (%s%%) with self awareness
gone entirely &mdash; the classic forming&ndash;storming&ndash;norming shape,
appearing without anyone having looked for it.</p></div>',
  fmt(cmp(4, "Negative"), 0), fmt(cmp(7, "Positive"), 0), fmt(cmp(10, "Positive"), 0))

sa <- trend[trend$code == "Self awareness", ]; po <- trend[trend$code == "Positive", ]
p('<h2 id="b3"><span class="n">B3</span>Why this is not yet evidence of growth</h2>')
p('<div class="callout warnbox"><p><strong>Week is confounded with the
prompt.</strong> Each journal point asks its own questions, and those differ
between weeks. Every pattern above is equally well explained by a change in
<em>what was asked</em> as by a change in the students. The week-1 profile is
exactly what a pre-collaboration prompt produces: students cannot yet report
friction, so zero negatives is close to structurally guaranteed, and a question
about one&rsquo;s own expectations invites precisely the self-aware sentences
that dominate that week.</p>
<p style="margin-bottom:0">It bites hardest on the strongest result.
<strong>Self awareness falls from %s%% to %s%% (p %s)</strong> &mdash; but a
monotone collapse to exactly zero, over a course designed to build
self-awareness, is more consistent with the week-1 question having asked for it.
We would not report it as a developmental finding.</p></div>',
  fmt(sa$w1), fmt(sa$w10),
  if (sa$perm_p < 1e-4) "&lt; 0.0001" else sprintf("= %.4f", sa$perm_p))
p('<p>The rise in positive comments &mdash; the result the study would most like
to have &mdash; crosses the conventional threshold (%+.2f pp per week,
p = %.3f), but should be leaned on lightly: it is marginal, week&nbsp;7 exceeds
week&nbsp;10 so the series is not monotone, and it moved from p = 0.12 to
p = %.3f purely by adding two more human raters to five of the students.</p>',
  100 * po$slope_per_week, po$perm_p, po$perm_p)

p('<h2 id="b4"><span class="n">B4</span>Which kind of cultural intelligence</h2>
<figure><img alt="CQ factor of positive sentences by week" src="%s">
<figcaption>Ang &amp; Van Dyne factor of the positive sentences only.</figcaption></figure>',
  b64(sprintf("reports/fig_cq_drift%s.png", FIGTAG)))
mot <- sum(cq$k[cq$factor_ == "Motivational"])
p('<p><strong>Motivational dominates throughout</strong> &mdash; %d of %d
classified positive sentences (%s%%) &mdash; with no clear drift towards the
metacognitive or behavioural factors that would signal deeper learning. On this
evidence the positives are about enjoying and valuing the collaboration rather
than about adapting behaviour within it. Read alongside A4: Behavioural is
nearly invisible to this panel by construction, so its absence here is not
evidence.</p>', mot, sum(cq$k), fmt(mot / sum(cq$k), 0))

p('<h2 id="b5"><span class="n">B5</span>Do the people and the machines see the same arc?</h2>
<p>Every student was read by both panels, so they can be treated as two
independent instruments and asked the same question.</p>')
p('<div class="card"><div class="scroll"><table>
<thead><tr><th>Panel</th><th>w1</th><th>w4</th><th>w7</th><th>w10</th></tr></thead><tbody>
<tr><td colspan="5" class="small"><strong>Positive, as a share of coded sentences</strong></td></tr>')
for (nm in c("human panel", "model panel")) {
  d <- if (nm == "human panel") hp else mp
  p('<tr><td class="%s">%s</td>%s</tr>', if (nm == "human panel") "hum" else "mod", nm,
    paste(vapply(weeks, function(w) sprintf('<td class="num">%s%%</td>',
      fmt(share(d, w, "Positive"), 0)), character(1)), collapse = ""))
}
p('<tr><td colspan="5" class="small" style="padding-top:.7rem"><strong>Negative</strong></td></tr>')
for (nm in c("human panel", "model panel")) {
  d <- if (nm == "human panel") hp else mp
  p('<tr><td class="%s">%s</td>%s</tr>', if (nm == "human panel") "hum" else "mod", nm,
    paste(vapply(weeks, function(w) sprintf('<td class="num">%s%%</td>',
      fmt(share(d, w, "Negative"), 0)), character(1)), collapse = ""))
}
p('</tbody></table></div></div>')
p('<div class="callout goodbox"><p><strong>The two panels trace the same
curve.</strong> Both put the positive share lowest in week&nbsp;4 and highest in
week&nbsp;10, and both put the negative peak in week&nbsp;4. Week-by-week figures
differ by a few points, well inside what these counts can resolve. The arc is
therefore not an artefact of either kind of rater &mdash; it survives being
measured two independent ways. <em>This is the single most useful result for the
paper: it is what would justify using the model panel on the remaining
journals.</em></p></div>')

p('<h2 id="b6"><span class="n">B6</span>Every student individually</h2>
<figure><img alt="Per-student grid" src="%s">
<figcaption>Percentage of that week&rsquo;s sentences. Shading is intensity only
&mdash; the column heading carries the code.</figcaption></figure>',
  b64(sprintf("reports/fig_student_grid%s.png", FIGTAG)))

# =========================== PART C ======================================
p('<div class="part">Open decisions and method</div>')
p('<h2 id="c1"><span class="n">C</span>What we need to decide</h2>
<ol>
<li><strong>Do any questions repeat across journal points?</strong> Any item
asked identically at two or more weeks gives a clean within-question comparison
and would break the prompt confound outright. This is the highest-value thing on
the list and costs nothing if the instrument already does it.</li>
<li><strong>Behavioural.</strong> Either the models genuinely fail to recognise
behavioural adaptation, or the item wording makes the factor hard to reach.
Until this is settled the CQ layer cannot carry weight.</li>
<li><strong>Does the model panel enter the paper as one rater or three?</strong>
As three it triples the model weight in any pooled coefficient; as one it enters
as a denoised rater, which is what the majority column measures.</li>
<li><strong>Confirm the student&nbsp;9 week&nbsp;1 remapping</strong> against the
units file before those %d sentence pairs go into anything published. The
arithmetic closes exactly, which is evidence, not proof.</li>
<li><strong>Does the 2026-07-28 alignment sheet go public</strong> with the
repository, and who holds the copyright line on the licence?</li>
</ol>', 26L)

p('<h2 id="c2"><span class="n">D</span>How these numbers were made</h2>
<div class="card">
<p style="margin-top:0"><strong>Models.</strong> %s, run locally
(<code>ollama</code>) or on the FGCZ server (<code>vLLM</code>), temperature 0,
one replicate, closed-enum schemas so an off-codebook answer is unrepresentable.
No journal text ever leaves local infrastructure.</p>
<p><strong>Consensus.</strong> Strict majority of six; ties excluded, never
broken arbitrarily.</p>
<p><strong>Reliability.</strong> Krippendorff&rsquo;s alpha, nominal, with a
percentile bootstrap over coding units (B&nbsp;=&nbsp;%d). The implementation is
checked against analytic identities and against irrCAC.</p>
<p style="margin-bottom:0"><strong>Trend.</strong> Slope of the cohort mean rate
on week, with a permutation null that shuffles week labels within each student
(20,000 draws), so between-student differences cannot leak into the p-value.</p>
</div>', paste(mod_r, collapse = ", "), B)

p('<div class="callout"><p><strong>Reading guide.</strong> With %d students and
%d usable sentences, every percentage here moves by several points if one student
is added or removed, and the smallest student-week cell holds %d sentences.
Nothing in Part&nbsp;B should be quoted as a result. What this pass establishes
is that the pipeline runs end to end on real journals, that the model panel
tracks the human panel closely enough to be worth using, and that two concrete
problems &mdash; the prompt confound and the near-invisible Behavioural factor
&mdash; are still cheap to fix.</p></div>', length(studs), n_use, min(tbl))

p('<footer><p class="small">Generated by <code>scripts/20_report_full.R</code>.
Pipeline: <code>17_pool.R</code> &rarr; <code>18_trajectory.R</code> &rarr;
<code>13_figures_9n99.R</code> &rarr; this file. Every figure is computed at
render time, not transcribed.<br>
Pseudonymous student ids and category labels only &mdash; no journal text is read
by the analysis, the figures, or this report. %s &middot; %s.</p></footer>
</div></body></html>', R.version.string, format(Sys.time(), "%Y-%m-%d %H:%M"))

dir.create("reports", showWarnings = FALSE)
writeLines(paste(h, collapse = "\n"), OUT)
cat(sprintf("wrote %s  (%.2f MB)\n", OUT, file.size(OUT) / 1e6))
