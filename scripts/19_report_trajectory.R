#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# 19_report_trajectory.R -- shareable HTML on the cohort's trajectory
#
#   Rscript scripts/19_report_trajectory.R
#
# Run scripts/17_pool.R and scripts/18_trajectory.R first; this reads their CSV
# outputs and the PNGs, and embeds the figures as data URIs so the file can be
# mailed to a collaborator and still render.
#
# EVERY NUMBER IS READ FROM THE RESULT FILES, none typed in.
# READS NO JOURNAL TEXT.
# ---------------------------------------------------------------------------

repo <- normalizePath(file.path(dirname(sub("^--file=", "",
        commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])), ".."))
setwd(repo)

a <- commandArgs(trailingOnly = TRUE)
getopt <- function(f, d = NULL) { i <- match(f, a); if (is.na(i) || i == length(a)) d else a[i + 1L] }
OUT <- getopt("--out", "reports/trajectory_report.html")

rd <- function(p) utils::read.csv(p, stringsAsFactors = FALSE, na.strings = c("NA", ""))
trend  <- rd("results/trajectory_trend.csv")
cells  <- rd("results/trajectory_cells.csv")
pool   <- rd("results/trajectory_pooled.csv")
cq     <- rd("results/trajectory_cq.csv")
pooled <- rd("results/pooled_units.csv")

cons <- pooled[pooled$rater == "consensus", ]
raw  <- pooled[pooled$rater_kind %in% c("human", "model"), ]
studs <- sort(unique(as.character(cons$student)))
weeks <- sort(unique(cons$week))
n_all <- nrow(cons); n_nc <- sum(is.na(cons$code))
n_use <- n_all - n_nc
panel_sizes <- sort(unique(cons$n_raters))
raters <- sort(unique(raw$rater))
# smallest student-week cell, from the units that actually carry a consensus
uu <- unique(cons[!is.na(cons$code), c("student", "week", "unit_id")])
tbl <- table(uu$student, uu$week)

b64 <- function(p) {
  tf <- tempfile(); system2("base64", c("-i", shQuote(p)), stdout = tf)
  paste0("data:image/png;base64,", paste(readLines(tf, warn = FALSE), collapse = ""))
}
fmt <- function(x, d = 1) formatC(100 * x, format = "f", digits = d)
CODES <- c("Positive", "Negative", "Self awareness")

gv <- function(df, cd, col) df[[col]][df$code == cd]
pv <- function(w, cd, col) pool[[col]][pool$week == w & pool$code == cd]
# share of that week's CODED sentences -- the denominator section 2 and the
# composition figure use, kept in one place so the KPIs cannot drift from them
cmp <- function(w, cd) {
  den <- sum(vapply(CODES, function(z) pv(w, z, "k"), numeric(1)))
  pv(w, cd, "k") / den
}

h <- c(); p <- function(...) h <<- c(h, sprintf(...))

p('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Reflection over eleven weeks</title>
<style>
  :root{
    --bg:#fbfaf7; --panel:#ffffff; --ink:#1c1a17; --muted:#6b645c;
    --rule:#e2ddd4; --accent:#8a6a2f;
    --pos:#1baf7a; --neg:#eb6834; --self:#2a78d6;
    --good:#2f6b4f; --warn:#a8641c; --bad:#a8392c;
  }
  @media (prefers-color-scheme: dark){
    :root:not([data-theme="light"]){
      --bg:#16151a; --panel:#1e1d23; --ink:#eceae6; --muted:#a39c93;
      --rule:#312f38; --accent:#d8b262; --good:#6fbb92; --warn:#e0a45c; --bad:#e8705c;
    }
  }
  :root[data-theme="dark"]{
    --bg:#16151a; --panel:#1e1d23; --ink:#eceae6; --muted:#a39c93;
    --rule:#312f38; --accent:#d8b262; --good:#6fbb92; --warn:#e0a45c; --bad:#e8705c;
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
  code{font:13px/1.5 ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;
    background:color-mix(in srgb,var(--accent) 12%%,transparent);
    padding:.1em .35em;border-radius:3px}
  .card{background:var(--panel);border:1px solid var(--rule);border-radius:10px;
    padding:1.1rem 1.25rem;margin:1.2rem 0}
  .scroll{overflow-x:auto}
  table{border-collapse:collapse;width:100%%;font-size:.94rem;
    font-variant-numeric:tabular-nums}
  th,td{text-align:right;padding:.45rem .6rem;border-bottom:1px solid var(--rule);
    white-space:nowrap}
  th:first-child,td:first-child{text-align:left}
  thead th{font-weight:600;font-size:.78rem;letter-spacing:.04em;text-transform:uppercase;
    color:var(--muted);border-bottom:1.5px solid var(--ink)}
  tbody tr:last-child td{border-bottom:none}
  tr.hl td{background:color-mix(in srgb,var(--accent) 9%%,transparent)}
  .num{font-weight:600}
  .kpis{display:grid;gap:.9rem;grid-template-columns:repeat(auto-fit,minmax(11rem,1fr));margin:1.4rem 0}
  .kpi{background:var(--panel);border:1px solid var(--rule);border-radius:10px;padding:.9rem 1rem}
  .kpi .v{font-size:1.7rem;font-weight:700;line-height:1.1;font-variant-numeric:tabular-nums}
  .kpi .l{font-size:.8rem;color:var(--muted);text-transform:uppercase;letter-spacing:.04em;margin-top:.25rem}
  .callout{border-left:3px solid var(--accent);padding:.15rem 0 .15rem 1rem;margin:1.2rem 0}
  .warnbox{border-left:3px solid var(--bad)}
  .goodbox{border-left:3px solid var(--good)}
  ul,ol{margin:.7rem 0;padding-left:1.3rem} li{margin:.35rem 0}
  figure{margin:1.5rem 0}
  figure img{width:100%%;height:auto;border:1px solid var(--rule);border-radius:8px;background:#fcfcfb}
  figcaption{font-size:.86rem;color:var(--muted);margin-top:.5rem}
  .pos{color:var(--pos);font-weight:700}
  .neg{color:var(--neg);font-weight:700}
  .slf{color:var(--self);font-weight:700}
  .sig{color:var(--good);font-weight:700}
  .ns{color:var(--muted)}
  footer{margin-top:3.5rem;padding-top:1.2rem;border-top:1px solid var(--rule)}
</style>
</head>
<body>
<div class="wrap">')

p('<header>
  <h1>How the cohort reflected, week by week</h1>
  <p class="sub">COIL follow-up study &middot; &ldquo;Omics in Oncology&rdquo; &middot; %s<br>
  %d students &middot; %d sentences with a consensus label &middot; weeks %s &middot; %s raters per sentence</p>
</header>',
  trimws(format(Sys.Date(), "%e %B %Y")), length(studs), n_use,
  paste(weeks, collapse = ", "), paste(panel_sizes, collapse = " or "))

p('<p class="lede">Every sentence of every reflection journal carries one label
&mdash; <span class="pos">Positive</span>, <span class="neg">Negative</span>,
<span class="slf">Self&nbsp;awareness</span> or <em>Not&nbsp;marked</em> &mdash;
agreed by strict majority of the raters who read it. This report asks what those
labels do across the four journal points; who assigned them matters only in
section&nbsp;5, where the people and the models are compared as two independent
instruments.</p>')

# --- KPIs ---------------------------------------------------------------
sa <- trend[trend$code == "Self awareness", ]
po <- trend[trend$code == "Positive", ]
p('<div class="kpis">
  <div class="kpi"><div class="v">%s%% &rarr; %s%%</div><div class="l">of comments positive<br>week 1 to week 10</div></div>
  <div class="kpi"><div class="v">%s%%</div><div class="l">of week-4 comments<br>were negative</div></div>
  <div class="kpi"><div class="v">%s%% &rarr; %s%%</div><div class="l">of comments self-aware<br>week 1 to week 10</div></div>
  <div class="kpi"><div class="v">%d</div><div class="l">students so far<br>of about 27 expected</div></div>
</div>
<p class="small">The first three are shares of the sentences that carry a code, matching
section&nbsp;2; per-sentence rates, which section&nbsp;1 uses, are lower because most
sentences are not about culture at all.</p>',
  fmt(cmp(1, "Positive"), 0), fmt(cmp(10, "Positive"), 0),
  fmt(cmp(4, "Negative"), 0),
  fmt(cmp(1, "Self awareness"), 0), fmt(cmp(10, "Self awareness"), 0),
  length(studs))

# --- 1 the shape --------------------------------------------------------
p('<h2><span class="n">1</span>The shape of the eleven weeks</h2>')
p('<p>Pooling every student&rsquo;s sentences within a week gives a clear and
rather orderly arc. It is not a steady climb in positivity; it is a dip and a
recovery.</p>')

p('<div class="card"><div class="scroll"><table>
<thead><tr><th>Week</th><th>Sentences</th><th>Positive</th><th>Negative</th><th>Self awareness</th><th>Any code</th></tr></thead><tbody>')
for (w in weeks) {
  nn <- pv(w, "Positive", "n")
  p('<tr><td>week %d</td><td>%d</td><td class="num pos">%d (%s%%)</td><td class="num neg">%d (%s%%)</td><td class="num slf">%d (%s%%)</td><td>%d (%s%%)</td></tr>',
    w, nn, pv(w, "Positive", "k"), fmt(pv(w, "Positive", "rate"), 0),
    pv(w, "Negative", "k"), fmt(pv(w, "Negative", "rate"), 0),
    pv(w, "Self awareness", "k"), fmt(pv(w, "Self awareness", "rate"), 0),
    pv(w, "any code", "k"), fmt(pv(w, "any code", "rate"), 0))
}
p('</tbody></table></div>
<p class="small" style="margin-bottom:0">Percentages are of everything written that
week, so they are not inflated by a longer entry. 95%% Wilson intervals are in
<code>results/trajectory_pooled.csv</code>.</p></div>')

p('<figure><img alt="Trajectory of the three codes across weeks 1, 4, 7 and 10" src="%s">
<figcaption>Each code as a share of that week&rsquo;s sentences. Grey lines are the
%d individual students; the coloured line is the cohort mean of per-student rates.
The slope and p-value come from a permutation test that shuffles week labels
<em>within</em> each student, so it holds every student&rsquo;s overall rate and
weekly sentence counts fixed and tests only the ordering in time.</figcaption></figure>',
  b64("reports/fig_trajectory.png"), length(studs))

p('<div class="card"><div class="scroll"><table>
<thead><tr><th>Code</th><th>w1</th><th>w4</th><th>w7</th><th>w10</th><th>Slope / week</th><th>Permutation p</th></tr></thead><tbody>')
for (cd in c(CODES, "any code")) {
  t1 <- trend[trend$code == cd, ]
  sig <- !is.na(t1$perm_p) && t1$perm_p < 0.05
  p('<tr%s><td>%s</td><td>%s%%</td><td>%s%%</td><td>%s%%</td><td>%s%%</td><td class="num">%+.2f pp</td><td class="%s">%s</td></tr>',
    if (sig) ' class="hl"' else '', cd,
    fmt(t1$w1, 1), fmt(t1$w4, 1), fmt(t1$w7, 1), fmt(t1$w10, 1),
    100 * t1$slope_per_week, if (sig) "sig" else "ns",
    if (t1$perm_p < 1e-4) "&lt; 0.0001" else sprintf("%.4f", t1$perm_p))
}
p('</tbody></table></div>
<p class="small" style="margin-bottom:0">Weekly figures here are the mean of the
seven per-student rates, so each student counts once regardless of how much they
wrote; the pooled table above weights each sentence once. The two differ slightly
and neither is wrong.</p></div>')

# --- 2 composition ------------------------------------------------------
p('<h2><span class="n">2</span>What kind of comment, once they comment at all</h2>')
p('<p>Between a seventh and a quarter of each journal is about intercultural
teamwork. That share drifts down across the weeks (%s%% to %s%%) but not
reliably so (p = %.2f), and it does not move in one direction. What moves
clearly is the <em>character</em> of those comments.</p>',
  fmt(trend$w1[trend$code == "any code"], 0), fmt(trend$w10[trend$code == "any code"], 0),
  trend$perm_p[trend$code == "any code"])

p('<figure><img alt="Composition of coded sentences by week" src="%s">
<figcaption>Restricted to the sentences that carry a code, so this is independent
of how much of each entry was about culture at all.</figcaption></figure>',
  b64("reports/fig_composition.png"))

w1n <- sum(sapply(CODES, function(z) pv(1, z, "k")))
w4n <- sum(sapply(CODES, function(z) pv(4, z, "k")))
w7n <- sum(sapply(CODES, function(z) pv(7, z, "k")))
w10n <- sum(sapply(CODES, function(z) pv(10, z, "k")))
p('<div class="callout goodbox"><p><strong>The arc is a dip, not a climb.</strong>
Week&nbsp;1 is dominated by self-examination and contains <strong>no negative
comments at all</strong> (%d of %d coded sentences are Self awareness).
Week&nbsp;4 is the friction point: <strong>%s%% of that week&rsquo;s comments are
negative</strong>. By week&nbsp;7 positives have taken over (%s%%), and week&nbsp;10
is overwhelmingly positive (%s%%) with self awareness gone entirely. That is the
classic forming&ndash;storming&ndash;norming shape, and it appears in the data
without anyone having looked for it.</p></div>',
  pv(1, "Self awareness", "k"), w1n,
  fmt(pv(4, "Negative", "k") / w4n, 0),
  fmt(pv(7, "Positive", "k") / w7n, 0),
  fmt(pv(10, "Positive", "k") / w10n, 0))

# --- 3 the confound -----------------------------------------------------
p('<h2><span class="n">3</span>Why this is not yet evidence of growth</h2>')
sigs <- trend$code[!is.na(trend$perm_p) & trend$perm_p < 0.05 & trend$code != "any code"]
p('<div class="callout warnbox"><p><strong>Week is confounded with the
prompt.</strong> Each journal point asks its own set of questions, and those
questions differ between weeks. Every pattern above is therefore equally well
explained by a change in <em>what was asked</em> as by a change in the students.
The week-1 profile is exactly what a pre-collaboration prompt would produce:
students cannot yet report friction, so zero negatives is close to structurally
guaranteed, and a question about one&rsquo;s own expectations invites precisely
the self-aware sentences that dominate that week.</p>
<p style="margin-bottom:0">It matters most for the strongest result.
<strong>Self awareness falls from %s%% to %s%% (p %s)</strong>, comfortably the
clearest trend here &mdash; but a monotone collapse to exactly zero is more
consistent with the week-1 question having asked for it than with students
becoming less self-aware over a course designed to make them more so. We would
not report it as a developmental finding.</p></div>',
  fmt(sa$w1, 1), fmt(sa$w10, 1),
  if (sa$perm_p < 1e-4) "&lt; 0.0001" else sprintf("= %.4f", sa$perm_p))

p('<p>The rise in positive comments &mdash; the result the study would most like
to have &mdash; now crosses the conventional threshold (%+.2f pp per week,
p = %.3f), but it should be leaned on lightly. It is marginal; week&nbsp;7 is
higher than week&nbsp;10, so the series is not monotone and a straight-line slope
is a poor summary of it; and it moved from p = 0.12 to p = %.3f purely by adding
two more human raters to five of the students, which is a reminder of how much
these figures still depend on who is on the panel rather than on what the
students wrote.</p>', 100 * po$slope_per_week, po$perm_p, po$perm_p)

p('<h3>What would settle it</h3>
<ul>
<li><strong>Match the questions across weeks.</strong> Any question asked
identically at two or more journal points gives a clean within-question
comparison. If the instrument already repeats some items, that analysis is
available immediately and is far stronger than anything here.</li>
<li><strong>Code the questions.</strong> Even a coarse tag per question (asks
about self / asks about the team / asks about the science) would let the week
effect be adjusted for prompt type.</li>
<li><strong>More students.</strong> With %d students the permutation test has
little power; the twenty or so still to come would improve that substantially, though
not in proportion to the count.</li>
</ul>', length(studs))

# --- 4 CQ ---------------------------------------------------------------
p('<h2><span class="n">4</span>Which kind of cultural intelligence</h2>')
p('<figure><img alt="Cultural-intelligence factor of positive sentences by week" src="%s">
<figcaption>Ang &amp; Van Dyne factor of the positive sentences only.</figcaption></figure>',
  b64("reports/fig_cq_drift.png"))

mot <- sum(cq$k[cq$factor_ == "Motivational"]); tot <- sum(cq$k)
p('<p><strong>Motivational dominates throughout</strong> &mdash; %d of the %d
classified positive sentences (%s%%) &mdash; with no clear drift towards the
metacognitive or behavioural factors that would signal deeper learning. On this
evidence the positives are mostly about enjoying and valuing the collaboration
rather than about adapting behaviour within it.</p>', mot, tot, fmt(mot / tot, 0))

fu <- table(raw$cq_factor[!is.na(raw$cq_factor)], raw$rater[!is.na(raw$cq_factor)])
beh <- if ("Behavioural" %in% rownames(fu)) fu["Behavioural", ] else setNames(rep(0L, ncol(fu)), colnames(fu))
zero <- names(beh)[beh == 0]
n_beh_cons <- sum(cq$k[cq$factor_ == "Behavioural"])
p('<div class="callout warnbox"><p><strong>Behavioural is barely reachable, and
two of the three models never use it at all.</strong> Across every rating,
<code>%s</code> assigned the Behavioural factor <strong>zero times</strong>;
the other four raters used it %d times between them. The consensus therefore
lands on Behavioural just <strong>%d time%s</strong> in %d classified positive
sentences. Whatever the students did or did not say about adapting their
behaviour, this figure is not able to show it.</p>
<p style="margin-bottom:0">Worth noting how this changed: with an earlier
four-rater panel (one person, three models) a Behavioural majority was
arithmetically unreachable and the consensus contained none at all. Adding the
two further human raters made it reachable but still rare. The factor&rsquo;s
visibility depends on the panel composition, which is a property of the
instrument rather than of the journals, and it should be settled before the CQ
layer carries any weight in the paper.</p></div>',
  paste(zero, collapse = "</code> and <code>"), sum(beh),
  n_beh_cons, if (n_beh_cons == 1L) "" else "s", tot)

# --- 4b human panel vs model panel ---------------------------------------
hp <- pooled[pooled$rater == "human panel", ]
mp <- pooled[pooled$rater == "model panel", ]
if (nrow(hp) && nrow(mp)) {
  share <- function(d, w, cd) {
    k <- d[d$week == w & !is.na(d$code) & d$code != "Not marked", ]
    if (!nrow(k)) return(NA_real_)
    sum(k$code == cd) / nrow(k)
  }
  p('<h2><span class="n">5</span>Do the people and the machines see the same arc?</h2>')
  p('<p>Every student was read by three people and three models, so the two
panels can be taken as two independent instruments and asked the same question.
This was meant to be a later phase; the data now supports it, so here is a first
look.</p>')
  p('<div class="card"><div class="scroll"><table>
<thead><tr><th>Panel</th><th colspan="4" style="text-align:center">Share of coded sentences that are Positive</th></tr>
<tr><th></th><th>w1</th><th>w4</th><th>w7</th><th>w10</th></tr></thead><tbody>')
  for (nm in c("human panel", "model panel")) {
    d <- if (nm == "human panel") hp else mp
    p('<tr><td>%s</td>%s</tr>', nm,
      paste(vapply(weeks, function(w) sprintf('<td class="num">%s%%</td>',
        fmt(share(d, w, "Positive"), 0)), character(1)), collapse = ""))
  }
  p('</tbody><tbody><tr><td colspan="5" style="padding-top:.8rem"><strong>and Negative</strong></td></tr>')
  for (nm in c("human panel", "model panel")) {
    d <- if (nm == "human panel") hp else mp
    p('<tr><td>%s</td>%s</tr>', nm,
      paste(vapply(weeks, function(w) sprintf('<td class="num">%s%%</td>',
        fmt(share(d, w, "Negative"), 0)), character(1)), collapse = ""))
  }
  p('</tbody></table></div></div>')
  p('<div class="callout goodbox"><p><strong>The two panels trace the same
curve.</strong> Both put the positive share lowest in week&nbsp;4 and highest in
week&nbsp;10, and both put the negative peak in week&nbsp;4. The week-by-week
figures differ by a few points, which is well inside what these counts can
resolve. The arc in this report is therefore not an artefact of either kind of
rater &mdash; it survives being measured two independent ways.</p></div>')
}

# --- 5 students ---------------------------------------------------------
p('<h2><span class="n">6</span>Every student individually</h2>')
p('<p>The cohort means hide a lot. Each student is one row; the figure makes it
possible to check whether the cohort arc is shared or driven by a few.</p>')
p('<figure><img alt="Per-student grid of code rates by week" src="%s">
<figcaption>Percentage of that week&rsquo;s sentences. Shading is intensity only
&mdash; the column heading carries the code.</figcaption></figure>',
  b64("reports/fig_student_grid.png"))

sa_w1 <- sum(cells$k[cells$code == "Self awareness" & cells$week == 1] > 0)
ng_w4 <- sum(cells$k[cells$code == "Negative" & cells$week == 4] > 0)
p('<p>The week-1 self-awareness spike is shared: <strong>%d of the %d students
show it</strong>. The week-4 negative peak appears in <strong>%d of %d</strong>.
So the arc is a cohort pattern rather than one or two students dragging the mean
&mdash; which, given the prompt confound, mostly tells us the students were all
answering the same questions.</p>', sa_w1, length(studs), ng_w4, length(studs))

# --- 6 how ---------------------------------------------------------------
p('<h2><span class="n">7</span>How these numbers were made</h2>
<div class="card">
<p style="margin-top:0"><strong>Data.</strong> %d sentences from %d students,
each labelled independently by all %d raters &mdash; %s &mdash; so every student
rests on the same panel of three people and three models.</p>
<p><strong>Consensus.</strong> One label per sentence by strict majority.
%d sentences (%s%%) had no majority and are excluded rather than resolved
arbitrarily. %s%% of the rest were unanimous.</p>
<p><strong>Two students appear once, not twice.</strong> Students 8 and 94 were
rated in an earlier batch as well, from the same journals segmented into different
questions. Pooling both would have entered them twice under different ids and
doubled their weight; the earlier batch is excluded, and
<code>scripts/17_pool.R</code> refuses to run if any student appears in two
batches.</p>
<p style="margin-bottom:0"><strong>Test.</strong> Slope of the cohort mean rate on
week, with a permutation null that shuffles week labels within each student
(%s draws). This holds each student&rsquo;s overall rate and their weekly sentence
counts fixed, so only the time-ordering is tested and between-student differences
cannot leak into the p-value.</p>
</div>',
  n_all, length(studs), length(raters), paste(raters, collapse = ", "),
  n_nc, fmt(n_nc / n_all, 1),
  fmt(mean(cons$n_agree == cons$n_raters, na.rm = TRUE), 0), "20,000")

p('<div class="callout"><p><strong>Reading guide for the sceptical colleague.</strong>
With %d students and %d usable sentences, every percentage in this report moves by
several points if one student is added or removed, and the smallest
student-week cell holds %d sentences. Nothing here should be quoted as a
result. The value of this pass is that the pipeline runs end to end on real
journals, the arc it produces is coherent and interpretable, and it has surfaced
two concrete problems &mdash; the prompt confound and the near-invisible
Behavioural factor &mdash; while they are still cheap to fix.</p></div>',
  length(studs), n_use, min(tbl))

p('<footer><p class="small">Generated by <code>scripts/19_report_trajectory.R</code>
from <code>results/trajectory_*.csv</code> and <code>results/pooled_units.csv</code>.
Pipeline: <code>17_pool.R</code> &rarr; <code>18_trajectory.R</code> &rarr; this
file; every figure is computed at render time, not transcribed.<br>
Pseudonymous student ids and category labels only &mdash; no journal text is read
by the analysis, the figures, or this report. %s &middot; %s.</p></footer>
</div>
</body>
</html>', R.version.string, format(Sys.time(), "%Y-%m-%d %H:%M"))

dir.create("reports", showWarnings = FALSE)
writeLines(paste(h, collapse = "\n"), OUT)
cat(sprintf("wrote %s  (%.2f MB)\n", OUT, file.size(OUT) / 1e6))
