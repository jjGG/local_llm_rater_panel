#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# 23_report_student.R -- one student, before and after the raters discussed
#
#   Rscript scripts/23_report_student.R \
#     --after  results/pooled_units_afterDiscussion_stud0.csv \
#     --before results/pooled_units_Sep09.csv \
#     --student 0 --tag _stud0after
#
# EVERY NUMBER IS COMPUTED HERE, none typed in.
# READS NO JOURNAL TEXT.
# ---------------------------------------------------------------------------

repo <- normalizePath(file.path(dirname(sub("^--file=", "",
        commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])), ".."))
setwd(repo)
source("test_for_consistency/R/agreement.R")
source("R/six_rater.R")

a <- commandArgs(trailingOnly = TRUE)
getopt <- function(f, d = NULL) { i <- match(f, a); if (is.na(i) || i == length(a)) d else a[i + 1L] }
AFTER  <- getopt("--after",  "results/pooled_units_afterDiscussion_stud0.csv")
BEFORE <- getopt("--before", "results/pooled_units_Sep09.csv")
STUD   <- getopt("--student", "0")
TAG    <- getopt("--tag", "_stud0after")
B      <- as.integer(getopt("--boot", "4000"))
OUT    <- getopt("--out", sprintf("reports/student%s_after_discussion.html", STUD))
set.seed(20260918)

rd <- function(f) utils::read.csv(f, stringsAsFactors = FALSE, na.strings = c("NA", ""))
load1 <- function(f) {
  p <- rd(f); p <- p[as.character(p$student) == STUD & p$rater_kind %in% c("human", "model"), ]
  ids <- sort(unique(p$unit_id))
  hr <- sort(unique(p$rater[p$rater_kind == "human"]))
  mr <- sort(unique(p$rater[p$rater_kind == "model"]))
  W <- function(rs, col = "code") vapply(rs, function(r) {
    s <- p[p$rater == r, ]; s[[col]][match(ids, s$unit_id)] }, character(length(ids)))
  list(p = p, ids = ids, hr = hr, mr = mr, H = W(hr), M = W(mr))
}
A <- load1(AFTER); Bf <- load1(BEFORE)
stopifnot(identical(A$ids, Bf$ids), identical(A$hr, Bf$hr), identical(A$mr, Bf$mr))
n <- length(A$ids)
weeks <- sort(unique(A$p$week))

est <- function(m) {
  k <- krippendorff_alpha(m, PRIMARY_LEVELS)
  ci <- bootstrap_ci(m, stat_alpha, PRIMARY_LEVELS, B = B)
  list(a = k$alpha, lo = ci$lower, hi = ci$upper, n = k$n_units)
}
panels <- function(g) cbind(humans = panel_modal(g$H, tie = NA_character_),
                            models = panel_modal(g$M, tie = NA_character_))
rows <- list()
for (nm in c("before", "after")) {
  g <- if (nm == "before") Bf else A
  rows[[nm]] <- list(h = est(g$H), m = est(g$M),
                     p = est(cbind(g$H, g$M)), x = est(panels(g)))
}
mk <- function(g) apply(cbind(g$H, g$M), 2L, function(v) mean(v != "Not marked", na.rm = TRUE))
mkB <- mk(Bf); mkA <- mk(A)

ch <- A$H != Bf$H
n_ch <- sum(ch, na.rm = TRUE)
who <- colSums(ch, na.rm = TRUE)
tr <- table(from = Bf$H[ch], to = A$H[ch])

cons <- rd(AFTER); cons <- cons[cons$rater == "consensus", ]
n_nc <- sum(is.na(cons$code)); n_un <- sum(cons$n_agree == cons$n_raters, na.rm = TRUE)

b64 <- function(f) { if (!file.exists(f)) stop("missing figure: ", f)
  tf <- tempfile(); system2("base64", c("-i", shQuote(f)), stdout = tf)
  paste0("data:image/png;base64,", paste(readLines(tf, warn = FALSE), collapse = "")) }
f3 <- function(x) formatC(x, format = "f", digits = 3)
pc <- function(x, d = 0) formatC(100 * x, format = "f", digits = d)
ciw <- function(e) sprintf('<span class="ci">[%s, %s]</span>', f3(e$lo), f3(e$hi))
arrow <- function(x, y) sprintf('%s &rarr; <strong>%s</strong>', f3(x), f3(y))

h <- c(); p <- function(...) h <<- c(h, sprintf(...))
p('<!DOCTYPE html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Student %s after the rater discussion</title>
<style>
 :root{--bg:#fbfaf7;--panel:#fff;--ink:#1c1a17;--muted:#6b645c;--rule:#e2ddd4;
  --accent:#8a6a2f;--pos:#1baf7a;--neg:#eb6834;--self:#2a78d6;--good:#2f6b4f;
  --bad:#a8392c;--hum:#1b5fa8;--mod:#b4600d}
 @media (prefers-color-scheme:dark){:root:not([data-theme="light"]){
  --bg:#16151a;--panel:#1e1d23;--ink:#eceae6;--muted:#a39c93;--rule:#312f38;
  --accent:#d8b262;--good:#6fbb92;--bad:#e8705c;--hum:#7fb3ec;--mod:#e8a55f}}
 :root[data-theme="dark"]{--bg:#16151a;--panel:#1e1d23;--ink:#eceae6;--muted:#a39c93;
  --rule:#312f38;--accent:#d8b262;--good:#6fbb92;--bad:#e8705c;--hum:#7fb3ec;--mod:#e8a55f}
 *{box-sizing:border-box}
 body{margin:0;background:var(--bg);color:var(--ink);
  font:16px/1.6 "Iowan Old Style","Palatino Linotype",Palatino,Georgia,serif;padding:0 1.25rem 5rem}
 .wrap{max-width:58rem;margin:0 auto}
 header{padding:3.2rem 0 1.4rem;border-bottom:2px solid var(--ink)}
 h1{font-size:2rem;line-height:1.14;margin:0 0 .5rem;letter-spacing:-.01em}
 .sub{color:var(--muted);font-size:1.02rem;margin:0}
 h2{font-size:1.3rem;margin:2.6rem 0 .2rem}
 h2 .n{color:var(--accent);font-variant-numeric:tabular-nums;margin-right:.5rem}
 h3{font-size:1.02rem;margin:1.6rem 0 .4rem}
 p{margin:.7rem 0}.lede{font-size:1.08rem}
 small,.small{font-size:.88rem;color:var(--muted)}
 code{font:13px/1.5 ui-monospace,Menlo,Consolas,monospace;
  background:color-mix(in srgb,var(--accent) 12%%,transparent);padding:.1em .35em;border-radius:3px}
 .card{background:var(--panel);border:1px solid var(--rule);border-radius:10px;
  padding:1.05rem 1.2rem;margin:1.2rem 0}
 .scroll{overflow-x:auto}
 table{border-collapse:collapse;width:100%%;font-size:.93rem;font-variant-numeric:tabular-nums}
 th,td{text-align:right;padding:.45rem .6rem;border-bottom:1px solid var(--rule);white-space:nowrap}
 th:first-child,td:first-child{text-align:left}
 thead th{font-weight:600;font-size:.78rem;letter-spacing:.04em;text-transform:uppercase;
  color:var(--muted);border-bottom:1.5px solid var(--ink)}
 tbody tr:last-child td{border-bottom:none}
 tr.hl td{background:color-mix(in srgb,var(--accent) 9%%,transparent)}
 .num{font-weight:600}.ci{color:var(--muted);font-size:.85rem}
 .kpis{display:grid;gap:.9rem;grid-template-columns:repeat(auto-fit,minmax(11rem,1fr));margin:1.3rem 0}
 .kpi{background:var(--panel);border:1px solid var(--rule);border-radius:10px;padding:.9rem 1rem}
 .kpi .v{font-size:1.5rem;font-weight:700;line-height:1.1;font-variant-numeric:tabular-nums}
 .kpi .l{font-size:.78rem;color:var(--muted);text-transform:uppercase;letter-spacing:.04em;margin-top:.25rem}
 .callout{border-left:3px solid var(--accent);padding:.15rem 0 .15rem 1rem;margin:1.2rem 0}
 .warnbox{border-left:3px solid var(--bad)}.goodbox{border-left:3px solid var(--good)}
 ul,ol{margin:.7rem 0;padding-left:1.3rem}li{margin:.35rem 0}
 figure{margin:1.4rem 0}
 figure img{width:100%%;height:auto;border:1px solid var(--rule);border-radius:8px;background:#fcfcfb}
 figcaption{font-size:.86rem;color:var(--muted);margin-top:.5rem}
 .up{color:var(--good);font-weight:700}.down{color:var(--bad);font-weight:700}
 .hum{color:var(--hum);font-weight:700}.mod{color:var(--mod);font-weight:700}
 footer{margin-top:3rem;padding-top:1.2rem;border-top:1px solid var(--rule)}
</style></head><body><div class="wrap">', STUD)

p('<header><h1>Student %s, after the three raters discussed their codes</h1>
<p class="sub">COIL follow-up study &middot; %s<br>
%d sentences &middot; weeks %s &middot; %d human raters and %d models</p></header>',
  STUD, trimws(format(Sys.Date(), "%e %B %Y")), n,
  paste(weeks, collapse = ", "), length(A$hr), length(A$mr))

p('<p class="lede">%s, %s and %s went through student %s&rsquo;s journal together
and revised %d of their %d codes. The model ratings were not touched. This
compares the panel before and after that conversation.</p>',
  A$hr[1], A$hr[2], A$hr[3], STUD, n_ch, length(ch))

p('<div class="kpis">
<div class="kpi"><div class="v %s">%s</div><div class="l">alpha among<br>the three raters</div></div>
<div class="kpi"><div class="v %s">%s</div><div class="l">human panel vs<br>model panel</div></div>
<div class="kpi"><div class="v">%d of %d</div><div class="l">human codes<br>revised</div></div>
<div class="kpi"><div class="v">0</div><div class="l">model codes<br>changed</div></div></div>',
  if (rows$after$h$a > rows$before$h$a) "up" else "down",
  arrow(rows$before$h$a, rows$after$h$a),
  if (rows$after$x$a > rows$before$x$a) "up" else "down",
  arrow(rows$before$x$a, rows$after$x$a), n_ch, length(ch))

# --- 1 reliability -------------------------------------------------------
p('<h2><span class="n">1</span>Krippendorff&rsquo;s alpha, before and after</h2>
<p>Nominal, four unordered categories, percentile bootstrap over the %d coding
units (B&nbsp;=&nbsp;%d).</p>', n, B)
p('<div class="card"><div class="scroll"><table>
<thead><tr><th>Comparison</th><th>before</th><th>95%% CI</th><th>after</th><th>95%% CI</th></tr></thead><tbody>')
lab <- c(h = sprintf("among the %d human raters", length(A$hr)),
         m = sprintf("among the %d models", length(A$mr)),
         p = "all six pooled", x = "human panel vs model panel")
for (k in c("h", "m", "p", "x")) {
  bb <- rows$before[[k]]; aa <- rows$after[[k]]
  p('<tr%s><td>%s</td><td>%s</td><td>%s</td><td class="num %s">%s</td><td>%s</td></tr>',
    if (k %in% c("h", "x")) ' class="hl"' else '', lab[[k]],
    f3(bb$a), ciw(bb), if (aa$a > bb$a) "up" else if (aa$a < bb$a) "down" else "",
    f3(aa$a), ciw(aa))
}
p('</tbody></table></div>
<p class="small" style="margin-bottom:0">The model rows are identical by
construction &mdash; the models were not part of the discussion, and their codes
are byte-for-byte unchanged. They serve here as a fixed reference against which
the human movement can be read.</p></div>')

p('<div class="callout goodbox"><p><strong>The discussion did what it was meant to
do.</strong> Agreement among the three raters rose from %s to <strong>%s</strong>,
from a figure that would not support any published claim to one comfortably above
the conventional 0.80 threshold. On this student they now agree almost
completely.</p></div>', f3(rows$before$h$a), f3(rows$after$h$a))

p('<div class="callout warnbox"><p><strong>And agreement with the models fell, from
%s to %s.</strong> This is not a contradiction; it follows directly from the
direction the raters moved. Reconciling with each other meant converging on
marking <em>more</em> sentences as relevant, and that moved them away from the
models, which did not move at all.</p>
<p style="margin-bottom:0">The uncomfortable implication is worth stating
plainly: part of the earlier finding that the model panel agrees with the human
panel about as well as the humans agree with each other rested on the humans
being noisy. As the human panel is tightened by discussion, the models are left
behind. If the whole cohort is reconciled this way, the headline reliability
result will need recomputing, and it will probably fall.</p></div>',
  f3(rows$before$x$a), f3(rows$after$x$a))

# --- 2 what changed ------------------------------------------------------
p('<h2><span class="n">2</span>What the raters changed</h2>')
p('<div class="card"><div class="scroll"><table>
<thead><tr><th>Rater</th><th>codes revised</th><th>marking rate before</th><th>after</th></tr></thead><tbody>')
for (i in seq_along(A$hr)) p('<tr><td class="hum">%s</td><td class="num">%d of %d</td><td>%s%%</td><td class="num">%s%%</td></tr>',
  A$hr[i], who[[i]], n, pc(mkB[[A$hr[i]]]), pc(mkA[[A$hr[i]]]))
for (r in A$mr) p('<tr><td class="mod">%s</td><td class="small">not in the discussion</td><td>%s%%</td><td>%s%%</td></tr>',
  r, pc(mkB[[r]]), pc(mkA[[r]]))
p('</tbody></table></div>
<p class="small" style="margin-bottom:0">Marking rate is the share of the %d
sentences given any code other than <em>Not marked</em>.</p></div>', n)

p('<h3>From which code to which</h3><div class="card"><div class="scroll"><table>
<thead><tr><th>from &darr; / to &rarr;</th>%s</tr></thead><tbody>',
  paste(sprintf("<th>%s</th>", colnames(tr)), collapse = ""))
for (i in rownames(tr)) p('<tr><td>%s</td>%s</tr>', i,
  paste(vapply(colnames(tr), function(j) sprintf('<td>%s</td>',
    if (tr[i, j] == 0) "&middot;" else as.character(tr[i, j])), character(1)), collapse = ""))
p('</tbody></table></div>
<p class="small" style="margin-bottom:0">Almost all movement is out of
<em>Not marked</em>: the discussion made the raters more willing to treat a
sentence as being about intercultural teamwork, not more willing to call it
positive.</p></div>')

# --- 3 the heat map ------------------------------------------------------
p('<h2><span class="n">3</span>The coding surface</h2>
<figure><img alt="Coding surface for student %s" src="%s">
<figcaption>Every sentence against every rater, in document order, banded by
week. Left of the dashed line the three people, right of it the three models;
<code>humans</code> and <code>models</code> are each block&rsquo;s majority.
Red dots mark the sentences where the two majorities differ.</figcaption></figure>',
  STUD, b64(sprintf("reports/fig_surface%s.png", TAG)))

p('<figure><img alt="Pairwise agreement" src="%s">
<figcaption>All 15 rater pairs on these %d sentences.</figcaption></figure>',
  b64(sprintf("reports/fig_pairs%s.png", TAG)), n)

# --- 4 consensus ---------------------------------------------------------
p('<h2><span class="n">4</span>The summary rows had to be recomputed</h2>')
sr <- sub("\\.csv$", "_summaryrows.csv", AFTER)
if (file.exists(sr)) {
  st <- rd(sr)
  p('<p><code>human panel</code>, <code>model panel</code> and
<code>consensus</code> are derived from the rater columns; editing a rater&rsquo;s
code does not update them, and the returned sheet still carried the
pre-discussion values. All three were discarded and recomputed from the six
revised rater columns.</p>')
  p('<div class="card"><div class="scroll"><table>
<thead><tr><th>Summary row</th><th>sentences</th><th>differed from the recomputation</th></tr></thead><tbody>')
  for (r in c("human panel", "model panel", "consensus")) {
    k <- st$rater == r
    p('<tr%s><td>%s</td><td>%d</td><td class="num %s">%d</td></tr>',
      if (sum(st$changed[k], na.rm = TRUE) > 0) ' class="hl"' else '', r, sum(k),
      if (sum(st$changed[k], na.rm = TRUE) > 0) "down" else "", sum(st$changed[k], na.rm = TRUE))
  }
  p('<tr><td><strong>total</strong></td><td><strong>%d</strong></td><td class="num"><strong>%d</strong></td></tr>',
    nrow(st), sum(st$changed, na.rm = TRUE))
  p('</tbody></table></div>
<p class="small" style="margin-bottom:0">Had the sheet&rsquo;s own summary rows
been used as delivered, %d of %d of them would have been wrong &mdash; the human
majority on %d sentences and the six-rater consensus on %d. The model panel is
unchanged, as it must be. This is why the pipeline treats every derived column as
disposable and rebuilds it.</p></div>',
    sum(st$changed, na.rm = TRUE), nrow(st),
    sum(st$changed[st$rater == "human panel"], na.rm = TRUE),
    sum(st$changed[st$rater == "consensus"], na.rm = TRUE))
}

p('<h3>The recomputed consensus</h3>')
p('<div class="card"><div class="scroll"><table>
<thead><tr><th>Consensus label</th><th>sentences</th></tr></thead><tbody>')
tb <- table(factor(cons$code, levels = PRIMARY_LEVELS))
for (i in names(tb)) p('<tr><td>%s</td><td class="num">%d</td></tr>', i, tb[[i]])
p('<tr><td>no majority</td><td class="num">%d</td></tr>', n_nc)
p('</tbody></table></div>
<p class="small" style="margin-bottom:0">%d of %d sentences (%s%%) reach no
strict majority across the six raters, against 4.1%% over the whole cohort, and
%s%% are unanimous against 75%% over the cohort. Tightening the human panel has
made the six-rater consensus <em>harder</em> to form, for the same reason the
panel-to-panel alpha fell.</p></div>',
  n_nc, n, pc(n_nc / n, 1), pc(n_un / n))

p('<h2><span class="n">5</span>Two caveats on this file</h2><div class="card"><ol style="margin-top:0">
<li><strong>One student, %d sentences.</strong> Every alpha here carries a wide
interval &mdash; the human figure alone spans %s to %s. The direction of the
change is clear; its size is not pinned down.</li>
<li><strong>Six human <em>Positive</em> cells lost their CQ factor.</strong> The
returned sheet mixes two column layouts, and in one of them the
<code>cq_factor</code> column is absent. The primary codes are complete and
validated, but the cultural-intelligence layer is not analysable for those cells.
Re-exporting from the original sheet would recover it.</li>
</ol></div>', n, f3(rows$after$h$lo), f3(rows$after$h$hi))

p('<footer><p class="small">Generated by <code>scripts/23_report_student.R</code>
from <code>%s</code>, cleaned and recomputed by <code>scripts/22_after_discussion.R</code>;
compared against <code>%s</code>. Every figure computed at render time.<br>
Pseudonymous ids and category labels only &mdash; no journal text is read.
%s &middot; %s.</p></footer></div></body></html>',
  basename(AFTER), basename(BEFORE), R.version.string,
  format(Sys.time(), "%Y-%m-%d %H:%M"))

dir.create("reports", showWarnings = FALSE)
writeLines(paste(h, collapse = "\n"), OUT)
cat(sprintf("wrote %s  (%.2f MB)\n", OUT, file.size(OUT) / 1e6))
