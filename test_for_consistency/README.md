# Inter-rater reliability for the COIL journal coding scheme

Krippendorff's alpha, Cohen's kappa, Fleiss' kappa and Gwet's AC1 for the human
coder alignment of the COIL "Omics in Oncology" reflection journals
(UU Utrecht / UZH Zurich).

This is the statistics half of the project. It is deliberately standalone and
dependency-free so that the same coefficients can later be applied to local-LLM
ratings by treating each model as an additional rater.

## Contents

| Path | What it is |
|---|---|
| `R/agreement.R` | All coefficients, base R only, no packages required |
| `R/io.R` | Reads and normalises the wide alignment sheet; codebook vocabularies |
| `validate_agreement.R` | 21 checks against analytic identities, hand-computed values and CRAN packages |
| `reliability.R` | The analysis; writes tables, figures and a report to `results/` |
| `260728 Coding alignment Omics in Oncology.csv` | Rater alignment sheet, 2026-07-28 meeting |

## Data protection

The alignment sheet holds **pseudonymous student IDs and rater labels only — no
journal text**, so it is safe to commit and share with collaborators. The
journals themselves are personal data under FADP/GDPR (see the top-level README)
and must never enter this folder or any cloud service.

## Running it

```bash
Rscript validate_agreement.R && Rscript reliability.R
```

Validate first — `reliability.R` is only as trustworthy as the coefficients
behind it. Optional cross-check packages: `install.packages(c("irr","irrCAC"))`.
Figures need `ggplot2`; everything else is base R.

To point the analysis at a different sheet:

```bash
Rscript reliability.R path/to/another_alignment.csv
```

## Input format

One row per coding unit, one column pair per rater:

```
ID, Serena, scqType, Thomas, tcqType, Jonas, jcqType, Week
```

- Primary code column is named after the rater; the CQ-type column is
  `<first letter lowercased>cqType`.
- CQ type is filled only when the primary code is `Positive`.
- **Rows are aligned across raters by position.** The sheet carries no unit
  identifier, so ids are generated as `<ID>_w<week>_u<n>` from row order within
  (ID, Week). Never re-sort the sheet, or the raters stop lining up.
- Trailing empty spreadsheet rows and a UTF-8 BOM are handled automatically.
- Spelling variants are mapped via `LABEL_ALIASES` in `R/io.R`; an unrecognised
  label is a hard error, so a typo can never silently become a new category.

## The six analyses

A single headline number would hide the structure of the disagreement, so
`reliability.R` reports:

| | Analysis | Question it answers |
|---|---|---|
| **A** | Primary code, 4 categories, all units | The headline figure |
| **B** | Detection: relevant vs `Not marked` | Did the raters flag the same passages? |
| **C** | Valence: Positive / Negative / Self awareness | Given all flagged it, do they read it the same way? |
| **D** | CQ type, every unit a rater coded `Positive` | Alpha handles the missing cells natively |
| **E** | CQ type, units *all* raters coded `Positive` | Strictest comparison, smallest n |
| **F** | Primary code by journal week | Descriptive only; n per week is far too small |

`disagreements.csv` lists every non-unanimous unit with its `kind`
(`detection`, `valence`, `cq_type`) — a worklist for the next alignment meeting.

## Reading the output

- Compare **A** against **B** and **C**. High B with low C means the raters find
  the same passages but interpret them differently; low B means the
  disagreement is about what counts as relevant at all.
- **AC1 is reported next to alpha on purpose.** The category distribution is
  skewed, and where alpha sits far below AC1 it is prevalence depressing alpha
  rather than the coders being unreliable (the "kappa paradox").
- Confidence intervals are percentile bootstrap over coding *units* (units are
  the independent observations; raters are fixed by design). Krippendorff's own
  bootstrap resamples pairable values instead — state which you used.
- Conventional thresholds: 0.80 as the customary target, 0.667 as the floor for
  tentative conclusions. Both are conventions, not laws — quote them as such.

## Two implementation gotchas worth knowing

**Do not use `irr::kripp.alpha` with 3+ raters and no missing cells.** Its
internal coincidence matrix weights each unit by `1` instead of
`(raters - 1)` in the complete-data branch, which scales the matrix by
`(m - 1)` and degrades Krippendorff's `(n - 1)` correction to `(n - 1/(m - 1))`.
On analysis A that biases alpha low by about 0.006 (0.6010763 instead of
0.6036500). The bug is silent with 2 raters and whenever any `NA` is present,
so it is easy to miss — and it hits exactly the complete 3-rater case the
primary-code analysis uses. `irrCAC::krippen.alpha.raw` agrees with
`R/agreement.R` exactly; section 5 of `validate_agreement.R` pins the deviation
down as a regression test.

**Gwet's AC1 depends on how many categories you declare,** because its chance
term is `sum_k pi_k (1 - pi_k) / (q - 1)`. Always pass the full codebook, since
that is what the raters chose from, even if a category goes unused in a small
sample. `irrCAC` defaults to the observed categories instead — pass its
`categ.labels` argument to compare like with like. Krippendorff's alpha and the
kappas are unaffected by unused categories.

## Next step

The same `R/agreement.R` will be reused for the local-LLM arm of the study: each
model (and each replicate of a model, for self-consistency) enters as an extra
column in the ratings matrix, so model-vs-human and model-vs-model reliability
use exactly the coefficients validated here.
