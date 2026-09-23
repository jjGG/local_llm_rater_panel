# `setting/` — the measurement instrument, supplied locally

This folder holds the Cultural Intelligence Scale. **Its contents are not in
version control**, because the scale and its source paper are third-party
copyright and are not ours to redistribute. This README is the only committed
file here.

## What you need to provide

`ClassificationScheme.txt` — the 20 CQS items, one per line, **item code first**:

```
Metacognitive CQ
----------------
MC1  <wording of metacognitive item 1>
MC2  <wording of metacognitive item 2>
...

Cognitive CQ:
----------------
COG1 <wording of cognitive item 1>
...
```

Rules the parser follows (`parse_item_text` in [`R/codebook.R`](../R/codebook.R)):

- A line beginning with an item code — letters then digits, e.g. `MC1`, `COG6`,
  `BEH4` — starts a new item; the rest of the line is its wording.
- **Wrapped items are fine.** A following line continues the item *only while
  that item does not yet end in terminal punctuation*. That rule is what stops
  section headings and `-----` rulers from being glued onto the item above them,
  so you can paste the scale in more or less as distributed.
- Blank lines and rulers reset the parser.
- Codes are matched case-insensitively against
  [`config/codebook.yml`](../config/codebook.yml) and must agree with it exactly:
  `MC1`–`MC4`, `COG1`–`COG6`, `MOT1`–`MOT5`, `BEH1`–`BEH5`.

Anything else in the file — title, instructions, the response scale, factor
headings — is ignored.

## Why it is split this way

`config/codebook.yml` carries the item **codes**, the **factor** each belongs to,
and whether **this course administered it**. Those are structural facts about the
instrument that the analysis needs, and they are not the copyrighted expression.
The item **wording** lives only here.

The prompts genuinely need the wording, so **a missing or mismatched file is a
hard error, not a fallback**. Coding against bare item codes would produce
confident-looking labels from a model that had never seen the items — the worst
possible failure, because nothing downstream would look wrong.

Verify your file is being read correctly with:

```bash
Rscript -e 'source("R/codebook.R"); cb <- load_codebook("config/codebook.yml"); print(cb$cq_items[, c("code","factor","course_item")]); cat("all wording present:", all(nzchar(cb$cq_items$text)), "\n")'
```

You can also see the exact stage-2 prompt the models will receive, wording
included, with:

```bash
Rscript scripts/02_rate.R data/units_synthetic.csv --dry-run
```

## Also expected here (optional)

The source paper for the scale, as a PDF, for reference while coding. Also
gitignored.

## Citation

Cite the scale from its original publication in any work using this pipeline. The
repository deliberately does not reproduce it, so it carries no citation of
convenience — look it up and cite it properly.
