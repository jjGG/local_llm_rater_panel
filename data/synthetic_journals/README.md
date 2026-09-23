# Synthetic journals — invented text, safe to share

`S01.txt` … `S05.txt` are **entirely fabricated**. No sentence comes from a real
student journal. They exist so the pipeline, the codebook and the human coding
protocol can be exercised, demonstrated and published without touching personal
data.

Because they are invented they are **committed to the repository on purpose**,
unlike everything under `test_journal/` or `data/units_*.csv`. Do not add real
journal text to this folder — the safety rules that protect the real journals do
not cover this path.

Student ids `S01`–`S05` are deliberately unlike the real pseudonymous ids
(`99`, `94`, `00`, `01`, `11`) so the two can never be confused in a results
table.

## What they are designed to contain

45 paragraphs across weeks 1, 4, 7 and 10, two to three per week per student
(S01 10, S02 9, S03 9, S04 8, S05 9). Between them they cover:

- **All four primary codes**, including passages that are purely about course
  content (`No comment`) and one deliberate abstention case.
- **Multi-label passages** — several paragraphs carry both `Positive` and
  `Self awareness`, and at least one carries both `Positive` and `Negative`.
  `S05 w1.1` is the long mixed case, the synthetic analogue of the 694-character
  paragraph that made the five models split 4:1.
- **All four CQ factors as self-awareness statements**, since v3 assigns the
  factor on `Self awareness`: knowledge of one's own or another culture
  (Cognitive), disposition towards intercultural contact (Motivational),
  awareness of one's own assumptions (Metacognitive), and how one speaks or acts
  (Behavioural).
- **The static-versus-adaptive boundary**, which is where the human raters
  disagreed most. `S03 w4.2` ("I automatically become more formal in English")
  is a stable trait; `S01 w4.2` and `S05 w4.1` are the same territory but
  described as noticing and adjusting in the moment.
- **Trajectories**, because the real study selected students who grew or
  declined: S01 goes hesitant → motivated, S02 enthusiastic → disillusioned,
  S03 is steadily positive, S04 and S05 are mixed.
- **Two splitter edge cases**: `S04 w1.1` ends in a question mark and `S03 w7.2`
  and `S04 w10.1` are very short. Earlier versions of the splitter silently
  discarded exactly this kind of paragraph.

## No answer key, deliberately

There is no reference coding shipped with these files, and that is the point.
An earlier fixture used one person's expectations and the models kept
"failing" on passages where a second code was perfectly defensible — the
expectations were wrong, not the models.

So code them the way the pilot was coded: the three raters independently,
blind. Passages all three agree on become the reference set; the rest are the
interesting ones to discuss. See `Questions2discuss.txt` item 5f.

## Using them

```bash
Rscript scripts/01_prepare_units.R --dir data/synthetic_journals \
        --out data/units_synth5.csv --preview
Rscript scripts/02_rate.R data/units_synth5.csv
```
