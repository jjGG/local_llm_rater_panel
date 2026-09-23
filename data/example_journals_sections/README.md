# Example journals — the real document layout

Five invented reflection journals in the layout the actual student documents use:
a heading per week, the journal's own questions bullet-marked, the student's
answer underneath.

**Every word here was written for this repository. No student wrote any of it**,
and nothing refers to the real course, its subject matter or its institutions.
That is why this folder is committable while `journals/` never is.

```bash
Rscript scripts/01_prepare_units.R --dir data/example_journals_sections \
        --out data/units_example_sections.csv --preview
Rscript scripts/02_rate.R data/units_example_sections.csv
Rscript scripts/07_alpha.R --label example_sections
```

| | |
|---|---|
| Students | 5 (`E01`–`E05`), one per discipline |
| Weeks | 1, 4, 7, 10 — two questions each |
| Answer paragraphs | 40 |
| Sentence units | 107 |
| Disciplines | architecture studio, marine field course, chamber ensemble, applied statistics, food product development |

Each student has an international element in their course, because the scheme
needs one to be about anything — but the subject matter is deliberately far from
the real course.

## What these are for

They exercise the thing that actually matters now: **most sentences must come
back `Not marked`.** Each week section carries only one or two sentences that are
genuinely about intercultural experience, buried in ordinary student
chatter — broken equipment, canteen food, the boat leaving at six, the workload.
A model that marks a large share of these has not applied the scope gate, and its
labels are then not measuring what the codebook says they measure.

They also deliberately include the hard cases:

- **Strong sentiment about nothing intercultural.** "The reading pack is
  enormous." "The canteen has stopped doing the good soup." "Heat the rehearsal
  room." All `Not marked`, however emphatic.
- **Generic self-description with no intercultural content.** "I like to sketch
  first and talk later." "Group work usually slows me down." First-person and
  about a stable trait, but not about intercultural work — so `Not marked`, not
  `Self awareness`.
- **Organisational description of the joint work.** "I now write a short agenda
  before every joint call." Logistics, not intercultural experience.
- **Very short answers.** "Yes." "Slowly." "Somewhat." Real journals contain
  these; `01_prepare_units.R` flags them as possible segmentation problems, and
  here they are genuine.
- **Change over time.** `E04` starts hostile to the joint element in week 1 and
  comes round by week 10. That is what the week dimension is for.

## Intended labels — one reading, NOT ground truth

Below is what these sentences were written to be. Treat it as a hypothesis to
argue with, not an answer key: several are genuinely arguable, and the two marked
**(?)** sit exactly on the `Positive` / `Self awareness` boundary that the human
raters have not settled either (`Questions2discuss.txt` item 0e). Where a model
disagrees, read its rationale before assuming it is wrong.

**33 of 107 sentences marked (31%).** That is higher than a real journal, because
these also have to exercise every code and a spread of CQ items.

| unit_id | code | CQ item | sentence (abbreviated) |
|---|---|---|---|
| `E01_w01_q01_s03` | Positive | MOT1 | curious about working with the partner school |
| `E01_w04_q01_s02` | Positive | COG3 † | their school treats a review as a debate |
| `E01_w04_q01_s03` | Positive | MOT1 | enjoyed it far more than expected |
| `E01_w04_q02_s02` | Self awareness | — | I am a blunt person, which probably lands differently |
| `E01_w07_q01_s02` | Positive | BEH1 | stopped using slang, half does not survive translation |
| `E01_w07_q02_s02` | Negative | — | the two groups never agreed what "public" meant |
| `E01_w10_q01_s02` | Positive | MOT2 | less nervous working with people trained differently |
| `E02_w01_q01_s03` | Positive | MOT1 | looking forward to sharing a room with the other group |
| `E02_w04_q01_s03` | Positive | COG3 † | the two labs teach different rules for a replicate |
| `E02_w07_q01_s01` **(?)** | Positive | BEH3 | ask people to repeat the plan back to me |
| `E02_w07_q01_s02` | Positive | MC1 | read the quieter members as uninterested |
| `E02_w10_q01_s02` | Self awareness | — | I assume everyone works the way I do |
| `E03_w01_q01_s03` | Positive | MOT1 | excited half the group is coming from abroad |
| `E03_w04_q01_s02` | Positive | COG5 | their tradition treats rubato differently |
| `E03_w04_q02_s02` | Negative | — | disagreement about how to behave in a rehearsal |
| `E03_w07_q01_s02` | Positive | BEH1 | stopped apologising, it read as lack of conviction |
| `E03_w07_q02_s01` | Positive | MC4 | ask what someone means rather than guessing |
| `E03_w10_q01_s02` | Self awareness | — | more rigid about rehearsal than I thought |
| `E03_w10_q01_s03` | Positive | MOT1 | would like to keep playing with two of them |
| `E04_w01_q01_s03` | Negative | — | do not see what the pairing adds |
| `E04_w04_q01_s02` | Negative | — | different notation cost us a session |
| `E04_w04_q01_s03` **(?)** | Positive | MC2 | write out what I mean, symbols are not universal |
| `E04_w07_q01_s02` | Positive | MC4 | checking against theirs caught two of my mistakes |
| `E04_w07_q01_s03` | Positive | COG1 | their legal framework for data sharing differs |
| `E04_w07_q02_s02` | Self awareness | — | I read a slow reply as disinterest |
| `E04_w10_q01_s02` | Positive | MC1 | forced me to explain myself to people without my assumptions |
| `E05_w01_q01_s03` | Positive | MOT1 | curious what they consider a normal breakfast |
| `E05_w04_q01_s02` | Positive | COG3 | "too salty" differs between the two countries |
| `E05_w04_q01_s03` | Negative | — | lost a week arguing past each other |
| `E05_w07_q01_s01` | Positive | BEH2 | let people finish, they work in their third language |
| `E05_w07_q01_s02` | Positive | MC1 | assuming everyone thinks in grams was arrogant |
| `E05_w10_q01_s02` | Self awareness | — | I assume my own habits are the default |
| `E05_w10_q01_s03` | Positive | MOT1 | would work with this team again tomorrow |

The table above is also available machine-readable as
[`expected_labels.csv`](expected_labels.csv), generated by
`tools/make_expected_labels.R`, which validates every id against the units file
and rolls each item up to its factor the same way the runner does. Regenerate it
rather than editing it, and use `scripts/08_vs_expected.R` to compare a run
against it.

Distribution: 23 `Positive`, 5 `Negative`, 5 `Self awareness`, 74 `Not marked`.

CQ items intended: MOT1 ×7, MOT2, COG1, COG3 ×3, COG5, MC1 ×3, MC2, MC4 ×2,
BEH1 ×2, BEH2, BEH3. By factor: Motivational 8, Metacognitive 6, Cognitive 5,
Behavioural 4.

`COG1` (legal and economic systems) and `COG5` (arts and crafts) are included on
purpose. They are items a course journal would almost never reach, and it is
worth knowing whether a model will use them when the text genuinely warrants it
or default to the familiar ones instead.

**† = no item really fits.** Both marked sentences are about differences in
*professional* convention — how a design review is run, what counts as a
replicate — and the Cultural Intelligence Scale has no item for that. `COG3`
(cultural values and beliefs) is the closest, which is not the same as correct.
Stage 2 forces a choice anyway, matching the human protocol, so these two are
useful evidence for `Questions2discuss.txt` item 0d: whether to keep the forced
choice or add an explicit "no item applies". Expect the models to disagree here,
and expect that disagreement to be about the *instrument*, not about the models.

## What the models actually did (2026-08-20)

Full run, `main` profile, temperature 0. `results/ratings_example_sections_main.csv`.

The run covered four models; `qwen3:14b` was retired on 2026-08-21 and the
**current panel is the other three**. Both readings are below, because the
comparison is what justified the retirement.

| panel | primary α | unanimous | CQ factor α | CQ item α | modal vs reference | ties |
|---|---|---|---|---|---|---|
| 4 models | 0.779 | 83% | 0.813 (76% unan.) | 0.600 (48%) | 97/101 | **6** |
| **3 models (current)** | 0.778 | **85%** | **0.864** (86%) | **0.704** (67%) | **102/106** | 1 |

Dropping the weakest stage-2 model left the primary α unchanged but raised the CQ
figures substantially, and removed the ties. Reproduce either with:

```bash
Rscript scripts/07_alpha.R --label example_sections --exclude qwen3:14b
Rscript scripts/08_vs_expected.R --label example_sections --exclude qwen3:14b
```

| model | exact vs reference | over-marked | missed | detection κ | choice | CQ factor | CQ item |
|---|---|---|---|---|---|---|---|
| DeepSeek-V4-Flash | 100/107 | **0**/74 | 6 | 0.862 | 96% | 89% | 68% |
| gpt-oss:20b | 100/107 | 3/74 | 3 | 0.869 | 97% | 95% | 76% |
| qwen3.6:latest | 98/106 | 4/74 | 4 | 0.821 | **100%** | 95% | **81%** |
| qwen3:14b | 97/106 | 1/74 | 7 | 0.811 | 96% | 76% | 47% |

**The scope gate holds.** 8 over-marks out of 296 opportunities. The residual
error is mostly the *opposite* failure — the models are conservative and miss
sentences the reference marks, which is the safer direction.

Agreement among the models, needing no reference (`scripts/07_alpha.R`): primary
α **0.779** [0.674–0.866], all four identical on 83% of units; CQ factor α
**0.813**, CQ item α 0.600. Nominal, MASI and Jaccard coincide exactly, as they
must with one label per sentence.

### Two things this set taught us about itself

**The reference leans on context the codebook says to ignore.** Three units drew
disagreement from 3 of the 4 models, and they are not model errors:

| unit | reference | models | why the models have a case |
|---|---|---|---|
| `E04_w07_q01_s02` | Positive | Negative / Not marked | opens "I still think it costs time, but…" — genuinely ambivalent |
| `E03_w10_q01_s02` | Self awareness | Not marked | "more rigid about how music ought to be rehearsed" — no intercultural content *in the sentence* |
| `E03_w04_q02_s02` | Negative | Not marked | the cultural cause is in the clause, but reads as ordinary interpersonal friction |

The pattern: the reference marked sentences whose intercultural content lives in
the *surrounding paragraph*, while the codebook says **code the sentence in front
of you, not the paragraph around it**. On those the models follow the written rule
more faithfully than the reference does. That tension is real and unresolved —
`Questions2discuss.txt` item 0e.

**An even panel cannot break ties** — which is why the panel is now three. With
four models, 6 of 107 units came back 2–2 with no majority answer at all:

```
E01_w07_q02_s02  Negative x2 | Not marked x2       "…never agreed on what the word public meant"
E02_w04_q01_s02  Negative x2 | Not marked x2       "We had a real argument about protocol on day three."
E02_w07_q01_s01  Positive x2 | Not marked x2       "…ask people to repeat the plan back to me"
E03_w01_q02_s02  Self awareness x2 | Not marked x2 "I follow rather than lead."
E03_w10_q01_s03  Positive x2 | Not marked x2       "…keep playing with two of them after this."
E04_w04_q01_s02  Negative x2 | Not marked x2       "…different notation … cost us an entire session"
```

Every one is a genuinely borderline sentence, so the ties are informative rather
than annoying: they are the panel pointing at where the codebook is undecided.
Needs either an odd-sized panel or a written tie-break rule.

## A caution about reusing these

The sentences are unusually *clean*: each marked one says one thing, and the
intercultural ones name the difference explicitly. Real journals are muddier, and
agreement measured here should be read as an **upper bound** on what to expect
from the real cohort, not as a prediction.
