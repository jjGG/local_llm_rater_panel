# Runbook — rating a set of journals end to end

Copy-paste commands for a full run on your own, with what to check at each step
and what to do when something fails. Every script also documents itself:

```bash
head -40 scripts/02_rate.R
```

---

## 0. One-time setup

```bash
cd /path/to/project
Rscript scripts/00_setup.R
```

Installs any missing R packages, validates the codebook and prompts, checks
every model server, and installs the git pre-commit hook.

**Expect to see** `setup complete.` and a line per provider ending `ok (...)`.

If Ollama is not running: `ollama serve` in another terminal.
The FGCZ provider only resolves inside the FGCZ network — see step 4.

---

## 1. Prepare the units

One plain-text file per student in a folder — the **complete journal** — named
after the pseudonymous student id (`99.txt` → `student_id` `99`). The real
documents have a heading per week and bullet-marked questions:

```
Week 1
- What did you expect from the collaboration?
The student's answer. It may run over several sentences.

- How did the first joint session go?
Another answer.

Week 4
- ...
```

**Every sentence of an answer becomes one unit** and gets one class label. Week
headings and the questions themselves are never rated — they set the time
dimension and group the sentences. Text before the first week heading (a title, a
name) is dropped as front matter.

Add `--paragraphs` for one unit per paragraph, the way the 2026-07-28 human pilot
was coded. The older `w1.1:` inline-marker layout is still read as well; the
script reports which layout it detected, and `--format sections|marker` forces
one if the detection is ever wrong.

**Try it on the invented set first.** `data/example_journals_sections/` is five
journals in exactly this layout, entirely made up, with the intended labels
written down in its README:

```bash
Rscript scripts/01_prepare_units.R --dir data/example_journals_sections \
        --out data/units_example_sections.csv --preview
```

If the real documents come from Word, export to plain text (`.txt`, UTF-8) and
check that the bullets survived. They do not have to be `•` — a hyphen, an asterisk or
Word's `o` all work — but if a question line comes through with **no** marker at
all it will only be recognised by its trailing `?`, and the script says how many
times that happened so you can check none of those was really a student sentence.

```bash
Rscript scripts/01_prepare_units.R --dir journals \
        --out data/units_journals.csv --preview
```

**Check three things in the output:**

| Line | What it means |
|---|---|
| `[sections layout]` / `[marker layout]` | the layout detected. If it is not the one you expect, force it with `--format` |
| `N block(s) NOT turned into units` | week headings and questions belong here; **a student's answer does not** |
| `questions found` | one line per question, per week. Check the count matches the documents — a missing question means its heading or bullet was not recognised |
| `units per student x week` | must cover weeks 1, 4, 7 and 10. A missing week is flagged, and means a heading was misread |
| `N question(s) had NO bullet` | recognised only by the trailing `?`. Verify none is really a student sentence |
| `M paragraph(s) had no week marker` | `marker` layout only — those inherited the previous paragraph's week. Should normally be 0 |
| `N paragraph(s) -> M sentence unit(s)` | the split. Roughly 3–6 sentences per paragraph is normal |
| `N sentence(s) look like a segmentation problem` | **read every one of these** |

Also check the `units per student x week` table matches what you put in, and
that the unit count is what you expect. Nothing is dropped silently, but
confirm the totals anyway.

> **This is the moment to check the segmentation, and the only cheap one.**
> Sentences over 400 characters usually mean a missing full stop; under 15
> characters, an abbreviation missing from `SENTENCE_ABBREV` in `R/units.R`. Add
> the abbreviation or fix the journal text now — re-splitting after a run changes
> every `unit_id`, which throws away the cache and breaks every join. Use
> `--preview` to read all the sentences.

---

## 2. Rate

Optional sanity check first — prints the exact prompt the models will receive
and stops without calling anything:

```bash
Rscript scripts/02_rate.R data/units_journals.csv --dry-run
```

Then the real run:

```bash
Rscript scripts/02_rate.R data/units_journals.csv
```

Outputs are named from the units file, so different datasets never overwrite
each other. `data/units_journals.csv` → `results/ratings_journals_main.csv`.
Override with `--label mylabel` if you want.

**Runtime.** The panel is three models: `qwen3.6:latest`, `gpt-oss:20b`,
`DeepSeek-V4-Flash-DSpark`. Stage 2 only fires on `Positive` sentences, so the
total depends on how much of the journal is intercultural.

Measured per-unit medians on sentence units with paragraph context (2026-08-20):

| model | median per unit |
|---|---|
| `DeepSeek-V4-Flash-DSpark` | 1.1 s |
| `gpt-oss:20b` | 3.2 s |
| `qwen3.6:latest` | 18.0 s |

So roughly **22 s per sentence for the whole panel** — about **40 minutes per 100
sentences**, plus stalls. The full 107-unit example run took 121 min, but that
included four models and two 300 s timeouts.

Always time a `--limit 20` run first and read the *median*, not the mean: one
stall distorts the mean badly enough to make the estimate useless. `02_rate.R`
prints total wall clock, and the per-unit `secs` column is in the ratings file.

**Interrupting is safe.** Every successful call is cached, so re-running the
same command resumes instead of restarting. Failed calls are deliberately not
cached, so they are retried.

**What throws the cache away.** The key is the exact prompt text plus the model
and sampling options, which is what makes it trustworthy — and also means any of
these forces a full re-run:

| Change | Effect |
|---|---|
| Editing `config/codebook.yml` — even adding one decision rule | every stage-1 call re-runs |
| Changing `context_mode` | every call re-runs |
| Editing a file in `prompts/` | the calls for that stage re-run |
| Re-splitting the journals | new `unit_id`s **and** new text, so nothing matches |
| Adding a model | only the new model runs |
| A different `--profile` | different temperature and seed, so no reuse from `main` |

None of this is a bug to work around; it is why a cached result can be trusted to
correspond to the prompt that produced it. But it does mean codebook edits and a
long run should not be interleaved. Settle the codebook, run the smoke test, then
start the cohort.

Useful flags:

```bash
--limit 20                      # first 20 units only, to time the full run
--models qwen3.6:latest         # one model, comma-separate for several
--refresh                       # ignore the cache and re-ask everything
--profile consistency           # 5 replicates at temperature 0.7 (see below)
--allow-unreachable             # proceed on cached answers when FGCZ is off-network
```

**Check at the end:**

- **Did every model answer every unit?** Look at the failure count *per model*,
  not just the total. A model that fails on *all* units has not been rated at
  all — it has silently dropped out of the panel, and every agreement figure
  computed afterwards is over the remaining models. `02_rate.R` prints the
  code distribution per model, so an all-`NA` row is the tell.
- **The `Not marked` share.** This is the number to look at first. Most sentences
  in a journal are not about intercultural experience, so a model marking a large
  fraction of them has not applied the scope gate — and its labels are then not
  measuring what the codebook says. The per-model code distribution is printed
  automatically.
- `N of 20 items were never used` — expected. Several CQS items describe things
  this course cannot produce. Worth reporting, not worth fixing.
- `N call(s) FAILED` — re-run the same command; successes are cached so only
  the failures are retried. A persistent failure is a real problem, not noise.
- `N answer(s) repaired` — the model's answer had to be coerced to satisfy the
  codebook. A handful is normal; a lot means the prompt needs work.
- `confidence outside [0,1]` — that model's confidence is unusable.

---

## 3. Agreement among the models — THE analysis step

**Decided 2026-08-20: this run is LLM-only.** There is no human comparison in the
current workflow, so step 3 is `07_alpha.R` and nothing depends on human labels.
Step 3b below is kept for when human data is generated again.

To ask "how well do the models agree with each
other", which you can do on any run including synthetic ones:

```bash
Rscript scripts/07_alpha.R --label journals
Rscript scripts/07_alpha.R --ratings results/bak/ratings_synth5_main.csv
Rscript scripts/07_alpha.R --label journals --metric jaccard
Rscript scripts/07_alpha.R --label journals --by rater_id   # replicates as raters
```

Reports Krippendorff's alpha under all three difference functions (nominal /
MASI / Jaccard) with bootstrap CIs, then per-code alpha, every rater pair,
leave-one-rater-out, breakdowns by student and week, and the CQ factor with each
rater's factor shares. Writes `results/alpha_*.csv`.

Read it in this order:

1. **Headline alpha.** With one label per sentence every rating is a singleton
   set, so `nominal`, `masi` and `jaccard` coincide — pick `nominal` and say so.
   The three only diverge for paragraph-level (`--paragraphs`) multi-label runs.
2. **Per code** — says *which* code the disagreement is about. Where alpha sits
   far below AC1, a skewed code is deflating alpha rather than the raters being
   unreliable, which is the expected pattern here given how many sentences are
   `Not marked`.
3. **Leave-one-out** — a large positive `delta_vs_all` means agreement improves
   without that model. Small deltas (±0.02) mean none is an outlier. With only
   four models, dropping one is a bigger decision than it was with six; do it
   only with a reason you can write down.
4. **CQ factor shares, then items** — if one factor dominates every model, a
   decent alpha can reflect a shared *default* rather than genuine
   discrimination. Read alpha together with the shares, and read the item-level
   figure with its interval, never as a point estimate.

To view every model's label for every sentence:

```bash
Rscript scripts/06_show_units.R --label journals --only-disagreed
```

### What an LLM-only design can and cannot claim

This matters for how the results are written up, so it is here rather than in a
footnote. Without human labels there is **no accuracy or validity measure** —
only reliability. Concretely:

- **You can say:** the models agree with each other to this degree; each model
  agrees with itself to this degree (`--profile consistency`); agreement is this
  much better/worse per code; the panel's modal answer is more stable than any
  single run.
- **You cannot say:** the models are right. Four models agreeing can equally mean
  four models sharing a bias — they have overlapping training data and are all
  reading the same prompt. High inter-model alpha is *consistency*, not
  *correctness*, and the paper has to be explicit about that.
- **The self-consistency profile therefore matters more, not less**, in an
  LLM-only design: it is the ceiling on everything else and the one number that
  needs no external reference.

The route back to a validity claim is a human sample later — which is why
`para_id`, `question_no` and `week` are kept on every row (see step 3b).

---

## 3b. Comparing against human coding — DEFERRED

Not part of the current workflow. Kept working, and kept possible, for when human
data is generated again.

```bash
cd test_for_consistency && Rscript validate_agreement.R && Rscript reliability.R && cd ..
Rscript scripts/03_compare.R --label journals
```

`validate_agreement.R` must print `36 passed, 0 failed` before you trust any
number from that module. It is worth running on its own occasionally regardless —
it is pure base R, takes seconds, and it is what makes every coefficient in the
repository trustworthy.

`03_compare.R` will **stop** with `no shared unit_id` against the 2026-07-28
sheet, and that is correct: the sheet is paragraph-level, current runs are
sentence-level, and the id shapes differ on purpose so nothing joins by accident.
It is not something to work around.

**What keeps the door open.** Nothing needs to be done now, but do not remove
these, because they are what a future human round will need:

| Kept | Why it matters later |
|---|---|
| `para_id` on every row | lets model sentences be aggregated up to the paragraph the humans coded |
| `question_no`, `week` | lets a human sample be drawn stratified by week and question |
| `--paragraphs` mode | reproduces the pilot's unit definition exactly |
| the frozen units file for each run | the numbered sentence list a human would code against |
| `test_for_consistency/` untouched | the validated coefficients and the pilot analysis |

When the time comes, the cheapest useful design is: draw a stratified sample of
sentences from the frozen units file, have the raters code that sample blind
against the same codebook, and compare on those sentences only. That gives a
validity estimate without re-coding everything, and because the raters would be
working from the written decision rules it also removes the protocol asymmetry
where the model follows a more specified protocol than the humans did.

---

## Asking about one sentence, right now

For "we were wondering how the models would code *this*". Edit one file, run one
command:

```bash
Rscript scripts/09_ask.R
```

The first run creates `ask.txt` with instructions and an example in it. Put your
sentence or paragraph there, run again, and you get every model's code side by
side with a consensus line and each model's reasoning. Or skip the file:

```bash
Rscript scripts/09_ask.R --text "I really enjoyed working with the other group."
```

It uses the same codebook, prompts, splitter, two-stage protocol and cache as a
real run, so the answer is the answer a real run would give. A **paragraph is
split into sentences** and each is coded separately, with the paragraph supplied
as context — because the sentence is the unit. `--whole` overrides that.

Useful flags: `--quiet` for just the table, `--models a,b` to ask fewer models.
Budget about **15 s per sentence per model**, so a four-sentence paragraph across
the panel takes a few minutes; drop `qwen3.6` for a fast answer.

Nothing is written to `results/` — it is a scratch probe, not a dataset. Answers
do land in the per-call cache, so asking the same thing twice is instant.

> `ask.txt` is gitignored **and** blocked by the pre-commit hook, because the
> obvious thing to paste into it is a real student sentence. Every model called
> runs on hardware we control, so the text never leaves the premises.
>
> **The remaining risk is the terminal, not the script.** With real journal text,
> run this in your own shell — do not ask a cloud AI assistant to run it for you,
> and do not paste the text or its output into a chat with one. That route is
> outside anything this repository can enforce. Invented text is unrestricted.

---

## 4. The FGCZ DeepSeek model

```bash
Rscript scripts/05_check_provider.R fgcz_vllm
```

Confirms the endpoint resolves, the model is served, and one real constrained
call works. Uses invented text, so it sends no student data.

- Reachable only from inside the FGCZ network.
- Uses **port 8000** (raw vLLM), not 8080 (Open WebUI).
- Runs in `prompt` mode because that vLLM build's schema enforcement is broken.
  If FGCZ upgrades the server, retry with `structured_output: schema` in
  `config/run.yml` — that would remove a methodological asymmetry.

To run without it (e.g. off-network), set `enabled: false` for that model in
`config/run.yml`. Otherwise preflight aborts the whole run.

---

## 5. Self-consistency

Whether a model agrees with *itself*. Five replicates at temperature 0.7 with
rotated label order — so five times the runtime, and no cache reuse from the
`main` run, because a different temperature and seed make different cache keys.
Time a `--limit 20` run and multiply: at sentence level the unit count is several
times higher than it was for paragraphs, so this profile is an overnight job.

```bash
Rscript scripts/02_rate.R data/units_journals.csv --profile consistency
Rscript scripts/07_alpha.R --label journals --profile consistency
```

**Use `07_alpha.R` here, not `03_compare.R`.** `03_compare.R` measures models
against *human* labels and will refuse if the units have none — self-consistency
is a question about the models alone.

`07_alpha.R` then reports, per model, the alpha across its own five replicates.
Two things to take from it:

- **Self-consistency is the ceiling on everything else.** A model cannot agree
  with anyone more reliably than it agrees with itself. If a model's
  self-consistency is below the between-model alpha, single runs of it are
  noise and you should use its modal answer or drop it.
- With replicates present, `07_alpha.R --by model` collapses each model to its
  **modal** code set before comparing models, so between-model figures are not
  polluted by run-to-run noise. Pass `--by rater_id` to treat all 30
  model×replicate combinations as separate raters instead.

---

## Troubleshooting

| Message | Cause and fix |
|---|---|
| `cannot reach provider 'local_ollama'` | Ollama not running → `ollama serve` |
| `cannot reach provider 'fgcz_vllm'` | Off the FGCZ network, or the host moved. Verify with `curl http://<vllm-host>:8000/v1/models`, or disable that model |
| `provider ... does not serve: X` | Model id typo. Ids are case-sensitive; the error lists what is served |
| `units file ... is missing column(s)` | Use `data/units_TEMPLATE.csv` as the reference, or build from `.txt` with step 1 |
| `no units found in <file>` | No blank lines between paragraphs, so the whole file read as one block |
| `unrecognised primary code: "..."` | A label in a hand-edited file is not in the codebook. Add a spelling variant to `aliases` in `config/codebook.yml` |
| `Scanner error` / `Parser error` from YAML | You edited `config/codebook.yml`. Two traps: never put `": "` inside an unquoted line, and never start a `- ` list item with a double quote |
| `no shared unit_id` | Units file and alignment sheet disagree — see step 3. At sentence level this is expected: the pilot is paragraph-level |
| `duplicate week.number marker(s)` | Two paragraphs share e.g. `w1.2:` in one file |
| A "sentence" holds two sentences | A missing full stop in the journal, or a `?`/`!` followed by a lower-case word. Fix the journal text |
| A sentence is cut mid-way | An abbreviation the splitter does not know. Add it to `SENTENCE_ABBREV` in `R/units.R`, then re-prepare |
| `context_mode='unit_context' needs a populated 'context' column` | The units file was built by hand, or with `--paragraphs`. Either add the column or set `context_mode: unit` |
| Every sentence comes back `Positive` | The scope gate is not landing. Run `scripts/04_smoke_test.R` — that is exactly what it tests — and read the rationales in `cache/smoke_test_with_rationales.csv` |
| **One model fails on EVERY unit, fast, with an empty response** | It could not be loaded into RAM. The local models are ~9, ~13 and ~22 GB and do not all fit in 36 GB, and Ollama keeps each warm for five minutes. `R/rate.R` unloads each model before the next, so this should not recur — verify with `ollama ps` mid-run that only one model is resident. Re-run to fill in the gap; failures are never cached |
| One unit takes minutes while the rest take seconds | A hung generation. It fails at `timeout_s` and is **not** retried, so it costs one timeout rather than three |
| `Timeout was reached ... after 300002 milliseconds` | The same thing, reported. Not a configuration error |
| The **same** unit hangs every time | Some hangs are deterministic. At temperature 0 the input is identical on every run, so `qwen3:14b` hangs on `E01_w01_q01_s03` of the example set every single time — re-running cannot help. Leave it: Krippendorff's alpha handles missing cells natively and `07_alpha.R` reports `n_units_pairable`, so one absent rating costs a little precision and nothing else. A `consistency` run at temperature 0.7 will usually get past it |
| Run is very slow | Normal. `qwen3.6:latest` is ~43 s/call and `gpt-oss:20b` ~30 s. Use `--models` to narrow, or interrupt and resume later |

---

## After changing the codebook

Editing `config/codebook.yml` changes the prompts, which changes the cache
keys, so the next run re-asks every model. Always re-run the prompt regression
test afterwards:

```bash
Rscript scripts/04_smoke_test.R --models all
```

It codes 14 invented passages with known expectations and reports per model.
A miss usually means the *wording* licensed the wrong answer — read the
rationale in `cache/smoke_test_with_rationales.csv` before changing the
expectation. Bump `version:` in the codebook when you change definitions; it is
recorded in every run manifest.

---

## What must never be committed

The pre-commit hook blocks these, but know why:

- journal text in any form — `*.docx`, `journals/`, `test_journal/`,
  `data/units_*.csv` (real ones)
- `cache/` — holds model rationales and reasoning traces, which quote passages

`results/` **is** committable: pseudonymous ids and labels only, no text.
`data/synthetic_journals/` is committable because it is invented.

Check before committing:

```bash
git status --short
```
