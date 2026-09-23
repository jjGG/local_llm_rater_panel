# Example journals — fully invented, safe to publish

Five fabricated student reflection journals, 42 passages, for trying the pipeline
without any real data. **Every word here was written for this repository.** No
passage comes from a real student, no real course is described, and nothing
identifies any institution, city or programme.

Use this set to see how the coding scheme behaves before pointing the pipeline at
anything sensitive.

```bash
Rscript scripts/01_prepare_units.R --dir data/example_journals --out data/units_example.csv --preview
Rscript scripts/02_rate.R data/units_example.csv
Rscript scripts/07_alpha.R --label example
```

## What is in it

| File | Course | Arc across the ten weeks |
|---|---|---|
| `P01.txt` | human geography, with an international field course | hesitant → motivated |
| `P02.txt` | Latin epigraphy seminar, taught jointly with a partner institution | enthusiastic → disillusioned |
| `P03.txt` | architectural history studio, mixed-nationality groups | steady, positive |
| `P04.txt` | statistics for the social sciences, international group project | terse, ambivalent |
| `P05.txt` | chamber ensemble with visiting players | reflective, one unresolved conflict |

Five different disciplines on purpose: the coding scheme is about intercultural
competence and collaboration, not about any one subject, and a set spread across
geography, classics, architecture, statistics and music demonstrates that.

Each journal covers weeks 1, 4, 7 and 10 with two or three passages per week.

## Why the courses all involve group work

The scheme codes intercultural competence, **including teamwork and
collaboration**. A journal purely about Latin grammar would return `No comment`
for every passage and exercise nothing. So each fictional course has
mixed-nationality group work — which keeps all four codes reachable while staying
completely unrelated to any particular field.

## Format

One file per student, named after the student id. Paragraphs separated by blank
lines, each carrying a numbered week marker:

```
w1.1: first passage to be coded ...

w1.2: second passage ...

w4.1: a passage from week four ...
```

The `.1` is a paragraph number, and it matters: without it, unit ids come from
paragraph *order*, so inserting one passage renumbers everything after it and
invalidates cached model calls. With explicit numbers the ids survive editing.

## What it exercises

Deliberately, not incidentally:

- **All four primary codes**, in reasonably balanced proportions — roughly 16
  positive, 10 negative, 13 self-awareness and 12 no-comment passages depending
  on the rater.
- **Multi-label passages** — several carry both `Positive` and `Self awareness`,
  and one carries both `Positive` and `Negative`. Observed mean is about 1.2
  codes per passage.
- **All four cultural-intelligence factors** as self-awareness statements,
  including the two that are self-referential by definition (Behavioural,
  Metacognitive) and are easy to make unreachable with a careless decision rule.
- **The static-vs-adaptive boundary** — passages describing a fixed trait sit
  next to passages describing adaptation during an interaction, which is the
  hardest distinction in the scheme.
- **Off-topic passages** that should be `No comment` — pure course content, and
  complaints about timetabling that are negative in tone but not about
  intercultural competence.
- **Two splitter edge cases**: a passage ending in a question mark (which a naive
  parser mistakes for the course's own question prompt) and passages of only 16
  characters (which a minimum-length filter would silently discard).

## There is no answer key, on purpose

Expected codes are not shipped with this set. Any key would be one person's
judgement, and tuning prompt wording against it would encode that judgement
rather than a shared scheme. If you want a gold standard, have several coders
label these passages independently and keep only the unanimous ones.

The one place this repository does use expected labels —
`data/units_synthetic.csv`, the 14-passage prompt regression test — carries that
caveat too.

## Do not put real journals in this folder

This directory is deliberately **not** excluded from version control, because its
contents are invented. Real journal text belongs in `journals/`, which is
excluded and additionally blocked by the pre-commit hook. Putting real passages
here would commit them.
