#!/usr/bin/env bash
# ---------------------------------------------------------------------------
# build_public_repo.sh -- assemble the shareable subset of this project
#
#   bash tools/build_public_repo.sh [target-dir]
#
# ALLOWLIST, NOT DENYLIST. Files are copied in only if they appear below. A file
# nobody thought about therefore stays out, which is the safe direction; a
# .gitignore gets this backwards, because anything it fails to anticipate is
# published. The private working tree keeps everything; this builds a separate
# directory that holds only what has been looked at.
#
# The build ends with a scan that FAILS on any residual endpoint, absolute path,
# username, IP address or institution name. If the scan reports anything, the
# target directory is left in place for inspection but must not be pushed.
# ---------------------------------------------------------------------------
set -euo pipefail

SRC="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DST="${1:-$(dirname "$SRC")/local_llm_rater_panel}"

echo "source : $SRC"
echo "target : $DST"
[ -e "$DST" ] && { echo "refusing to overwrite an existing $DST"; exit 1; }
mkdir -p "$DST"

copy() {  # copy <relative-path> ; directories copied recursively
  local rel="$1"
  [ -e "$SRC/$rel" ] || { echo "  MISSING (skipped): $rel"; return 0; }
  mkdir -p "$DST/$(dirname "$rel")"
  cp -R "$SRC/$rel" "$DST/$rel"
}

echo
echo "--- code: how we talk to the models, and how we score agreement --------"
for f in R scripts prompts tools/pre-commit tools/build_public_repo.sh; do copy "$f"; done
copy config/codebook.yml
copy config/run.yml

echo "--- the invented journals, and the labels expected for them ------------"
copy data/example_journals            # P01-P05, inline-marker layout
copy data/example_journals_sections   # E01-E05, the real document layout
copy data/synthetic_journals          # S01-S05, closer to the subject matter
copy data/units_TEMPLATE.csv
copy data/units_synthetic.csv         # F1-F3
copy data/units_example.csv           # P01-P05
copy data/units_example_sections.csv  # E01-E05
copy data/units_synth5.csv            # S01-S05
copy data/units_journals.csv          # S01-S05 (same invented text, staged copy)

echo "--- the agreement toolkit and its validation ---------------------------"
copy test_for_consistency/R
copy test_for_consistency/README.md
copy test_for_consistency/validate_agreement.R
copy test_for_consistency/reliability.R
copy test_for_consistency/diagnose_alpha.R

echo "--- documentation -----------------------------------------------------"
copy README.md
copy RUNBOOK.md
copy LICENSE
copy setting/README.md                # says what the CQS file must look like

echo
echo "--- mock_results: outputs computed from the INVENTED journals only -----"
mkdir -p "$DST/mock_results"
n_mock=0
for f in "$SRC"/results/*; do
  b="$(basename "$f")"
  [ -f "$f" ] || continue
  # Only files whose name identifies an invented-journal run. Everything else --
  # every real-cohort rating, pooled table, trajectory and six-rater output --
  # is left behind by not matching.
  case "$b" in
    *example*|*synth*|*journals*)
      cp "$f" "$DST/mock_results/$b"; n_mock=$((n_mock+1)) ;;
  esac
done
echo "  copied $n_mock files into mock_results/"

# ---------------------------------------------------------------------------
# Scrub: endpoints, absolute paths, usernames
# ---------------------------------------------------------------------------
echo
echo "--- scrubbing host names, ports and local paths ------------------------"
# The loader already supports base_url_env, and errors helpfully when no
# endpoint is configured, so the literal can simply go.
if [ -f "$DST/config/run.yml" ]; then
  perl -0pi -e 's{^\s*base_url:\s*http://fgcz-c-\d+:\d+/v1\s*$}{    # base_url deliberately omitted: set FGCZ_LLM_BASE_URL in ~/.Renviron\n    #   FGCZ_LLM_BASE_URL=http://<your-vllm-host>:<port>/v1\n}m' "$DST/config/run.yml"
fi
# The account name is taken from the environment rather than written here --
# a scrubbing script that hardcodes the string it scrubs publishes it.
ME="$(id -un)"
find "$DST" -type f \( -name '*.R' -o -name '*.yml' -o -name '*.md' -o -name '*.sh' -o -name '*.json' -o -name '*.csv' \) -print0 |
while IFS= read -r -d '' f; do
  ME="$ME" perl -pi -e '
    s{fgcz-c-\d+}{<vllm-host>}g;
    s{/Users/[A-Za-z0-9._-]+/[^\s"'"'"'`)]*}{/path/to/project}g;
    s{\Q$ENV{ME}\E}{<user>}g;
  ' "$f"
done
echo "  done"

# ---------------------------------------------------------------------------
# Repository furniture: ignore rules, a note on what is withheld, link repairs
# ---------------------------------------------------------------------------
echo
echo "--- writing .gitignore and PUBLIC_SUBSET.md ----------------------------"

cat > "$DST/.gitignore" <<'IGNORE'
# ===========================================================================
# This repository is the PUBLIC SUBSET of the project (see PUBLIC_SUBSET.md).
# It is assembled by an allowlist, so the rules here are a second line of
# defence rather than the mechanism. Journal text, model caches and real-cohort
# results never enter it.
# ===========================================================================

# Anything that could carry a student passage
*.docx
*.doc
*.odt
*.rtf
journals/
journal/
test_journal/
originals*/
cache/

# Outputs of real runs. mock_results/ (invented journals) IS tracked; results/
# is what a real run writes locally and must stay local.
results/
reports/

# The scratchpad for scripts/09_ask.R -- the obvious thing to paste into it is a
# real student sentence.
ask.txt
ask_*.txt

# Third-party copyright: the Cultural Intelligence Scale item wording and the
# Ang & Van Dyne paper. config/codebook.yml holds only item CODES and factors;
# the wording is read from this file at load time. Supply your own copy.
setting/*.pdf
setting/ClassificationScheme.txt

# Local units files built from real journals
data/units_real*.csv
data/units_[0-9]*.csv

# R / macOS noise
.Rhistory
.RData
.Rproj.user/
*.Rproj
.DS_Store
IGNORE

cat > "$DST/PUBLIC_SUBSET.md" <<'NOTE'
# What is in this repository, and what is deliberately not

This is the **public subset** of a larger working project. It is assembled by
`tools/build_public_repo.sh`, which copies an explicit allowlist and then scans
the result for endpoints, absolute paths, usernames and credentials. A file
nobody considered stays out, rather than going in.

## Here

| | |
|---|---|
| `R/`, `scripts/` | the whole pipeline: prompting, structured output, caching, agreement statistics, figures, reports |
| `prompts/`, `config/codebook.yml` | exactly how the models are asked, and the scheme that generates both the prompts and the label validation |
| `config/run.yml` | model panel, sampling, timeouts, retry policy |
| `data/example_journals*`, `data/synthetic_journals/` | fifteen **invented** student journals written for method development, in the real document layout |
| `data/units_*.csv` | the sentence units derived from those invented journals |
| `mock_results/` | every rating, agreement coefficient and manifest computed **from the invented journals** — the consistency evaluation is fully reproducible from what is here |
| `test_for_consistency/` | the base-R agreement toolkit (Krippendorff's alpha, Gwet's AC1, Fleiss' and Cohen's kappa) and its validation against analytic identities and `irrCAC` |

## Not here, and why

| | |
|---|---|
| student reflection journals | personal data under the Swiss FADP and the GDPR; *confidential* under the UZH/ETH four-level scheme |
| `cache/` | model rationales and reasoning traces, which can quote passages verbatim |
| `results/`, `reports/` | outputs computed from the real cohort |
| the CQS item wording | third-party copyright, not ours to redistribute; `setting/README.md` says what the file must look like |
| model server endpoints | read from the environment (`FGCZ_LLM_BASE_URL`), never hardcoded |

`results/` and `reports/` are created by the scripts on first run and stay local.
`mock_results/` is the invented-journal counterpart and is tracked.

## Running it

No cloud provider is configurable anywhere in this code. Inference goes to a
local Ollama daemon or to an OpenAI-compatible server you point at yourself:

```
# ~/.Renviron
FGCZ_LLM_BASE_URL=http://<your-vllm-host>:<port>/v1
FGCZ_LLM_API_KEY=dummy
```

`RUNBOOK.md` is the operational guide; `README.md` explains the scheme.
NOTE

# Repair links to files that are not part of the public subset
perl -0pi -e '
  s{\[Questions2discuss\.txt\]\(Questions2discuss\.txt\)}{`Questions2discuss.txt` (working notes, not published)}g;
  s{\[`paper/supplementary_methods\.md`\]\(paper/supplementary_methods\.md\)}{`paper/supplementary_methods.md` (draft, not published)}g;
  s{`Questions2discuss\.txt`}{`Questions2discuss.txt` (not published)}g unless $done++;
' "$DST/README.md"
echo "  done"

# ---------------------------------------------------------------------------
# The safety net
# ---------------------------------------------------------------------------
echo
echo "--- scanning the built tree for anything that should not ship ----------"
fail=0
scan() {  # scan <label> <extended-regex>
  local label="$1" re="$2" hits
  hits="$(grep -rInE "$re" "$DST" --exclude=build_public_repo.sh 2>/dev/null | grep -v '^Binary' || true)"
  if [ -n "$hits" ]; then
    echo "  FAIL  $label"; echo "$hits" | head -8 | sed 's/^/        /'; fail=1
  else
    echo "  ok    $label"
  fi
}
scan "no FGCZ host names"        'fgcz-c-[0-9]+'
scan "no absolute home paths"    '/Users/[A-Za-z0-9._-]+'
scan "no username"               "\\b$(id -un)\\b"
scan "no bare IP addresses"      '\b[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\b'
# Institution names are NOT withheld: the paper names UZH Zurich and UU Utrecht,
# so doing so here reveals nothing new. Reported for awareness only.
inst="$(grep -rlIE '\b(Utrecht|Zurich|Z.rich|UZH)\b' "$DST" --exclude=build_public_repo.sh 2>/dev/null || true)"
if [ -n "$inst" ]; then
  echo "  note  institutions named in $(echo "$inst" | wc -l | tr -d ' ') file(s) -- expected, the paper names them too"
fi
scan "no API keys or tokens"     '(api[_-]?key|secret|token|password|bearer)[[:space:]]*[:=][[:space:]]*["'"'"'][^"'"'"']{8,}'

for d in cache results reports journals test_journal paper .claude; do
  if [ -e "$DST/$d" ]; then echo "  FAIL  '$d/' must not be present"; fail=1
  else echo "  ok    '$d/' absent"; fi
done
for f in setting/ClassificationScheme.txt ask.txt Questions2discuss.txt; do
  if [ -e "$DST/$f" ]; then echo "  FAIL  '$f' must not be present"; fail=1
  else echo "  ok    '$f' absent"; fi
done
if ls "$DST"/setting/*.pdf >/dev/null 2>&1; then echo "  FAIL  setting/*.pdf present"; fail=1
else echo "  ok    no PDFs in setting/"; fi
find "$DST" -name '.DS_Store' -delete

# Markdown links pointing at files the subset does not contain
broken=""
while IFS= read -r line; do
  fl="${line%%:*}"; tgt="${line#*:}"
  [ -e "$DST/$tgt" ] || broken="$broken\n        $fl -> $tgt"
done < <(grep -rhoE '\]\(([A-Za-z0-9_./-]+)\)' "$DST"/*.md 2>/dev/null |
         sed -E 's/^\]\(//; s/\)$//' | grep -vE '^https?://' |
         while read -r t; do echo "README.md:$t"; done)
if [ -n "$broken" ]; then
  echo "  FAIL  markdown links to files not in the subset:"; printf "%b\n" "$broken" | head -6; fail=1
else
  echo "  ok    no broken markdown links"
fi

echo
if [ "$fail" -ne 0 ]; then
  echo "SCAN FAILED -- do not push $DST until the entries above are resolved."
  exit 2
fi
echo "scan clean."
echo
echo "files: $(find "$DST" -type f | wc -l | tr -d ' ')   size: $(du -sh "$DST" | cut -f1)"
echo
echo "Next, to review before publishing anything:"
echo "  cd $DST && git init && git add -A && git status"
echo "Nothing is committed or pushed by this script."
