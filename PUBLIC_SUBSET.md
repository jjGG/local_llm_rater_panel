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
