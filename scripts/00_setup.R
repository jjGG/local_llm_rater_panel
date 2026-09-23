#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# 00_setup.R -- check the environment and install what is missing
#
#   Rscript scripts/00_setup.R
# ---------------------------------------------------------------------------

repo <- normalizePath(file.path(dirname(sub("^--file=", "",
        commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])), ".."))
setwd(repo)

REQUIRED <- c("httr2", "jsonlite", "yaml", "digest")
OPTIONAL <- c("irrCAC", "ggplot2", "readxl")

cat("== R packages ==\n")
install_if_missing <- function(pkgs, kind) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  for (p in pkgs) {
    cat(sprintf("  %-10s %s\n", p,
        if (requireNamespace(p, quietly = TRUE)) as.character(utils::packageVersion(p))
        else "MISSING"))
  }
  if (length(missing)) {
    cat("  installing ", kind, ": ", paste(missing, collapse = ", "), "\n", sep = "")
    utils::install.packages(missing, repos = "https://cloud.r-project.org", quiet = TRUE)
    still <- missing[!vapply(missing, requireNamespace, logical(1), quietly = TRUE)]
    if (length(still) && kind == "required") {
      stop("could not install: ", paste(still, collapse = ", "), call. = FALSE)
    }
  }
}
install_if_missing(REQUIRED, "required")
install_if_missing(OPTIONAL, "optional")

cat("\n== config ==\n")
source("R/codebook.R")
cb  <- load_codebook("config/codebook.yml")
cfg <- load_run_config("config/run.yml")
cat(sprintf("  codebook v%s: %s | %s\n", cb$version,
            paste(cb$primary_labels, collapse = "/"),
            paste(cb$cq_labels, collapse = "/")))
cat(sprintf("  context_mode: %s\n", cfg$context_mode))
cat(sprintf("  profiles: %s\n", paste(names(cfg$profiles), collapse = ", ")))

cat("\n== prompts ==\n")
for (p in c(cfg$paths$prompts_primary, cfg$paths$prompts_cq)) {
  t <- read_prompt_template(p)
  cat(sprintf("  %-22s system %d chars, user %d chars\n", basename(p),
              nchar(t$system), nchar(t$user)))
}

cat("\n== ollama ==\n")
source("R/llm.R")
ok <- tryCatch({ preflight(cfg); TRUE }, error = function(e) {
  cat("  ", conditionMessage(e), "\n", sep = ""); FALSE })

cat("\n== git ==\n")
if (dir.exists(".git")) {
  # Hooks live in .git/hooks, which is not itself version controlled, so a fresh
  # clone starts with none. They are reinstalled from tools/ on every setup.
  hooks <- c(
    "pre-commit" = "blocks journal text from being committed",
    "pre-push"   = "refuses to push this tree anywhere at all")
  for (h in names(hooks)) {
    src <- file.path("tools", h); dst <- file.path(".git", "hooks", h)
    if (file.exists(src)) {
      file.copy(src, dst, overwrite = TRUE)
      Sys.chmod(dst, "0755")
      cat(sprintf("  installed %-11s (%s)\n", h, hooks[[h]]))
    }
  }
} else {
  cat("  not a git repository yet; run:  git init\n")
}

cat(sprintf("\n%s\n", if (ok) "setup complete." else "setup incomplete -- see ollama note above."))
