#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# 16_verify_remap.R -- prove the remapping pairs the SAME sentences
#
# RUN THIS LOCALLY. It is the only script here that opens the units file, which
# contains the sentences themselves. It prints no journal text: only unit ids,
# hashes and a verdict, so its output is safe to paste anywhere.
#
#   Rscript scripts/16_verify_remap.R \
#     --units ../rate_journals/prepare_for_AI/Res_9n99/units_9n99.csv \
#     --human results/ratings_9n99_Human.tsv \
#     --remap results/remap_9n99.csv
#
# The units path is whatever `units_file` says in the run manifest.
#
# HOW IT WORKS. The human sheet carries a text_sha1 for each of its units. This
# recomputes the same hash from the model run's units file and checks that every
# mapped pair has an identical hash. If they all match, the mapping is proven,
# not inferred. If any differ, the mapping is wrong and the report must go back
# to dropping the block.
#
# WHICH HASH. The human sheet's hashes were produced by some pipeline whose exact
# normalisation we do not know, so this tries several plausible ones and reports
# whichever reproduces the human column on the units both files already agree
# about. That calibration step is what makes the comparison meaningful; without a
# hash function that reproduces the KNOWN-good ids, a mismatch on the remapped
# ones would tell us nothing.
# ---------------------------------------------------------------------------

repo <- normalizePath(file.path(dirname(sub("^--file=", "",
        commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])), ".."))
setwd(repo)
source("R/six_rater.R")

a <- commandArgs(trailingOnly = TRUE)
getopt <- function(f, d = NULL) { i <- match(f, a); if (is.na(i) || i == length(a)) d else a[i + 1L] }
U_IN <- getopt("--units", NA)
H_IN <- getopt("--human", "results/ratings_9n99_Human.tsv")
R_IN <- getopt("--remap", "results/remap_9n99.csv")
TXT  <- getopt("--text-col", NA)

if (is.na(U_IN)) {
  cat("give --units <path to the units csv the model run used>\n")
  cat("the path is the `units_file` field in results/manifest_*_main.json\n")
  quit(save = "no", status = 2)
}
if (!requireNamespace("digest", quietly = TRUE))
  stop("needs the digest package: install.packages(\"digest\")")

hum <- read_ratings(H_IN)
map <- utils::read.csv(R_IN, stringsAsFactors = FALSE)
un  <- utils::read.csv(U_IN, stringsAsFactors = FALSE)

if (is.na(TXT)) {
  cand <- intersect(c("text", "sentence", "unit_text", "content"), names(un))
  if (!length(cand)) stop("cannot find the text column; pass --text-col <name>. columns: ",
                          paste(names(un), collapse = ", "))
  TXT <- cand[1]
}
stopifnot("unit_id" %in% names(un))
cat(sprintf("units file: %d rows, text column `%s`\n", nrow(un), TXT))

# --- calibrate the hash on ids both files already share ------------------
hs <- unique(hum[, c("unit_id", "text_sha1")])
shared <- intersect(hs$unit_id, un$unit_id)
shared <- setdiff(shared, map$model_unit_id)   # exclude the remapped block
cat(sprintf("ids common to both files and outside the remap: %d\n", length(shared)))
if (length(shared) < 20L) stop("too few shared ids to calibrate the hash")

norms <- list(
  raw          = function(x) x,
  trimmed      = function(x) trimws(x),
  squished     = function(x) gsub("[[:space:]]+", " ", trimws(x)),
  lower_squish = function(x) tolower(gsub("[[:space:]]+", " ", trimws(x))))
algos <- c("sha1")

hv <- hs$text_sha1[match(shared, hs$unit_id)]
uv <- un[[TXT]][match(shared, un$unit_id)]
best <- NULL
for (nm in names(norms)) for (ag in algos) {
  got <- vapply(norms[[nm]](uv), function(s)
    digest::digest(s, algo = ag, serialize = FALSE), character(1))
  hit <- mean(got == hv, na.rm = TRUE)
  cat(sprintf("  %-13s %-5s reproduces %5.1f%% of the human hashes\n", nm, ag, 100 * hit))
  if (is.null(best) || hit > best$hit) best <- list(nm = nm, ag = ag, hit = hit)
}
cat(sprintf("\nbest: %s / %s at %.1f%%\n", best$nm, best$ag, 100 * best$hit))
if (best$hit < 0.99) {
  cat("\nCANNOT VERIFY. No tried normalisation reproduces the human sheet's own\n")
  cat("hashes on ids the two files already agree about, so a comparison on the\n")
  cat("remapped ids would be meaningless. Find out how text_sha1 was produced and\n")
  cat("add that normalisation to `norms` above, then re-run.\n")
  quit(save = "no", status = 3)
}

# --- the actual test -----------------------------------------------------
f <- function(x) vapply(norms[[best$nm]](x), function(s)
  digest::digest(s, algo = best$ag, serialize = FALSE), character(1))
want <- hs$text_sha1[match(map$human_unit_id, hs$unit_id)]
got  <- f(un[[TXT]][match(map$model_unit_id, un$unit_id)])
ok <- !is.na(want) & !is.na(got) & want == got

cat(sprintf("\n%s\n", strrep("=", 70)))
cat(sprintf("REMAP VERIFICATION: %d of %d pairs hash-identical\n", sum(ok), nrow(map)))
cat(sprintf("%s\n", strrep("=", 70)))
res <- data.frame(map, human_sha1 = want, model_sha1 = got, match = ok,
                  stringsAsFactors = FALSE)
if (all(ok)) {
  cat("\nPROVEN. Every mapped pair is the same sentence. The recovered units are\n")
  cat("safe to publish, and the note in the HTML report can be strengthened from\n")
  cat("\"inferred\" to \"verified\".\n")
} else {
  cat("\nFAILED for these pairs -- the mapping is WRONG, do not publish the\n")
  cat("recovered units; re-run the analysis with --remap none:\n")
  print(res[!ok, c("human_unit_id", "model_unit_id")], row.names = FALSE)
}
utils::write.csv(res, "results/remap_verification.csv", row.names = FALSE)
cat("\nwrote results/remap_verification.csv (ids and hashes only, no text)\n")
