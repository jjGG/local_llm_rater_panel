#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# 15_remap.R -- recover a block the two files numbered differently
#
#   Rscript scripts/15_remap.R \
#     --human results/ratings_9n99_Human.tsv \
#     --llm   results/ratings_9n99_main.csv \
#     --out   results/remap_9n99.csv
#
# THE PROBLEM. For one student x week the two files split the same text into
# different QUESTIONS, so the unit_ids do not correspond even though the
# underlying sentences do. Dropping the block loses real data; matching on the
# ids would compare unrelated sentences. Neither is acceptable.
#
# THE STRUCTURE THAT MAKES RECOVERY POSSIBLE. The human sheet records a
# paragraph (para_id) inside each question; the model run made each paragraph its
# own question in the affected block. So the human PARAGRAPH sequence and the
# model QUESTION sequence describe the same blocks in the same document order,
# and the sentence order within a block is the same in both. Where a block has
# the same size on both sides, the pairing is forced. Where it does not, the
# difference must be fully explained by sentences the model's parser dropped as
# question prompts -- identified WITHOUT reading text, by a text_sha1 that occurs
# under more than one student, which only boilerplate does. If the arithmetic
# does not come out exactly, the block is left unmapped rather than guessed.
#
# WHAT THIS IS NOT. The mapping is inferred from block sizes and hashes, not
# verified against the sentences themselves. Run scripts/16_verify_remap.R
# locally, where the units file is readable, to confirm it before trusting the
# recovered units in anything published.
#
# READS NO JOURNAL TEXT.
# ---------------------------------------------------------------------------

repo <- normalizePath(file.path(dirname(sub("^--file=", "",
        commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])), ".."))
setwd(repo)
source("R/six_rater.R")

a <- commandArgs(trailingOnly = TRUE)
getopt <- function(f, d = NULL) { i <- match(f, a); if (is.na(i) || i == length(a)) d else a[i + 1L] }
H_IN <- getopt("--human", "results/ratings_9n99_Human.tsv")
L_IN <- getopt("--llm",   "results/ratings_9n99_main.csv")
OUT  <- getopt("--out",   "results/remap_9n99.csv")

hum <- read_ratings(H_IN); llm <- read_ratings(L_IN)
for (cl in c("para_id", "sent_in_para", "question_no", "text_sha1")) {
  if (!cl %in% names(hum)) stop("human file needs a `", cl, "` column to remap")
}

hu <- unique(hum[, c("unit_id", "student_id", "week", "question_no",
                     "para_id", "sent_in_para", "text_sha1")])
lids <- unique(llm$unit_id)
k <- regmatches(lids, regexec("^(.+)_w([0-9]+)_q([0-9]+)_s([0-9]+)$", lids))
lu <- data.frame(unit_id = lids, st = vapply(k, `[`, character(1), 2),
                 week = as.integer(vapply(k, `[`, character(1), 3)),
                 q    = as.integer(vapply(k, `[`, character(1), 4)),
                 s    = as.integer(vapply(k, `[`, character(1), 5)),
                 stringsAsFactors = FALSE)

# --- boilerplate, from hashes shared across students ---------------------
sh <- table(hu$text_sha1, hu$student_id)
shared <- rownames(sh)[rowSums(sh > 0) > 1L]
hu$boiler <- hu$text_sha1 %in% shared
cat(sprintf("boilerplate hashes seen under more than one student: %d, covering %d units\n",
            length(shared), sum(hu$boiler)))

dv <- divergent_blocks(hum$unit_id, llm$unit_id)
if (!length(dv$blocks)) { cat("nothing to remap; the id sets already agree\n"); quit(save = "no") }

out <- list(); notes <- c()
for (b in dv$blocks) {
  st <- sub(" .*", "", b); wk <- as.integer(sub(".* ", "", b))
  hs <- hu[hu$student_id == st | hu$student_id == as.integer(st), ]
  hs <- hs[hs$week == wk, ]
  hs$pn <- as.integer(sub("^.*_p", "", hs$para_id))
  hs <- hs[order(hs$question_no, hs$pn, hs$sent_in_para), ]
  ls <- lu[lu$st == st & lu$week == wk, ]
  ls <- ls[order(ls$q, ls$s), ]

  hpara <- unique(hs$para_id)                 # human blocks, document order
  lq    <- unique(ls$q)                       # model blocks, document order
  cat(sprintf("\nstudent %s week %d\n  human paragraphs %d, model questions %d\n",
              st, wk, length(hpara), length(lq)))
  if (length(hpara) != length(lq)) {
    notes <- c(notes, sprintf("student %s week %d: %d human paragraphs vs %d model questions -- block counts differ, not remapped",
                              st, wk, length(hpara), length(lq)))
    next
  }

  for (i in seq_along(hpara)) {
    hi <- hs[hs$para_id == hpara[i], ]
    li <- ls[ls$q == lq[i], ]
    nh <- nrow(hi); nl <- nrow(li)
    if (nh == nl) {
      out[[length(out) + 1L]] <- data.frame(
        human_unit_id = hi$unit_id, model_unit_id = li$unit_id,
        basis = "equal block size", stringsAsFactors = FALSE)
      cat(sprintf("  block %d  %-22s %d = %d  paired in order\n",
                  i, paste0("q", hi$question_no[1], "/", hpara[i]), nh, nl))
    } else {
      drop <- hi$unit_id[hi$boiler]
      if (nh - nl == length(drop) && length(drop) > 0L) {
        keep <- hi[!hi$boiler, ]
        out[[length(out) + 1L]] <- data.frame(
          human_unit_id = keep$unit_id, model_unit_id = li$unit_id,
          basis = "size differs by the boilerplate the model dropped",
          stringsAsFactors = FALSE)
        cat(sprintf("  block %d  %-22s %d vs %d  %d boilerplate sentence(s) dropped by the model; remainder paired in order\n",
                    i, paste0("q", hi$question_no[1], "/", hpara[i]), nh, nl, length(drop)))
      } else {
        notes <- c(notes, sprintf("student %s week %d block %d (%s): %d human vs %d model units, %d boilerplate found -- arithmetic does not close, left unmapped",
                                  st, wk, i, hpara[i], nh, nl, length(drop)))
        cat(sprintf("  block %d  %-22s %d vs %d  UNMAPPED (%d boilerplate found, need %d)\n",
                    i, paste0("q", hi$question_no[1], "/", hpara[i]), nh, nl,
                    length(drop), nh - nl))
      }
    }
  }
}

map <- if (length(out)) do.call(rbind, out) else
  data.frame(human_unit_id = character(0), model_unit_id = character(0),
             basis = character(0))
stopifnot(!anyDuplicated(map$human_unit_id), !anyDuplicated(map$model_unit_id))

cat(sprintf("\n%d id pairs recovered\n", nrow(map)))
if (length(notes)) {
  cat("\nLEFT UNMAPPED\n")
  for (nt in notes) cat("  -", nt, "\n")
}
utils::write.csv(map, OUT, row.names = FALSE)
cat("wrote", OUT, "\n")

cat("\nBoilerplate units NOT on the omit list -- question prompts still being rated:\n")
om <- read_omit("results/Omit_ids.txt")
miss <- sort(hu$unit_id[hu$boiler & !hu$unit_id %in% om])
if (length(miss)) cat(paste("  ", miss, collapse = "\n"), "\n") else cat("  none\n")
cat("\nVERIFY BEFORE PUBLISHING: run scripts/16_verify_remap.R locally, where the\n")
cat("units file with the sentences is readable. This mapping is inferred from\n")
cat("block sizes and hashes alone.\n")
