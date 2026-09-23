#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# 22_after_discussion.R -- rebuild a pooled table from a hand-edited sheet
#
#   Rscript scripts/22_after_discussion.R \
#     --in  results/pooled_units_Sep09_afterDiscussion_stud0.csv \
#     --out results/pooled_units_afterDiscussion_stud0.csv \
#     --before results/pooled_units_Sep09.csv
#
# The human raters revised their codes for one student after discussing them, and
# returned the pooled table with those cells edited. Two things therefore have to
# happen before it can be analysed.
#
# THE SUMMARY ROWS ARE STALE. `human panel`, `model panel` and `consensus` are
# derived quantities; editing a rater's code does not update them. They are
# dropped and recomputed from the six rater rows, so a majority always reflects
# the labels actually present.
#
# THE ROWS ARE NOT ALL THE SAME SHAPE. The sheet mixes two layouts, both nine
# fields wide, so nothing about the file signals the problem:
#
#   A  batch,unit_id,student,week,rater,rater_kind,code,cq_factor,cq_item
#   B  batch,unit_id,student,week,rater,rater,     rater_kind,code,cq_item
#
# In B the rater name is duplicated where `rater_kind` belongs, every later field
# sits one column to the left of its name, and `cq_factor` is gone. The header
# row matches B. Read on that header, the 31 units in layout A would have their
# CQ factor read as the primary code -- `Motivational` where `Positive` belongs --
# and the primary code would be silently lost for two thirds of the file.
#
# Shape is therefore decided per row from the contents of column 6, and every
# column is checked against the vocabulary it must contain. A row that fits
# neither layout stops the script rather than being guessed at.
#
# READS NO JOURNAL TEXT.
# ---------------------------------------------------------------------------

repo <- normalizePath(file.path(dirname(sub("^--file=", "",
        commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])), ".."))
setwd(repo)
source("R/six_rater.R")

a <- commandArgs(trailingOnly = TRUE)
getopt <- function(f, d = NULL) { i <- match(f, a); if (is.na(i) || i == length(a)) d else a[i + 1L] }
IN     <- getopt("--in",  "results/pooled_units_Sep09_afterDiscussion_stud0.csv")
OUT    <- getopt("--out", "results/pooled_units_afterDiscussion_stud0.csv")
BEFORE <- getopt("--before", "results/pooled_units_Sep09.csv")

KINDS <- c("human", "model", "panel", "consensus")

raw <- utils::read.csv(IN, stringsAsFactors = FALSE, header = FALSE, skip = 1,
                       na.strings = c("NA", ""), colClasses = "character")
n_raw <- nrow(raw)
raw <- raw[!is.na(raw[[2]]) & nzchar(trimws(raw[[2]])), , drop = FALSE]
for (i in seq_len(ncol(raw))) {
  v <- trimws(raw[[i]]); v[!nzchar(v)] <- NA_character_; raw[[i]] <- v
}
cat(sprintf("read %s\n  %d rows, %d blank padding rows dropped, %d real rows\n",
            IN, n_raw, n_raw - nrow(raw), nrow(raw)))
if (ncol(raw) != 9L) stop("expected 9 columns, found ", ncol(raw))

# Column 7 is the discriminator: a primary code in layout A, a rater kind in
# layout B, and the two vocabularies are disjoint. Column 6 cannot be used --
# for the derived rows the rater NAME is "consensus" and its KIND is also
# "consensus", so "V6 is a kind" and "V6 duplicates V5" are both true there.
isB <- !is.na(raw$V7) & raw$V7 %in% KINDS
isA <- !isB
bad <- isA & !is.na(raw$V7) & !raw$V7 %in% PRIMARY_LEVELS
if (any(bad))
  stop(sum(bad), " row(s) match neither layout, e.g. row ", which(bad)[1], ": ",
       paste(raw[which(bad)[1], ], collapse = "|"))
if (any(isA & !is.na(raw$V6) & !raw$V6 %in% KINDS))
  stop("layout-A rows whose column 6 is not a rater kind")
if (any(isB & !is.na(raw$V6) & raw$V6 != raw$V5))
  stop("layout-B rows whose duplicated rater column does not match")

d <- data.frame(
  batch      = raw$V1,
  unit_id    = raw$V2,
  student    = raw$V3,
  week       = as.integer(raw$V4),
  rater      = raw$V5,
  rater_kind = ifelse(isA, raw$V6, raw$V7),
  code       = ifelse(isA, raw$V7, raw$V8),
  cq_factor  = ifelse(isA, raw$V8, NA_character_),
  cq_item    = raw$V9,
  stringsAsFactors = FALSE)
cat(sprintf("  layout A %d rows (%d units), layout B %d rows (%d units)\n",
            sum(isA), length(unique(d$unit_id[isA])),
            sum(isB), length(unique(d$unit_id[isB]))))

must_be <- function(cl, allowed, what) {
  bad <- setdiff(unique(d[[cl]][!is.na(d[[cl]])]), allowed)
  if (length(bad))
    stop("column `", cl, "` does not look like ", what, ": unexpected value(s) ",
         paste(utils::head(bad, 5), collapse = ", "))
  invisible(TRUE)
}
must_be("rater_kind", KINDS, "a rater kind")
must_be("code", PRIMARY_LEVELS, "a primary code")
must_be("cq_factor", names(FACTOR_CANON), "a CQ factor")
if (any(!is.na(d$cq_item) & !grepl("^(MC|COG|MOT|BEH)[0-9]+$", d$cq_item)))
  stop("column `cq_item` does not look like CQ item codes")
cat("  column mapping validated against all four vocabularies\n")

n_lost <- sum(isB & d$rater_kind == "human" & d$code == "Positive", na.rm = TRUE)
if (n_lost > 0L)
  cat(sprintf("  NOTE: %d human Positive cells in layout B carry no CQ factor --\n",
              n_lost),
      "        that column is absent from those rows and cannot be recovered\n", sep = "")

d$cq_factor <- canon_factor(d$cq_factor)
d$batch <- sub("\\.", "", d$batch)          # "Sep.09" -> "Sep09"

# --- drop the stale derived rows and recompute --------------------------
# Keep a copy first: the sheet's summary rows were computed BEFORE the raters
# revised anything, so comparing them against the recomputation shows how far
# they had gone out of date -- which is the reason for recomputing at all.
derived <- c("human panel", "model panel", "consensus")
stale <- d[d$rater %in% derived, c("unit_id", "rater", "code")]
n_drop <- sum(d$rater %in% derived)
d <- d[!d$rater %in% derived, , drop = FALSE]
cat(sprintf("  dropped %d stale summary rows (%s) -- recomputed below\n",
            n_drop, paste(derived, collapse = ", ")))

hum_r <- sort(unique(d$rater[d$rater_kind == "human"]))
mod_r <- sort(unique(d$rater[d$rater_kind == "model"]))
ids <- unique(d$unit_id)
meta <- d[match(ids, d$unit_id), c("batch", "unit_id", "student", "week")]
cat(sprintf("  %d units | %d human raters (%s) | %d model raters (%s)\n",
            length(ids), length(hum_r), paste(hum_r, collapse = ", "),
            length(mod_r), paste(mod_r, collapse = ", ")))

wide <- function(rs, col) vapply(rs, function(r) {
  s <- d[d$rater == r, ]; s[[col]][match(ids, s$unit_id)]
}, character(length(ids)))
Mh <- wide(hum_r, "code"); Mm <- wide(mod_r, "code")
Fh <- wide(hum_r, "cq_factor"); Fm <- wide(mod_r, "cq_factor")
Im <- wide(mod_r, "cq_item")

side <- function(M, F, I, lab) {
  mj <- panel_modal(M, tie = NA_character_)
  mf <- panel_modal(F, tie = NA_character_); mf[is.na(mj) | mj != "Positive"] <- NA_character_
  mi <- if (is.null(I)) NA_character_ else {
    x <- panel_modal(I, tie = NA_character_); x[is.na(mj) | mj != "Positive"] <- NA_character_; x
  }
  data.frame(meta, rater = lab, rater_kind = "panel", code = mj,
             cq_factor = mf, cq_item = mi, n_raters = NA_integer_,
             n_agree = NA_integer_, stringsAsFactors = FALSE)
}

# consensus over all six raters, strict majority, ties left unresolved
A <- cbind(Mh, Mm); AF <- cbind(Fh, Fm)
cons_code <- apply(A, 1L, function(v) {
  v <- v[!is.na(v)]; tb <- sort(table(v), decreasing = TRUE)
  if (!length(tb) || (length(tb) > 1L && tb[1] == tb[2])) NA_character_ else names(tb)[1]
})
cons_fac <- apply(AF, 1L, function(v) {
  v <- v[!is.na(v)]; tb <- sort(table(v), decreasing = TRUE)
  if (!length(tb) || (length(tb) > 1L && tb[1] == tb[2])) NA_character_ else names(tb)[1]
})
cons_fac[is.na(cons_code) | cons_code != "Positive"] <- NA_character_
n_rat <- apply(A, 1L, function(v) sum(!is.na(v)))
n_agr <- apply(A, 1L, function(v) { v <- v[!is.na(v)]
  if (!length(v)) 0L else as.integer(max(table(v))) })

d$n_raters <- NA_integer_; d$n_agree <- NA_integer_
out <- rbind(
  d[, c("batch", "unit_id", "student", "week", "rater", "rater_kind",
        "code", "cq_factor", "cq_item", "n_raters", "n_agree")],
  side(Mh, Fh, NULL, "human panel"),
  side(Mm, Fm, Im, "model panel"),
  data.frame(meta, rater = "consensus", rater_kind = "consensus",
             code = cons_code, cq_factor = cons_fac, cq_item = NA_character_,
             n_raters = n_rat, n_agree = n_agr, stringsAsFactors = FALSE))

cat(sprintf("\nrecomputed consensus: %d units | no majority on %d (%.1f%%) | unanimous %.0f%%\n",
            length(ids), sum(is.na(cons_code)), 100 * mean(is.na(cons_code)),
            100 * mean(n_agr == n_rat)))
print(table(consensus = cons_code, useNA = "ifany"))

# --- what the discussion changed ----------------------------------------
if (!is.null(BEFORE) && nzchar(BEFORE) && file.exists(BEFORE)) {
  b <- utils::read.csv(BEFORE, stringsAsFactors = FALSE, na.strings = c("NA", ""))
  b <- b[b$student %in% unique(d$student) & b$rater_kind == "human", ]
  key <- function(x) paste(x$unit_id, x$rater)
  now <- d[d$rater_kind == "human", ]
  m <- match(key(now), key(b))
  cat(sprintf("\ncompared with %s\n  %d of %d human cells matched\n",
              BEFORE, sum(!is.na(m)), nrow(now)))
  ch <- !is.na(m) & (now$code != b$code[m] |
                     xor(is.na(now$code), is.na(b$code[m])))
  cat(sprintf("  %d human codes CHANGED after the discussion (%.1f%%)\n",
              sum(ch), 100 * mean(ch, na.rm = TRUE)))
  if (any(ch)) {
    cat("\n  who changed what\n")
    print(table(rater = now$rater[ch], from = b$code[m][ch]))
    cat("\n  from -> to\n")
    print(table(from = b$code[m][ch], to = now$code[ch]))
    utils::write.csv(data.frame(unit_id = now$unit_id[ch], rater = now$rater[ch],
                                before = b$code[m][ch], after = now$code[ch]),
                     sub("\\.csv$", "_changes.csv", OUT), row.names = FALSE)
    cat("\n  wrote", sub("\\.csv$", "_changes.csv", OUT), "\n")
  }
  # models must NOT have changed -- they were not part of the discussion
  bm <- b <- utils::read.csv(BEFORE, stringsAsFactors = FALSE, na.strings = c("NA", ""))
  bm <- bm[bm$student %in% unique(d$student) & bm$rater_kind == "model", ]
  nm <- d[d$rater_kind == "model", ]
  mm <- match(paste(nm$unit_id, nm$rater), paste(bm$unit_id, bm$rater))
  n_mch <- sum(!is.na(mm) & nm$code != bm$code[mm])
  cat(sprintf("  model codes changed: %d (expected 0 -- the models were not in the discussion)\n",
              n_mch))
}

# --- how stale were the summary rows we replaced? -----------------------
if (nrow(stale)) {
  nw <- out[out$rater %in% derived, c("unit_id", "rater", "code")]
  mi <- match(paste(stale$unit_id, stale$rater), paste(nw$unit_id, nw$rater))
  diff <- (stale$code != nw$code[mi]) | xor(is.na(stale$code), is.na(nw$code[mi]))
  cat(sprintf("\nstale summary rows replaced: %d of %d differed from the recomputation\n",
              sum(diff, na.rm = TRUE), nrow(stale)))
  st <- data.frame(rater = stale$rater, stale = stale$code,
                   recomputed = nw$code[mi], changed = diff, stringsAsFactors = FALSE)
  for (r in derived) cat(sprintf("  %-12s %d of %d changed\n", r,
      sum(st$changed[st$rater == r], na.rm = TRUE), sum(st$rater == r)))
  utils::write.csv(st, sub("\\.csv$", "_summaryrows.csv", OUT), row.names = FALSE, na = "")
  cat("  wrote", sub("\\.csv$", "_summaryrows.csv", OUT), "\n")
}

utils::write.csv(out, OUT, row.names = FALSE, na = "")
cat("\nwrote", OUT, "\n")
