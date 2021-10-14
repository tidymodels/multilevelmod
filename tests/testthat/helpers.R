
data(riesby)

# ------------------------------------------------------------------------------

riesby_tr <- riesby[-(1:8), ]
riesby_te <- riesby[ (1:8), c("week", "imipramine")]
riesby_te$subject <- "1000"

f <- depr_score ~ week + imipramine + (1 | subject)

# ------------------------------------------------------------------------------

riesby_bin <- riesby
riesby_bin$depressed <- ifelse(riesby_bin$depr_score < -10, "low", "high")
riesby_bin$depressed <- factor(riesby_bin$depressed)
riesby_bin$binary <- ifelse(riesby_bin$depr_score < -10, 1, 0)
riesby_bin_tr <- riesby_bin[-(1:8), ]
riesby_bin_te <- riesby_bin[ (1:8), c("week", "imipramine")]
riesby_bin_te$subject <- "1000"

f_bin <- depressed ~ week + imipramine + (1 | subject)

# ------------------------------------------------------------------------------

counts_tr <- longitudinal_counts[-(1:10), ]
counts_te <- longitudinal_counts[ (1:10), c("time", "x", "subject")]
counts_te$subject <- "1000"

f_counts <- y ~ time + x + (1 | subject)
