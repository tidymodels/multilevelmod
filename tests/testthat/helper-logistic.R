
data(riesby)
riesby$depressed <- ifelse(riesby$depr_score < -10, "low", "high")
riesby$depressed <- factor(riesby$depressed)
riesby$binary <- ifelse(riesby$depr_score < -10, 1, 0)
riesby_tr <- riesby[-(1:8), ]
riesby_te <- riesby[ (1:8), c("week", "imipramine")]
riesby_te$subject <- "1000"

f <- depressed ~ week + imipramine + (1 | subject)
