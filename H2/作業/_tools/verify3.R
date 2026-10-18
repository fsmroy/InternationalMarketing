suppressMessages(library(arules))
setwd("d:/OneDrive/OneDrive - PEGATRON CORPORATION/交大/國際行銷/H2/作業")

bank <- read.csv("MKT_data/bank.csv", sep = ";")
bank <- bank[, setdiff(names(bank), c("job", "contact", "pdays", "poutcome", "month", "day"))]
bank$age <- cut(bank$age, c(-Inf, 35, 55, Inf), c("young", "adult", "old"))
for (v in c("balance", "duration", "campaign", "previous")) {
  med <- median(bank[[v]]); bank[[v]] <- ifelse(bank[[v]] <= med, "low", "high")
}
bank[] <- lapply(bank, factor)
tr2 <- as(bank, "transactions")
rn <- sort(apriori(tr2, parameter = list(supp = 0.03, conf = 0.6),
                   appearance = list(rhs = "y=no", default = "lhs")), by = "lift")
cat("### Q2 rules_no top6 by lift\n"); inspect(head(rn, 6))
cat("### Q2 rules_no top5 by conf\n"); inspect(head(sort(rn, by = "confidence"), 5))

pima <- read.csv("data/Pima.csv")
zc <- c("Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI")
p2 <- pima[apply(pima[, zc], 1, function(r) all(r != 0)), ]
for (v in setdiff(names(p2), "Outcome")) {
  q <- quantile(p2[[v]], c(0.25, 0.75))
  p2[[v]] <- cut(p2[[v]], c(-Inf, q[1], q[2], Inf), c("low", "medium", "high"))
}
p2$Outcome <- factor(ifelse(p2$Outcome == 1, "Yes", "No"))
p2[] <- lapply(p2, factor)
tr3 <- as(p2, "transactions")
r3n <- sort(apriori(tr3, parameter = list(supp = 0.05, conf = 0.6),
                    appearance = list(rhs = "Outcome=No", default = "lhs")), by = "lift")
cat("### Q3 rules_no top6 by lift\n"); inspect(head(r3n, 6))
r3y <- sort(apriori(tr3, parameter = list(supp = 0.05, conf = 0.6),
                    appearance = list(rhs = "Outcome=Yes", default = "lhs")), by = "lift")
cat("### Q3 rules_yes top8 by lift\n"); inspect(head(r3y, 8))
