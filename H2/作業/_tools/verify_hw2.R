# 驗證用：補充 Q1 的組間平均/參加率，並輸出 Q2/Q3/Q4 關鍵數字
suppressMessages(library(arules))
setwd("d:/OneDrive/OneDrive - PEGATRON CORPORATION/交大/國際行銷/H2/作業")

cat("=== Q1 補充 ===\n")
bank <- read.csv("MKT_data/bank.csv", sep = ";")
drop_cols <- c("job", "contact", "pdays", "poutcome", "month", "day")
b <- bank[, setdiff(names(bank), drop_cols)]
cat("n =", nrow(b), " yes =", sum(b$y == "yes"), " no =", sum(b$y == "no"),
    " rate =", round(mean(b$y == "yes") * 100, 2), "%\n")
for (v in c("marital", "education", "default", "housing", "loan")) {
  t <- prop.table(table(b[[v]], b$y), 1)
  cat("\n--", v, "--\n"); print(round(t[, "yes"] * 100, 1))
}
for (v in c("age", "balance", "duration", "campaign", "previous")) {
  m <- tapply(b[[v]], b$y, mean)
  cat("\n--", v, "-- yes:", round(m["yes"], 1), " no:", round(m["no"], 1),
      " ratio:", round(m["yes"] / m["no"], 2), "\n")
}

cat("\n=== Q2 ===\n")
b2 <- b
b2$age <- cut(b2$age, breaks = c(-Inf, 35, 55, Inf), labels = c("young", "adult", "old"))
for (v in c("balance", "duration", "campaign", "previous")) {
  med <- median(b2[[v]]); b2[[v]] <- ifelse(b2[[v]] <= med, "low", "high")
}
b2[] <- lapply(b2, factor)
tr2 <- as(b2, "transactions")
cat("transactions:", length(tr2), " items:", ncol(tr2), "\n")
ry <- sort(apriori(tr2, parameter = list(supp = 0.02, conf = 0.35),
                   appearance = list(rhs = "y=yes", default = "lhs")), by = "lift")
rn <- sort(apriori(tr2, parameter = list(supp = 0.03, conf = 0.6),
                   appearance = list(rhs = "y=no", default = "lhs")), by = "lift")
cat("rules_yes:", length(ry), " rules_no:", length(rn), "\n")
cat("--- rules_yes top5 ---\n"); inspect(head(ry, 5))
lhs_feature_freq <- function(rules) {
  items <- unlist(LIST(lhs(rules)))
  sort(table(sapply(strsplit(items, "="), `[`, 1)), decreasing = TRUE)
}
cat("--- freq_yes ---\n"); print(lhs_feature_freq(ry))
cat("--- item_freq_yes ---\n"); print(sort(table(unlist(LIST(lhs(ry)))), decreasing = TRUE))
cat("--- freq_no (head 12) ---\n"); print(head(lhs_feature_freq(rn), 12))
cat("--- item_freq_no (head 12) ---\n"); print(head(sort(table(unlist(LIST(lhs(rn)))), decreasing = TRUE), 12))

cat("\n=== Q3 ===\n")
pima <- read.csv("data/Pima.csv")
zero_cols <- c("Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI")
keep <- apply(pima[, zero_cols], 1, function(r) all(r != 0))
p2 <- pima[keep, ]
cat("orig:", nrow(pima), " -> kept:", nrow(p2),
    " No:", sum(p2$Outcome == 0), " Yes:", sum(p2$Outcome == 1), "\n")
num_cols <- setdiff(names(p2), "Outcome")
for (v in num_cols) {
  q <- quantile(p2[[v]], c(0.25, 0.75))
  p2[[v]] <- cut(p2[[v]], breaks = c(-Inf, q[1], q[2], Inf), labels = c("low", "medium", "high"))
}
p2$Outcome <- factor(ifelse(p2$Outcome == 1, "Yes", "No"))
p2[] <- lapply(p2, factor)
tr3 <- as(p2, "transactions")
r3y <- sort(apriori(tr3, parameter = list(supp = 0.05, conf = 0.6),
                    appearance = list(rhs = "Outcome=Yes", default = "lhs")), by = "lift")
r3n <- sort(apriori(tr3, parameter = list(supp = 0.05, conf = 0.6),
                    appearance = list(rhs = "Outcome=No", default = "lhs")), by = "lift")
cat("rules Yes:", length(r3y), " No:", length(r3n), "\n")
cat("--- Yes top5 ---\n"); inspect(head(r3y, 5))
cat("--- freq_yes ---\n"); print(lhs_feature_freq(r3y))
cat("--- item_freq_yes ---\n"); print(sort(table(unlist(LIST(lhs(r3y)))), decreasing = TRUE))
cat("--- freq_no ---\n"); print(lhs_feature_freq(r3n))
cat("--- item_freq_no (head 15) ---\n"); print(head(sort(table(unlist(LIST(lhs(r3n)))), decreasing = TRUE), 15))

cat("\n=== Q4 ===\n")
scores <- read.csv("phone_scores.csv")
agg <- aggregate(cbind(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) ~ product, data = scores, FUN = mean)
avg <- as.matrix(agg[, -1]); rownames(avg) <- agg$product
print(round(avg, 2))
d <- dist(avg); m <- cmdscale(d, k = 2, eig = TRUE)
print(round(m$points, 3))
cat("eig:", round(m$eig[1:6], 3), "\n")
cat("pct:", round(m$eig[1:2] / sum(m$eig[m$eig > 0]) * 100, 1), "\n")
cat("cor with attrs:\n")
print(round(cor(avg, m$points), 2))
cat("n members:", length(unique(scores$member)), " rows:", nrow(scores), "\n")
