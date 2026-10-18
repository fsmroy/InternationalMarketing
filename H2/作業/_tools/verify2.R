# 取得 Q2 中位數切點 / Q3 四分位切點 / Q4 評分來源
setwd("d:/OneDrive/OneDrive - PEGATRON CORPORATION/交大/國際行銷/H2/作業")

cat("=== Q2 medians ===\n")
bank <- read.csv("MKT_data/bank.csv", sep = ";")
for (v in c("balance", "duration", "campaign", "previous")) {
  cat(v, "median =", median(bank[[v]]), "\n")
}
cat("age table:\n"); print(table(cut(bank$age, c(-Inf, 35, 55, Inf), c("young","adult","old"))))

cat("\n=== Q2 y split by level ===\n")
for (v in c("balance", "duration", "campaign", "previous")) {
  lv <- ifelse(bank[[v]] <= median(bank[[v]]), "low", "high")
  cat(v, ":\n"); print(round(prop.table(table(lv, bank$y), 1)[, "yes"] * 100, 1))
}

cat("\n=== Q3 quantiles ===\n")
pima <- read.csv("data/Pima.csv")
zero_cols <- c("Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI")
keep <- apply(pima[, zero_cols], 1, function(r) all(r != 0))
p2 <- pima[keep, ]
for (v in setdiff(names(p2), "Outcome")) {
  q <- quantile(p2[[v]], c(0.25, 0.5, 0.75))
  cat(sprintf("%-25s Q1=%.2f  median=%.2f  Q3=%.2f\n", v, q[1], q[2], q[3]))
}

cat("\n=== Q4 sources ===\n")
s <- read.csv("phone_scores.csv")
print(table(s$member))
print(unique(s$product))
