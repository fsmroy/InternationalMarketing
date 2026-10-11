# ============================================================
# 國際行銷 HW2 — 題目三：關聯規則（pima 資料集，醫療洞察）
# 第一次使用前，在 Console 執行：install.packages("arules")
# ============================================================

# install.packages("arules")   # ← 已安裝可跳過
library(arules)

# --- 0. 環境設定：自動定位「作業」資料夾（不寫死絕對路徑） ---
# 1) Rscript / source()：讀命令列 --file= 參數
# 2) RStudio 開啟本檔按 Ctrl+Enter：用 rstudioapi 取得本檔路徑
# 3) 保險：若目前不在含 MKT_data 的資料夾，自動往上找一層
local({
  p <- ""
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(a)) p <- sub("^--file=", "", a[1])
  if (!nzchar(p) && requireNamespace("rstudioapi", quietly = TRUE) &&
      isTRUE(rstudioapi::isAvailable())) {
    ctx <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
    if (!is.null(ctx) && nzchar(ctx$path)) p <- ctx$path
  }
  if (nzchar(p)) setwd(dirname(dirname(normalizePath(p))))   # R/ 的上一層 = 作業
  if (!dir.exists("MKT_data")) {                            # 保險：往上找作業資料夾
    d <- getwd()
    repeat {
      up <- dirname(d)
      if (identical(up, d)) break
      d <- up
      if (dir.exists(file.path(d, "MKT_data"))) { setwd(d); break }
    }
  }
  if (!dir.exists("MKT_data"))
    stop("無法自動定位作業資料夾（找不到 MKT_data）：請確認本檔位於 H2/作業/R/ 且結構未變動。")
})
cat("工作目錄:", getwd(), "\n")

# --- 1. 讀取 ---
pima <- read.csv("data/Pima.csv")
names(pima)

# --- 2. 移除指定欄位為 0 的樣本（0 視為量測缺失） ---
zero_cols <- c("Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI")
keep <- apply(pima[, zero_cols], 1, function(r) all(r != 0))
pima2 <- pima[keep, ]
cat("移除前:", nrow(pima), "筆 → 移除後:", nrow(pima2), "筆\n")

# --- 3. Q1 / Q3 切三等級：low / medium / high ---
# 切點：(-Inf, Q1] = low、(Q1, Q3] = medium、(Q3, Inf) = high
num_cols <- setdiff(names(pima2), "Outcome")
for (v in num_cols) {
  q <- quantile(pima2[[v]], c(0.25, 0.75))
  pima2[[v]] <- cut(pima2[[v]], breaks = c(-Inf, q[1], q[2], Inf),
                    labels = c("low", "medium", "high"))
}
pima2$Outcome <- factor(ifelse(pima2$Outcome == 1, "Yes", "No"))

# --- 4. 轉 transactions ---
# 注意：as(data.frame, "transactions") 會自動把 item 命名為「欄位名=水準」
# （例如 Outcome=Yes、Glucose=high），不需要自己加前綴
pima2[] <- lapply(pima2, factor)
trans <- as(pima2, "transactions")

rules_yes <- apriori(trans, parameter = list(supp = 0.05, conf = 0.6),
                     appearance = list(rhs = "Outcome=Yes", default = "lhs"))
rules_no  <- apriori(trans, parameter = list(supp = 0.05, conf = 0.6),
                     appearance = list(rhs = "Outcome=No",  default = "lhs"))

inspect(head(sort(rules_yes, by = "lift"), 10))   # RHS=Yes 前 10 名
inspect(head(sort(rules_no,  by = "lift"), 10))   # RHS=No  前 10 名

# --- 5. 頻率統計：各設定下的關鍵指標 ---
lhs_feature_freq <- function(rules) {
  items <- unlist(LIST(lhs(rules)))
  feats <- sapply(strsplit(items, "="), `[`, 1)
  sort(table(feats), decreasing = TRUE)
}
freq_yes <- lhs_feature_freq(rules_yes)
freq_no  <- lhs_feature_freq(rules_no)
freq_yes   # RHS=Yes 時最常出現在 LHS 的特徵（含等級可再往下看 items）
freq_no    # RHS=No 同理

# 補充：想看「特徵+等級」的組合頻率（例如 Glucose=high 出現幾次）
item_freq_yes <- sort(table(unlist(LIST(lhs(rules_yes)))), decreasing = TRUE)
item_freq_no  <- sort(table(unlist(LIST(lhs(rules_no)))),  decreasing = TRUE)
item_freq_yes
item_freq_no
