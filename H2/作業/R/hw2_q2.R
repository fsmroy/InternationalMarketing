# ============================================================
# 國際行銷 HW2 — 題目二：關聯規則（bank 資料集，y 當 RHS）
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

# --- 1. 讀取與清理 ---
bank <- read.csv("MKT_data/bank.csv", sep = ";")
drop_cols <- c("job", "contact", "pdays", "poutcome", "month", "day")
bank <- bank[, setdiff(names(bank), drop_cols)]

# --- 2. 離散化 ---
# age → 三等級：young<=35、adult 35~55、old>55
bank$age <- cut(bank$age, breaks = c(-Inf, 35, 55, Inf),
                labels = c("young", "adult", "old"))

# 其餘數值欄位 → 中位數切 low / high
num_cols <- c("balance", "duration", "campaign", "previous")
for (v in num_cols) {
  med <- median(bank[[v]])
  bank[[v]] <- ifelse(bank[[v]] <= med, "low", "high")
}

# --- 3. 轉 transactions ---
# 注意：as(data.frame, "transactions") 會自動把 item 命名為「欄位名=水準」
# （例如 y=yes、duration=high），不需要（也不能）自己再加前綴，否則會變成雙重前綴
bank[] <- lapply(bank, factor)   # 確保所有欄位都是 factor
trans <- as(bank, "transactions")
summary(trans)

# (a) 顯著規則：RHS 分別為 y=yes / y=no
# y=yes 樣本僅約 11%，門檻需較寬才找得到規則（實測 supp=0.03/conf=0.6 時為 0 條）
rules_yes <- apriori(trans, parameter = list(supp = 0.02, conf = 0.35),
                     appearance = list(rhs = "y=yes", default = "lhs"))
rules_no  <- apriori(trans, parameter = list(supp = 0.03, conf = 0.6),
                     appearance = list(rhs = "y=no",  default = "lhs"))

rules_yes <- sort(rules_yes, by = "lift")
rules_no  <- sort(rules_no,  by = "lift")
inspect(head(rules_yes, 10))   # y=yes 的前 10 名規則
inspect(head(rules_no, 10))    # y=no  的前 10 名規則

# --- 5. (b) LHS 特徵頻率統計 → 特徵優先順序 ---
lhs_feature_freq <- function(rules) {
  if (length(rules) == 0) return("（此 RHS 設定下無規則，請調降 supp/conf）")
  items <- unlist(LIST(lhs(rules)))
  feats <- sapply(strsplit(items, "="), `[`, 1)
  sort(table(feats), decreasing = TRUE)
}
freq_yes <- lhs_feature_freq(rules_yes)
freq_no  <- lhs_feature_freq(rules_no)
freq_yes   # y=yes 規則中，各特徵在 LHS 出現次數（由高到低 = 優先順序）
freq_no    # y=no 同理

# --- 6. (c) 管理意涵方向 ---
# freq_yes 高的特徵 = 「會參與促銷」的關鍵特徵 → 行銷資源優先投入該客群
# freq_no  高的特徵 = 「不參與」的輪廓 → 可用於排除或改變溝通策略
