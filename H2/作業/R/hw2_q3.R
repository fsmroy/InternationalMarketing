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
  # 建立空字串 p，稍後用來存放「本檔案的路徑」
  p <- ""
  # 從命令列參數找 --file= 開頭者（用 Rscript 執行時會有）
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  # 若有抓到，去掉 "--file=" 前綴，存成乾淨的檔案路徑
  if (length(a)) p <- sub("^--file=", "", a[1])
  # 若還拿不到路徑，且 rstudioapi 可用 → 向 RStudio 問「目前編輯器開的是哪個檔」
  if (!nzchar(p) && requireNamespace("rstudioapi", quietly = TRUE) &&
      isTRUE(rstudioapi::isAvailable())) {
    # 取目前來源編輯器資訊；任何錯誤都回 NULL（避免中斷整個流程）
    ctx <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
    # 若成功取得且有路徑，就採用該檔路徑
    if (!is.null(ctx) && nzchar(ctx$path)) p <- ctx$path
  }
  # 有路徑時，往上兩層（R/ → 作業）並設為工作目錄
  if (nzchar(p)) setwd(dirname(dirname(normalizePath(p))))   # R/ 的上一層 = 作業
  # 若工作目錄沒有 MKT_data，就一層一層往上找含 MKT_data 的資料夾
  if (!dir.exists("MKT_data")) {                            # 保險：往上找作業資料夾
    # 從目前工作目錄開始找
    d <- getwd()
    # 重複往上，直到找到或到根目錄為止
    repeat {
      # 取上一層目錄路徑
      up <- dirname(d)
      # 若已到根目錄（無法再上）→ 跳出迴圈
      if (identical(up, d)) break
      # 移到上一層
      d <- up
      # 若該層含 MKT_data，就設為工作目錄並停止
      if (dir.exists(file.path(d, "MKT_data"))) { setwd(d); break }
    }
  }
  # 找一輪後仍找不到 → 直接停止並提示（不讓它靜默失敗）
  if (!dir.exists("MKT_data"))
    stop("無法自動定位作業資料夾（找不到 MKT_data）：請確認本檔位於 H2/作業/R/ 且結構未變動。")
})
# 印出最終決定的工作目錄，方便確認是否正確
cat("工作目錄:", getwd(), "\n")

# --- 1. 讀取 ---
# 讀入 Pima.csv（逗號分隔，預設即可）
pima <- read.csv("data/Pima.csv")
# 看欄位名稱，確認資料結構
names(pima)

# --- 2. 移除指定欄位為 0 的樣本（0 視為量測缺失） ---
# 這些欄位的 0 在醫學上無意義，視為缺值
zero_cols <- c("Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI")
# 逐列檢查（apply 對第 1 維 = 列）：該列這 5 欄全部不為 0 才保留（keep=TRUE）
keep <- apply(pima[, zero_cols], 1, function(r) all(r != 0))
# 依 keep 篩選出保留的樣本
pima2 <- pima[keep, ]
# 印出移除前後筆數，確認清掉多少筆
cat("移除前:", nrow(pima), "筆 → 移除後:", nrow(pima2), "筆\n")

# --- 3. Q1 / Q3 切三等級：low / medium / high ---
# 切點：(-Inf, Q1] = low、(Q1, Q3] = medium、(Q3, Inf) = high
# 取除 Outcome（結果標記）以外的所有欄位名稱
num_cols <- setdiff(names(pima2), "Outcome")
# 逐一將每個數值欄位分成三個等級
for (v in num_cols) {
  # 計算該欄的 25%（Q1）與 75%（Q3）分位數
  q <- quantile(pima2[[v]], c(0.25, 0.75))
  # 依 Q1、Q3 切成三級，並附上 low/medium/high 標籤
  pima2[[v]] <- cut(pima2[[v]], breaks = c(-Inf, q[1], q[2], Inf),
                    labels = c("low", "medium", "high"))
}
# Outcome 1→"Yes"、0→"No"，轉成 factor（類別）
pima2$Outcome <- factor(ifelse(pima2$Outcome == 1, "Yes", "No"))

# --- 4. 轉 transactions ---
# 注意：as(data.frame, "transactions") 會自動把 item 命名為「欄位名=水準」
# （例如 Outcome=Yes、Glucose=high），不需要自己加前綴
# 把每個欄位都轉成 factor（apriori 需要類別型資料）
pima2[] <- lapply(pima2, factor)
# 把 data.frame 轉成關聯規則用的 transactions 物件
trans <- as(pima2, "transactions")

# 找 RHS 固定為 Outcome=Yes 的規則（supp=0.05、conf=0.6）
rules_yes <- apriori(trans, parameter = list(supp = 0.05, conf = 0.6),
                     appearance = list(rhs = "Outcome=Yes", default = "lhs"))
# 找 RHS 固定為 Outcome=No 的規則
rules_no  <- apriori(trans, parameter = list(supp = 0.05, conf = 0.6),
                     appearance = list(rhs = "Outcome=No",  default = "lhs"))

# RHS=Yes 依 lift 排序取前 10 名
inspect(head(sort(rules_yes, by = "lift"), 10))   # RHS=Yes 前 10 名
# RHS=No 依 lift 排序取前 10 名
inspect(head(sort(rules_no,  by = "lift"), 10))   # RHS=No  前 10 名

# --- 5. 頻率統計：各設定下的關鍵指標 ---
# 定義函式：統計一組規則左邊(LHS)各特徵出現次數
lhs_feature_freq <- function(rules) {
  # 把每條規則左邊的項目攤平成單一字串向量
  items <- unlist(LIST(lhs(rules)))
  # 用 "=" 切開，取前半段（欄位名，去掉 "=水準"）
  feats <- sapply(strsplit(items, "="), `[`, 1)
  # 統計各特徵出現次數並由多到少排序
  sort(table(feats), decreasing = TRUE)
}
# 對 RHS=Yes 的規則統計特徵頻率
freq_yes <- lhs_feature_freq(rules_yes)
# 對 RHS=No 的規則統計特徵頻率
freq_no  <- lhs_feature_freq(rules_no)
# 印出 RHS=Yes 的特徵頻率
freq_yes   # RHS=Yes 時最常出現在 LHS 的特徵（含等級可再往下看 items）
# 印出 RHS=No 的特徵頻率
freq_no    # RHS=No 同理

# 補充：想看「特徵+等級」的組合頻率（例如 Glucose=high 出現幾次）
# 統計 RHS=Yes 時每個「欄位=水準」完整項目的出現次數
item_freq_yes <- sort(table(unlist(LIST(lhs(rules_yes)))), decreasing = TRUE)
# 統計 RHS=No 時每個完整項目的出現次數
item_freq_no  <- sort(table(unlist(LIST(lhs(rules_no)))),  decreasing = TRUE)
# 印出組合頻率
item_freq_yes
item_freq_no
