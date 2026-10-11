# ============================================================
# 國際行銷 HW2 — 題目一：市場區隔統計檢定（bank 資料集）
# 用法：RStudio 開啟本檔 → 逐段選取 → Ctrl+Enter 執行
# ============================================================

# --- 0. 環境設定：自動定位「作業」資料夾（不寫死絕對路徑） ---
# 1) Rscript / source()：讀命令列 --file= 參數
# 2) RStudio 開啟本檔按 Ctrl+Enter：用 rstudioapi 取得本檔路徑
# 3) 保險：若目前不在含 MKT_data 的資料夾，自動往上找一層
local({
  # 建立空字串 p，稍後用來存放「本檔案的路徑」
  p <- ""
  # 從命令列參數找 --file= 開頭者（用 Rscript 執行時會有，例如 --file=hw2_q1.R）
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
  # 有路徑時，往上兩層（R/ → 作業）並設為工作目錄（dirname 取上一層）
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
  # 找一輪後仍找不到 → 直接停止並提示（不讓它靜默失敗、以免後面讀檔全錯）
  if (!dir.exists("MKT_data"))
    stop("無法自動定位作業資料夾（找不到 MKT_data）：請確認本檔位於 H2/作業/R/ 且結構未變動。")
})
# 印出最終決定的工作目錄，方便確認是否正確
cat("工作目錄:", getwd(), "\n")

# --- 1. 讀取資料（注意：分隔符是分號 ;） ---
# 讀入 bank.csv；sep=";" 指定用「分號」分隔（此檔非預設的逗號）
bank <- read.csv("MKT_data/bank.csv", sep = ";")
# str()：檢視每個欄位的型態（數值 / 類別）與前幾個範例值
str(bank)   # 檢查欄位型態
# head()：看前 6 列，快速確認資料讀進來是否正確
head(bank)

# --- 2. 移除指定欄位 ---
# 欲刪除的欄位名稱（題目指定不要的變數）
drop_cols <- c("job", "contact", "pdays", "poutcome", "month", "day")
# setdiff() 取其餘欄位名；再用 [ , ] 只留這些欄 → 得到清理後的資料 bank2
bank2 <- bank[, setdiff(names(bank), drop_cols)]
# 印出 bank2 剩下的欄位名，確認刪對了
names(bank2)

# --- 3. 欄位分類 ---
# 類別型欄位（要做卡方檢定）
cat_cols <- c("marital", "education", "default", "housing", "loan")   # 類別型
# 數值型欄位（要做 F / T 檢定）
num_cols <- c("age", "balance", "duration", "campaign", "previous")   # 數值型

# --- 4. 類別型特徵：卡方檢定 ---
# H0：該特徵與 y 互相獨立；p < 0.05 → 與是否參與促銷顯著相關
# 先建一張空結果表（欄位：特徵、卡方值、自由度、p 值）
chi_table <- data.frame(feature = cat_cols,
                        chi2    = numeric(length(cat_cols)),
                        df      = numeric(length(cat_cols)),
                        p.value = numeric(length(cat_cols)))
# 逐一對每個類別欄位做卡方檢定
for (i in seq_along(cat_cols)) {
  # 先建交叉表（該欄 × y）再交給 chisq.test 做卡方檢定
  ct <- chisq.test(table(bank2[[cat_cols[i]]], bank2$y))
  # 把結果填進表格：特徵名、卡方統計量(取 2 位小數)、自由度、p 值(4 位有效數字)
  chi_table[i, ] <- list(cat_cols[i],
                         round(unname(ct$statistic), 2),
                         unname(ct$parameter),
                         signif(ct$p.value, 4))
}
# 印出卡方結果表
chi_table   # ← 寫報告用：p < 0.05 的特徵適合做市場區隔

# --- 5. 數值型特徵：F 檢定（變異數）→ T 檢定（平均數） ---
# F 檢定 H0：兩組變異數相等；p >= 0.05 → 變異數相等 → 用 var.equal=TRUE 的 T 檢定
# T 檢定 H0：兩組平均數相等；p < 0.05 → 兩組平均數顯著不同
# 先建一張空結果表（特徵、F 檢定 p 值、是否假設變異數相等、t 統計量、t 檢定 p 值）
num_table <- data.frame(feature   = num_cols,
                        F.p.value = numeric(length(num_cols)),
                        var.equal = logical(length(num_cols)),
                        t.stat    = numeric(length(num_cols)),
                        t.p.value = numeric(length(num_cols)))
# 逐一對每個數值欄位做「先 F 後 T」的兩階段檢定
for (i in seq_along(num_cols)) {
  # 目前處理的欄位名稱
  v  <- num_cols[i]
  # 步驟 1：F 檢定 → 用 y 分兩組，比較兩組變異數是否相等
  ft <- var.test(bank2[[v]] ~ bank2$y)                  # 步驟 1：F 檢定
  # 是否假設變異數相等（p >= 0.05 表示無顯著差異 → 當作相等）
  ve <- ft$p.value >= 0.05
  # 步驟 2：T 檢定 → var.equal 參數依 F 檢定結果決定
  tt <- t.test(bank2[[v]] ~ bank2$y, var.equal = ve)    # 步驟 2：依 F 結果選 T 檢定
  # 把結果填入表格：欄位名、F 的 p 值、是否變異數相等、t 統計量、T 的 p 值
  num_table[i, ] <- list(v, signif(ft$p.value, 4), ve,
                         round(unname(tt$statistic), 2), signif(tt$p.value, 4))
}
# 印出數值檢定結果表
num_table   # ← t.p.value < 0.05 的特徵適合做市場區隔

# --- 6. 結論方向（報告用） ---
# 適合區隔的特徵 = 卡方 p<0.05 的類別特徵 + T 檢定 p<0.05 的數值特徵
# 管理意涵：針對顯著特徵刻畫目標客群輪廓，分配行銷資源
