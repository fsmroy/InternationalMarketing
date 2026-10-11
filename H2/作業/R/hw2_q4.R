# ============================================================
# 國際行銷 HW2 — 題目四：MDS 產品定位（智慧型手機）
# ⚠ 需先準備組員評分資料 phone_scores.csv，資料齊了才能執行
# ============================================================

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

# --- 1. 準備評分資料 ---
# 請將組員評分整理成 phone_scores.csv 放在作業資料夾，格式：
#
#   member,product,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10
#   Roy,iPhone Pro,4,3,5,4,4,5,4,4,5,4
#   Roy,iPhone Pro Max,4,2,5,4,4,5,4,4,5,4
#   ...（每位組員 × 每個產品一列，1~5 分）
#
# 屬性：A1品牌知名度 A2價格親民 A3拍照 A4觸控 A5電池健康
#       A6生態系 A7防水 A8介面 A9系統效能 A10錄影

# 讀入組員評分檔（放在作業資料夾下，相對路徑）
scores_raw <- read.csv("phone_scores.csv")   # ← 資料就緒後執行

# --- 2. 計算平均評分（每位組員評分 → 各產品平均） ---
# aggregate()：以 product 分組，對 A1~A10 各欄取平均
# cbind(...) ~ product 代表「左邊這 10 欄依 product 分組計 mean」
agg <- aggregate(cbind(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) ~ product,
                 data = scores_raw, FUN = mean)
# 去掉第一欄（產品名）轉成矩陣，方便當距離矩陣輸入
avg <- as.matrix(agg[, -1])
# 把產品名設為矩陣列名（否則後面畫圖沒有標籤）
rownames(avg) <- agg$product
# 印出平均分數（取 2 位小數）
round(avg, 2)

# --- 3. 距離矩陣 + 古典 MDS ---
# dist()：計算各產品間的歐氏距離，得到距離矩陣
d   <- dist(avg)                        # 歐氏距離
# cmdscale()：古典多維尺度分析，把高維距離壓成 k=2 個維度（座標）；eig=TRUE 另外回傳特徵值
mds <- cmdscale(d, k = 2, eig = TRUE)   # stats 套件內建，不需另外安裝

# 檢查適配度（stress 越小越好；eigen 值可看各維度解釋量）
# 看前兩個特徵值（越大代表該維度解釋越多變異）
mds$eig[1:2]

# --- 4. 2D 定位圖 ---
# 先畫空白畫布（type="n" = 只設坐标、不畫點），設定軸標題、主標題與略大的範圍
plot(mds$points[, 1], mds$points[, 2], type = "n",
     xlab = "Dimension 1", ylab = "Dimension 2",
     main = "Smartphone Product Positioning (MDS)",
     xlim = range(mds$points[, 1]) * 1.2, ylim = range(mds$points[, 2]) * 1.2)
# 在各座標位置寫上產品名稱（為了辨識各點是哪個產品）
text(mds$points[, 1], mds$points[, 2], labels = rownames(avg),
     col = "steelblue", cex = 1.2, font = 2)

# --- 5. 報告方向 ---
# (1) 看 Dim1 / Dim2 兩端各落哪些產品，對照 avg 屬性分數推斷軸的意義
#     （例如 Dim1 = 高階旗艦 vs. 平價、Dim2 = 生態系完整度）
# (2) 找出市場空白區（沒有產品佔據的象限）→ 產品機會點
# (3) 若未使用全部產品系列，引用產品評測報告的專家評論佐證定位
