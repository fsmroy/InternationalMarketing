# ============================================================
# 國際行銷 HW2 — 題目四：MDS 產品定位（智慧型手機）
# ⚠ 需先準備組員評分資料 phone_scores.csv，資料齊了才能執行
# ============================================================

setwd("D:/OneDrive/OneDrive - PEGATRON CORPORATION/交大/國際行銷/H2/作業")

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

scores_raw <- read.csv("phone_scores.csv")   # ← 資料就緒後執行

# --- 2. 計算平均評分（每位組員評分 → 各產品平均） ---
agg <- aggregate(cbind(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) ~ product,
                 data = scores_raw, FUN = mean)
avg <- as.matrix(agg[, -1])
rownames(avg) <- agg$product
round(avg, 2)

# --- 3. 距離矩陣 + 古典 MDS ---
d   <- dist(avg)                        # 歐氏距離
mds <- cmdscale(d, k = 2, eig = TRUE)   # stats 套件內建，不需另外安裝

# 檢查適配度（stress 越小越好；eigen 值可看各維度解釋量）
mds$eig[1:2]

# --- 4. 2D 定位圖 ---
plot(mds$points[, 1], mds$points[, 2], type = "n",
     xlab = "Dimension 1", ylab = "Dimension 2",
     main = "Smartphone Product Positioning (MDS)",
     xlim = range(mds$points[, 1]) * 1.2, ylim = range(mds$points[, 2]) * 1.2)
text(mds$points[, 1], mds$points[, 2], labels = rownames(avg),
     col = "steelblue", cex = 1.2, font = 2)

# --- 5. 報告方向 ---
# (1) 看 Dim1 / Dim2 兩端各落哪些產品，對照 avg 屬性分數推斷軸的意義
#     （例如 Dim1 = 高階旗艦 vs. 平價、Dim2 = 生態系完整度）
# (2) 找出市場空白區（沒有產品佔據的象限）→ 產品機會點
# (3) 若未使用全部產品系列，引用產品評測報告的專家評論佐證定位
