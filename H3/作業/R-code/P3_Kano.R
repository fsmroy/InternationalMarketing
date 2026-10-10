############################################################
# International Marketing HW3 -- Problem 3: Kano Model
# 評估智慧型手機四項功能：water proof / optical zoom /
#                        wireless charging / foldable screen
# 依團隊成員的 Kano 分類百分比，推導 delight 與 disgust
# 公式 (與課程範本 多準則決策.xlsx 的 Kano 工作表一致)：
#   delight = (A + O - I) / (A + M + O + R + I)
#   disgust = -(M + O - R) / (A + M + O + R + I)
# 另附 entropy 客觀加權 (供 Problem 4 使用)
# 環境：純 base R，不需任何套件
############################################################

## ---------- 0. 基本設定 ------------------------------------------------
features <- c("WaterProof", "OpticalZoom", "WirelessCharging", "FoldableScreen")
labels_zh <- c("water proof (防水)", "optical zoom (光學變焦)",
               "wireless charging (無線充電)", "foldable screen (摺疊螢幕)")

## ---------- 1. 輸入資料：Kano 分類百分比 -------------------------------
## 欄位：A(魅力) M(必要) O(一維) R(反向) I(無差異) Q(可疑)
## ▶ 以下為示範值，請換成你們團隊問卷統計出的實際百分比。
##   每個功能的 A+M+O+R+I+Q 應 = 1 (或全為同一基數的計數)。
kano_pct <- rbind(
  #          A     M     O     R     I     Q
  WaterProof      = c(0.15, 0.45, 0.35, 0.00, 0.05, 0),
  OpticalZoom     = c(0.40, 0.10, 0.42, 0.00, 0.08, 0),
  WirelessCharging= c(0.38, 0.12, 0.40, 0.00, 0.10, 0),
  FoldableScreen  = c(0.30, 0.08, 0.20, 0.25, 0.17, 0)
)
colnames(kano_pct) <- c("A", "M", "O", "R", "I", "Q")
rownames(kano_pct) <- features

## 檢查每列和為 1
rs <- rowSums(kano_pct)
stopifnot(all(abs(rs - 1) < 1e-6))

## ---------- 2. 計算 delight / disgust ----------------------------------
AMO <- kano_pct[, "A"] + kano_pct[, "M"] + kano_pct[, "O"] +
       kano_pct[, "R"] + kano_pct[, "I"]          # 分母 (不含 Q)
delight <- (kano_pct[, "A"] + kano_pct[, "O"] - kano_pct[, "I"]) / AMO
disgust <- -(kano_pct[, "M"] + kano_pct[, "O"] - kano_pct[, "R"]) / AMO

## Kano 分類：取 A/M/O/I/R 中百分比最高者
cat_of <- apply(kano_pct[, c("A","M","O","I","R"), drop = FALSE], 1,
                function(v) c("A","M","O","I","R")[which.max(v)])
cat_name <- c(A = "Attractive 魅力", M = "Must-be 必要",
              O = "One-dimensional 一維", I = "Indifferent 無差異",
              R = "Reverse 反向")

## ---------- 3. Entropy 客觀加權 (與 Excel 範本一致) ---------------------
## 以 A..I 五類占比算 entropy，1-entropy 後正規化成權重。
## 用途：Problem 4 最大化 delight 時，可作為各功能的權重。
p_mat <- kano_pct[, c("A","M","O","R","I"), drop = FALSE]
H <- -rowSums(ifelse(p_mat > 0, p_mat * log(p_mat), 0)) / log(5)  # 除以 ln(5)
H[is.nan(H)] <- 0
d <- 1 - H
w_entropy <- d / sum(d)

## ---------- 4. 輸出 -----------------------------------------------------
res <- data.frame(
  Feature     = features,
  Feature_zh  = labels_zh,
  A = kano_pct[, "A"], M = kano_pct[, "M"], O = kano_pct[, "O"],
  R = kano_pct[, "R"], I = kano_pct[, "I"],
  Category    = cat_name[cat_of],
  Delight     = round(delight, 4),
  Disgust     = round(disgust, 4),
  Entropy     = round(H, 4),
  Weight      = round(w_entropy, 4),
  row.names   = NULL
)

cat("==== Kano 分類百分比 ====\n"); print(kano_pct)
cat("\n==== delight / disgust / 權重 ====\n"); print(res)

cat("\n※ delight 越大 = 提供該功能帶來越高滿意；disgust 越負 = 不提供時越不滿。\n")
cat("※ Category 為 A/M/O/I/R 中占比最高者。\n")

## ---------- 5. 繪圖：delight vs disgust 散布 ---------------------------
png("P3_Kano_plot.png", width = 1000, height = 700, res = 120)
par(mar = c(5, 5, 4, 2))
cols <- c(A = "#2e7d32", M = "#c62828", O = "#1565c0",
          I = "#9e9e9e", R = "#6a1b9a")[cat_of]
plot(disgust, delight, pch = 19, cex = 1.8, col = cols,
     xlim = c(-1, 0), ylim = c(0, 1),
     xlab = "Disgust  (越左越不滿)", ylab = "Delight  (越高越滿意)",
     main = "Kano: Delight vs Disgust (smartphone features)")
abline(h = 0, v = 0, col = "grey70", lty = 3)
text(disgust, delight, labels = features, pos = 4, cex = 0.85)
legend("topright", legend = c("Attractive", "Must-be", "One-dim",
                              "Indifferent", "Reverse"),
       col = c("#2e7d32","#c62828","#1565c0","#9e9e9e","#6a1b9a"),
       pch = 19, bty = "n", cex = 0.85)
dev.off()
cat("\n[圖檔已輸出] P3_Kano_plot.png\n")

## ---------- 6. 匯出供 Problem 4 使用 -----------------------------------
## 把 delight / disgust / weight 寫成 RDS，P4 可直接讀取
saveRDS(list(delight = delight, disgust = disgust,
             weight = w_entropy, features = features,
             category = cat_of),
        "P3_kano_result.rds")
cat("[資料已輸出] P3_kano_result.rds (供 Problem 4 讀取)\n")
