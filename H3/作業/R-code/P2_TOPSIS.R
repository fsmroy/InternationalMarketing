############################################################
# International Marketing HW3 -- Problem 2: TOPSIS (level-2)
# 承接 Problem 1：把「第二階層」的 AHP 評估改用 TOPSIS
#  - 五點李克特量表 (1~5)
#  - PIS = (5,5,5,5,5,5), NIS = (1,1,1,1,1,1)  ← 題目指定
#  - 沿用 Problem 1 求得的準則權重
# 產出：三城市的加權 TOPSIS 分數 (貼近度 CC) 與排名，並與 P1 比較
# 環境：純 base R，不需任何套件
############################################################

## ---------- 0. 基本設定 ------------------------------------------------
cities   <- c("Kaohsiung", "Busan", "Tokyo")
criteria <- c("LivingCost", "NatEnvironment", "CityTourism",
              "Food", "Transportation", "Courtesy")

## ---------- 1. 輸入資料 -------------------------------------------------
## 1a. 沿用 Problem 1 的準則權重 (w_mean)。
##     ▶ 若你在 P1 改了權重，請把這裡同步成 P1 印出的值。
w <- c(LivingCost = 0.22, NatEnvironment = 0.18, CityTourism = 0.16,
       Food = 0.16, Transportation = 0.15, Courtesy = 0.13)
stopifnot(abs(sum(w) - 1) < 1e-9)

## 1b. 三城市在六項準則的評分 (五點李克特量表, 1=很差 ~ 5=很好)。
##     ▶ 以下為示範值，請換成你們團隊問卷/評分的實際結果。
##     注意「成本」屬效益型：分數越高代表成本越令人滿意 (越便宜)。
X <- rbind(
  Kaohsiung = c(5, 4, 3, 4, 4, 4),
  Busan     = c(4, 4, 4, 4, 4, 4),
  Tokyo     = c(3, 4, 5, 3, 4, 3)
)
colnames(X) <- criteria
stopifnot(all(X >= 1 & X <= 5))          # 落在李克特 1~5 範圍

## 1c. 題目指定之正/負理想解
PIS <- rep(5, length(criteria))
NIS <- rep(1, length(criteria))
names(PIS) <- names(NIS) <- criteria

## ---------- 2. TOPSIS 計算 ---------------------------------------------
## 題目直接指定 PIS=5 / NIS=1，屬「李克特錨定」的 TOPSIS，
## 不需再做向量正規化，直接以權重 w 對原始 1~5 分數計算加權歐氏距離：
##   D+ = sqrt( sum_i w_i * (x_i - PIS_i)^2 )
##   D- = sqrt( sum_i w_i * (x_i - NIS_i)^2 )
##   CC = D- / (D+ + D-)          (貼近度, 0~1, 越大越好)
d_plus  <- sqrt(rowSums(sweep(X, 2, PIS, "-")^2 * matrix(w, nrow(X), 6, byrow = TRUE)))
d_minus <- sqrt(rowSums(sweep(X, 2, NIS, "-")^2 * matrix(w, nrow(X), 6, byrow = TRUE)))
cc      <- d_minus / (d_plus + d_minus)

## 加權總分 (與 P1 相同定義) 一併列出方便對照
weighted_sum <- as.vector(X %*% w)

cat("==== 評分矩陣 X (city x criterion) ====\n"); print(X)
cat("\n==== 準則權重 (沿用 P1) ====\n"); print(round(w, 4))
cat("\n==== TOPSIS 距離與貼近度 ====\n")
tbl <- data.frame(City = cities,
                  D_plus  = round(d_plus,  4),
                  D_minus = round(d_minus, 4),
                  CC      = round(cc,      4),
                  TopsisRank = rank(-cc, ties.method = "min"),
                  WeightedSum = round(weighted_sum, 4),
                  row.names = NULL)
print(tbl)

cat("\n※ CC (貼近度) 越大越好，排名以 CC 為準 (靠近 PIS 且遠離 NIS)。\n")

## ---------- 3. 與 Problem 1 (AHP) 結果比較 ------------------------------
## P1 的加權總分 (若你改過 P1 輸入，請同步更新)
p1_score <- c(Kaohsiung = 0.3524, Busan = 0.3400, Tokyo = 0.3076)
p1_rank  <- rank(-p1_score, ties.method = "min")

cmp <- data.frame(City = cities,
                  P1_Score = round(p1_score, 4),
                  P1_Rank  = p1_rank,
                  P2_CC    = round(cc, 4),
                  P2_Rank  = rank(-cc, ties.method = "min"),
                  RankChanged = rank(-p1_score) != rank(-cc),
                  row.names = NULL)
cat("\n==== Problem 1 (AHP) vs Problem 2 (TOPSIS) ====\n"); print(cmp)
cat(sprintf("\n排名是否改變：%s\n",
            ifelse(any(cmp$RankChanged), "是 (兩種方法排序不同)",
                                             "否 (排序相同)")))

## ---------- 4. 繪圖 ----------------------------------------------------
png("P2_TOPSIS_scores.png", width = 1100, height = 600, res = 120)
par(mfrow = c(1, 2), mar = c(4, 5, 3, 1))

## 左：CC 貼近度
bp <- barplot(cc, names.arg = cities, col = "#cfe3ff", border = "grey40",
              ylim = c(0, 1), ylab = "Closeness Coefficient (CC)",
              main = "P2 TOPSIS: Closeness Coefficient")
abline(h = 0.5, lty = 2, col = "grey50")
text(bp, cc, labels = sprintf("%.3f", cc), pos = 3, cex = 0.9)

## 右：與 P1 分數對照
m <- rbind(P1 = p1_score/max(p1_score), P2 = cc/max(cc))  # 各自最大歸一化方便比較
barplot(m, beside = TRUE, col = c("#e6f0d5", "#ffe6cc"),
        names.arg = cities, legend.text = c("P1 AHP", "P2 TOPSIS"),
        args.legend = list(x = "topright", bty = "n", cex = 0.85),
        ylab = "Normalized Score", main = "P1 vs P2 (各自最大值歸一化)")
dev.off()
cat("\n[圖檔已輸出] P2_TOPSIS_scores.png\n")
