############################################################
# International Marketing HW3 -- Problem 2: TOPSIS (level-2)
# 承接 Problem 1：把「第二階層」的 AHP 評估改用 TOPSIS
#  - 五點李克特量表 (1~5)，資料來源：city_scores.csv 各城市平均
#  - PIS = (5,5,5,5,5,5), NIS = (1,1,1,1,1,1)  ← 題目指定
#  - 準則權重自動讀取 P1 產出的 P1_ahp_result.rds（不需手動同步）
# 產出：三城市的加權 TOPSIS 分數 (貼近度 CC) 與排名，並與 P1 比較
# 環境：純 base R，不需任何套件
############################################################

## ---------- 0. 環境設定：自動定位「作業」資料夾（同 P1） ----------------
local({
  p <- ""
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(a)) p <- sub("^--file=", "", a[1])
  if (!nzchar(p) && requireNamespace("rstudioapi", quietly = TRUE) &&
      isTRUE(rstudioapi::isAvailable()))
    tryCatch({ ctx <- rstudioapi::getSourceEditorContext()
               if (!is.null(ctx) && nzchar(ctx$path)) p <- ctx$path },
             error = function(e) NULL)
  if (nzchar(p)) {
    p2 <- suppressWarnings(tryCatch(normalizePath(p), error = function(e) ""))
    if (nzchar(p2) && file.exists(p2)) setwd(dirname(dirname(p2)))  # R-code -> 作業
  }
  if (!file.exists("ahp_weights.csv")) {
    d <- getwd()
    repeat {
      up <- dirname(d); if (identical(up, d)) break
      d <- up
      if (file.exists(file.path(d, "ahp_weights.csv"))) { setwd(d); break }
    }
  }
  if (!file.exists("ahp_weights.csv"))
    stop("無法自動定位作業資料夾（找不到 ahp_weights.csv）：請確認本檔位於 H3/作業/R-code/ 且結構未變動。")
})
cat("工作目錄:", getwd(), "\n")

## ---------- 1. 基本設定 ------------------------------------------------
cities   <- c("Kaohsiung", "Busan", "Tokyo")
criteria <- c("LivingCost", "NatEnvironment", "CityTourism",
              "Food", "Transportation", "Courtesy")

## ---------- 2. 讀入資料 -------------------------------------------------
## 2a. 準則權重：自動讀 P1 產出的 P1_ahp_result.rds。
##     ▶ 若找不到（例如單獨執行本檔），才改用下方手動值（請與 P1 保持一致）。
if (file.exists("P1_ahp_result.rds")) {
  p1   <- readRDS("P1_ahp_result.rds")
  w    <- p1$weights
  p1_score <- p1$total_score                     # 供第 4 節與 P1 比較
  message("[info] 已讀取 P1_ahp_result.rds（權重與 P1 自動同步）")
} else {
  ## ▶ 手動填入 P1 的平均權重（執行 P1 後照其輸出更新）
  w <- c(LivingCost = 0.22, NatEnvironment = 0.18, CityTourism = 0.16,
         Food = 0.16, Transportation = 0.15, Courtesy = 0.13)
  ## ▶ 手動填入 P1 的加權總分（僅供比較圖，照 P1 輸出更新）
  p1_score <- c(Kaohsiung = 0.3471, Busan = 0.3421, Tokyo = 0.3108)
  message("[info] 找不到 P1_ahp_result.rds，改用內建示範權重")
}
stopifnot(abs(sum(w) - 1) < 1e-6)

## 2b. 三城市在六項準則的評分 (五點李克特量表, 1=很差 ~ 5=很好)。
##     讀 city_scores.csv（P1 Level-2 同一份資料），取各城市平均。
##     注意「成本」屬效益型：分數越高代表成本越令人滿意 (越便宜)。
survey <- read.csv("city_scores.csv")
stopifnot(all(unlist(survey[, criteria]) >= 1 & unlist(survey[, criteria]) <= 5))
X <- sapply(criteria, function(cr) tapply(survey[[cr]], survey$city, mean))
X <- X[cities, , drop = FALSE]
colnames(X) <- criteria

## 2c. 題目指定之正/負理想解
PIS <- rep(5, length(criteria))
NIS <- rep(1, length(criteria))
names(PIS) <- names(NIS) <- criteria

## ---------- 4. TOPSIS 計算 ---------------------------------------------
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

## ---------- 5. 與 Problem 1 (AHP) 結果比較 ------------------------------
## p1_score 已在第 2 節由 P1_ahp_result.rds 自動帶入（或手動備援值）
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

## ---------- 6. 繪圖 ----------------------------------------------------
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
