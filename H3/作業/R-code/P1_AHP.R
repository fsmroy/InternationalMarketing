############################################################
# International Marketing HW3 -- Problem 1: AHP (two-level)
# 目的：層級分析法評估三個港口城市 Kaohsiung / Busan / Tokyo
# 準則：living cost, natural environment, city tourism,
#       food, transportation, courtesy  (共 6 項)
# 資料：讀取作業資料夾下的 ahp_weights.csv（成員準則權重）與
#       city_scores.csv（城市 × 準則 1~5 評分，P2 也用這份）
# 產出：1) 階層圖  2) 準則平均權重(含一致性)  3) 各準則下城市得分
#       4) 城市加權總分  5) P1_ahp_result.rds（供 P2 讀取）
# 環境：純 base R，不需安裝任何套件 (R >= 4.x)
############################################################

## ---------- 0. 環境設定：自動定位「作業」資料夾（不寫死絕對路徑） --------
## 1) Rscript：從命令列 --file= 抓本檔路徑
## 2) RStudio 開啟本檔按 Ctrl+Enter：用 rstudioapi 取得本檔路徑
## 3) 保險：往上逐層找含 ahp_weights.csv 的資料夾
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
cities   <- c("Kaohsiung", "Busan", "Tokyo")          # 三個城市
criteria <- c("LivingCost", "NatEnvironment", "CityTourism",
              "Food", "Transportation", "Courtesy")   # 六項準則
stopifnot(length(cities) == 3, length(criteria) == 6)

## ---------- 2. 讀入團隊資料 ---------------------------------------------
## 2a. ahp_weights.csv：每位成員對六項準則的權重（每列一人，各列和 = 1）。
##     平均權重 = 各成員權重取「欄平均」。
##     ▶ 正式作答時，請把 CSV 換成你們團隊的實際資料。
##     AHP 1-9 尺度：1=同等重要、3=稍重要、5=重要、7=很重要、9=極重要；
##     成對比較矩陣的標準寫法是「比值」a_ij = w_i / w_j。
##     若你拿到的是成員的上三角 1-9 整數，改用 pairwise_from_upper() 輸入。
ahp_raw <- read.csv("ahp_weights.csv")
stopifnot(all(abs(rowSums(ahp_raw[, criteria]) - 1) < 1e-6))   # 每列權重和 = 1

## ---------- 2. 輔助函式 -------------------------------------------------
## (a) 由上三角向量組出完整正倒數矩陣 (a_ji = 1 / a_ij)
pairwise_from_upper <- function(upper, labels) {
  n <- length(labels)
  M <- matrix(0, n, n, dimnames = list(labels, labels))
  M[upper.tri(M)] <- upper
  M <- M + t(M)
  diag(M) <- 1
  M
}
## (b) 由權重向量生成完全一致的成對比較矩陣 (a_ij = w_i / w_j)
pairwise_from_weights <- function(w) {
  M <- outer(w, w, "/")
  dimnames(M) <- list(names(w), names(w))
  M
}

## 隨機指標 RI (Saaty)，依矩陣階數 n
RI_table <- c(`1` = 0, `2` = 0, `3` = 0.58, `4` = 0.90, `5` = 1.12,
              `6` = 1.24, `7` = 1.32, `8` = 1.41, `9` = 1.45,
              `10` = 1.49)

## AHP 權重計算（Saaty 特徵向量法，並回報一致性）
ahp_weights <- function(M) {
  n     <- nrow(M)
  ev    <- eigen(M)                       # 求特徵值/特徵向量
  lam   <- Re(ev$values[1])               # 最大特徵值
  w     <- Re(ev$vectors[, 1])            # 對應特徵向量
  w     <- abs(w) / sum(abs(w))           # 正規化使和為 1
  names(w) <- rownames(M)
  CI <- (lam - n) / (n - 1)               # 一致性指標
  CR <- if (n > 2) CI / RI_table[as.character(n)] else 0  # 一致性比率
  list(weights = w, lambda_max = lam, CI = CI, CR = CR)
}

## ---------- 4. Level-1：準則權重 ----------------------------------------
## 成員權重取欄平均 → 由「權重比」生成成對比較矩陣（完全一致）→ AHP 求解
w_mean <- colMeans(ahp_raw[, criteria])
A      <- pairwise_from_weights(w_mean)
res_A  <- ahp_weights(A)

## ▶ 若你是「手動填上三角」，改用下一行即可（1-9 整數、行 vs 欄）：
##   A <- pairwise_from_upper(c(3,5,2,3,4, 2,1/2,1/3,2, 3,2,1, 1/3,1/3, 1/2),
##                            criteria)

cat("==== Level-1 準則層 ====================\n")
cat("成對比較矩陣 A =\n"); print(round(A, 3))
cat("\n準則權重 (特徵向量法) =\n"); print(round(res_A$weights, 4))
cat(sprintf("\nlambda_max = %.4f\nCI = %.4f\nCR = %.4f  ->  %s\n",
            res_A$lambda_max, res_A$CI, res_A$CR,
            ifelse(res_A$CR < 0.1, "通過一致性 (<0.1)", "未通過，需修正矩陣")))

cat("\n【準則平均權重】(各成員權重 colMeans)\n"); print(round(w_mean, 4))

cat("\n各成員權重一覽：\n"); print(ahp_raw)

## ---------- 5. Level-2：各準則下的城市得分 ----------------------------
## 讀 city_scores.csv（member,city + 六準則 1~5 評分），先取各城市平均，
## 再把每項準則內的評分「比例化」為三城市的相對權重（分數越高相對越佳），
## 對每項準則用該權重建立 3x3 成對比較矩陣後求權重。
survey  <- read.csv("city_scores.csv")
stopifnot(all(unlist(survey[, criteria]) >= 1 & unlist(survey[, criteria]) <= 5))
X_mean <- sapply(criteria, function(cr) tapply(survey[[cr]], survey$city, mean))
X_mean <- X_mean[cities, , drop = FALSE]                        # 3 x 6
cat("\n各城市 × 準則 平均評分 (1~5)：\n"); print(round(X_mean, 2))

score_by_criteria <- sapply(criteria, function(cr) {
  w <- setNames(X_mean[, cr] / sum(X_mean[, cr]), cities)       # 比例化
  ahp_weights(pairwise_from_weights(w))$weights
})
score_by_criteria <- score_by_criteria[cities, , drop = FALSE]  # 3 x 6
cat("\n==== Level-2 城市 x 準則 得分矩陣 ====\n")
print(round(score_by_criteria, 4))

## ---------- 6. 城市加權總分 --------------------------------------------
total_score <- as.vector(score_by_criteria %*% w_mean)
names(total_score) <- cities
rank_city <- rank(-total_score, ties.method = "min")

cat("\n==== 加權總分與排名 ====\n")
out <- data.frame(City = cities,
                  WeightedScore = round(total_score, 4),
                  Rank = rank_city,
                  row.names = NULL)
print(out)

## ---------- 7. 繪圖：階層圖 + 分數長條圖 --------------------------------
png("P1_AHP_hierarchy.png", width = 1100, height = 650, res = 120)

par(mar = c(1, 1, 3, 1))
plot(NA, xlim = c(0, 10), ylim = c(0, 10), axes = FALSE,
     xlab = "", ylab = "", main = "AHP Hierarchy & Weighted Scores")

## 節點座標
goal_x <- 5;    goal_y <- 9.2
crit_y <- 5.5;  city_y <- 1.8
crit_x <- seq(0.9, 9.1, length.out = 6)
city_x <- c(2.5, 5, 7.5)

## 連線：Goal -> 準則
for (i in 1:6) segments(goal_x, goal_y - 0.5, crit_x[i], crit_y + 0.5,
                        col = "grey60")

## 連線：準則 -> 城市
for (i in 1:6) for (j in 1:3)
  segments(crit_x[i], crit_y - 0.5, city_x[j], city_y + 0.5, col = "grey80")

## 畫節點
draw_box <- function(x, y, w, h, label, fill, cex = 1) {
  rect(x - w/2, y - h/2, x + w/2, y + h/2, col = fill, border = "grey40")
  text(x, y, label, cex = cex)
}
draw_box(goal_x, goal_y, 3.4, 0.9, "Goal:\nBest Harbor", "#cfe3ff", 1.05)
for (i in 1:6)
  draw_box(crit_x[i], crit_y, 1.5, 1.0,
           sprintf("%s\n%.3f", criteria[i], w_mean[i]), "#e6f0d5", 0.85)
for (j in 1:3)
  draw_box(city_x[j], city_y, 2.0, 1.0,
           sprintf("%s\n%.3f", cities[j], total_score[j]), "#ffe6cc", 0.95)

## 右上角加上排名註解
legend("right", legend = paste0("#", rank_city, " ", cities),
       bty = "n", cex = 0.9)

dev.off()

cat("\n[圖檔已輸出] P1_AHP_hierarchy.png (階層圖 + 加權分數)\n")
cat("\n==== 結果摘要 ====\n")
print(out)

## ---------- 8. 匯出供 Problem 2 使用 -----------------------------------
## 把權重、城市得分矩陣、加權總分寫成 RDS，P2 可直接讀取（取代手動同步）
saveRDS(list(weights = w_mean, score_by_criteria = score_by_criteria,
             total_score = total_score, cities = cities, criteria = criteria),
        "P1_ahp_result.rds")
cat("[資料已輸出] P1_ahp_result.rds (供 Problem 2 讀取)\n")
