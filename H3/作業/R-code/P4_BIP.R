############################################################
# International Marketing HW3 -- Problem 4: BIP price segmentation
# 承接 Problem 3，用二元整數規劃 (BIP) 做價格基礎顧客分群
#  - 基礎成本 base = 10000 TWD
#  - 功能額外成本：waterproof 2500, optical zoom 3500,
#                  wireless charging 1500, foldable screen 155000(*)
#  - 目標：最大化 delight  或  最小化 disgust
#  - 三段價格：high 30000, medium 22000, low 15000
#  (*) 題目原文 foldable = 155000，遠大於其他功能且超過所有價格上限，
#      疑為筆誤，程式在最後做敏感度分析 (5000 / 15000) 供對照。
# 環境：純 base R (窮舉 2^4=16 種組合)。附 lpSolve 寫法於檔尾註解。
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
  if (!file.exists("kano_survey.csv")) {
    d <- getwd()
    repeat {
      up <- dirname(d); if (identical(up, d)) break
      d <- up
      if (file.exists(file.path(d, "kano_survey.csv"))) { setwd(d); break }
    }
  }
  if (!file.exists("kano_survey.csv"))
    stop("無法自動定位作業資料夾（找不到 kano_survey.csv）：請確認本檔位於 H3/作業/R-code/ 且結構未變動。")
})
cat("工作目錄:", getwd(), "\n")

## ---------- 1. 基本設定 ------------------------------------------------
features <- c("WaterProof", "OpticalZoom", "WirelessCharging", "FoldableScreen")
cost <- c(WaterProof = 2500, OpticalZoom = 3500,
          WirelessCharging = 1500, FoldableScreen = 155000)   # 原文數值
base_cost <- 10000
budget <- c(high = 30000, medium = 22000, low = 15000)

## ---------- 2. 讀取 Problem 3 的 delight / disgust ----------------------
## P3_Kano.R 會輸出 P3_kano_result.rds；存在就讀，否則用內建示範值。
if (file.exists("P3_kano_result.rds")) {
  k <- readRDS("P3_kano_result.rds")
  delight <- k$delight; disgust <- k$disgust; kweight <- k$weight
  message("[info] 已讀取 P3_kano_result.rds")
} else {
  delight <- c(WaterProof = 0.45, OpticalZoom = 0.74,
               WirelessCharging = 0.68, FoldableScreen = 0.33)
  disgust <- c(WaterProof = -0.80, OpticalZoom = -0.52,
               WirelessCharging = -0.52, FoldableScreen = -0.03)
  kweight <- c(WaterProof = 0.33, OpticalZoom = 0.33,
               WirelessCharging = 0.29, FoldableScreen = 0.06)
  message("[info] 找不到 P3_kano_result.rds，改用內建示範值")
}

## ---------- 3. 窮舉所有產品組合 ----------------------------------------
## 每一組 y = (y1..y4) 的 0/1 向量；共 2^4 = 16 組
grid <- expand.grid(rep(list(c(0, 1)), length(features)))
colnames(grid) <- features
Y <- as.matrix(grid[, features, drop = FALSE])            # 16 x 4 的 0/1 設計矩陣

grid$TotalCost <- base_cost + as.vector(Y %*% cost)
grid$Delight   <- as.vector(Y %*% delight)     # 最大化目標
grid$Disgust   <- as.vector(Y %*% disgust)     # 最小化目標 (值越負越好)

## ---------- 4. 對每個價格分群求解 BIP ----------------------------------
solve_segment <- function(bud, obj = c("delight", "disgust")) {
  obj <- match.arg(obj)
  feas <- grid[grid$TotalCost <= bud, , drop = FALSE]        # 成本限制式
  if (nrow(feas) == 0) return(NULL)
  if (obj == "delight") {
    best <- feas[which.max(feas$Delight), ]                  # max Σ delight*y
  } else {
    best <- feas[which.min(feas$Disgust), ]                  # min Σ disgust*y
  }
  on <- unlist(best[1, features, drop = TRUE]) == 1
  feat_on <- features[on]
  list(config = if (length(feat_on)) feat_on else "(無)",
       cost = best$TotalCost, delight = best$Delight, disgust = best$Disgust)
}

report <- function(obj) {
  cat(sprintf("\n==== 目標：%s ====\n",
              ifelse(obj == "delight", "最大化 delight",
                                    "最小化 disgust (值越負越好)")))
  out <- data.frame()
  for (seg in names(budget)) {
    r <- solve_segment(budget[seg], obj)
    out <- rbind(out, data.frame(
      Segment = seg, Budget = budget[seg],
      Configuration = paste(r$config, collapse = " + "),
      TotalCost = r$cost, Delight = round(r$delight, 3),
      Disgust = round(r$disgust, 3), row.names = NULL))
  }
  print(out)
  invisible(out)
}

cat("==== 功能成本與 Kano 指標 ====\n")
print(data.frame(feature = features, cost = as.vector(cost),
                 delight = as.vector(delight), disgust = as.vector(disgust),
                 row.names = NULL))

rep_delight <- report("delight")
rep_disgust <- report("disgust")

## ---------- 5. 敏感度分析：把 foldable 成本改成 5000 / 15000 -----------
## 說明題目 155000 是否為筆誤，並觀察最佳解是否改變。
sens <- function(foldable_cost) {
  cost2 <- cost; cost2["FoldableScreen"] <- foldable_cost
  g <- grid
  g$TotalCost <- base_cost + as.vector(Y %*% cost2)
  ans <- sapply(names(budget), function(seg) {
    feas <- g[g$TotalCost <= budget[seg], , drop = FALSE]
    if (!nrow(feas)) return("(無可行解)")
    b <- feas[which.max(feas$Delight), ]
    on <- unlist(b[1, features, drop = TRUE]) == 1
    z <- features[on]
    if (length(z)) paste(z, collapse = "+") else "(無)"
  })
  ans
}
cat("\n==== 敏感度分析 (最大 delight；foldable 成本不同) ====\n")
cat("foldable = 155000 (原文):", paste(sens(155000), collapse = " | "), "\n")
cat("foldable =  15000 (假設):", paste(sens(15000),  collapse = " | "), "\n")
cat("foldable =   5000 (假設):", paste(sens(5000),   collapse = " | "), "\n")

## ---------- 6. 繪圖：各分群最佳配置 ------------------------------------
png("P4_BIP_config.png", width = 1100, height = 600, res = 120)
par(mfrow = c(1, 2), mar = c(6, 4, 3, 1))
draw <- function(rep, title) {
  cfg <- strsplit(rep$Configuration, " \\+ ")
  allf <- features
  mat <- t(sapply(cfg, function(z) as.integer(allf %in% z)))
  colnames(mat) <- allf
  barplot(t(mat), beside = FALSE, col = rainbow(4),
          legend.text = allf, args.legend = list(x = "topright", bty = "n", cex = 0.8),
          names.arg = rep$Segment, las = 1,
          ylab = "included (1) / not (0)", main = title)
}
draw(rep_delight, "Max delight: best config")
draw(rep_disgust, "Min disgust: best config")
dev.off()
cat("\n[圖檔已輸出] P4_BIP_config.png\n")

############################################################
# 附錄：若安裝 lpSolve，可改寫成標準 BIP (以 high 段、max delight 為例)
# ----------------------------------------------------------
#   library(lpSolve)
#   f.obj <- delight                                   # 最大化係數
#   f.con <- matrix(cost, nrow = 1)                    # 成本限制式
#   f.dir <- "<="
#   f.rhs <- budget["high"] - base_cost                # 可支配預算
#   sol <- lp("max", f.obj, f.con, f.dir, f.rhs, all.bin = TRUE)
#   sol$solution                                       # 0/1 最佳解
#   # 最小化 disgust：lp("min", disgust, ...)
############################################################
