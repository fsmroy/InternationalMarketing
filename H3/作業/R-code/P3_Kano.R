############################################################
# International Marketing HW3 -- Problem 3: Kano Model
# 評估智慧型手機四項功能：water proof / optical zoom /
#                        wireless charging / foldable screen
# 讀取作業資料夾下的 kano_survey.csv（成員 × 功能的分類計數），
# 統計 A/M/O/R/I/Q 百分比後推導 delight 與 disgust。
# 公式 (與課程範本 多準則決策.xlsx 的 Kano 工作表一致)：
#   delight = (A + O - I) / (A + M + O + R + I)
#   disgust = -(M + O - R) / (A + M + O + R + I)
# 另附 entropy 客觀加權 (供 Problem 4 使用)
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
labels_zh <- c("water proof (防水)", "optical zoom (光學變焦)",
               "wireless charging (無線充電)", "foldable screen (摺疊螢幕)")

## ---------- 2. 讀入資料：Kano 問卷統計 ----------------------------------
## kano_survey.csv 格式：member,feature,A,M,O,R,I,Q。
## ▶ 資料設計：一列 = 一份受訪問卷對該功能的單一判定（1 或 0；本示範檔
##   為了精確重現團隊統計，以「計數」呈現 — 例如某成員回收 25 份問卷，
##   其中 3 份判 A、11 份判 M…）。兩種格式都可用：只要每欄是計數、
##   加總後換算百分比即可。
kcols   <- c("A", "M", "O", "R", "I", "Q")
kano_raw <- read.csv("kano_survey.csv")
stopifnot(all(kano_raw$feature %in% features))

cnt <- sapply(features, function(f)
  colSums(kano_raw[kano_raw$feature == f, kcols, drop = FALSE]))  # 6 x 4
cnt <- t(cnt)[features, kcols, drop = FALSE]                       # 4 x 6

kano_pct <- cnt / rowSums(cnt)
colnames(kano_pct) <- kcols
rownames(kano_pct) <- features

## 檢查每列和為 1
rs <- rowSums(kano_pct)
stopifnot(all(abs(rs - 1) < 1e-6))
cat("問卷計數總表（feature x 分類）：\n"); print(cnt)
cat("\n各功能有效問卷份數：", paste0(features, "=", rowSums(cnt),
                                        collapse = "  "), "\n")

## ---------- 3. 計算 delight / disgust ----------------------------------
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

## ---------- 4. Entropy 客觀加權 (與 Excel 範本一致) ---------------------
## 以 A..I 五類占比算 entropy，1-entropy 後正規化成權重。
## 用途：Problem 4 最大化 delight 時，可作為各功能的權重。
p_mat <- kano_pct[, c("A","M","O","R","I"), drop = FALSE]
H <- -rowSums(ifelse(p_mat > 0, p_mat * log(p_mat), 0)) / log(5)  # 除以 ln(5)
H[is.nan(H)] <- 0
d <- 1 - H
w_entropy <- d / sum(d)

## ---------- 5. 輸出 -----------------------------------------------------
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

## ---------- 6. 繪圖：delight vs disgust 散布 ---------------------------
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

## ---------- 7. 匯出供 Problem 4 使用 -----------------------------------
## 把 delight / disgust / weight 寫成 RDS，P4 可直接讀取
saveRDS(list(delight = delight, disgust = disgust,
             weight = w_entropy, features = features,
             category = cat_of),
        "P3_kano_result.rds")
cat("[資料已輸出] P3_kano_result.rds (供 Problem 4 讀取)\n")
