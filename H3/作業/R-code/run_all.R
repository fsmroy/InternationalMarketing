############################################################
# International Marketing HW3 -- 總控腳本
# 依序執行 Problem 1~4 的 R 程式，產生所有表格與圖檔
# 各子程式會自動定位「作業」資料夾（讀 CSV、寫輸出都在那裡），
# 因此從任何工作目錄執行都可以：
#   Rscript R-code/run_all.R        (在作業資料夾下)
#   Rscript "H3/作業/R-code/run_all.R"  (從專案根目錄)
# 或於 RStudio：開啟本檔 → Source
############################################################

## 自動定位：切到「作業」資料夾（run_all.R 的上兩層），讓圖檔輸出在作業層
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
  if (!dir.exists("R-code")) {
    d <- getwd()
    repeat {
      up <- dirname(d); if (identical(up, d)) break
      d <- up
      if (dir.exists(file.path(d, "R-code"))) { setwd(d); break }
    }
  }
})
cat("工作目錄:", getwd(), "\n")

scripts <- file.path("R-code", c("P1_AHP.R", "P2_TOPSIS.R", "P3_Kano.R", "P4_BIP.R"))
for (s in scripts) {
  cat("\n\n##################################################\n")
  cat("# 執行:", s, "\n")
  cat("##################################################\n")
  source(s, echo = FALSE)   # echo=FALSE 只看輸出；要看程式碼改 TRUE
}
cat("\n\n==== 全部完成。產出圖檔：====\n")
print(list.files(pattern = "\\.png$"))
