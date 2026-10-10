############################################################
# International Marketing HW3 -- 總控腳本
# 依序執行 Problem 1~4 的 R 程式，產生所有表格與圖檔
# 用法：把工作目錄設到本資料夾後
#   Rscript run_all.R
# 或於 RStudio：setwd() 到此資料夾 → source("run_all.R")
############################################################

scripts <- c("P1_AHP.R", "P2_TOPSIS.R", "P3_Kano.R", "P4_BIP.R")
for (s in scripts) {
  cat("\n\n##################################################\n")
  cat("# 執行:", s, "\n")
  cat("##################################################\n")
  source(s, echo = FALSE)   # echo=FALSE 只看輸出；要看程式碼改 TRUE
}
cat("\n\n==== 全部完成。產出圖檔：====\n")
print(list.files(pattern = "\\.png$"))
