# R 環境測試 — 游標放任一行按 Ctrl+Enter 逐行送進 R 終端機執行
x <- rnorm(200, mean = 170, sd = 8)   # 模擬 200 筆身高資料
summary(x)                            # 統計摘要 → 應出現在 R 終端機
hist(x, col = "steelblue", main = "R 環境測試")   # 直方圖 → 應出現在 httpgd 繪圖視窗
