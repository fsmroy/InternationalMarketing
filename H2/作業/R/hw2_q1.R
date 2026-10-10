# ============================================================
# 國際行銷 HW2 — 題目一：市場區隔統計檢定（bank 資料集）
# 用法：RStudio 開啟本檔 → 逐段選取 → Ctrl+Enter 執行
# ============================================================

# --- 0. 環境設定 ---
setwd("D:/OneDrive/OneDrive - PEGATRON CORPORATION/交大/國際行銷/H2/作業")

# --- 1. 讀取資料（注意：分隔符是分號 ;） ---
bank <- read.csv("MKT_data/bank.csv", sep = ";")
str(bank)   # 檢查欄位型態
head(bank)

# --- 2. 移除指定欄位 ---
drop_cols <- c("job", "contact", "pdays", "poutcome", "month", "day")
bank2 <- bank[, setdiff(names(bank), drop_cols)]
names(bank2)

# --- 3. 欄位分類 ---
cat_cols <- c("marital", "education", "default", "housing", "loan")   # 類別型
num_cols <- c("age", "balance", "duration", "campaign", "previous")   # 數值型

# --- 4. 類別型特徵：卡方檢定 ---
# H0：該特徵與 y 互相獨立；p < 0.05 → 與是否參與促銷顯著相關
chi_table <- data.frame(feature = cat_cols,
                        chi2    = numeric(length(cat_cols)),
                        df      = numeric(length(cat_cols)),
                        p.value = numeric(length(cat_cols)))
for (i in seq_along(cat_cols)) {
  ct <- chisq.test(table(bank2[[cat_cols[i]]], bank2$y))
  chi_table[i, ] <- list(cat_cols[i],
                         round(unname(ct$statistic), 2),
                         unname(ct$parameter),
                         signif(ct$p.value, 4))
}
chi_table   # ← 寫報告用：p < 0.05 的特徵適合做市場區隔

# --- 5. 數值型特徵：F 檢定（變異數）→ T 檢定（平均數） ---
# F 檢定 H0：兩組變異數相等；p >= 0.05 → 變異數相等 → 用 var.equal=TRUE 的 T 檢定
# T 檢定 H0：兩組平均數相等；p < 0.05 → 兩組平均數顯著不同
num_table <- data.frame(feature   = num_cols,
                        F.p.value = numeric(length(num_cols)),
                        var.equal = logical(length(num_cols)),
                        t.stat    = numeric(length(num_cols)),
                        t.p.value = numeric(length(num_cols)))
for (i in seq_along(num_cols)) {
  v  <- num_cols[i]
  ft <- var.test(bank2[[v]] ~ bank2$y)                  # 步驟 1：F 檢定
  ve <- ft$p.value >= 0.05
  tt <- t.test(bank2[[v]] ~ bank2$y, var.equal = ve)    # 步驟 2：依 F 結果選 T 檢定
  num_table[i, ] <- list(v, signif(ft$p.value, 4), ve,
                         round(unname(tt$statistic), 2), signif(tt$p.value, 4))
}
num_table   # ← t.p.value < 0.05 的特徵適合做市場區隔

# --- 6. 結論方向（報告用） ---
# 適合區隔的特徵 = 卡方 p<0.05 的類別特徵 + T 檢定 p<0.05 的數值特徵
# 管理意涵：針對顯著特徵刻畫目標客群輪廓，分配行銷資源
