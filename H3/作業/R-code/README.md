# International Marketing HW3 — R 程式

本資料夾為 HW3 四題自撰的 R 程式。**全部僅用 base R，不需安裝任何套件。**
環境：R 4.x（本機驗證版本 R 4.6.1）。

## 檔案

| 檔案 | 對應題目 | 內容 |
| --- | --- | --- |
| `P1_AHP.R` | Problem 1 | 二階層 AHP：準則權重、一致性 CR、各準則下城市得分、加權總分、階層圖 |
| `P2_TOPSIS.R` | Problem 2 | 以 TOPSIS 取代第二階層，PIS=5/NIS=1、沿用 P1 權重、CC 貼近度與排名、與 P1 比較 |
| `P3_Kano.R` | Problem 3 | Kano：delight / disgust / entropy 權重、分類、散布圖，並輸出 `.rds` |
| `P4_BIP.R` | Problem 4 | 二元整數規劃：最大化 delight / 最小化 disgust、三段價格最佳配置、敏感度分析 |
| `run_all.R` | 全部 | 依序執行四題 |
| `check_data.py` | 驗證 | 檢查三個 CSV 能否重現手冊上的所有數字（全部通過印 `ALL PASS`） |

## 資料檔（在作業資料夾，R-code 的上一層）

| 檔案 | 內容 | 哪些題用到 |
| --- | --- | --- |
| `ahp_weights.csv` | 各成員對六準則的權重（每列加總 = 1） | P1 |
| `city_scores.csv` | 各城市 × 六準則的 1~5 評分（同組員同城市可多列，程式取平均） | P1、P2 |
| `kano_survey.csv` | 各成員 × 功能的 A/M/O/R/I/Q 計數（非負整數） | P3 |

**要換資料直接改 CSV**（用 Excel 開可編輯，記得存檔後重跑）；程式會自動算平均與百分比。

## 執行方式

程式開頭會**自動定位**到作業資料夾（Console 印「工作目錄: ...」），
因此**從任何工作目錄執行都可以**，圖與 rds 都輸出在作業資料夾：

```
Rscript "...\H3\作業\R-code\run_all.R"
```

或在 RStudio 開啟 `run_all.R` → Source。

## 產出（都在作業資料夾，不是 R-code 裡）

- 圖檔：`P1_AHP_hierarchy.png`、`P2_TOPSIS_scores.png`、`P3_Kano_plot.png`、`P4_BIP_config.png`
- 中介資料：`P1_ahp_result.rds`（P2 讀取）、`P3_kano_result.rds`（P4 讀取）
  - rds 不存在時，P2/P4 會改用內建示範值並印提示訊息，**請按 P1→P2→P3→P4 順序執行**。

## 已知原文疑義（程式中已處理）

- **Problem 4**：foldable screen 額外成本原文為 **155000**，遠大於其他功能且超過所有價格上限；
  程式最後附**敏感度分析**（15000 / 5000）供對照。建議向老師確認正確數值。
- **Problem 1**：題目寫 "the average weights of the four criteria"，但全文列 **六項** 準則，
  程式一律以六項準則處理。
- **Kano 公式**：本程式採課程範本 `多準則決策.xlsx` 的版本
  `delight=(A+O−I)/N`、`disgust=−(M+O−R)/N`（分母 N 不含 Q）；若講義版本不同請自行替換。
