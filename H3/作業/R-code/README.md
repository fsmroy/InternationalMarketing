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

## 執行方式

在終端機（工作目錄設為本資料夾）：

```
Rscript run_all.R
```

或在 RStudio 開啟 `run_all.R` → Source。

## 產出

- 圖檔：`P1_AHP_hierarchy.png`、`P2_TOPSIS_scores.png`、`P3_Kano_plot.png`、`P4_BIP_config.png`
- 中介資料：`P3_kano_result.rds`（P4 會自動讀取）

## 重要提醒（作答前務必替換示範資料）

程式中標有 `▶` 的區塊為**示範值**，請換成你們團隊的實際資料：

1. **P1**：`w_crit_demo`（六項準則權重）與 `w_city_demo`（三城市相對得分）。
   若你有成員的成對比較矩陣，改用 `pairwise_from_upper()`；多人時把各成員權重取 `colMeans` 即為平均權重。
2. **P2**：`w`（沿用 P1 結果）與 `X`（三城市六準則的 1~5 李克特評分）。
3. **P3**：`kano_pct`（四功能的 A/M/O/R/I/Q 百分比）。
4. **P4**：`cost`、`base_cost`、`budget`（通常不變）；功能成本沿用題目數值。

## 已知原文疑義（程式中已處理）

- **Problem 4**：foldable screen 額外成本原文為 **155000**，遠大於其他功能且超過所有價格上限；
  程式最後附**敏感度分析**（15000 / 5000）供對照。建議向老師確認正確數值。
- **Problem 1**：題目寫 "the average weights of the four criteria"，但全文列 **六項** 準則，
  程式一律以六項準則處理。
- **Kano 公式**：本程式採課程範本 `多準則決策.xlsx` 的版本
  `delight=(A+O−I)/N`、`disgust=−(M+O−R)/N`；若講義版本不同請自行替換。
