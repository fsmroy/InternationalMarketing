# -*- coding: utf-8 -*-
"""HW3 資料檔自檢：三個 CSV 是否能重現原始 demo 數值。

用法（任何工作目錄皆可）：python R-code/check_data.py
CSV 位置：自動定位到本檔的上一層（作業資料夾）。
"""
import csv, os, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

def rows(name):
    with open(os.path.join(ROOT, name), encoding="utf-8-sig") as f:
        r = list(csv.DictReader(f))
    return r

ok = True
def check(label, cond, detail=""):
    global ok
    print(("PASS" if cond else "FAIL"), label, detail)
    ok = ok and cond

# --- 1. ahp_weights.csv：每列和=1；平均 = 0.22/0.18/0.16/0.16/0.15/0.13
w = rows("ahp_weights.csv")
cols = ["LivingCost","NatEnvironment","CityTourism","Food","Transportation","Courtesy"]
expect = [0.22,0.18,0.16,0.16,0.15,0.13]
for r in w:
    s = sum(float(r[c]) for c in cols)
    check(f"ahp row {r['member']} sum", abs(s-1) < 1e-9, f"={s}")
avg = [sum(float(r[c]) for r in w)/len(w) for c in cols]
check("ahp mean == demo", all(abs(a-e) < 1e-12 for a, e in zip(avg, expect)),
      f"avg={['%.4f'%a for a in avg]}")

# --- 2. city_scores.csv：平均 = P2 demo X 矩陣
s = rows("city_scores.csv")
demoX = {"Kaohsiung":[5,4,3,4,4,4], "Busan":[4,4,4,4,4,4], "Tokyo":[3,4,5,3,4,3]}
for city, exp in demoX.items():
    got = [sum(int(r[c]) for r in s if r["city"]==city)/sum(1 for r in s if r["city"]==city) for c in cols]
    check(f"likert mean {city}", all(abs(a-e) < 1e-12 for a, e in zip(got, exp)),
          f"got={got}")

# --- 3. kano_survey.csv：計數總和/20 = demo kano_pct
k = rows("kano_survey.csv")
demo_pct = {"WaterProof":[.15,.45,.35,0,.05,0],
            "OpticalZoom":[.40,.10,.42,0,.08,0],
            "WirelessCharging":[.38,.12,.40,0,.10,0],
            "FoldableScreen":[.30,.08,.20,.25,.17,0]}
kcols = ["A","M","O","R","I","Q"]
for feat, exp in demo_pct.items():
    tot = [sum(int(r[c]) for r in k if r["feature"]==feat) for c in kcols]
    got = [t/sum(tot) for t in tot]
    check(f"kano pct {feat}", all(abs(a-e) < 1e-12 for a, e in zip(got, exp)),
          f"counts={tot}")

print("\nALL PASS" if ok else "\nSOME FAILED")
sys.exit(0 if ok else 1)
