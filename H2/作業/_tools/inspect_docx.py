# -*- coding: utf-8 -*-
"""檢視 HW2 docx 結構（唯讀）。"""
import sys, io, docx
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding="utf-8")

PATH = r"d:\OneDrive\OneDrive - PEGATRON CORPORATION\交大\國際行銷\H2\作業\2026-MKT-HW2.docx"
d = docx.Document(PATH)

for i, p in enumerate(d.paragraphs):
    has_num = p._p.pPr is not None and p._p.pPr.numPr is not None
    runs = [(r.text, r.bold, r.italic, r.font.size.pt if r.font.size else None) for r in p.runs]
    print(f"=== para {i} | style={p.style.name} | numbering={has_num} | runs={len(runs)} ===")
    for t, b, it, sz in runs:
        print(f"    run len={len(t):4d} bold={b} italic={it} size={sz} :: {t[:80]!r}")
    print()
