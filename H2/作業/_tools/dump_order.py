# -*- coding: utf-8 -*-
"""按 body 實際順序列出段落/表格（唯讀）。"""
import sys, io, docx
from docx.table import Table
from docx.text.paragraph import Paragraph

sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding="utf-8")
PATH = r"d:\OneDrive\OneDrive - PEGATRON CORPORATION\交大\國際行銷\H2\作業\2026-MKT-HW2.docx"
d = docx.Document(PATH)
body = d.element.body
i = 0
for child in body.iterchildren():
    tag = child.tag.split("}")[1]
    if tag == "p":
        p = Paragraph(child, d)
        t = p.text.strip()
        print(f"{i:3d} P  | {t[:78]}")
        i += 1
    elif tag == "tbl":
        tb = Table(child, d)
        first = " / ".join(c.text for c in tb.rows[0].cells)
        print(f"    TBL ({len(tb.rows)}x{len(tb.columns)}) | {first[:78]}")
    else:
        print(f"    <{tag}>")
