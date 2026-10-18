# -*- coding: utf-8 -*-
"""列出 HW2 docx 可用的樣式（唯讀）。"""
import sys, io, docx
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding="utf-8")

PATH = r"d:\OneDrive\OneDrive - PEGATRON CORPORATION\交大\國際行銷\H2\作業\2026-MKT-HW2.docx"
d = docx.Document(PATH)
for s in d.styles:
    print(s.type, "|", s.name)
