# -*- coding: utf-8 -*-
"""傾印 docx 的「純文字 + 表格內容」順序，用來比對美化前後內容是否一致。
用法: python dump_text.py <docx路徑> <輸出txt路徑>"""
import sys
import docx
from docx.oxml.ns import qn

src, out = sys.argv[1], sys.argv[2]
d = docx.Document(src)
lines = []
for el in d.element.body.iterchildren():
    tag = el.tag.split("}")[1]
    if tag == "p":
        lines.append("P | " + "".join(n.text or "" for n in el.iter(qn("w:t"))))
    elif tag == "tbl":
        for tr in el.iter(qn("w:tr")):
            cells = ["".join(t.text or "" for t in tc.iter(qn("w:t")))
                     for tc in tr.findall(qn("w:tc"))]
            lines.append("R | " + " | ".join(cells))
open(out, "w", encoding="utf-8").write("\n".join(lines))
print("wrote", out, len(lines), "lines")
