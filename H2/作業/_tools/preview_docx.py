# -*- coding: utf-8 -*-
"""用 Word COM 把 docx 轉 PDF，再用 PyMuPDF 把每頁渲染成 PNG，方便檢視排版。"""
import os
import sys
import fitz
import win32com.client

src = os.path.abspath(sys.argv[1])
pdf = os.path.abspath(sys.argv[2])
outdir = os.path.abspath(sys.argv[3])
os.makedirs(outdir, exist_ok=True)

word = win32com.client.Dispatch("Word.Application")
word.Visible = False
try:
    d = word.Documents.Open(src, ReadOnly=True)
    d.SaveAs(pdf, FileFormat=17)  # 17 = wdFormatPDF
    d.Close(False)
finally:
    word.Quit()

doc = fitz.open(pdf)
print("pages:", doc.page_count)
for i, page in enumerate(doc):
    pix = page.get_pixmap(dpi=110)
    fn = os.path.join(outdir, f"page{i+1}.png")
    pix.save(fn)
print("rendered", doc.page_count, "pages ->", outdir)
