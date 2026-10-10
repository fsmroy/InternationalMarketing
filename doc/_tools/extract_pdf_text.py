"""抽取投影片 PDF 的內文（PyMuPDF）。

用法：
    python extract_pdf_text.py <PDF資料夾> <輸出資料夾>

每個 PDF 產出一個同名 .txt，內含逐頁文字並以 ===== p{N} ===== 分隔。
若某 PDF 的總字元數偏低（< 200），代表可能是掃描/圖片式投影片，需靠檔名判讀。
"""
import sys
import pathlib

import fitz  # PyMuPDF


def main() -> None:
    src = pathlib.Path(sys.argv[1])
    out = pathlib.Path(sys.argv[2])
    out.mkdir(parents=True, exist_ok=True)
    for pdf in sorted(src.glob("*.pdf")):
        doc = fitz.open(pdf)
        pages = [f"\n===== p{i} =====\n{page.get_text().strip()}"
                 for i, page in enumerate(doc, 1)]
        text = "\n".join(pages)
        (out / f"{pdf.stem}.txt").write_text(text, encoding="utf-8")
        print(f"{pdf.name}|pages={len(doc)}|chars={len(text)}")


if __name__ == "__main__":
    main()
