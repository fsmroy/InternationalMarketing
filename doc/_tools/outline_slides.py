"""列出每份投影片每一頁的標題行（跳過校系/實驗室頁眉）。"""
import pathlib
import re
import sys

SKIP = re.compile(r"大學|實驗室|電子化企業|^\s*$|^\s*\d+\s*$|Service Quality|交通大學")
src = pathlib.Path(sys.argv[1])
for txt in sorted(src.glob("*.txt")):
    print(f"\n########## {txt.stem} ##########")
    pages = txt.read_text(encoding="utf-8").split("===== p")
    for chunk in pages[1:]:
        head, _, body = chunk.partition("=====")
        lines = [l.strip() for l in body.splitlines() if l.strip()]
        title = next((l for l in lines if not SKIP.search(l)), "")
        if title:
            print(f"p{head.strip()}: {title[:70]}")
