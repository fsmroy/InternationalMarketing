"""列出資料集：檔名、列數、欄位數、欄位名（前 12 個）。

用法（PowerShell 5.1）：
    & $py doc\\_tools\\list_datasets.py <本機路徑> <顯示相對根目錄>
一次處理多個資料夾，輸出可直接貼進 md 表格。
"""
import sys, os, csv, io

ROOT = os.path.abspath(sys.argv[1])
SHOW = sys.argv[2] if len(sys.argv) > 2 else ROOT
MAXCOL = 14


def probe(path):
    """回傳 (rows, ncol, cols) 或 None。支援 csv / txt / tsv。"""
    for enc in ('utf-8-sig', 'utf-8', 'big5', 'latin-1'):
        try:
            f = io.open(path, 'r', encoding=enc, newline='')
        except OSError:
            return None
        try:
            # 自動判分隔符（csv 逗號、txt 有時 tab 或空白）
            sample = f.read(4096)
            f.seek(0)
            delim = ','
            if path.lower().endswith('.txt'):
                delim = '\t' if sample.count('\t') > sample.count(',') else ','
                if '\t' not in sample and ',' not in sample:
                    return None  # 空白分隔的自由格式，交由人工判讀
            rdr = csv.reader(f, delimiter=delim)
            rows = [r for r in rdr if r and any(c.strip() for c in r)]
            f.close()
            if not rows:
                return None
            # 跳過 R 匯出常見的 row.names 首欄空白標題
            hdr = [c.strip().strip('"') for c in rows[0]]
            if hdr and hdr[0] == '':
                hdr[0] = '(row.names)'
            return len(rows) - 1, len(rows[0]), hdr
        except UnicodeDecodeError:
            f.close()
            continue
        except Exception:
            f.close()
            return None
    return None


def main():
    files = []
    for dp, _dn, fn in os.walk(ROOT):
        for n in fn:
            if n.lower().endswith(('.csv', '.txt', '.tsv')):
                files.append(os.path.join(dp, n))
    files.sort()
    for p in files:
        rel = os.path.relpath(p, ROOT).replace(os.sep, '/')
        r = probe(p)
        if r is None:
            print('%s|?|?|無法解析（自由格式/編碼問題）' % rel)
            continue
        rows, ncol, cols = r
        shown = ', '.join(cols[:MAXCOL]) + ('…' if len(cols) > MAXCOL else '')
        print('%s|%d|%d|%s' % (rel, rows, ncol, shown))


main()
