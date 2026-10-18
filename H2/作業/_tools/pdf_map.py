import sys, fitz

pdf = sys.argv[1]
d = fitz.open(pdf)
for i, pg in enumerate(d, 1):
    imgs = pg.get_images(full=True)
    lines = [x.strip() for x in pg.get_text().splitlines() if x.strip()]
    head = " / ".join(lines[:2])
    print(f"p{i}: imgs={len(imgs)} | {head[:90]}")
