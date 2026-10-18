# 產生 Q4 MDS 二維定位圖 PNG
setwd("d:/OneDrive/OneDrive - PEGATRON CORPORATION/交大/國際行銷/H2/作業")

s <- read.csv("phone_scores.csv")
avg <- aggregate(s[, paste0("A", 1:10)], list(product = s$product), mean)
rownames(avg) <- avg$product
m <- as.matrix(avg[, -1])
d <- dist(m)
fit <- cmdscale(d, k = 2, eig = TRUE)
pts <- fit$points
eig <- fit$eig
pct <- round(100 * eig[1:2] / sum(eig[eig > 0]), 1)

png("_tools/mds_plot.png", width = 1500, height = 1050, res = 160)
par(mar = c(5, 5, 3, 1))
plot(pts, type = "n", xlab = "", ylab = "",
     xlim = c(min(pts[, 1]) - 1.6, max(pts[, 1]) + 1.2),
     ylim = c(min(pts[, 2]) - 0.7, max(pts[, 2]) + 0.6),
     axes = FALSE, main = "Smartphone Product Positioning (2D MDS)")
box()
axis(1, at = -4:3); axis(2, at = seq(-1, 1.5, 0.5))
abline(h = 0, v = 0, col = "grey75", lty = 2)
points(pts, pch = 19, col = "#1f5fa8", cex = 1.15)

# 逐點指定標籤方位（1下 2左 3上 4右），避開疊在一起的點
pos <- c("iPhone Pro Max" = 3, "iPhone Pro" = 1, "Samsung S" = 3,
         "Oppo X" = 1, "Xiaomi" = 4, "Samsung A" = 1, "Oppo A" = 3,
         "Redmi" = 4)
text(pts, labels = rownames(pts), pos = pos[rownames(pts)], cex = 0.9, offset = 0.55)

mtext(sprintf("Dim 1 (%.1f%%)   high-end flagship  ->  budget entry", pct[1]),
      side = 1, line = 3, cex = 0.85)
mtext(sprintf("Dim 2 (%.1f%%)   interface/ecosystem  ->  value for money", pct[2]),
      side = 2, line = 3, cex = 0.85)
dev.off()
cat("plot saved\n")
