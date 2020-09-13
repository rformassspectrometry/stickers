library("Features")

data(hlpsms)
hl <- readFeatures(hlpsms, ecol = 1:10, name = "psms")
hl <- combineFeatures(hl, "psms", "Sequence", name = "peptides")
hl <- combineFeatures(hl, "peptides", "ProteinGroupAccessions", name = "proteins")
hl


f1 <- hl["Q9Z265", ][[2]]
stat3 <- hl["P42227-2", ][[2]]
f3 <- hl["Q9WUU7", ][[2]]

pal <- hcl.colors(15, "Blues", rev = TRUE)
svg()
par(mar = c(0, 0, 0, 0))

## f1: 3/2:1/1
svg("f1-1.svg", width = 10, height = 3)
par(mar = c(0, 0, 0, 0))
image(t(assay(f1)), xaxt = "n", yaxt = "n", bty = "n", col = pal)
dev.off()

svg("f1-2%03d.svg", width = 10, height = 1)
par(mar = c(0, 0, 0, 0))
image(t(t(colMedians(assay(f1)[1:2, ]))), xaxt = "n", yaxt = "n", bty = "n", col = pal)
image(t(t(assay(f1)[3, ])), xaxt = "n", yaxt = "n", bty = "n", col = pal)
image(t(t(colMedians(assay(f1)))), xaxt = "n", yaxt = "n", bty = "n", col = pal)
dev.off()

## stat3: 8/3:2:3/1
svg("stat3-1.svg", width = 10, height = 8)
par(mar = c(0, 0, 0, 0))
image(t(assay(stat3)), xaxt = "n", yaxt = "n", bty = "n", col = pal)
dev.off()

svg("stat3-2%03d.svg", width = 10, height = 1)
par(mar = c(0, 0, 0, 0))
image(t(t(colMedians(assay(stat3)[1:3, ]))), xaxt = "n", yaxt = "n", bty = "n", col = pal)
image(t(t(colMedians(assay(stat3)[4:5, ]))), xaxt = "n", yaxt = "n", bty = "n", col = pal)
image(t(t(colMedians(assay(stat3)[6:8, ]))), xaxt = "n", yaxt = "n", bty = "n", col = pal)
image(t(t(colMedians(assay(stat3)))), xaxt = "n", yaxt = "n", bty = "n", col = pal)
dev.off()

## f3: 4/2:2/1
svg("f3-1.svg", width = 10, height = 4)
par(mar = c(0, 0, 0, 0))
image(t(assay(f3)), xaxt = "n", yaxt = "n", bty = "n", col = pal)
dev.off()

svg("f3-2%03d.svg", width = 10, height = 1)
par(mar = c(0, 0, 0, 0))
image(t(t(colMedians(assay(f3)[1:2, ]))), xaxt = "n", yaxt = "n", bty = "n", col = pal)
image(t(t(colMedians(assay(f3)[3:4, ]))), xaxt = "n", yaxt = "n", bty = "n", col = pal)
image(t(t(colMedians(assay(f3)))), xaxt = "n", yaxt = "n", bty = "n", col = pal)
dev.off()

## In inkscape, combine all feature heatmap, resizing at width = 500
## and heigth =  50 x nb samples.

## Export to png at 1577 px width
