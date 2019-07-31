library("Features")

data(hlpsms)
hl <- readFeatures(hlpsms, ecol = 1:10, name = "psms")
hl <- combineFeatures(hl, "psms", "Sequence", name = "peptides")
hl <- combineFeatures(hl, "peptides", "ProteinGroupAccessions", name = "proteins")
hl


f1 <- hl["Q9Z265", ][[2]]
stat3 <- hl["P42227-2", ][[2]]
f3 <- hl["Q9WUU7", ][[2]]

svg()
par(mar = c(0, 0, 0, 0))

## f1: 3/2:1/1
image(t(assay(f1)), xaxt = "n", yaxt = "n", bty = "n")
image(t(t(colMedians(assay(f1)[1:2, ]))), xaxt = "n", yaxt = "n", bty = "n")
image(t(t(assay(f1)[3, ])), xaxt = "n", yaxt = "n", bty = "n")
image(t(t(colMedians(assay(f1)))), xaxt = "n", yaxt = "n", bty = "n")


## stat2: 8/3:2:3/1
image(t(assay(stat3)), xaxt = "n", yaxt = "n", bty = "n")
image(t(t(colMedians(assay(stat3)[1:3, ]))), xaxt = "n", yaxt = "n", bty = "n")
image(t(t(colMedians(assay(stat3)[4:5, ]))), xaxt = "n", yaxt = "n", bty = "n")
image(t(t(colMedians(assay(stat3)[6:8, ]))), xaxt = "n", yaxt = "n", bty = "n")
image(t(t(colMedians(assay(stat3)))), xaxt = "n", yaxt = "n", bty = "n")

## f3: 4/2:2/1
image(t(assay(f3)), xaxt = "n", yaxt = "n", bty = "n")
image(t(t(colMedians(assay(f3)[1:2, ]))), xaxt = "n", yaxt = "n", bty = "n")
image(t(t(colMedians(assay(f3)[3:4, ]))), xaxt = "n", yaxt = "n", bty = "n")
image(t(t(colMedians(assay(f3)))), xaxt = "n", yaxt = "n", bty = "n")

dev.off()

## In inkscape, combine all feature heatmap, resizing at width = 500
## and heigth =  50 x nb samples.

## Export to png at 1577 px width
