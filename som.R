library(kohonen2)
data("wines")
library("RColorBrewer")

wines.sc <- scale(wines)
wine.som <- som(data = wines.sc, grid = somgrid(5, 4, "hexagonal"))
plot(wine.som, type = "property",proterty= wine.sc$a,main = "Wine data",palette.name=colorRampPalette(brewer.pal(8,"Blues"))))




