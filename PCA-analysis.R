### row is case and col is variables
library(ggbiplot)
data(wine)
wine.pca <- prcomp(wine, scale. = TRUE)
ggbiplot(wine.pca, obs.scale = 1, var.scale = 1,
  groups = wine.class, ellipse = TRUE, circle = TRUE) + # show label and circle
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')


