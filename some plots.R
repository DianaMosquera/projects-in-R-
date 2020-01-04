library(devtools)
install_github("vqv/ggbiplot")

library(ggbiplot)
base_SB
class(base_SB) 
dim(base_SB)
#base_SB <- read.csv("C:/Users/USER/Desktop/PCA+Clusters/base_SB.text", sep="")
res.pca <- prcomp(base_SB, scale. = TRUE)
screeplot(res.pca, npcs=10, main="Gr?fico de sedimentaci?n de PCA")
grupos <- kmeans(res.pca$x[, 1:2], centers = 3)

ggbiplot(res.pca, obs.scale = 1, var.scale = 1,
         groups = factor(grupos$cluster), ellipse = TRUE, circle = TRUE) +
  geom_point(data=data.frame(grupos$centers), aes(x=PC1, y=PC2), size=4) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')
_

