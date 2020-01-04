#________________________________________________________#
# Project: Perceptual Map Microcredito  Churn
# Area: Data Analitics
#________________________________________________________#


# *** ST: libraries
library(ggplot2) # 
library(factoextra) # 
library(FactoMineR)#

# *** END: libraries

# *** ST: Loading data
base_CBS
class(base_CBS) 
# *** END: Loading data  

#PCA
names(base_CBS[,-c(1:2)])
pca <- princomp(base_CBS[,-1] [,-c(1:2)])
class(pca)
plot(pca)
pca$scores

#pca opcion 2 

plot(hclust(dist(cor(base_CBS))), hang = -1, cex=0.5)
m.pca <- prcomp(base_CBS[,-1],center =T, scale. = T) 
summary(m.pca)
plot(m.pca)

plot(m.pca, c(1,2), )

#kmeans
fr<-kmeans(base_CBS[,-1], centers = 4)
table(fr$cluster)
barplot(table(fr$cluster))
kmeans(m.pca$rotation)
dim(m.pca$x)
class(m.pca$x)


m.pca$x[,c(1,2)]

frq<-kmeans(m.pca$x[,c(1,2)], centers = 6)
plot(m.pca$x[,1], m.pca$x[,2], col=frq$cluster)

library(ggplot2)
dff <- data.frame(xxx=m.pca$rotation[,1], yyy=m.pca$rotation[,2])
df <- data.frame(xx=m.pca$x[,1], yy=m.pca$x[,2], cluster=frq$cluste)
ggplot() + geom_point(data=df, aes(x=xx, y=yy, col=cluster), size=0.5) +
  geom_point(data=dff, aes(x=xxx, y=yyy), size=1, col="red") +
  geom_label  (row.names(dff), aes(x=xxx, y=yyy))

class(dff$yyy)

library(FactoMineR)
library(factoextra)

#loadingFactomineR

library(FactoMineR)
res<-PCA(base_CBS[,1:10])
summary(res)
cor(base_CBS)






library(factoextra)

res.pca <- PCA(base_CBS, graph = FALSE)
#fviz_screeplot(res.pca,addlabels = TRUE, ylim = c(0, 5))
fviz_pca_var(res.pca, col.var="contrib",axes = c(1, 5),
#             gradient.cols = c("#00AFBB", "#FC4E07"),
             geom = c("arrow", "text"))


cor(base_CBS)

Factor Analysis
# Principal Axis Factor Analysis
library(psych)
fit <- factor.pa(base_CBS, nfactors=3, rotation="varimax")
fit # print results


library(FactoMineR)
grupos <- kmeans(base_CBS, centers = 3)
res.pca <- PCA(base_CBS, graph = FALSE)
fviz_screeplot(res.pca,addlabels = TRUE, ylim = c(0, 100))
fviz_pca_var(res.pca, col.var="contrib",axes = c(1, 2),
             gradient.cols = c("#00AFBB", "#FC4E07"),
             geom = c("arrow", "text"))

fviz_pca_biplot(res.pca, label = "var", habillage=grupos$cluster,
                addEllipses=TRUE, ellipse.level=0.95,
                ggtheme = theme_minimal())




# ************** Multidimentional Scaling

base_CBS.dist <- dist(base_CBS.mean)
base_
sdat.mds <- cmdscale(sdat.dist)

plot(sdat.mds, type = "n")
text(sdat.mds,rownames(sdat.mds),cex = 2)
# similar a lo obtenido en el mapa perceptual


# NON metric MDS
sdat.rank <- data.frame(lapply(sdat.mean, function(x) ordered(rank(x))))
str(sdat.rank)


library(cluster)
sdat.dist.r <- daisy(sdat.rank, metric="gower")


sdat.mds.r <- isoMDS(sdat.dist.r)

plot(sdat.mds.r$points, type="n")
text(sdat.mds.r$points, levels(sdat$brand), cex=2)

_
install.packages("devtools") 
library(devtools) 
install_github("vqv/ggbiplot")
_



library(devtools)
install_github("vqv/ggbiplot")

library(ggbiplot)
base_CBS
class(base_CBS) 
dim(base_CBS)
#base_CBS <- read.csv("C:/Users/USER/Desktop/PCA+Clusters/base_CBS.text", sep="")
res.pca <- prcomp(base_CBS, scale. = TRUE)
screeplot(res.pca, npcs=10, main="Gr?fico de sedimentaci?n de PCA")
grupos <- kmeans(res.pca$x[, 1:2], centers = 3)

ggbiplot(res.pca, obs.scale = 1, var.scale = 1,
         groups = factor(grupos$cluster), ellipse = TRUE, circle = TRUE) +
  geom_point(data=data.frame(grupos$centers), aes(x=PC1, y=PC2), size=4) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')
_

