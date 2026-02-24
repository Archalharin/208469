newiris <- iris
newiris$Species <- NULL
#แบ่งกลุ่ม
kc <- kmeans(newiris, 3, algorithm = "MacQueen")

table(iris$Species, kc$cluster)

plot(newiris[c("Sepal.Length", "Sepal.Width")], col=kc$cluster)
points(kc$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=8, cex=2)

wss <- (nrow(newiris)-1)*sum(apply(newiris,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(newiris,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")


setwd("C:/Users/User 56/Downloads")
df <- read.csv("weather_new-1.csv",header=T)
df$play <- NULL
df$outlook <- as.factor(df$outlook)
df$windy <- as.factor(df$windy)
d <- dist(as.matrix(df), method="euclidean")
hclust(d, method="complete", members=NULL)

# คำสั่ง hclust สำหรับจัดกลุ่มข้อมูล
onehot = model.matrix(~ outlook -1, data=df)
dt = data.frame(onehot, df)
d1 <- dist(as.matrix(dt), method="euclidean")

hc <- hclust(d1, method="complete")
plot(hc, cex=0.6, hang=-1)
#cut tree into 4 group
sub_grp <- cutree(hc, k=4)
sub_grp
table(sub_grp)

#ราสามารถวิเคราะห์แบ่งกลุ่มแบบ Agglomerative ได้โดยใช้ Hierarchical Cluster โดยใช้คำสั่ง agnes()ในpackage “Cluster”
library(cluster)
hc_agnes <-agnes(d, diss = inherits(d, "dist"), metric = "euclidean", stand =
                   FALSE, method = "average")
hc_agnes$ac

#Dedrogram visualization
pltree(hc_agnes, cex = 0.6, hang = -1, main = "Dendrogram of agnes")

#cut tree into 3 group
sub_grp <- cutree(hc_agnes, k=3)
sub_grp

#diana
dv <- diana(df, stand = FALSE) # default: Euclidean
dv

#plot(as.hclust(dv), cex=0.6, hang=-1)
pltree(dv, cex = 0.6, hang = -1, main = "Dendrogram of Diana")

#cut tree into 3 group
dv_grp <- cutree(as.hclust(dv), k = 3)
dv_grp

plot(dt, col=dv_grp)
