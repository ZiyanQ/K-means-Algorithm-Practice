#input and view the data
protein = read.csv("Desktop/protein.csv")
View(protein)
#remove the Country varieble
newprotein=protein
newprotein$Country = NULL
#set the seed
set.seed(125)

# compute multiple cluster solutions
grpA2=kmeans(newprotein,centers=2)
grpA3=kmeans(newprotein,centers=3)
grpA4=kmeans(newprotein,centers=4)
grpA5=kmeans(newprotein,centers=5)
grpA6=kmeans(newprotein,centers=6)
grpA7=kmeans(newprotein,centers=7)
grpA8=kmeans(newprotein,centers=8)
grpA9=kmeans(newprotein,centers=9)
grpA10=kmeans(newprotein,centers=10)
grpA15=kmeans(newprotein,centers=15)
grpA20=kmeans(newprotein,centers=20)
# compute between and within SS
kclust=c(2:10,15,20)
bss=c(grpA2$betweenss,
      grpA3$betweenss,grpA4$betweenss,grpA5$betweenss,grpA6$betweenss,
      grpA7$betweenss,grpA8$betweenss,grpA9$betweenss,grpA10$betweenss,
      grpA15$betweenss,grpA20$betweenss)
wss=c(grpA2$tot.withinss,
      grpA3$tot.withinss,grpA4$tot.withinss,grpA5$tot.withinss,grpA6$tot.withinss,
      grpA7$tot.withinss,grpA8$tot.withinss,grpA9$tot.withinss,grpA10$tot.withinss,
      grpA15$tot.withinss,grpA20$tot.withinss)
# plot the results and look for the "Hockey-Stick" effect
par(mfrow=c(1,1))
plot(kclust,bss,type="l",main="Between SS for k-means")
points(kclust,bss)
plot(kclust,wss,type="l",main="Within SS for k-means")
points(kclust,wss)
plot(kclust,bss/(wss+bss),type="l",main="R-Squared for k-means")
points(kclust,bss/(wss+bss))
# by viewing the plot, the elbow is at k=4 indicating that the optimal k for this dataset is 7.
kc=kmeans(newprotein,7)
print(kc) 
# compare the Country with the cluster
table(protein$Country, kc$cluster)
# scatter plot of each cluster with a different color for each cluster
plot(newprotein[c("Eggs", "Milk")], col=kc$cluster)
plot(newprotein[c("Fish", "Cereals")], col=kc$cluster)
# plot the cluster centroids
points(kc$centers[,c("Fish", "Cereals")], col=1:7, pch=8, cex=2)
legend("topright",legend=as.character(1:7),col=1:7,pch=8,bty='n')
#compare the cluster centroids
parallelplot(kc$centers,main="Clusters",auto.key=list(text=c("1","2","3","4","5","6","7"),space="top",columns=7,lines=T))
#try it again it the same scale
parallelplot(kc$centers,main="Clusters",common.scale=TRUE,auto.key=list(text=c("1","2","3","4","5","6","7"),space="top",columns=7,lines=T))




