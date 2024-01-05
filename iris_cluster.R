
# Clustering example with Iris data.  There are four different measurements
# for a number of irises measured by a botanistic on a particular day in Canada.
# This is very popular dataset and you will see it frequently.
# Before clustering we do several visualizations.



##########################################################################################
# setup dataset
##########################################################################################

# prepare data
data(iris)    # load the data into memory

# summarize the dataset
summary(iris)



##########################################################################################
# understand the structure of the data [optional]
##########################################################################################

# let's understand what the "iris" object is:
typeof(iris)  # check type of object
ls(iris)      # what are the objects within the list iris
dim(iris)     # what is the size or dimension of the object
class(iris)   # many objects have "classes" that define their behavior
str(iris)     # let's head detailed information about the object

# we can also look at the objects within the object
# R uses the $ sign as a way of accessing an object that is within a list
# notice that iris has several variables including Petal.Width
# Each variable in turn is an object which has its own properties.
typeof(iris$Petal.Width)   # tells us that Petal.Width is a double precision number
class(iris$Petal.Width)    # Petal.Width is in the numeric class
length(iris$Petal.Width)   # Petal.Width is a vector with 150 elements

# first look at the data
# the 1:6 is an expression that generates a list of c(1,2,3,4,5,6)
# the brackets allow us to extract just the first 6 rows and all columns
print(iris[1:6,])

# notice that this command gives us the same thing
# when you enter the object (or variable) by itself it will print it
iris[c(1,2,3,4,5,6),]

# often there are functions that do similar things
# we can do the same thing with head
head(iris)

# let's list out the first 6 rows and 2 columns
iris[1:6,c(1,2)]   # here we think of the data frame as a table and get the first 6 rows and 2 columns
iris[1:6,c("Sepal.Length","Sepal.Width")]  # another way to do the same thing using the names of the columns
varnames=c("Sepal.Length","Sepal.Width")   # create a vector of the variable names
iris[1:6,varnames]  # another way using another object that has the variables
iris$Sepal.Length[1:6]   # yet another way if we just want to access a column directly



##########################################################################################
# visualize the data
##########################################################################################

# plot iris data in boxplot
par(mfrow=c(4,1),mar=c(3,4,1,1))  # mfrow=c(4,1) tells R to plot 4 rows and 1 column, and mar is the margin
# boxplot (as well as many other functions in R) use what is known as formula syntax
# the basics of formula is left hand side is the dependent variance and the right side are
# the independent variables. So for instance Petal.Length~Species tells boxplot to 
# have Petal.Length in the y-axis against Species in the x-axis.  One nice thing is that
# when use formula syntax R knows that Species is a factor variable so it knows to 
# automatically create dummy variables for each of its levels "setosa", "versicolor" and
# "virginica".  Notice if you execute the command boxplot(Petal.Length) then we only
# get a single boxplot that has all species.
boxplot(Petal.Length~Species,data=iris,ylab="Petal Length")
boxplot(Petal.Width~Species,data=iris,ylab="Petal Width")
boxplot(Sepal.Length~Species,data=iris,ylab="Sepal Length")
boxplot(Sepal.Width~Species,data=iris,ylab="Sepal Width")
par(mfrow=c(1,1))  # reset to one graph in the panel

# one more visualization is a parallel lines plot
if (!require(lattice)) {install.packages("lattice"); library(lattice)}
# the ~ ... | ... in the following line is a
# formula in R, and says the input variables
# are the first four columns of iris and are
# conditional upon the category Species
parallelplot(~iris[1:4]|Species,data=iris)



##########################################################################################
# cluster the data
##########################################################################################

# before performing k-means clusters with iris data
# remove the Species variable by setting it to NULL
newiris=iris
newiris$Species = NULL

# apply kmeans and save the clustering result to kc.
# the paranethses tell R to print/evaluate the object, alternatively we could enter
# kc=kmeans(newiris,3); print(kc)  but this gives us a simpler way to do both in one line
# there are two inputs to kmeans the dataset of newiris and setting K to the value 3
# to understand the inputs and outputs you can ask for help from R using help(kmeans) or ?kmeans
# however, the help is really meant to be syntax help not help in understanding the algorithm
set.seed(1248765792)   # set the seed so the random initialization of the clusters is the same
( kc = kmeans(newiris, 3) )

# notice that kc is a list.  Quite frequently R returns lists as the result, so that it can
# organize many different variables into a single group.  In this case kmeans returns:
#   kc$cluster:   a vector of integers (from 1:K) indicating the cluster to which each point is allocated
#   kc$centers:   a matrix of cluster centres (the rows correspond to the clusters, columns are variables used for kmeans)
#   kc$totss:     total sum of squares, which says how much variation that originally was in the dataset
#   kc$withinss:  within-cluster sum of squares, vector with K dimensions, each element corresponds to a cluster
#   kc$betweenss: the between-cluster sum of squares which equals is the difference between totss and betweenss
#   kc$size:      the number of pionts in each cluster
#   kc$iter:      the number of iterations
#   kc$ifault:    integer that indicates a possible algorithm problem
str(kc)

# compare the Species label with the cluster result
table(iris$Species, kc$cluster)

# plot the clusters and their centers. Note that there are four dimensions
# in the data and that only the first two dimensions are used to draw the plot
# below.  Some black points close to the green center (asterisk) are actually
# closer to the black center in the four dimensional space

# scatter plot of each cluster with a different color for each cluster
plot(newiris[c("Sepal.Length", "Sepal.Width")], col=kc$cluster)

# plot the cluster centroids
points(kc$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=8, cex=2)
legend("topright",legend=as.character(1:3),col=1:3,pch=8,bty='n')

# look at the cluster solution
print(kc$centers)  # the centers or centroids are the averages of observation within a cluster
mean(newiris$Sepal.Length[kc$cluster==1])   # example: compute average of variable for specific cluster

# we can use parallel plot to see the effects, the auto.key plots a legend above the parallellines
parallelplot(iris[1:4],groups=iris$Species,main="Species",auto.key=list(space="top",columns=3,lines=T))
parallelplot(iris[1:4],groups=kc$cluster,main="Clusters",auto.key=list(space="top",columns=3,lines=T))

# we can also compare the cluster centroids
parallelplot(kc$centers,main="Clusters",auto.key=list(text=c("1","2","3"),space="top",columns=3,lines=T))
# notice the scales are relative for each variable, try it again it the same scale
parallelplot(kc$centers,main="Clusters",common.scale=TRUE,auto.key=list(text=c("1","2","3"),space="top",columns=3,lines=T))



###############################################################################
## let's determine how many clusters to use by creating kmeans solutions
## with k from 2 to 30 and then we can plot the sum of square errors to 
## understand how much variation each solution explains
###############################################################################

# set the random number seed so the samples will be the same if regenerated
set.seed(125)

# compute multiple cluster solutions
grpA2=kmeans(newiris,centers=2)
grpA3=kmeans(newiris,centers=3)
grpA4=kmeans(newiris,centers=4)
grpA5=kmeans(newiris,centers=5)
grpA6=kmeans(newiris,centers=6)
grpA7=kmeans(newiris,centers=7)
grpA8=kmeans(newiris,centers=8)
grpA9=kmeans(newiris,centers=9)
grpA10=kmeans(newiris,centers=10)
grpA15=kmeans(newiris,centers=15)
grpA20=kmeans(newiris,centers=20)
grpA30=kmeans(newiris,centers=30)

# compute between and within SS
kclust=c(2:10,15,20,30)
bss=c(grpA2$betweenss,
      grpA3$betweenss,grpA4$betweenss,grpA5$betweenss,grpA6$betweenss,
      grpA7$betweenss,grpA8$betweenss,grpA9$betweenss,grpA10$betweenss,
      grpA15$betweenss,grpA20$betweenss,grpA30$betweenss)
wss=c(grpA2$tot.withinss,
      grpA3$tot.withinss,grpA4$tot.withinss,grpA5$tot.withinss,grpA6$tot.withinss,
      grpA7$tot.withinss,grpA8$tot.withinss,grpA9$tot.withinss,grpA10$tot.withinss,
      grpA15$tot.withinss,grpA20$tot.withinss,grpA30$tot.withinss)
# plot the results and look for the "Hockey-Stick" effect
par(mfrow=c(1,1))
plot(kclust,bss,type="l",main="Between SS for k-means")
points(kclust,bss)
plot(kclust,wss,type="l",main="Within SS for k-means")
points(kclust,wss)
plot(kclust,bss/(wss+bss),type="l",main="R-Squared for k-means")
points(kclust,bss/(wss+bss))

# let's take a closer look at solution with k=7
print(grpA7$centers)  # these are the centroids
table(iris$Species, grpA7$cluster)  # the centroids seem to match well with the species
parallelplot(grpA7$centers,main="Clusters",auto.key=list(text=as.character(1:7),space="top",columns=4,lines=T))




##########################################################################################
# hierarchical cluster analysis [optional]
##########################################################################################

### below is an optional part of the exercise to create a hierarchical cluster analysis
### instead of grouping observations into K clusters, we build up the cluster by finding
### individual observations that are most like another observation or previous grouping
### you can turn a hierarchical cluster into K clusters by deciding what is the distance
### at which you want to define your clusters


# try a hierarchical cluster on the flowers
hc=hclust(dist(newiris),method="complete")  # try "single", "complete", "average", "ward"
plot(hc,cex=.7)
hc3id = cutree(hc,k=3)  # divide the tree into three clusters
print(hc3id)  # these are the clusters if with divide into three clusters
table(hc3id,kc$cluster)  # notice that the division is similar

# if we want to color the lables according to the species
if (!require(dendextend)) {install.packages("dendextend"); library(dendextend)}  # need new package
dend=as.dendrogram(hc)  # create a new object that stores the dendogram
colorCodes = c(setoas="red",versicolor="green",virginica="yellow")  # the elements of the list correspond to labels
labels_colors(dend) = colorCodes[iris$Species][order.dendrogram(dend)]  # notice we have to reorder the objects according to where they are
plot(dend)  # let's plot the denodogram again
# for an extended analysis see https://cran.r-project.org/web/packages/dendextend/vignettes/Cluster_Analysis.html#iris---edgar-andersons-iris-data

# color the plot according to the species
if (!require(sparcl)) {install.packages("sparcl"); library(sparcl)}  # need new package
species_as_numeric = as.numeric(iris$Species)
ColorDendrogram(hc,y=species_as_numeric,branchlength=2)


