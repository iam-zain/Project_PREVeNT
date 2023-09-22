setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250")

NonMot1 = read.csv('NMI_All45Feats_Categorized.csv')
NonMot2 <- NonMot1  %>% select(c(3))#Remove PATNO
NonMot2<-as.data.frame(NonMot2[-1,])
NonMot2 <- na.omit(NonMot2)
# Perform k-means clustering
set.seed(4)
k_values <- 2:10
clusters <- kmeans(NonMot2, centers = 6, nstart = 10)
wss <- sapply(k_values, function(k){kmeans(NonMot2, k, nstart = 10)$tot.withinss})
#plot(clusters$cluster)
# Create the elbow plot
ggplot(data.frame(k = k_values, wss = wss), aes(x = k, y = wss)) +geom_line() +geom_point() +
  ggtitle("Elbow Plot") + xlab("Number of Clusters (K)") + ylab("Within Sum of Squares (WSS)")

NonMot2$id <-1:45 
str(NonMot2)

# Plot the clusters
#fviz_cluster(clusters, data = NonMot2,palette = c(1:6),geom = "point",ellipse.type = "convex",ggtheme = theme_bw())

