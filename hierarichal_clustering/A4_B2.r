#mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
#oooooooooooooooooo Problem B2 ooooooooooooooooooooo
#oooooooooo ISL Section 8.4 Exercise 8 ooooooooooooo
#mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm

data("USArrests")

# Perform hierarchical clustering with complete linkage
hc_complete <- hclust(dist(USArrests), method = "complete")
plot(hc_complete, main = "Hierarchical Clustering with Complete Linkage")

# Determine clusters and print summary
k <- 3
clusters <- cutree(hc_complete, k = k)
states <- row.names(USArrests)
for (i in 1:k) {
  cat(paste0("Cluster ", i, "\n"))
  cat(paste(sum(clusters == i), "states\n"))
  cat(states[clusters == i], "\n")
}

# Scale data and perform hierarchical clustering again
USArrests_sc <- scale(USArrests)
hc_complete_sc <- hclust(dist(USArrests_sc), method = "complete")
plot(hc_complete_sc, main = "Hierarchical Clustering with Complete Linkage, Scaled Features")

# Determine clusters and print summary
sc_cluster <- cutree(hc_complete_sc, k = k)

for (i in 1:k) {
  cat(paste0("Cluster ", i, "\n"))
  cat(paste(sum(sc_cluster == i), "states\n"))
  cat(states[sc_cluster == i], "\n")
}

# Compare standard deviation before and after scaling
sd_unscaled <- apply(USArrests, 2, sd)
sd_scaled <- apply(USArrests_sc, 2, sd)
cat("Standard Deviations (Not Scaled):\n")
print(sd_unscaled)
cat("Standard Deviations (Scaled):\n")
print(sd_scaled)

# Compare means of each variable within clusters before and after scaling
cat("Means of Each Variable within Clusters (Not Scaled):\n")
cat("Cluster 1: ")
print(apply(USArrests[clusters == 1, ], 2, mean))
cat("Cluster 2: ")
print(apply(USArrests[clusters == 2, ], 2, mean))
cat("Cluster 3: ")
print(apply(USArrests[clusters == 3, ], 2, mean))

cat("Means of Each Variable within Clusters (Scaled):\n")
cat("Cluster 1: ")
print(apply(USArrests[sc_cluster == 1, ], 2, mean))
cat("Cluster 2: ")
print(apply(USArrests[sc_cluster == 2, ], 2, mean))
cat("Cluster 3: ")
print(apply(USArrests[sc_cluster == 3, ], 2, mean))

# Visualize inter-observation dissimilarities before and after scaling
library(lattice)
dissim_unscaled <- as.dist(1 - cor(t(USArrests)))
levelplot(data.matrix(dissim_unscaled), main = 'Inter-observation Dissimilarities (Not Scaled)')

dissim_scaled <- as.dist(1 - cor(t(USArrests_sc)))
levelplot(data.matrix(dissim_scaled), main = 'Inter-observation Dissimilarities (Scaled)')

# Conclusion
cat("Scaling the variables leads to quite different clusters. Without scaling, the three clusters have size 16, 14, and 20. With scaling, clusters have size 8, 11, and 31. Hence, scaling has a big impact on clustering in this case.\n")
cat("The features are measured in different units (number of cases per 100,000 for Murder, Assault, and Rape, and percent for UrbanProp). Moreover, the variable Assault has a much larger
