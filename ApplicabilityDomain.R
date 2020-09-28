train_data <- load("normalised_train_data.Rdata")
dist_train <- as.matrix(train_data[, 1:9])
colnames(dist_train)
dim(dist_train)
distance <- sapply(1:nrow(dist_train), function(i) sqrt(rowSums(sweep(dist_train, 2, dist_train[i, ])^2)))
dim(distance)
distance_19=apply(distance, 2, function(x) sort(x)[1:20])
distance_19[,1]
dim(distance_19)
dist19_means = apply(distance_19,2, mean)
class(dist19_means)
#mean and sd of the sampling distribution of the means
mean(dist19_means)
sd(dist19_means)
#90% 95% 99%	1.645 1.96 2.576
AD_limit = mean(dist19_means)+1.96*sd(dist19_means)
#AD_limit = 2.191064 for the transformed but imbalanced training set @n=21, z = 1.654
#AD_limit = 2.124666 for the transformed but imbalanced training set @n=19, z = 1.654
#AD_limit = 2.29755516414159 for the transformed but imbalanced training set @ z = 1.96, n=19 (sqrt(339) = 18.41 ~ 19)
print(paste("Applicability domain Limit", AD_limit))

applicability <- function(instance,AD_limit=AD_limit){
  dist=numeric()
  for (i in 1:nrow(train_data)) {
    dist[i] = 0.0
    for (j in 1:9) {
      dist[i] = dist[i] + (instance[j] - train_data[i,j])^2
    }
    dist[i] = sqrt(dist[i])
  }
  Di = sum(sort(dist)[1:22])/22
  if ( Di < AD_limit) {
    print(paste("Di of instance", Di))
    print(paste("Applicability domain Limit", AD_limit))
    return(1)
  }
  else {
    print(paste("Di of instance", Di))
    print(paste("Applicability domain Limit", AD_limit))
    return(0)
  }
}  


