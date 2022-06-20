## Membangkitkan Data
# Data bangkitan merupakan data berbentuk lingkaran yang dibangkitkan dengan 
# fungsi get_spectral() dari package 'lsdbc'
library(lsdbc)
set.seed(127)
data20<-get_spectral(20)
data50<-get_spectral(50)
data100<-get_spectral(100)
data200<-get_spectral(200)
data400<-get_spectral(400)
data1000<-get_spectral(1000)
data1600<-get_spectral(1600)
data2000<-get_spectral(2000)
plot(data1000)
## Cluster data bangkitan
# vigilance parameter (rho) = 0.5
# alpha 
fuzzyart20 <- fuzzyart_training(data20,rho = 0.5, alpha = 0.000001, beta = 1)
fuzzyart50 <- fuzzyart_training(data50,rho = 0.5, alpha = 0.000001, beta = 1)
fuzzyart100 <- fuzzyart_training(data100,rho = 0.5, alpha = 0.000001, beta = 1)
fuzzyart200 <- fuzzyart_training(data200,rho = 0.5, alpha = 0.000001, beta = 1)
fuzzyart400 <- fuzzyart_training(data400,rho = 0.5, alpha = 0.000001, beta = 1)
fuzzyart1000 <- fuzzyart_training(data1000,rho = 0.5, alpha = 0.000001, beta = 1)
fuzzyart1600 <- fuzzyart_training(data1600,rho = 0.5, alpha = 0.000001, beta = 1)
fuzzyart2000 <- fuzzyart_training(data2000,rho = 0.5, alpha = 0.000001, beta = 1)

#vigilance parameter (rho) = 0.40 - 0.80

fuzzyart40r <- fuzzyart_training(data100,rho = 0.40, alpha = 0.000001, beta = 1)
fuzzyart50r <- fuzzyart_training(data100,rho = 0.50, alpha = 0.000001, beta = 1)
fuzzyart60r <- fuzzyart_training(data100,rho = 0.60, alpha = 0.000001, beta = 1)
fuzzyart70r <- fuzzyart_training(data100,rho = 0.70, alpha = 0.000001, beta = 1)

fuzzyart40r$num_clusters
fuzzyart50r$num_clusters 
fuzzyart60r$num_clusters
fuzzyart70r$num_clusters

fuzzyart40r1000 <- fuzzyart_training(data1000,rho = 0.40, alpha = 0.000001, beta = 1)
fuzzyart50r1000 <- fuzzyart_training(data1000,rho = 0.50, alpha = 0.000001, beta = 1)
fuzzyart60r1000 <- fuzzyart_training(data1000,rho = 0.60, alpha = 0.000001, beta = 1)
fuzzyart70r1000 <- fuzzyart_training(data1000,rho = 0.70, alpha = 0.000001, beta = 1)

fuzzyart40r1000$num_clusters
fuzzyart50r1000$num_clusters 
fuzzyart60r1000$num_clusters
fuzzyart70r1000$num_clusters

## FAKMCT

tfa40r <- fuzzyart_testing(data100,fuzzyart40r)
tfa50r <- fuzzyart_testing(data100,fuzzyart50r)
tfa60r <- fuzzyart_testing(data100,fuzzyart60r)
tfa70r <- fuzzyart_testing(data100,fuzzyart70r)

tfa40r$num_clusters
tfa50r$num_clusters 
tfa60r$num_clusters
tfa70r$num_clusters

tfa40r1000 <- fuzzyart_testing(data1000,fuzzyart40r1000)
tfa50r1000 <- fuzzyart_testing(data1000,fuzzyart50r1000)
tfa60r1000 <- fuzzyart_testing(data1000,fuzzyart60r1000)
tfa70r1000 <- fuzzyart_testing(data1000,fuzzyart70r1000)

tfa40r1000$num_clusters
tfa50r1000$num_clusters 
tfa60r1000$num_clusters
tfa70r1000$num_clusters

kmttfa<-kmeans_train(book3, k = tfa$num_clusters, centroids = ttfa$centroids, max_iter = 200, km_out = list(clusters=NA, centroids = NA,  labels=NA))
kmttfa

kmtfa40r <- kmeans_train(data100,k = tfa40r$num_clusters, centroids = tfa40r$centroids)
kmtfa50r <- kmeans_train(data100,k = tfa50r$num_clusters, centroids = tfa50r$centroids)
kmtfa60r <- kmeans_train(data100,k = tfa60r$num_clusters, centroids = tfa60r$centroids)
kmtfa70r <- kmeans_train(data100,k = tfa70r$num_clusters, centroids = tfa70r$centroids)

# kmtfa40r$num_clusters
# kmtfa50r$num_clusters 
# kmtfa60r$num_clusters
# kmtfa70r$num_clusters

kmtfa40r1000 <- kmeans_train(data1000,k = tfa40r1000$num_clusters, centroids = tfa40r1000$centroids)
kmtfa50r1000 <- kmeans_train(data1000,k = tfa50r1000$num_clusters, centroids = tfa50r1000$centroids)
kmtfa60r1000 <- kmeans_train(data1000,k = tfa60r1000$num_clusters, centroids = tfa60r1000$centroids)
kmtfa70r1000 <- kmeans_train(data1000,k = tfa70r1000$num_clusters, centroids = tfa70r1000$centroids)

kmtfa40r1000
kmtfa50r1000$num_clusters
kmtfa60r1000$num_clusters
kmtfa70r1000$num_clusters

dist(data1000)


fuzzyart40r$running_time
fuzzyart50r$running_time 
fuzzyart60r$running_time
fuzzyart70r$running_time
fuzzyart40r1000$running_time
fuzzyart50r1000$running_time 
fuzzyart60r1000$running_time
fuzzyart70r1000$running_time

## UJI PERFORMANCE

AKHIRNYA <- ffakmct(data100, alpha = 0.3, rho = 0.5, beta = 0.08, max_epochs = 1000)
AKHIRNYA 
fkm1

fkm40r <- ffakmct(data100,rho = 0.40, alpha = 0.000001, beta = 1)
fkm50r <- ffakmct(data100,rho = 0.50, alpha = 0.000001, beta = 1)
fkm60r <- ffakmct(data100,rho = 0.60, alpha = 0.000001, beta = 1)
fkm70r <- ffakmct(data100,rho = 0.70, alpha = 0.000001, beta = 1)
fkm40r1000 <- ffakmct(data1000,rho = 0.40, alpha = 0.000001, beta = 1)
fkm50r1000 <- ffakmct(data1000,rho = 0.50, alpha = 0.000001, beta = 1)
fkm60r1000 <- ffakmct(data1000,rho = 0.60, alpha = 0.000001, beta = 1)
fkm70r1000 <- ffakmct(data1000,rho = 0.70, alpha = 0.000001, beta = 1)

fkm40r$running_time
fkm50r$running_time 
fkm60r$running_time
fkm70r$running_time
fkm40r1000$running_time
fkm50r1000$running_time 
fkm60r1000$running_time
fkm70r1000$running_time

fkm40r$num_clusters
fkm50r$num_clusters 
fkm60r$num_clusters
fkm70r$num_clusters
fkm40r1000$num_clusters
fkm50r1000$num_clusters 
fkm60r1000$num_clusters
fkm70r1000$num_clusters

fkm40r1 <- ffakmct(data100,rho = 0.40, alpha = 0.000001, beta = 1)
fkm40r2 <- ffakmct(data100,rho = 0.401, alpha = 0.000001, beta = 0.8)
fkm40r3 <- ffakmct(data100,rho = 0.40, alpha = 0.1, beta = 1)
fkm40r4 <- ffakmct(data100,rho = 0.40, alpha = 0.1, beta = 0.8)
fkm40r5 <- ffakmct(data1000,rho = 0.40, alpha = 0.000001, beta = 1)
fkm40r6 <- ffakmct(data1000,rho = 0.40, alpha = 0.000001, beta = 0.8)
fkm40r7 <- ffakmct(data1000,rho = 0.40, alpha = 0.1, beta = 1)
fkm40r8 <- ffakmct(data1000,rho = 0.40, alpha = 0.1, beta = 0.8)

fkm40r1$running_time
fkm40r2$running_time 
fkm40r3$running_time
fkm40r4$running_time
fkm40r5$running_time
fkm40r6$running_time 
fkm40r7$running_time
fkm40r8$running_time

fkm40r1$num_clusters
fkm40r2$num_clusters 
fkm40r3$num_clusters
fkm40r4$num_clusters
fkm40r5$num_clusters
fkm40r6$num_clusters
fkm40r7$num_clusters
fkm40r8$num_clusters

fa40r1 <- fuzzyart_training(data100,rho = 0.40, alpha = 0.000001, beta = 1)
fa40r2 <- fuzzyart_training(data100,rho = 0.401, alpha = 0.000001, beta = 0.1)
fa40r3 <- fuzzyart_training(data100,rho = 0.40, alpha = 0.1, beta = 1)
fa40r4 <- fuzzyart_training(data100,rho = 0.40, alpha = 0.1, beta = 0.1)
fa40r5 <- fuzzyart_training(data100,rho = 0.70, alpha = 0.000001, beta = 1)
fa40r6 <- fuzzyart_training(data100,rho = 0.68, alpha = 0.000001, beta = 0.8)
fa40r7 <- fuzzyart_training(data100,rho = 0.70, alpha = 0.1, beta = 1)
fa40r8 <- fuzzyart_training(data100,rho = 0.68, alpha = 0.1, beta = 0.8)
fa40r1$running_time
fa40r2$running_time 
fa40r3$running_time
fa40r4$running_time
fa40r5$running_time
fa40r6$running_time 
fa40r7$running_time
fa40r8$running_time



fkm40r1 <- ffakmct(data100,rho = 0.70, alpha = 0.000001, beta = 1)
fkm40r2 <- ffakmct(data100,rho = 0.68, alpha = 0.000001, beta = 0.8)
fkm40r3 <- ffakmct(data100,rho = 0.70, alpha = 0.1, beta = 1)
fkm40r4 <- ffakmct(data100,rho = 0.68, alpha = 0.1, beta = 0.8)
fkm40r5 <- ffakmct(data100,rho = 0.70, alpha = 0.000001, beta = 1)
fkm40r6 <- ffakmct(data100,rho = 0.68, alpha = 0.000001, beta = 0.8)
fkm40r7 <- ffakmct(data100,rho = 0.70, alpha = 0.1, beta = 1)
fkm40r8 <- ffakmct(data100,rho = 0.68, alpha = 0.1, beta = 0.8)

fkm40r1$size

##TAMBAHAN RHO 0.8
fkm80r1 <- ffakmct(data100,rho = 0.85, alpha = 0.000001, beta = 1)




library(cluster)
km100c5 <- kmeans(data100, 5, nstart = 25)
km100c5$size
km100c5$centers
kmalfi <- kmeans_train(data100,5)
kmalfi$size
kmalfiplusc <- kmeans_train(data100,5,centroids = km100c5$centers)
kmalfiplusc$size
