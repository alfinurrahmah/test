get_spectral <- function(n){
  x <- matrix(NA, 5*n, 2)
  x1 <- c()
  x2 <- c()
  distance <- vector(length = 5*n)

  for(i in 1:(5*n)){
    x[i,] <- runif(2,-1,1)
    distance[i] <- sqrt((x[i,1]^2)+(x[i,2]^2))

    if(distance[i] <= 1 && distance[i] >=0.9){
      x1 <- c(x1,x[i,1])
      x2 <- c(x2,x[i,2])
    } else if(distance[i] <= 0.5 && distance[i] >=0.4){
      x1 <- c(x1,x[i,1])
      x2 <- c(x2,x[i,2])
    } else if(distance[i] <= 0.1){
      x1 <- c(x1,x[i,1])
      x2 <- c(x2,x[i,2])
    }
  }
  return(cbind(x1[1:n],x2[1:n]))
}

set.seed(20)
dt100<-get_spectral(100)
tail(dt100)
plot(dt100)

nd100<-normalize_data(dt100)
tail(nd100)

data1000 <- get_spectral(1000)
plot(data1000)

data2000 <- get_spectral(2000)
plot(x=data2000[,1], y=data2000[,2])
