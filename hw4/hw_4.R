#Ethan Grant uni: erg2145
#Stats W4400

MultionmialEM <- function(H,K,tau){
  #install packages
  #install.packages("ppls")  
  #library("ppls", lib.loc="C:/Program Files/R/R-3.2.2/library")
  
  #sanitize data
  H <- H+.001
  
  #sample k histograms and normalize
  K=3
  t <- H[sample(dim(H)[1], K),]
  for(i in 1:K){
    t[i,] = normalize.vector(t[i,])
  }
  
  #set up variables
  phi = matrix(, dim(H)[1], K)
  a = matrix(, dim(H)[1], K)
  c <- rep(1/3, 3)
  
  #calculate every phi value
  for(k in 1:K){
    sumVec <- H %*% log(t[k,])
    for(i in 1: dim(H)[1]){
      phi[i,k] = exp(sumVec[i])
    }
  }

  #compute posterior probability
  for(i in 1:dim(H)[1]){
    sum <- sum(c %*% phi[i,])
    for(k in 1:K){
      a[i,k] <- c[k]*phi[i,k]/sum
    }
  }
  
  #mstep
  for(k in 1:K){
    c[k,] <- colSums(a)
    b[k] <- colSums(a[,k]*H)
    t[k] <- b[k]/rowSums(b[k])
  }
  
}

tau = .5
K= 3
H <- matrix(readBin("histograms.bin", "double", 640000), 40000, 16)
m <-MultionmialEM(H,K,tau)
