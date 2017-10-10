#Inputs
#w:  w[1:d] is the normal vector of a hyperplane, 
#    w[d+1] = -c is the negative offset parameter. 
#n: sample size

#Outputs
#S: n by (d+1) sample matrix with last col 1
#y: vector of the associated class labels

fakedata <- function(w, n){

if(! require(MASS))
{
	install.packages("MASS")
}
if(! require(mvtnorm))
{
	install.packages("mvtnorm")
}

require(MASS)
require(mvtnorm)

# obtain dimension
d <- length(w)-1

# compute the offset vector and a Basis consisting of w and its nullspace
offset <- -w[length(w)] * w[1:d] / sum(w[1:d]^2)
Basis <- cbind(Null(w[1:d]), w[1:d])	 

# Create samples, correct for offset, and extend
# rmvnorm(n,mean,sigme) ~ generate n samples from N(0,I) distribution
S <- rmvnorm(n, mean=rep(0,d),sigma = diag(1,d)) %*%  t(Basis) 
S <- S + matrix(rep(offset,n),n,d,byrow=T)
S <- cbind(S,1)

# compute the class assignments
y <- as.vector(sign(S %*% w))

# add corrective factors to points that lie on the hyperplane.
S[y==0,1:d] <- S[y==0,1:d] + runif(1,-0.5,0.5)*10^(-4)
y = as.vector(sign(S %*% w))
return(list(S=S, y=y))

} # end function fakedata








#inputs:
# S = Linearly seperaable data
# z = normal vector to hyperplane and offset
#outputs:
# classification = a vector containing classification for each point 
classify <- function(S,z){
  classification =  sign(S %*% z)
  for(i in length(classification))
  {
    if(classification[i]==0)
    {
      classification[i]<-1
    }
  }
  return(classification)
}



# inputs:
# S = sample of linearlly seperable data in a matrix
# y = classification for each data point
# outputs:
# norm = normal vector of the hyperplane
# norm_history = matrix with the history of the normal vector
perceptrain <- function(S,y){
  
  S[y==-1,] <- -S[y==-1,]
  
  norm <-  S[sample.int(dim(S)[1] , size=1),]
  k <- 1
  norm_history <- norm
  
  classifications <- S %*% norm
  mistakes <- (classifications <= 0)
  cost = sum(mistakes)
  while(cost > 0){
    if(cost==1){
      norm = norm + 1/k*S[mistakes,]
    }else{
      norm = norm + 1/k*colSums(S[mistakes,])
    }
   
    k <- k+1
    
    #evaluate new line and add to history
    classifications <- S %*% norm
    rbind(norm_history, norm)
    
    #check for mistakes and calc cost
    mistakes <- (classifications<=0)
    cost = sum(mistakes)
  }
  output <- list(norm=norm, norm_history=norm_history)
  return(output)
}


print("starting")
z <- runif(4)
train_data <- fakedata(z, 100)
sample <- train_data$S
class <- train_data$y
percept <- perceptrain(sample, class)
norm <- percept$norm

classes <- classify(sample, norm)
tab <- table(class, classes)
tab
if(all(class==classes)){
  print("works")
}

test_data <- fakedata(z, 100)
sample <- test_data$S
class <- test_data$y

classes <- classify(sample, norm)
tab <- table(class, classes)
tab

ggplot(two.d.array(sample))
print("ending")
