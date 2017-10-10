#compute m
#  if(mistakes_for_each_split[best_index]<0){
#    m[i] <- 1
#  }
#  else{
#    m[i] <- -1
#  }

#correct for multiple of same value for x_{j}
#prev<- x_sort[1]
#for(r in 2:dim(X)[1]){
# if(x_sort[r]==prev){
#  mistakes_for_each_split[r]<- NA
#}
#else{
# prev <- x_sort[r]
#}

w_cum [ duplicated(x_j)==1] <- NA
mm <- max ( abs ( w_cum ) , na.rm = TRUE )
maxIndx <- min ( which ( abs ( w_cum )== mm ))

mode [ i ] <- ( w_cum [ maxIndx ] < 0) * 2 - 1
theta [ i ] <- x_j [ maxIndx ]
c <- (( x_j > theta [ i ]) * 2 - 1) * mode [ i ]
loss2[ i ] <- w %*% ( c != y )

prev<- x_sort[1]
for(r in 2:dim(X)[1]){
  if(x_sort[r]==prev){
    mistakes_for_each_split[r]<- NA
  }
  else{
    prev <- x_sort[r]
  }
}