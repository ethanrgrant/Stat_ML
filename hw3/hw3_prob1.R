# Ethan Grant
# uni: erg2145
# class: STAT W4400
# hw number: HW03

#inputs:
# x = training vectors
# w = vector containing weights
# y= vector of class lables
#outputs:
#list containing triplet (j, theta, m) that specifies the decision stump
AdaBoost <- function(data, y, B){
  w <- rep(0,dim(data)[1])
  pars <- rep(list(list()), B)
  alpha <- rep(0, dim(data)[1])
  identity_funct <- rep(0, dim(data)[1])
  
  for(i in 1:dim(data)[1]){
    w[i]<- 1/dim(data)[1]
  }
  
  #train b classifiers while modifying weights
  for(i in 1:B){
    #train weak learners
    pars[[i]] <- train(data, w, y)
    
    #find the labels
    label <- classify(data,pars[[i]])
    identity_funct <- (y!=label)
    error <- (w %*% identity_funct/sum(w))[1]

    #voting weights
    alpha[i] = log((1-error)/error)
    w <- w*exp(alpha[i]*identity_funct)
  }
  
  return(list(pars = pars, alpha=alpha))
  
}

#uses the aggregated pars to classify with aggregation
#inputs:
#x = data to be classified
#alpha = alphas for data
#pars = collection of weak learners
#ouput: agg_class = aggregated classifications
agg_class<- function(X, alpha, pars){
  agg_classes <- matrix(0, dim(X)[1], length(alpha))
  #computes classification without weights for each classifier
  for(classifier in 1:length(alpha)){
    indiv_class <- classify(X, pars[[classifier]])
    agg_classes[,classifier] <- indiv_class
  }
  
  #computes classfication with weights
  agg_classes <- agg_classes %*% alpha
  classes_signs <- sign(agg_classes)
  
  return(classes_signs=classes_signs)
}

#trains a weak learner
#inputs:
#x = data
#w = weights
#y = correct class
#outputs:
# pars the data that defines a weak learner
train <- function(X, w, y){
  
  #initalize the things that define the stump
  losses <- rep(0,dim(X)[2])
  stump_split_spot <- rep(0,dim(X)[2])
  m <- rep(0,dim(X)[2])

  #compute splitting point for a given dim
  for(i in 1:dim(X)[2]){
    
    #sort all dimensions into ascending order
    sort_order <- order(X[,i])
    x_sort <- X[sort_order,i]
    
    mistakes_for_each_split <- cumsum(w[sort_order]*y[sort_order])
    
    #find and mark duplicates as 0
    duplicates <- duplicated(x_sort)
    mistakes_for_each_split[duplicates] <- 0
    
    #find absolute max val note: 0's disregarded as can't be max
    best_stump <- max(abs(mistakes_for_each_split))

    #find the first index where the max val occurs
    best_index <- min(which(abs(mistakes_for_each_split)==best_stump))
   if(mistakes_for_each_split[best_index]<0){
        m[i] <- 1
    }
    else{
      m[i] <- -1
    }
    #find best split point
    stump_split_spot[i] <- x_sort[best_index]


    class <- rep(0, length(x_sort))
    for(j in 1:length(class)){
      if(x_sort[j]>stump_split_spot[i]){
        class[j] <- m[i]
      }
      else{
        class[j] <- -m[i]
      }
    }

    losses[i] <- w %*% (class!=y)
   
  }
  #best split point has lowest loss
  best_split <- which.min(losses)
  
  pars <-list(best_split=best_split, stump_split= stump_split_spot[best_split], m=m[best_split])
  return(pars)
}

#classifies X based on a single learner pars
#inputs
#x data
#pars = weak learner
#output
#label = class labels
classify<-function(x,pars){
  stump_split <- pars$stump_split
  m <- pars$m
  split <- pars$best_split
  label <- rep(0, dim(x)[1])
  
  for(i in 1:dim(x)[1]){
    if(x[i,split]>stump_split){
      label[i] = m
    }
    else{
      label[i] = -m
    }
  }

  return(label)
}

#main functionf or adaboost
main <-function(){
  data <- read.table("uspsdata.txt")
  class <- read.table("uspscl.txt")
  
  test_index <- sample(1:dim(data)[1], dim(data)[1]/4)
  
  #divide data into test and trian
  test_data <- data[test_index,]
  test_class <- class[test_index,]
  
  train_data <- data[-test_index,]
  train_class <- class[-test_index,]
  
  #create matrices to hold results from validating/testing learners
  validation_err <- matrix(0, 50, 5) 
  test_err <- matrix(0, 50, 5)
  
  #5 fold cross validation
  for(j in 1:5){
    #split up into validation/train sets
    validation_index <- ((j-1)*dim(test_data)[1]/5):((j)*dim(test_data)[1]/5)
    new_train <- train_data[-validation_index,]
    validation_data <- train_data[validation_index,]
    new_class <- train_class[-validation_index]
    validation_class <-train_class[validation_index]
      
    #run adaBoost and get 50 weak learners
    result <- AdaBoost(new_train, new_class, 50)
    pars <- result$pars
    alpha <- result$alpha
    print("learners trained!")
    
    for(i in 1:50){
      #get results for different numbers of aggregated weak learners on validation and compute/store error
      result_class_validation <- agg_class(train_data, alpha[1:i], pars[1:i])
      validation_err[i,j] <- mean(train_class!= result_class_validation)

      #get results for different numbers of aggregated weak learners on test and compute/store error
      result_class_test <- agg_class(test_data, alpha[1:i], pars[1:i])
      test_err[i,j] <- mean(test_class!= result_class_test)
    
    }
    
  }
  print("plotting!")
  matplot ( validation_err , type = "l" , lty =1: 5 , main = "train err" ,  xlab = "weak learners" , ylab = "error rate on validation" , ylim = c (0 ,0.75))
  
  matplot ( test_err , type = "l" , lty =1: 5 , main = " test error " , xlab = "weak learners " , ylab = " error rate on test" , ylim = c (0 ,0.575))
  
  
}

main()

