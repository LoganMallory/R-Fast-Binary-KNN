library(purrr) #for detect_index in calc_pred (ties)
  
knn_fit <- function(X_train, y_train, k_max, cv = 5, repeats = 3, type = 'classification', use_all=T, pure_binary=T, verbose=F){
  #:X_train: a Data Table (not matrix) of features
  #:y_train: a vector of numeric data with two unique values
  #:k_max: the highest K to evaluate for KNN
  #:cv: the number of k-folds for k-folds cross validation
  #:repeats: the number of repeats for k-folds cross validation
  #:type: the type of KNN measure ("classification" or "regression") - accuracy vs MSE
  #:use_all: should all values of points in y_train be used in the event of a tie between distances?
  #:pure_binary: is the data purely 1's and 0's? If so, faster distance methods can be used
  #:verbose: TRUE if function should print iteration information

  #if data is binary, matrix cross product is faster than dist() for calculations
  if(pure_binary){ 
    if(verbose){print('dist: crossprod()')}
    distances <- data.table(dist_X(as.matrix(X_train))) #full matrix, not sparse
    
    #if data has more than 300 columns, dist() will be extremely slow
  } else if(ncol(X_train) > 300){ 
    if(verbose){print('dist: distlower()')}
    distances <- data.table(as.matrix(as.dist(distlower(X_train))))

  } else {
    if(verbose){print('dist: dist()')}
    distances <- data.table(as.matrix(dist(X_train)))
  }
  
  #cross validation, return MSE or Accuracy for each k through k_max
  if(cv > 1){ 
    k_mse_vec <- rep(0,k_max)
    k_acc_vec <- rep(0,k_max)
    
    if(verbose){print(paste0('Cross Validation | ', cv, ' folds | ', repeats, ' repeats'))}
    for(r in 1:repeats){
      #vector of cv group for each row (neighbor)
      cvgroup_vec <- sample(rep.int(1:cv, times = (nrow(X_train)/cv)), nrow(X_train))
      #get boolean vectors of indexes for training data
      #loop through cv groups, create matrix of nrow(X_train)Xcv | 7000x5
      train_indexes <- vapply(1:cv, function(testgroup){
        #for every row, if the cv group is not the testing group for the current loop,
        #then TRUE (point is in training) else FALSE (point is in testing for that round)
        vapply(cvgroup_vec, function(i) ifelse(i != testgroup, TRUE, FALSE), logical(1))
      }, logical(nrow(X_train)))
      
      for(i in 1:cv){
        if(verbose){cat('\r',paste0('r: ', r, ' | cv: ', i))}
        #remove test group from training (rows), remove training from testing (cols)
        testing_group  <- distances[train_indexes[,i],!train_indexes[,i], with=F] 
        y_train_group  <- y_train[train_indexes[,i]] #y should align with rows
        y_test_group   <- y_train[!train_indexes[,i]]
        #for every user (col), calculate a prediction using k=1 to k=k_max
        if(use_all){
          #kxn matrix of predictions, 1 column for each user, 1 row for each k
          predictions <- data.table(t(vapply(1:ncol(testing_group), function(colnum){
            calc_pred(testing_group[[colnum]], y_train_group, k_max, test_in_nbrs=F)}, 
            numeric(k_max), USE.NAMES = F)))
        } else {
          predictions <- data.table(t(vapply(1:ncol(testing_group), function(colnum){
            nbr_order <- order(testing_group[[colnum]])[1:k_max];
            cumsum(y_train_group[nbr_order])/(1:k_max)}, numeric(k_max), USE.NAMES = F)))
        }
        
        if(type == 'classification'){
          #calculate MSE of each k for the iteration
          k_acc_vec <- k_acc_vec + vapply(1:ncol(predictions), function(colnum){
                                          temp       <- predictions[[colnum]];
                                          pred_class <- ifelse(temp > 0.5, 1, 0);
                                          sum(pred_class == y_test_group)/length(pred_class)
                                          }, numeric(1), USE.NAMES = F)
          
        } else if(type == 'regression'){
          k_mse_vec <- k_mse_vec + vapply(1:ncol(predictions), function(colnum){
                                          mean((predictions[[colnum]]-y_test_group)^2)
                                          }, numeric(1), USE.NAMES = F)
        }
      }
    }
    k_mse_vec <- k_mse_vec/(repeats*cv)
    k_acc_vec <- k_acc_vec/(repeats*cv)
    if(type == 'classification'){return(k_acc_vec)}
    else if (type == 'regression'){return(k_mse_vec)}
    
    ###END OF CROSS VALIDATION###
    
  } else { #no cross-validation, return predictions (probabilities)
    if(use_all){
      #kxn matrix of predictions, 1 column for each user, 1 row for each k
      predictions <- data.table(t(vapply(1:ncol(distances), function(colnum){
        calc_pred(distances[[colnum]], y_train, k_max, test_in_nbrs=T)}, 
        numeric(k_max), USE.NAMES = F)))
    } else {
      predictions <- data.table(t(vapply(1:ncol(distances), function(colnum){
        nbr_order <- order(distances[[colnum]])[2:(k_max+1)];
        cumsum(y_train[nbr_order])/(1:k_max)}, numeric(k_max), USE.NAMES = F)))
    }
    return(predictions)
  }
  
}


calc_pred     <- function(neighbors_col, y_train, k_max, test_in_nbrs){
  ###MAKES PREDICTIONS ACCOUNTING FOR TIES###
  nbr_order      <- order(neighbors_col) #vector of indexes
  y_train_ordered<- y_train[nbr_order[(1+test_in_nbrs):(k_max*4)]]
  distances_vec  <- neighbors_col[nbr_order[(1+test_in_nbrs):(k_max*4)]]
  
  k_pred_vec      <- c()
  k = 1
  while(k <= k_max){
    if(distances_vec[k] == distances_vec[k+1]){
      #tie
      kth_dist    <- distances_vec[k]
      tie_end_ind <- detect_index(distances_vec, function(d) d > kth_dist)#detect index where tie ends
      k_pred_vec[k:(tie_end_ind-1)] <- mean(y_train_ordered[1:(tie_end_ind-1)])
      #print(paste(k, tie_end_ind))
      k           <- tie_end_ind
    } else {
      k_pred_vec[k]<- mean(y_train_ordered[1:k])
      k           <- k+1
    }
  }
  return(k_pred_vec[1:k_max])
}