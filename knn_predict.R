library(purrr) #for detect_index in calc_pred (ties)

knn_predict <- function(X_train, X_test, y_train, k, test_in_nbrs = F, use_all=T, pure_binary=F){
  #generate predictions for a single K on a test data set
  #:X_train: a Data Table (not matrix) of features (neighbors)
  #:X_test: a Data Table (not matrix) of features (testing group to make predictions for)
  #:y_train: class/outcome vector for X_train
  #:k: the k nearest neighbors to use for predictions
  #:test_in_nbrs: this function could be called to predict X_train using X_train ->
  #               (knn_predict(X_train=traindata, X_test=traindata, y_train=...),
  #               so this option specifies whether the testing group is present in the training group
  #               but you should just use knn_fit(X_train, ..., cv=0)) if you want to predict X_train on X_train
  #:use_all: should all values of points in y_train be used in the event of a tie between distances?
  #:pure_binary: is the data purely 1's and 0's? If so, faster distance methods can be used

  distances <- data.table(dist_X_Y(as.matrix(X_train), as.matrix(X_test))) #full matrix, not sparse
  
  if(use_all){
    predictions <- data.table(vapply(1:ncol(distances), function(colnum){
      #cat('\r',colnum);
      calc_pred_k(distances[[colnum]], y_train, k, test_in_nbrs = test_in_nbrs)}, 
      numeric(1), USE.NAMES = F)) #returns a dataframe of predictions for each user
  } else {
    predictions <- data.table(vapply(1:ncol(distances), function(colnum){
      nbr_order <- order(distances[[colnum]])[(1+test_in_nbrs):(k+test_in_nbrs)];
      mean(y_train[nbr_order])}, numeric(1), USE.NAMES = F))
  }
  return(predictions)
}

calc_pred_k <- function(neighbors_col, y_train, k, test_in_nbrs = F){
  ###MAKES PREDICTIONS ACCOUNTING FOR TIES###
  nbr_order      <- order(neighbors_col) #vector of indexes
  y_train_ordered<- y_train[nbr_order[(1+test_in_nbrs):(k*4)]]
  distances_vec  <- neighbors_col[nbr_order[(1+test_in_nbrs):(k*4)]]
  
  if(distances_vec[k] == distances_vec[k+1]){
    #tie
    kth_dist    <- distances_vec[k]
    tie_end_ind <- detect_index(distances_vec, function(d) d > kth_dist) #detect index where tie ends
    return(mean(y_train_ordered[1:(tie_end_ind-1)]))
  }
  return(mean(y_train_ordered[1:k]))
}
