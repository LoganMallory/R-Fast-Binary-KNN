distlower <- function(X){
  #computes the Euclidean distance (doesn't sqrt) between all rows in X (dim nxm)
  #runtime only increases as n increases
  #starts beating dist() when m > 100
  
  #transpose for faster access to rows (which become columns)
  X     <- data.table(t(X))
  ncols <- ncol(X)
  #compute lower triangle of distance matrix
  dlist <- sapply(2:ncols, function(col_i){ 
    vapply(1:(col_i-1), function(col_k){
      sum((X[[col_i]]-X[[col_k]])^2);
    }, numeric(1))
  }, USE.NAMES = F)
  #unlist the list returned by sapply
  dvec  <- unlist(dlist, F, F) 
  #move dvec into lower triangle of matrix, not sparse (yet...)
  dmat <- matrix(nrow=ncols, ncol=ncols)
  dmat[upper.tri(dmat, diag=FALSE)] <- dvec
  return(t(dmat))
}


#for binary data:
dist_X <- function(X) {
  #computes Euclidean distance (doesn't sqrt) between all rows in X
  #does not handle NA's
  #:X: a matrix of dim nxm
  #returns full (not sparse) matrix
  D <- (!X) %*% t(X)
  return(D + t(D))
}

dist_X_Y <- function(X, Y) {
  #computes Euclidean distance (doesn't sqrt) between rows in X and Y
  #does not handle NA's
  #:X: a matrix of dim nxm
  #:Y: a matrix of dim jxm
  #returns full (not sparse) matrix
  H <- X %*% t(Y)
  H <- H + (!X) %*% t(!Y)
  return(ncol(X) - H)
}
