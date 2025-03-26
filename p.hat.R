# Logistic regression
p.hat <- function(object, X) {
   X <- as.matrix(X)
   term <- cbind(rep(1,dim(X)[1]), X)%*%as.matrix(object$coefficients)
   1/(1+exp(-term))
}