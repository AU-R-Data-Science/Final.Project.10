#' Uniform data format for predictor variables
#' @description This function converts the input predictor variables into matrix format and adds intercepts variable accord by user
#' @param X An \eqn{n \times p} \code{double} value of the matrix containing the values of the predictors.
#' @param intercept A logical variable indicating whether to add an intercept to the predictor variables
#' @return A matrix containing the predictor variables
#' @author Fangjian Yang
#' @export
#'
X.format <- function(X,intercept=F)
{
  X=as.matrix(X)
  if(dim(X)[2]==1){colnames(X) <- "Predictor"}
  if(intercept){
    intercept <- rep(1,nrow(X))
    X <- as.matrix(cbind(intercept,X))
  }
  return(X)
}

#' Calculate the initial beta value
#' @description {Initial values for optimization obtained from the least-squares formula \deqn{(X^{T}X)^{−1}X^{T}Y}\\}
#' @param X An \eqn{n \times p} \code{double} value of the matrix containing the values of the predictors(Not including intercept).
#' @param Y A factor vector indicating the category of response
#' @return The initial beta vector for loss function
#' @author Fangjian Yang
#' @export
#'
Beta.init <- function(X,Y){
  Xi=X.format(X,intercept = T)
  Y=as.numeric(Y)-1
  Beta <- solve(t(Xi)%*%Xi)%*%t(Xi)%*%Y
  return(Beta)
}


#' Calculate the Pi vector for loss function
#' @description {the Pi vector for loss function from the formula \deqn{P_{i}=\frac{1}{1+e^{-x^{T}_{i}\beta}}}\\}
#' @param beta  The current beta vector(Including intercept).
#' @param Xi An \eqn{n \times p} \code{double} value of the matrix containing the values of the predictors(Including intercept).
#' @return The Pi value for loss function
#' @author Fangjian Yang
#' @export
#'
P.i <- function(beta,Xi){
  X=X.format(Xi,intercept = F)
  output <- rep(NA,nrow(X))
  for(i in 1:nrow(X)){
    output[i] <- 1/(1+exp(-t(X[i,])%*%beta))
  }
  return(output)
}

#' The numerical optimization loss function
#' @description {The estimator to be computed using numerical optimization is the following: \deqn{\hat\beta=\underset{\beta}{argmin}\sum_{i=1}^{n}（-y_{i}\cdot ln(p_{i})-(1-y_{i} \cdot ln(1-p_{i}))）}\\}
#' @param beta  The current beta vector(Including intercept).
#' @param y  A training factor vector indicating the category of response
#' @param xi An \eqn{n \times p} \code{double} value of the matrix containing the values of the predictors(Including intercept).
#' @return The estimator of loss function
#' @author Fangjian Yang
#' @export
#'
loss_func <- function(beta,y,xi){
  p <- P.i(beta,xi)
  temp <- -y*log(p)-(1-y)*log(1-p)
  return(sum(temp))
}
