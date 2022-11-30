logistic_plot <- function(X,Y){
  library(ggplot2)
  n=dim(data.frame(X))[2]
  if(n==1)
  {
    plot=data.frame(X=X,response=as.factor(Y))
  }else{
    plot=data.frame(X,response=as.factor(Y))
  }
  plot.list <- list()
  var.list <- colnames(plot)
  for(i in 1:n)
  {
    temp.data <- data.frame(x=plot[,i],y=as.numeric(plot$response)-1)
    gp <- ggplot(temp.data, aes(x=x, y=y)) +
      geom_point(alpha=.5) +
      labs(x=var.list[i],y = "Response")+
      stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial),col="red", lty=2)
    plot.list[i] <- list(gp)
  }
  return(plot.list)
}

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


Beta.init <- function(X,Y){
  Xi=X.format(X,intercept = T)
  Y=as.numeric(Y)-1
  Beta <- solve(t(Xi)%*%Xi)%*%t(Xi)%*%Y
  return(Beta)
}

P.i <- function(beta,X){
  X=X.format(X,intercept = F)
  output <- rep(NA,nrow(X))
  for(i in 1:nrow(X)){
    output[i] <- 1/(1+exp(-t(X[i,])%*%beta))
  }
  return(output)
}

loss_func <- function(beta,y,x){
  p <- P.i(beta,x)
  temp <- -y*log(p)-(1-y)*log(1-p)
  return(sum(temp))
}

Beta.hat <-  function(X,Y,method="BFGS"){
  Beta <- Beta.init(X,Y)
  Xi=X.format(X,intercept = T)
  Y=as.numeric(Y)-1
  Beta.hat <- optim(Beta,loss_func,y=Y,x=Xi,method=method)$par
  return(Beta.hat)
}


boot.confi <- function(X,Y,alpha,B=20){
  X=X.format(X,intercept = F)
  n=nrow(X)
  boot_mat <- matrix(data = NA,nrow = B,ncol = ncol(X)+1)
  colnames(boot_mat) <- c("intercept",colnames(X))
  for(i in 1:B){
    resample <- sample(1:n, replace = TRUE)
    boot_mat[i,]  <- Beta.hat(X[resample,],Y[resample])
  }
  confi.interval <- apply(boot_mat,2,quantile,probs=c(alpha/2,1-alpha/2))
  return(confi.interval)
}


logistic.regression <- function(X.temp, Y.temp, method="BFGS", cutoff=0.5, alpha=0.1,B=20){
  beta.initial <- Beta.init(X.temp, Y.temp)
  model <- Beta.hat(X.temp, Y.temp,method)
  CI <- boot.confi(X.temp, Y.temp, alpha,B=20)
  predict <- logistic_pred(model, X.temp)
  actual.value <- as.numeric(Y.temp)-1
  Analysis <- confusion.matrix(predict, actual.value, cutoff=cutoff)

  Yi <- as.numeric(Y.temp)-1
  level <- as.character(unique(Y.temp))
  names(level) <- unique(Yi)
  matrix <- matrix(Analysis$matrix,nrow=2,ncol=2)
  rownames(matrix) <- c(paste("Actual.", level["1"],sep=""),
                        paste("Actual.", level["0"],sep=""))
  colnames(matrix) <- c(paste("Predicted.", level["1"],sep=""),
                        paste("Predicted.", level["0"],sep=""))

  beta.info <- data.frame(model,beta.initial, t(CI))
  colnames(beta.info) <- c("Beta.hat","Beta.initial",paste("CI:",alpha/2,"%",sep=""),paste("CI:",1-alpha/2,"%",sep=""))
  plot <- metrics.plot(X.temp,Y.temp)
  result.list <- list("Level"=level,"Beta"=beta.info,"Confusion.Matrix"=matrix,"Metrics"=Analysis$metrics,"Plot"=plot)
  result.list
  return(result.list)
}


