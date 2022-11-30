

metrics.plot <- function(X, Y, interval=c(0.1,0.9), step=0.1){
  library(ggplot2)
  model = Beta.hat(X, Y)
  predict <- logistic_pred(model, X)
  actual.value <- as.numeric(Y)-1
  cutoff.list <- seq(interval[1],interval[2],step)
  n = length(cutoff.list)
  data <- matrix(NA,nrow=n,ncol=6)
  for(i in 1:n){
    data[i, ] <- confusion.matrix(predict,
                                  actual.value,
                                  cutoff=cutoff.list[i])$metrics
  }
  image <- data.frame(cutoff=rep(cutoff.list,6),
                      value=c(data[ ,1],
                              data[ ,2],
                              data[ ,3],
                              data[ ,4],
                              data[ ,5],
                              data[ ,6]),
                      group=rep(c("Prevalence",
                                  "Accuracy",
                                  "Sensitivity",
                                  "Specificity",
                                  "False.Discovery.Rate",
                                  "Diagnostic.Odds.Ratio"),
                                n,
                                each=n))
  plot <- ggplot(data=image,aes(x=cutoff, y=value, color=group)) + geom_line(size=1)
  return(plot)
}



