
confusion.matrix <- function(pred.value, actual.value, cutoff=0.5){
  Confusion <- data.frame(pred=pred.value, actual=actual.value)
  Confusion[Confusion >= cutoff]=1
  Confusion[Confusion < cutoff]=0
  Confusion[,c(1,2)]=lapply

  N = nrow(Confusion)
  TP = nrow(Confusion[Confusion$pred=="1" & Confusion$actual=="1", ])
  TN = nrow(Confusion[Confusion$pred=="0" & Confusion$actual=="0", ])
  FP = nrow(Confusion[Confusion$pred=="1" & Confusion$actual=="0", ])
  FN = nrow(Confusion[Confusion$pred=="0" & Confusion$actual=="1", ])

  Accuracy <- (TP+TN)/N
  Prevalence <- (TP+FN)/N
  Sensitivity <- TP/(TP+FN)
  Specificity <- TN/(TN+FP)
  False.Discovery.Rate <- FP/(TP+FP)
  Diagnostic.Odds.Ratio <- (TP*TN)/(FN*FP)

  metrics <- c(Prevalence,Accuracy,Sensitivity,Specificity,False.Discovery.Rate,Diagnostic.Odds.Ratio)
  names(metrics) <-c("Prevalence",
                     "Accuracy",
                     "Sensitivity",
                     "Specificity",
                     "False.Discovery.Rate",
                     "Diagnostic.Odds.Ratio")

  matrix <- c(TP,FP,FN,TN)
  names(matrix) <- c("TP","FP","FN","TN")

  output.list <- list("metrics" = metrics,
                      "matrix" = matrix)
  return(output.list)
}


