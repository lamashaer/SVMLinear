

rm(list = ls())
library(e1071)

# load data
x <- quadX_norm_out_dd6   # covariate   
y <- y_out_dd6...Copy

xrows<-nrow(x)
x<-as.matrix(x[1:xrows,1:44])
y<- as.matrix(y[1:xrows,1])

svm_model <- svm(x,y, kernel = 'linear')
summary(svm_model)
pred <- predict(svm_model,x)
true <- numeric(xrows)
for (j in 1:xrows)
{
   if (sign(y[j]) == sign(pred[j])){
      true[j] <- 1
   }
}

sheet1 <- cbind(pred,y,x)


print(sum(true>0))
##Check if output > 0 or less than 0 
index1 <- which(pred==min(pred[pred]>0))
index2 <- which(pred==max(pred[pred]<0))
bnew <- (-0.5)*pred[index1]+pred[index2]
