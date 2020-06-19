#IMPORTING DATA SET
Test <- read.csv("C:/Users/Abhishek/Downloads/5. ECOMMERCE CASE STUDY - CLASSIFICATION/test.csv",header=TRUE,sep = ",")
Train <-read.csv("C:/Users/Abhishek/Downloads/5. ECOMMERCE CASE STUDY - CLASSIFICATION/train.csv",header=TRUE,sep = ",")
install.packages(c('dplyr','ROSE','caret','pROC','e1071','ROCR'))
#LOADING PACKAGES
library(ROSE)
library(caret)
library(dplyr)          
library(pROC)  
library(e1071)        
library(ROCR)
library(nnet)
library(metrics)
#USER DEFINE FUNCTION
mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.10)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.90)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+3*s
  LC <- m-3*s
  outlier_flag<- max>UC | min<LC
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}
options(scipen = 999)
vars <- c("unique_id","metric1","metric2","metric3","metric4","metric5","binary_var1","binary_var2",
          "page1_top","page1_exits","page2_exits","page3_exits","page4_exits","page5_exits","page6_exits","page2_top",
          "page3_top","page4_top","page5_top","page6_top","visited_page1","visited_page2","visited_page3","visited_page4",
          "visited_page5","visited_page6","target")

vars1 <-c("unique_id","metric1","metric2","metric3","metric4","metric5","binary_var1","binary_var2",
          "page1_top","page1_exits","page2_exits","page3_exits","page4_exits","page5_exits","page6_exits","page2_top",
          "page3_top","page4_top","page5_top","page6_top","visited_page1","visited_page2","visited_page3","visited_page4",
          "visited_page5","visited_page6")

diag_stats<-t(data.frame(apply(Train[vars], 2, mystats)))
diag_stats1<-t(data.frame(apply(Test[vars1], 2, mystats)))

#OUTLIER TREATMENT
Train$unique_id[Train$unique_id>344362.600000]<-344362.600000
Train$metric1[Train$metric1>87.000000]<-87.000000
Train$metric2[Train$metric2>5.000000]<-5.000000
Train$metric3[Train$metric3>768.000000]<-768.000000
Train$metric4[Train$metric4>2492.000000]<-2492.000000
Train$page1_top[Train$page1_top>5.716667]<-5.716667
Train$page1_exits[Train$page1_exits>0.000000]<-0.000000
Train$page2_exits[Train$page2_exits> -1.000000]<- -1.000000
Train$page3_exits[Train$page3_exits>1.000000]<-1.000000
Train$page4_exits[Train$page4_exits>0.000000]<-0.000000
Train$page5_exits[Train$page5_exits> -1.000000]<- -1.000000
Train$page6_exits[Train$page6_exits>2.000000]<-2.000000
Train$page2_top[Train$page2_top>0.000000]<-0.000000
Train$page3_top[Train$page3_top>146.918056]<-146.918056
Train$page4_top[Train$page4_top>6.000000]<-6.000000
Train$page5_top[Train$page5_top>0.00000]<-0.00000
Train$page6_top[Train$page6_top>581.000000]<-581.000000
Train$visited_page2[Train$visited_page2>0.000000]<-0.000000
Train$visited_page5[Train$visited_page5>0.000000]<-0.000000

Test$unique_id[Test$unique_id>86091.40]<-86091.40
Test$metric1[Test$metric1>69.00]<-69.00
Test$metric2[Test$metric2>4.00]<-4.00
Test$metric3[Test$metric3>806.40]<-806.40
Test$metric4[Test$metric4>2085.00]<-2085.00
Test$page1_top[Test$page1_top>0.00]<-0.00
Test$page1_exits[Test$page1_exits> -1.00]<- -1.00
Test$page2_exits[Test$page2_exits> -1.00]<- -1.00
Test$page3_exits[Test$page3_exits>1.000000]<-1.000000
Test$page4_exits[Test$page4_exits> -1.00]<- -1.00
Test$page5_exits[Test$page5_exits> -1.000000]<- -1.000000
Test$page6_exits[Test$page6_exits>2.000000]<-2.000000
Test$page2_top[Test$page2_top>0.000000]<-0.000000
Test$page3_top[Test$page3_top>92.28]<-92.28
Test$page4_top[Test$page4_top>0.00]<-0.00
Test$page5_top[Test$page5_top>0.00000]<-0.00000
Test$page6_top[Test$page6_top>417.80]<-417.80
Test$visited_page1[Test$visited_page1>0.000000]<-0.000000
Test$visited_page2[Test$visited_page2>0.000000]<-0.000000
Test$visited_page4[Test$visited_page4>0.000000]<-0.000000
Test$visited_page5[Test$visited_page5>0.000000]<-0.000000

#
Train$page1_exits <- NULL
Train$page2_exits<-NULL
Train$page3_exits<-NULL
Train$page4_exits<-NULL
Train$page5_exits<-NULL
Train$page6_exits<-NULL
Train$region<-NULL
Train$sourceMedium<-NULL
Train$device<-NULL
Train$country<-NULL
Train$dayHourMinute<-NULL

Test$page1_exits <- NULL
Test$page2_exits<-NULL
Test$page3_exits<-NULL
Test$page4_exits<-NULL
Test$page5_exits<-NULL
Test$page6_exits<-NULL
Test$region<-NULL
Test$sourceMedium<-NULL
Test$device<-NULL
Test$country<-NULL
Test$dayHourMinute<-NULL

#
trainset <- Train[1:2500, ]
testset <- Test[1:1500, ]

trainset$page2_top<-NULL
trainset$visited_page2<- NULL
trainset$page5_top<-NULL
trainset$visited_page5<-NULL
trainset$unique_id<-NULL


trainset$target<-factor(trainset$target)

#MODEL 1
mysvm1<- svm(target ~ .,data=trainset,type='C',kernel='linear')

#confusion Matrix
pred1 = predict(mysvm1,trainset[,-1])
table(pred1, trainset[,1])
mean(pred1==trainset[,1])

#ROC
roc.curve(trainset$target, pred1, plotit = T)
accuracy.meas(trainset$target,pred1)

#MODEL 2
mysvm2<-svm(target~.,data = trainset,type='C',kernel='polynomial',degree=2)
#confusion Matrix
pred2 = predict(mysvm2,trainset[,-1])
table(pred2, trainset[,1])
mean(pred2==trainset[,1])

#ROC
roc.curve(trainset$target, pred2, plotit = T)
accuracy.meas(trainset$target,pred2)

ininstall.packages('neuralnet')
#MODEL 3
#ANN
table(trainset$target)
myann1 <- multinom(target~., data=trainset, maxit=500, trace=T)

#confusion matrix
predan1 = predict(myann1,trainset[,-1])
table(predan1, trainset[,1])
mean(predan1==trainset[,1])

#ROC
roc.curve(trainset$target, predan1, plotit = T)
accuracy.meas(trainset$target,predan1)

#MODEL 4
#naives bayesian
nb_model <- naiveBayes(target~.,data = trainset, trControl=trainControl(method='cv',number=10))
nb_test_predict <- predict(nb_model,trainset[,-1])

#confusion matrix
table(pred=nb_test_predict,true=trainset[,1])
mean(nb_test_predict==trainset[,1])

#ROC
roc.curve(trainset$target, nb_test_predict, plotit = T)
accuracy.meas(trainset$target,nb_test_predict)

#Predicting on testing data using MODEL 2
predtarget = predict(mysvm2,testset)
predfinal<-cbind.data.frame(testset$unique_id,predtarget)

#EXPORTING DATA
write.csv(predfinal,file = 'C:/Users/Abhishek/Desktop/ecomm excel/Target.csv')

