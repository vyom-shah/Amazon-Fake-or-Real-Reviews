#NAIVE BAYES

rm(list=ls())

#LOAD THE DATA
filename<-file.choose()
file<-read.csv(filename)


#CLASSIFYING INTO TRAINING AND TEST DATA
set.seed(123)

idx<-sort(sample(nrow(file),as.integer(.70*nrow(file))))
training<-file[idx,-1]
test<-file[-idx,-1]

#IMPLEMENT NAIVE BAYES
library(class)
#install.packages('e1071')
library(e1071)
naive_imp<-naiveBayes(formula=LABEL~RATING +VERIFIED_PURCHASE,data=training)
naive_predict<-predict(naive_imp,test)

#PRINT
table(NAIVE=naive_predict,class=test$LABEL)

#CALCULATE THE ERROR AND FINDING THE ACCURACY
error=sum(naive_predict!=test$LABEL)
error_rate<-error/length(naive_predict)
accuracy<-100-(error_rate*100)
 
error
error_rate
accuracy 

print(paste('Accuracy for Naive Bayes:', accuracy)) 
