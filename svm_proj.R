# First Name: Hiloni
#Last Name: Mehta
#Project: KDDM SVM 
###################################################################
remove(list=ls())

filename<-file.choose()
file<-  read.csv(filename) 

# create test and training dataset
set.seed(123) 
index <- seq (1,nrow(file),by=5)
test<-file[index,]
training<-file[-index,] 
library(e1071)



?svm
svm.model <- svm(LABEL ~RATING+VERIFIED_PURCHASE+TOTAL_WORDS+TOTAL_SENTENCES+TOTAL_PUNCTUATIONS+PRODUCTNAME_IN_REVIEW+TITLE_CHARACTERS+TOTAL_STOPWORDS+TITLE_PUNCTUATIONS, data =training  )
svm.pred <- predict(svm.model,  test )

table(actual=test[,2],svm.pred )

SVM_wrong<- (test$LABEL!=svm.pred)
error_rate<-sum(SVM_wrong)/length(SVM_wrong)
error_rate
accuracy<-100-(error_rate*100)
accuracy
print(paste('The accuracy of SVM is :', accuracy)) 
