#Random Forest

library(randomForest) 

#CLEARING
rm(list = ls())

#LOAD CSV FILE
filename<-file.choose()
reviewdata<-read.csv(filename )
 
#VIEW DATA
View(reviewdata) 

#SEED
set.seed(1234)

#CREATING TESTING AND TRAINING DATASET REMOVING 1ST COLUMN
index<-sort(sample(nrow(reviewdata),round(.30*nrow(reviewdata))))
training<-reviewdata[-index,c(-1)]
test<-reviewdata[index,c(-1)]

#IMPLEMENT RANDOM FOREST
#install.packages("randomForest")
library(randomForest)
fit <- randomForest( LABEL ~RATING+VERIFIED_PURCHASE+PRODUCT_CATEGORY+TOTAL_WORDS+TOTAL_SENTENCES+TOTAL_PUNCTUATIONS+PRODUCTNAME_IN_REVIEW+TITLE_CHARACTERS+TOTAL_STOPWORDS, data = training, importance = TRUE, ntree = 500)

importance(fit)
varImpPlot(fit)
Prediction <- predict(fit, test)
table(actual=test[,1],Prediction)

#MEASURE ERROR RATE
wrong<- (test[,1]!=Prediction )
errorRate<-sum(wrong,na.rm = TRUE)/length(wrong)  
errorRate 

#MEASURING ACCURACY RATE
accuracy <- 1-errorRate
accuracy 

print(paste('Accuracy of Random Forest:', accuracy))

