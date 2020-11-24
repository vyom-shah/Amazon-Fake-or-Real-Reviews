##################################################

remove(list=ls())

filename<-file.choose()
file<-  read.csv(filename) 


#install.packages("rpart")
#install.packages("rpart.plot")     # Enhanced tree plots
#install.packages("rattle")         # Fancy tree plot
#install.packages("RColorBrewer")   # colors needed for rattle
library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer)     # colors needed for rattle

set.seed(123)
index <- seq (1,nrow(file),by=5) 
test<-file[index,]
training<-file[-index,]

?rpart()
#Grow the tree  
 
CART_class<-rpart(formula = LABEL ~RATING+VERIFIED_PURCHASE+TOTAL_WORDS+TOTAL_SENTENCES+TOTAL_PUNCTUATIONS+PRODUCTNAME_IN_REVIEW+TITLE_CHARACTERS+TOTAL_STOPWORDS+TITLE_PUNCTUATIONS, data=training[,-1])
#rpart.plot(CART_class)
CART_predict2<-predict(CART_class,test, type="class")
df<-as.data.frame(cbind(test,CART_predict2))
table(Actual=test[,"LABEL"],CART=CART_predict2)

CART_wrong<-sum(test[,"LABEL"]!=CART_predict2)

error_rate=CART_wrong/length(test$LABEL)
error_rate
accuracy<-100-(error_rate*100)
accuracy

print(paste("Accuracy of cart:",accuracy))

dev.off()






