remove(list=ls())

library(kknn)

reviews <- read.csv(file.choose(), header = TRUE)
reviews$LABEL = factor(reviews$LABEL, labels = c('Fake', 'Genuine'))

# Use min-max normalization to normalize data
mmnorm <- function(x,minx,maxx) {
  z <- ((x - minx)/(maxx - minx))
  return(z) 
}

# Define 'x', 'minx' and 'maxx' for 'mmnorm' function
reviews_norm <- as.data.frame(cbind(
  DOC_ID = reviews$DOC_ID,
  LABEL = reviews$LABEL,
  RATING = mmnorm(as.numeric(reviews[,3]), min(as.numeric(reviews[,3])), max(as.numeric(reviews[,3]))), 
  VERIFIED_PURCHASE = mmnorm(as.numeric(reviews[,4]), min(as.numeric(reviews[,4])), max(as.numeric(reviews[,4]))), 
  PRODUCT_CATEGORY = reviews$PRODUCT_CATEGORY,
  PRODUCT_ID = reviews$PRODUCT_ID,
  PRODUCT_TITLE = reviews$PRODUCT_TITLE,
  REVIEW_TITLE = reviews$REVIEW_TITLE,
  REVIEW_TEXT = reviews$REVIEW_TEXT,
  TOTAL_WORDS = mmnorm(as.numeric(reviews[,10]), min(as.numeric(reviews[,10])), max(as.numeric(reviews[,10]))), 
  TOTAL_SENTENCES = mmnorm(as.numeric(reviews[,11]), min(as.numeric(reviews[,11])), max(as.numeric(reviews[,11]))),
  TOTAL_PUNCTUATIONS = mmnorm(as.numeric(reviews[,12]), min(as.numeric(reviews[,12])), max(as.numeric(reviews[,12]))),
  PRODUCTNAME_IN_REVIEW = mmnorm(as.numeric(reviews[,13]), min(as.numeric(reviews[,13])), max(as.numeric(reviews[,13]))),
  TITLE_CHARACTERS = mmnorm(as.numeric(reviews[,14]), min(as.numeric(reviews[,14])), max(as.numeric(reviews[,14]))),
  TOTAL_STOPWORDS = mmnorm(as.numeric(reviews[,15]), min(as.numeric(reviews[,15])), max(as.numeric(reviews[,15]))), 
  TITLE_PUNCTUATIONS = mmnorm(as.numeric(reviews[,16]), min(as.numeric(reviews[,16])), max(as.numeric(reviews[,16])))
)
)

# Factor the 'LABEL' column in the dataframe
reviews_norm$LABEL = factor(reviews_norm$LABEL, labels = c('Fake', 'Genuine'))


# Divide the data : 70% training 30% testing
index <- sort(sample(nrow(reviews_norm), as.integer(.70*nrow(reviews_norm))))
training <- reviews_norm[index,]
testing <- reviews_norm[-index,]


# k-Nearest Neighbour for k = 5 
predict_k5 <- kknn(formula = LABEL ~RATING+VERIFIED_PURCHASE+TOTAL_WORDS+TOTAL_SENTENCES+TOTAL_PUNCTUATIONS+PRODUCTNAME_IN_REVIEW+TITLE_CHARACTERS+TOTAL_STOPWORDS+TITLE_PUNCTUATIONS+REVIEW_TITLE+REVIEW_TEXT, training, testing, k=5, kernel = "rectangular")

fit_k1 <- fitted(predict_k5)
table(testing$LABEL, fit_k1) 

# calculate error rate
kknn_wrong_k5 <- sum(fit_k1 != testing$LABEL)
kknn_error_rate_k5 <- kknn_wrong_k5/length(fit_k1)
kknn_error_rate_k5

# k-Nearest Neighbour for k = 10
predict_k10 <- kknn(formula = LABEL ~RATING+VERIFIED_PURCHASE+TOTAL_WORDS+TOTAL_SENTENCES+TOTAL_PUNCTUATIONS+PRODUCTNAME_IN_REVIEW+TITLE_CHARACTERS+TOTAL_STOPWORDS+TITLE_PUNCTUATIONS+REVIEW_TITLE+REVIEW_TEXT, training, testing, k=10, kernel = "rectangular")

fit_k2 <- fitted(predict_k10)
table(testing$LABEL, fit_k2)

# calculate error rate
kknn_wrong_k10 <- sum(fit_k2 != testing$LABEL)
kknn_error_rate_k10 <- kknn_wrong_k10/length(fit_k2)
kknn_error_rate_k10

# k-Nearest Neighbour for k = 15 - BEST
predict_k15 <- kknn(formula = LABEL ~RATING+VERIFIED_PURCHASE+PRODUCT_TITLE+PRODUCT_CATEGORY+TOTAL_WORDS+TOTAL_SENTENCES+TOTAL_PUNCTUATIONS+PRODUCTNAME_IN_REVIEW+TITLE_CHARACTERS+TOTAL_STOPWORDS+TITLE_PUNCTUATIONS+REVIEW_TITLE+REVIEW_TEXT, training, testing, k=15, kernel = "rectangular")

fit_k3 <- fitted(predict_k15)
table(testing$LABEL, fit_k3)

# calculate error rate
kknn_wrong_k15 <- sum(fit_k3 != testing$LABEL)
kknn_error_rate_k15 <- kknn_wrong_k15/length(fit_k3)
kknn_error_rate_k15

accuracy_k5 <- 100-(kknn_error_rate_k5*100)
accuracy_k10 <- 100-(kknn_error_rate_k10*100)
accuracy_k15 <- 100-(kknn_error_rate_k15*100)

print(paste('Knn accuracy rate for k=5 is :', accuracy_k5))
print(paste('Knn accuracy rate for k=10 is :', accuracy_k10))
print(paste('Knn accuracy rate for k=15 is :', accuracy_k15))


