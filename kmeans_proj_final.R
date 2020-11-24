rm(list = ls())
 
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


reviews_dist <- dist(reviews_norm[, c(-2, -1, -5, -6, -7)])
hclust_resutls <- hclust(reviews_dist)
plot(hclust_resutls)
hclust_2 <- cutree(hclust_resutls, 2)
table(hclust_2, as.numeric(reviews_norm[,2]))


kmeans_2 <- kmeans(reviews_norm[,-2], 2, nstart = 10)
kmeans_2$cluster
table(kmeans_2$cluster, reviews_norm[,2])

kmeans_wrong <- (kmeans_2$cluster != as.numeric(reviews_norm[,2]))
rate <-sum(kmeans_wrong)/length(kmeans_wrong)
rate

accuracy <- 1-rate
accuracy
