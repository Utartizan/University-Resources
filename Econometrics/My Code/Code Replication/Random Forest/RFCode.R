set.seed(1)

install.packages(c("tm", "caTools", "rpart", "randomForest"))

library(tm)
library(caTools)
library(rpart)
library(randomForest)

emails <- read.csv("emails.csv", stringsAsFactors = FALSE)

table(emails$spam)

grineer <- VCorpus(VectorSource(emails$text))

grineer <- tm_map(grineer, content_transformer(toupper))

grineer <- tm_map(grineer, removePunctuation)

grineer <- tm_map(grineer, removeWords, stopwords("English"))

grineer <- tm_map(grineer, stemDocument)



documentTermMatrix <- DocumentTermMatrix(grineer)

sparce_dTM <- removeSparseTerms(documentTermMatrix, 0.95)

emails_data <- as.data.frame(as.matrix(sparce_dTM))

colnames(emails_data) <- make.names(colnames(emails_data))


emails_data$spam <- as.factor(emails$spam)

split <- sample.split(emails_data$spam, SplitRatio = 0.8)

train <- subset(emails_data,
                split == TRUE)

test <- subset(emails_data,
               split == FALSE)

randomTree <- rpart(spam ~ ., data = train, method = "class")

rfModel <- randomForest(spam ~ ., data = train)


tree_preds <- predict(randomTree, newdata = test)[, 2]  

rf_preds <- predict(rfModel, newdata = test, type = "prob")[, 2]


tree_table <- table(test$spam, tree_preds > 0.5)

rf_table <- table(test$spam, rf_preds > 0.5)

print("Decision Tree Confusion Matrix:")

print(tree_table)

print("Random Forest Confusion Matrix:")

print(rf_table)