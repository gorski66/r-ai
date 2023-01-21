library(caTools)
library(class)


letterrec <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/letter-recognition/letter-recognition.data", header = FALSE,stringsAsFactors = TRUE)
letterrec$V17 <- as.integer(as.factor(letterrec$V17))
l <- c()

for (x in 1:100)
{
  split <- sample.split(letterrec, SplitRatio = 0.25)
  train_set <- subset(letterrec, split == "TRUE")
  test_set <- subset(letterrec, split == "FALSE")
  train <- train_set[, 2:17]
  test <- test_set[, 2:17]
  cl_train <- train_set[, 1]
  cl_test <- test_set[, 1]
  knn <- knn(train, test, cl = cl_train, k = 3)
  cm <- table(knn, cl_test)
  print(cm)
  p <- sum(diag(cm)) / sum(cm)
  l <- append(l, p)
}
sd(l)
mean(l)
