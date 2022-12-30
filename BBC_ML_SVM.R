library(dplyr)
library(tidytext)
library(magrittr)
library(tm)
library(e1071)
library(caret)

CM <- function(org.class, pred.class) 
{
  CM <- table(org.class, pred.class)
  return(sum(diag(CM)) / sum(CM))
}

data.bbc <- as_tibble(read.table("http://www.fizyka.pw.edu.pl/~julas/TEXT/lab/data_bbc.csv", stringsAsFactors = F))

plus <- data.bbc[data.bbc$emo == 1,] %>% slice(c(1:250))
zero <- data.bbc[data.bbc$emo == 0,] %>% slice(c(1:500))
minus <- data.bbc[data.bbc$emo == -1,] %>% slice(c(1:250))
minus$emo = 1

subiective <- as_tibble(mapply(rbind,plus,minus,SIMPLIFY=TRUE))
data.bbc <- as_tibble(mapply(rbind,subiective,zero,SIMPLIFY=TRUE))

data.bbc$text <- sapply(data.bbc$text, enc2utf8)
data.bbc$emo <- as.factor(data.bbc$emo)
data.bbc <- data.bbc[sample(1:nrow(data.bbc)),]
data.bbc$doc_id <- 1:nrow(data.bbc)

source <- DataframeSource(as.data.frame(data.bbc))
corpus <- VCorpus(source)

corpus %<>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeNumbers)

tdm <- DocumentTermMatrix(corpus)
tdm <- tdm[, apply(tdm, 2, sum) > 4]

tdm <- as.matrix(tdm)
ind <- apply(tdm, 1, sum) > 1
tdm <- tdm[ind, ]
class <- data.bbc$emo[ind]

bbc.svml <- svm(tdm, class, type = "C-classification", kernel = "linear")
bbc.svml.pred <- predict(bbc.svml, tdm)

#Before Cross Validation
table(class, bbc.svml.pred)
CM(class, bbc.svml.pred) #0.9989837

#After...
levels(class) <- c("subjective", "objective")
data <- cbind(as.data.frame(tdm), class.out = class)

fit <- trainControl(method = "cv", number = 10)
model <- train(class.out ~ ., data = data, method = "svmLinear", trControl = fit)

#After...
model$results$Accuracy #0.6768433
