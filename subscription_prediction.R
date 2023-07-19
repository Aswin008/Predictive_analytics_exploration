setwd("/Users/aswin/Downloads")
music_data = read.csv("XYZData.csv")
IrisData = read.csv("iris.csv")

library(dplyr)
music_data %>% filter(adopter == 0) %>% count(n())
library(caret)
train_rows = createDataPartition(y = music_data$adopter, p = 0.7, list = FALSE)
music_data_train = music_data[train_rows,]
music_data_test = music_data[-train_rows,]


library(ROSE)
oversampled_data <- ovun.sample(adopter ~., data = music_data_train,
                                method = "over", p=0.3, seed = 123)
oversampled_data_train = oversampled_data$data

oversampled_data_train %>% filter(adopter == 0) %>% count(n())

normalize = function(x){
  return ((x - min(x))/(max(x) - min(x)))}

data_normaized = oversampled_data_train %>% mutate_at(2:27, normalize)
train_rows_normal = createDataPartition(y = data_normaized$adopter, p = 0.7, list = FALSE)

data_normaized_train = data_normaized[train_rows_normal,]
data_normaized_test = data_normaized[-train_rows_normal,]

library(class)

pred_knn = knn(train = data_normaized_train[,2:26],
               test = data_normaized_test[,2:26],
               cl = data_normaized_train[,27],
               k = 3)

confusionMatrix(data = pred_knn,
                reference = factor(data_normaized_test[,27]),
                mode = "prec_recall",
                positive = '1')

library(rpart)
tree = rpart(adopter ~ ., data = oversampled_data_train,
             method = "class",
             parms = list(split = "information"))
library(rpart.plot)
prp(tree, varlen = 0)


pred_tree = predict(tree, music_data_test, type = "class")

confusionMatrix(data = pred_tree,
                reference = factor(music_data_test[,27]),
                mode = "prec_recall",
                positive = '1')


