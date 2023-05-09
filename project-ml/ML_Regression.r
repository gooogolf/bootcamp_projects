#install.packages("caret")
library(caret)


mtcars

################################
## split data ##
################################

#create function train_test_split() for 80:20
train_test_split <- function(data) {
  set.seed(42)
  n <- nrow(data)
  id <- sample(n, size=0.8*n)
  train_data <- data[id, ]
  test_data <- data[-id, ]
  return(list(train_data, test_data))
}

#apply data 'mtcars' into function 'train_test_split' and assign to 'split_data'
split_data <- train_test_split(mtcars)

#subseting data to view the value
split_data[[1]]
split_data[[2]]

################################
## train model ##
################################
lm_model <- train(mpg ~ hp, 
                  data = split_data[[1]],
                  method = "lm")

################################
## score and evaluate ##
################################
# create prediction vector
p <- predict(lm_model, 
             newdata = split_data[[2]])

error <- split_data[[2]]$mpg - p
rmse <- sqrt(mean(error**2)) #this is test data's RMSE

#calling 'lm_model' training's data to compare RMSE
lm_model

