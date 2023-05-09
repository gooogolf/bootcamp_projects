# Install packages if not already installed
# install.packages(c("tidyverse",
#                    "titanic",
#                    "ggthemes"))
library(tidyverse)
library(titanic)
library(ggthemes)

head(titanic_train)
#tail(titanic_train)

glimpse(titanic_train)
#View(titanic_train)

# Drop NA (missing value)
titanic_train <- na.omit(titanic_train)
nrow(titanic_train)

## Convert the data type of the Survived column to factor
titanic_train <- titanic_train %>% 
  mutate(Survived = factor(Survived,
                           levels = c(0, 1),
                           labels = c("Died", "Survived")))

# Recheck if 'Survived' column is factor
head(titanic_train)
glimpse(titanic_train)

## Split Data
set.seed(34)
n <- nrow(titanic_train)
id <- sample(1:n, size = n*0.7) ## 70% train 30% test
train_data <- titanic_train[id, ]
test_data <- titanic_train[-id, ] # minus for the rest

# check split row numbers
nrow(train_data)
nrow(test_data)

## Train Model
# Fit Logistic Regression with trained dataset
model_train <- glm(Survived ~ Pclass + Age, data = train_data, family = "binomial")
summary(model_train)

train_data$prob_survived <- predict(model_train, type="response") ## probability
train_data$pred_survived <- if_else(train_data$prob_survived >= 0.5, "Survived", "Died")

error_train <- train_data$prob_survived
rmse_train <- sqrt(mean( error_train**2 ))
## Evaluate Model
# confusion matrix
train_conM <- table(train_data$pred_survived, train_data$Survived,
                    dnn = c("Predicted", "Actual")) #dnn = the dimnames names


train_acc <- (train_conM[1,1] + train_conM[2,2]) / sum(train_conM)
train_prec <- train_conM[2,2] / (train_conM[2,1] +train_conM[2,2])
train_rcall <- train_conM[2,2] / (train_conM[1,2] +train_conM[2,2])
train_f1 <- 2 * ( (train_prec*train_rcall)/(train_prec+train_rcall) )

cat("Training data's RMSE:", rmse_train,
    "\nTraining data's Confusion Matrix",
    "\n    train_Accuracy:", train_acc,
    "\n    train_Precision:", train_prec,
    "\n    train_Recall:", train_rcall,
    "\n    train_F1 Score:", train_f1)


#############################################


## Test Model
# Fit Logistic Regression with tested dataset
model_test <- glm(Survived ~ Pclass + Age, data = test_data, family = "binomial")
summary(model_test)

test_data$prob_survived <- predict(model_test, type="response") ## probability
test_data$pred_survived <- if_else(test_data$prob_survived >= 0.5, "Survived", "Died")

error_test <- test_data$prob_survived
rmse_test <- sqrt(mean( error_test**2 ))

## Evaluate Model
# confusion matrix
test_conM <- table(test_data$pred_survived, test_data$Survived,
              dnn = c("Predicted", "Actual")) #dnn = the dimnames names


test_acc <- (test_conM[1,1] + test_conM[2,2]) / sum(test_conM)
test_prec <- test_conM[2,2] / (test_conM[2,1] +test_conM[2,2])
test_rcall <- test_conM[2,2] / (test_conM[1,2] +test_conM[2,2])
test_f1 <- 2 * ( (test_prec*test_rcall)/(test_prec+test_rcall) )

cat("Testing data's RMSE:", rmse_test,
    "\nTesting data's Confusion Matrix",
    "\n    test_Accuracy:", test_acc,
    "\n    test_Precision:", test_prec,
    "\n    test_Recall:", test_rcall,
    "\n    test_F1 Score:", test_f1)


#############################################

cat("Summary:\nTraining data's RMSE:", rmse_train,
    "\nTraining data's Confusion Matrix",
    "\n    train_Accuracy:", train_acc,
    "\n    train_Precision:", train_prec,
    "\n    train_Recall:", train_rcall,
    "\n    train_F1 Score:", train_f1,
    "\nTesting data's RMSE:", rmse_test,
    "\nTesting data's Confusion Matrix",
    "\n    test_Accuracy:", test_acc,
    "\n    test_Precision:", test_prec,
    "\n    test_Recall:", test_rcall,
    "\n    test_F1 Score:", test_f1)


combine_model_result <- data.frame(
  Type = c('Train', 'Test'),
  Accuracy = c(train_acc, test_acc),
  Precision = c(train_prec, test_prec),
  Recall = c(train_rcall, test_rcall),
  F1_Score = c(train_f1, test_f1))


# Prep data by pivot data from wide to long
prep_data <- combine_model_result %>%
  pivot_longer( - Type ,
               names_to = "ModEval_Type",
               values_to = "percent")

ggplot(prep_data, aes(ModEval_Type, percent, fill = Type)) +
  geom_bar(stat='identity', position = 'dodge') + #stat='identity' for mapping values to the y aesthetic
  coord_cartesian(ylim = c(0.45, 0.8)) + #adjust y-axis scale limits
  theme_minimal() +
  labs(title = "Train & Test Model Evaluation",
       x ="Model Evaluation", y="Percentage",
       caption = "Source: Titanic package")
