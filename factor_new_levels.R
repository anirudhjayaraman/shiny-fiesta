library(data.table)

train <- fread('train.csv')
test <- fread('test.csv')

# consolidate the 2 data sets after creating a variable indicating train / test 
train$flag <- 0; test$flag <- 1
dat <- rbind(train,test)

# change outcome, var2 and var5 into factor var
dat$outcome <- factor(dat$outcome)
dat$var_b <- factor(dat$var_b)
dat$var_e <- factor(dat$var_e)

# get back the train and test data
train <- subset(dat, flag == 0); test <- subset(dat, flag == 1)
train$flag <- NULL; test$flag <- NULL

# Build Logit Model using train data
logitModel <- glm(outcome ~ ., data = train, family = 'binomial')

# Model Predictions on train data
preds_train <- predict(logitModel, type = 'response')

# Model Predictions on test data
preds_test <- predict(logitModel, newdata = test, type = 'response')
# running the above code gives us the following error:
# factor var2 has new levels 16060, 17300, 17980, 19060, 21420, 21820, 
# 25220, 29340, 30300, 33260, 34100, 38340, 39660, 44300, 45460

# Workaround:
source('remove_missing_levels.R')
preds_test <- predict(logitModel, 
                      newdata = remove_missing_levels(fit = logitModel, test_data = test), 
                      type = 'response')
