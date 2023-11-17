library(readr)
library(glmnet)
train_df <- read_csv("~/206/dmba/framingham_train.csv")
valid_df <- read_csv("~/206/dmba/framingham_valid.csv")

y <- train_df$'TenYearCHD'
X <- model.matrix(TenYearCHD ~ ., train_df)[, -1] #remove intercept as glmnet will add intercept

# lasso L1
# Find the best lambda using cross-validation
set.seed(123) 
cv.lasso <- cv.glmnet(X, y, alpha = 1, family = "binomial")
# Fit the final model on the training data
modelL1 <- glmnet(X, y, alpha = 1, family = "binomial",
                  lambda = cv.lasso$lambda.min)
# Display regression coefficients
coef(modelL1)

# ELASTIC NET WITH 0 < ALPHA < 1
a <- seq(0.1, 0.9, 0.05)
library(foreach)
search <- foreach(i = a, .combine = rbind) %dopar% {
  cv <- cv.glmnet(X, y, family = "binomial", nfold = 10, type.measure = "deviance", paralle = FALSE, 
                  alpha = i)
  data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.1se], lambda.1se = cv$lambda.1se, alpha = i)
}
cv3 <- search[search$cvm == min(search$cvm), ]
cv3
md3 <- glmnet(X, y, family = "binomial", lambda = cv3$lambda.1se, alpha = cv3$alpha)
coef(md3)
