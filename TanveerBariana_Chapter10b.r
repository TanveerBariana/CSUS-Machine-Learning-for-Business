library(readr)
ebay_train_y <- read_csv("~/206/dmba/ebay_train_y.csv")
ebay_train_X <- read_csv("~/206/dmba/ebay_train_X.csv")
ebay_valid_y <- read_csv("~/206/dmba/ebay_valid_y.csv")
ebay_valid_X <- read_csv("~/206/dmba/ebay_valid_X.csv")

train.df <- cbind(ebay_train_X, ebay_train_y)
valid.df <- cbind(ebay_valid_X, ebay_valid_y)

########################################################
#do this part to remove close price if you want to exclude it for realistic purposes
train.df <- subset(train.df,select=-c(ClosePrice,const))
valid.df <- subset(valid.df,select=-c(ClosePrice,const))
########################################################33

y <- train.df$'Competitive'
X <- model.matrix(Competitive ~ ., train.df)[, -2] #remove intercept as glmnet will add intercept

library(glmnet)

# logistic Model
# run logistic model, and show coefficients and odds
lm.fit <- glm(Competitive ~ ., data=train.df, family = "binomial")
data.frame(summary(lm.fit)$coefficients, odds = exp(coef(lm.fit)))


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

coef(modelL1)
coef(md3)
