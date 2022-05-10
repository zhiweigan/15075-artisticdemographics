#Single Linear Regression
##The first thing that we do is build scatter plots for all relevant variables
pairs(~SURV_LANG+GENDER+AGE7+EDUC4,data=data, main="Scatterplot Matrix for All Relevant Variables")

qqnorm(data$ART_SCORE, pch = 1, frame = FALSE)
qqline(data$ART_SCORE, col = "steelblue", lwd = 2)

qqnorm(data$SURV_LANG, pch = 1, frame = FALSE)
qqline(data$SURV_LANG, col = "steelblue", lwd = 2)

qqnorm(data$GENDER, pch = 1, frame = FALSE)
qqline(data$GENDER, col = "steelblue", lwd = 2)

qqnorm(data$AGE7, pch = 1, frame = FALSE)
qqline(data$AGE7, col = "steelblue", lwd = 2)

qqnorm(data$EDUC4, pch = 1, frame = FALSE)
qqline(data$EDUC4, col = "steelblue", lwd = 2)


x1 <- c(SURV_LANG)
x2 <- c(GENDER)
x3 <- c(AGE7)
x4 <- C(EDUC4)
y1 <- c(ART_SCORE)

relation <- lm(y1~x1, data = data)
summary(relation)

relation2 <- lm(y1~x2, data = data)
relation3 <- lm(y1~x3, data = data)
relation4 <- lm(y1~x4, data = data)

summary(relation2)
summary(relation3)
summary(relation4)


multiple_model <- lm(ART_SCORE ~ (SURV_LANG + GENDER + AGE7 + EDUC4)^2, data=data)
summary(multiple_model)
#y <- ART_SCORE
#x <- data.matrix(mtcars[, c('SURV_LANG', 'GENDER', 'AGE7', 'EDUC4')])
#new_data <- data.frame(x = x, y = y)
#model <- lm(y ~ x, data = new_data)
#plot(data$x, data$y)
#abline(model)

#We see that there are high correlation values between variables, so consider a
#logistic-regression approach
#Code for the regression, which is used as an approach to regression when our
#dependent variable is dichotomous or binary
#make this example reproducible
set.seed(1)


data = dfList$data_factor_na_is_no
dfList$data_factor_na_is_no$ART_INDICATOR_2 <- as.factor(dfList$data_factor_na_is_no$ART_INDICATOR_2)                              # Convert character vector to factor
#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7,0.3))
train <- data[sample, ]
test <- data[!sample, ] 

train$ART_INDICATOR_2 = as.factor(train$ART_INDICATOR_2)
str(train)

library(pROC)
#fit logistic regression model
model <- glm(ART_INDICATOR ~ (SURV_LANG + GENDER + AGE7 + EDUC4)^2, family="binomial", data=train)
summary(model)
predicted <- predict(model, test, type="response")
auc(test$ART_INDICATOR, predicted)
g

#fit a linear regression with the same data points
lm_data = lm(ART_SCORE ~ (SURV_LANG + GENDER + AGE7 + EDUC4)^2, data=data)
summary(lm_data)
predicted <- predict(lm_data, test, type="response")
auc(test$ART_SCORE, predicted)