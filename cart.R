data = load(file="37853-0001-Data.rda")
colnames(da37853.0001)

demographics = c("SURV_LANG",
                 "GENDER", 
                 "AGE7", 
                 "RACETHNICITY",
                 "EDUC4",
                 "EMPLOY",
                 "INCOME",
                 "REGION4",
                 "INTERNET")

artistic = c("Q7A",
             "Q7B",
             "Q7C",
             "Q7D",
             "Q7E",
             "Q7F",
             "Q7G",
             "Q7H",
             "Q7J",
             "Q7N",
             "Q7P")

data_factor_no_fill = da37853.0001[c(demographics, artistic)]
all_na = apply(data_factor_no_fill[artistic], 1, function(x) all(is.na(x)))
data_factor_no_fill = data_factor_no_fill[!all_na, ]

# What are the types of each column?
str(data_factor_no_fill)

# Missing Data Evaluation
head(rowSums(is.na(data_factor_no_fill)))
colSums(is.na(data_factor_no_fill))

# 1. Make NA's 2's (No)
data_factor_na_is_no = data.frame(data_factor_no_fill)
data_factor_na_is_no[is.na(data_factor_no_fill)] = "(2) No"
data_factor_na_is_no

1:ncol(data_factor_no_fill)

# 2. Impute using KNN
library(DMwR)
data_factor_knn_impute = knnImputation(data_factor_no_fill)
data_factor_knn_impute
anyNA(data_factor_knn_impute)

# compute artistic score for several dataframes
dfList = list(data_factor_no_fill=data_factor_no_fill, 
              data_factor_na_is_no=data_factor_na_is_no,
              data_factor_knn_impute=data_factor_knn_impute)
dfList = lapply(dfList, function(df) {
  df[artistic] = sapply(df[artistic],as.numeric)
  df[artistic] = 1 - (df[artistic] - 1)
  df$ART_SCORE = rowMeans(subset(df, select = artistic), na.rm = TRUE)
  df
})

boxplot(dfList$data_factor_na_is_no$ART_SCORE,
        main = "Artistic Score Box Plot",
        xlab = "Artistic Score",
        horizontal = TRUE,
        notch = FALSE
)

library(rpart)
library(rpart.plot)
library(randomForest)

library(dplyr)

# Split artistic score into several categories so we can do CART and Random Forests
dfList = lapply(dfList, function(df) {
  df$ART_INDICATOR_2 <- ifelse(df$ART_SCORE >= 0 & df$ART_SCORE < 3/11, "Less",
                                ifelse(df$ART_SCORE >= 3/11, "More", NA))
  df
})

dfList$data_factor_no_fill

# Train/Test
set.seed(1)
as.numeric(dfList$data_factor_na_is_no[, "GENDER"])

for(col in demographics) {
  dfList$data_factor_na_is_no[, col] = as.numeric(dfList$data_factor_na_is_no[, col])
}

sample <- sample(c(TRUE, FALSE), nrow(dfList$data_factor_na_is_no), replace=TRUE, prob=c(0.7,0.3))
train.data <- dfList$data_factor_na_is_no[sample, ]
test.data <- dfList$data_factor_na_is_no[!sample, ] 

# CART Model
CART1 = rpart(ART_INDICATOR_2 ~ 
                SURV_LANG + 
                GENDER + 
                AGE7 + 
                INCOME + 
                INTERNET +
                EDUC4 + 
                REGION4, 
              method = "class", 
              data = train.data,
              control=rpart.control(minsplits = 2, minbucket=5, cp=0.006529851))

CART1$cptable[which.min(CART1$cptable[,"xerror"]),"CP"]

printcp(CART1)
# par(xpd = NA) # Avoid clipping the text in some device
# plotcp(CART1)
summary(CART1)
prp(CART1, type = 2, extra = 2)
rpart.plot(CART1)
text(CART1, digits = 3)
post(CART1)

pruned <- prune(CART1, cp=0.003814262)

predicted.classes <- pruned %>% predict(test.data, type = "class")
head(predicted.classes)
mean(predicted.classes == test.data$ART_INDICATOR_2)

table(predicted.classes, test.data$ART_INDICATOR_2)
retrieved = 279 + 38
precision = 279 / retrieved
recall = 279 / (279 + 327)
Fmeasure = 2 * precision * recall / (precision + recall)
Fmeasure

test.data$ART_INDICATOR = as.factor(test.data$ART_INDICATOR)
summary(test.data$ART_INDICATOR)


cor(as.numeric(dfList$data_factor_na_is_no$AGE7), as.numeric(dfList$data_factor_na_is_no$EDUC4))


# install.packages("caret")
# library(caret)
# 
# set.seed(123)
# CART2 <- train(
#   ART_INDICATOR ~., 
#   data = train.data, method = "rpart",
#   trControl = trainControl("cv", number = 10),
#   tuneLength = 10
# )

# Random Forest
# install.packages("randomForest")
library(randomForest)
set.seed(0)
fit <- randomForest(as.factor(ART_INDICATOR_2) ~ SURV_LANG + GENDER + AGE7 + EDUC4, data=train.data)
print(fit) # view results
importance(fit) # importance of each 
plot(fit,  main="Random Forest Model Error")


install.packages("caret")
library(caret)
p1 <- predict(fit, test.data)
confusionMatrix(p1, as.factor(test.data$ART_INDICATOR_2))

