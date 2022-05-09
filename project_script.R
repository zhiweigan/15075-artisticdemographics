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

# Make NA's 2's (No)
data_factor_na_is_no = data.frame(data_factor_no_fill)
data_factor_na_is_no[is.na(data_factor_no_fill)] = "(2) No"
data_factor_na_is_no

1:ncol(data_factor_no_fill)

# Impute using KNN
# install.packages("abind")
# install.packages("zoo")
# install.packages("quantmod")
# install.packages("xts")
# install.packages("/Users/zhiweigan/Downloads/DMwR_0.4.1.tar.gz", repos = NULL, type="source")
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

# Ananya's CV Model
y <- dfList$data_factor_na_is_no$ART_SCORE
x <- data.matrix(dfList$data_factor_na_is_no[, demographics])
x

# install.packages("glmnet")
library(glmnet)
model <- glmnet(x, y, alpha=1)
summary(model)

cv_model <- cv.glmnet(x, y, alpha=1)
best_lambda <- cv_model$lambda.min
best_lambda

plot(cv_model)

#We see that there are high correlation values between variables, so consider a
#logistic-regression approach
#Code for the regression, which is used as an approach to regression when our
#dependent variable is dichotomous or binary
#make this example reproducible
set.seed(1)


data = dfList$data_factor_na_is_no
dfList$data_factor_na_is_no$ART_INDICATOR_2 <- as.factor(dfList$data_factor_na_is_no$ART_INDICATOR_2)                              # Convert character vector to factor
#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7,0.3))
train <- data[sample, ]
test <- data[!sample, ] 

train$ART_INDICATOR_2 = as.factor(train$ART_INDICATOR_2)
str(train)

library(pROC)
#fit logistic regression model
model <- glm(ART_INDICATOR_2 ~ (SURV_LANG + GENDER + AGE7 + EDUC4)^2, family="binomial", data=train)
summary(model)
predicted <- predict(model, test, type="response")
auc(test$ART_INDICATOR_2, predicted)

#fit a linear regression with the same data points
lm_data = lm(ART_SCORE ~ (SURV_LANG + GENDER + AGE7 + EDUC4)^2, data=data)
summary(lm_data)
predicted <- predict(lm_data, test, type="response")
auc(test$ART_SCORE, predicted)


# https://stackoverflow.com/questions/20241065/r-barplot-wrapping-long-text-labels
# Core wrapping function
wrap.it <- function(x, len)
{ 
  sapply(x, function(y) paste(strwrap(y, len), 
                              collapse = "\n"), 
         USE.NAMES = FALSE)
}


# Call this function with a list or vector
wrap.labels <- function(x, len)
{
  if (is.list(x))
  {
    lapply(x, wrap.it, len)
  } else {
    wrap.it(x, len)
  }
}


demographics = c("SURV_LANG",
                 "GENDER", 
                 "AGE7", 
                 "RACETHNICITY",
                 "EDUC4",
                 "EMPLOY",
                 "INCOME",
                 "REGION4",
                 "INTERNET")

boxplot(ART_SCORE~SURV_LANG,
        data=dfList$data_factor_no_fill,
        main = "Survey Language: Artistic Score Box Plot",
        xlab = "Response",
        ylab = "Artistic Score",
        notch = FALSE,
        names = levels(dfList$data_factor_no_fill$SURV_LANG)
)

boxplot(ART_SCORE~GENDER,
        data=dfList$data_factor_no_fill,
        main = "Gender: Artistic Score Box Plot",
        xlab = "Response",
        ylab = "Artistic Score",
        notch = FALSE,
        names = levels(dfList$data_factor_no_fill$GENDER)
)

boxplot(ART_SCORE~AGE7,
        data=dfList$data_factor_no_fill,
        main = "Age: Artistic Score Box Plot",
        xlab = "Response",
        ylab = "Artistic Score",
        notch = FALSE,
        names = levels(dfList$data_factor_no_fill$AGE7)
)

boxplot(ART_SCORE~RACETHNICITY,
        data=dfList$data_factor_no_fill,
        main = "Race/Ethnicity: Artistic Score Box Plot",
        xlab = "Response",
        ylab = "Artistic Score",
        notch = FALSE,
        names = levels(dfList$data_factor_no_fill$RACETHNICITY)
)

boxplot(ART_SCORE~EDUC4,
        data=dfList$data_factor_no_fill,
        main = "Education: Artistic Score Box Plot",
        xlab = "Response",
        ylab = "Artistic Score",
        notch = FALSE,
        names = levels(dfList$data_factor_no_fill$EDUC4)
)

boxplot(ART_SCORE~EMPLOY,
        data=dfList$data_factor_no_fill,
        main = "Employment: Artistic Score Box Plot",
        xlab = "Response",
        ylab = "Artistic Score",
        names = c("Working", "Self-Employed", "Temp Layoff", "Looking for Work", "Retired", "Disabled", "Other"),
        notch = FALSE
)

boxplot(ART_SCORE~INCOME,
        data=dfList$data_factor_no_fill,
        main = "Income: Artistic Score Box Plot",
        xlab = "",
        ylab = "Artistic Score",
        cex.axis = 0.4,
        las = 2,
        notch = FALSE,
        names = levels(dfList$data_factor_no_fill$INCOME)
)

boxplot(ART_SCORE~REGION4,
        data=dfList$data_factor_no_fill,
        main = "Region: Artistic Score Box Plot",
        xlab = "",
        ylab = "Artistic Score",
        notch = FALSE,
        names = levels(dfList$data_factor_no_fill$REGION4)
)

boxplot(ART_SCORE~INTERNET,
        data=dfList$data_factor_no_fill,
        main = "Internet: Artistic Score Box Plot",
        xlab = "",
        ylab = "Artistic Score",
        notch = FALSE,
        names = levels(dfList$data_factor_no_fill$INTERNET)
)

boxplot(dfList$data_factor_na_is_no$ART_SCORE,
        main = "Artistic Score Box Plot",
        xlab = "Artistic Score",
        horizontal = TRUE,
        notch = FALSE
)

library(ggplot2)
ggplot(dfList$data_factor_na_is_no) + 
  geom_histogram(aes(x = dfList$data_factor_na_is_no$ART_SCORE, y = (..count..)/sum(..count..)), 
                 bins = 11, fill = "gray", colour = "black") +
  xlab("Artistic Score") +
  ylab("Relative Frequency") +
  ggtitle("Artistic Score NA-is-No Histogram") + 
  scale_x_continuous(breaks = 0:10)
ggplot(dfList$data_factor_no_fill) + 
  geom_histogram(aes(x = dfList$data_factor_no_fill$ART_SCORE, y = (..count..)/sum(..count..)), 
                 bins = 11, fill = "gray", colour = "black") +
  xlab("Artistic Score") +
  ylab("Relative Frequency") +
  ggtitle("Artistic Score No Fill Histogram") + 
  scale_x_continuous(breaks = 0:10)
ggplot(dfList$data_factor_knn_impute) + 
  geom_histogram(aes(x = dfList$data_factor_knn_impute$ART_SCORE, y = (..count..)/sum(..count..)), 
                 bins = 11, fill = "gray", colour = "black") +
  xlab("Artistic Score") +
  ylab("Relative Frequency") +
  ggtitle("Artistic Score KNN Impute Histogram") + 
  scale_x_continuous(breaks = 0:10)

# Split artistic score into several categories so we can do CART and Random Forests
dfList = lapply(dfList, function(df) {
  df$ART_INDICATOR <- ifelse(df$ART_SCORE >= 0 & df$ART_SCORE < 1/11, "None",
                        ifelse(df$ART_SCORE >= 1/11 & df$ART_SCORE < 2/11, "One",
                           ifelse(df$ART_SCORE >= 2/11 & df$ART_SCORE < 6/11, "Some",
                              ifelse(df$ART_SCORE >= 6/11, "Many", NA))))
  df
})

library(rpart)
library(rpart.plot)
library(randomForest)

demographics = c("SURV_LANG",
                 "GENDER", 
                 "AGE7", 
                 "RACETHNICITY",
                 "EDUC4",
                 "EMPLOY",
                 "INCOME",
                 "REGION4",
                 "INTERNET")

str(dfList$data_factor_no_fill)


CART1 = rpart(ART_INDICATOR ~ SURV_LANG + GENDER + AGE7 + INCOME + INTERNET + EDUC4 + REGION4, 
              method = "class", 
              data = dfList$data_factor_no_fill, 
              control=rpart.control(minsplit=2, minbucket=5, cp=0.003))

printcp(CART1)
prp(CART1)

CARTPredict1 = predict(CART1, newdata=WikiTest, type="class")
table(WikiTest$Vandal, CARTPredict1)

# clustering algo
# install.packages('klaR')
library(klaR)
num_clusters = 2
clusters = kmodes(dfList$data_factor_no_fill[demographics], num_clusters, iter.max = 1000, weighted = FALSE, fast = TRUE)
print("No Fill")
for(i in 1:num_clusters) {
  im = mean(dfList$data_factor_no_fill[clusters$cluster == i, ]$ART_SCORE, na.rm=TRUE)
  print(im)
}

clusters = kmodes(dfList$data_factor_na_is_no[demographics], num_clusters, iter.max = 1000, weighted = FALSE, fast = TRUE)
print("NA is No")
for(i in 1:num_clusters) {
  im = mean(dfList$data_factor_na_is_no[clusters$cluster == i, ]$ART_SCORE, na.rm=TRUE)
  print(im)
}

clusters = kmodes(dfList$data_factor_knn_impute[demographics], num_clusters, iter.max = 1000, weighted = FALSE, fast = TRUE)
print("Knn")
for(i in 1:num_clusters) {
  im = mean(dfList$data_factor_knn_impute[clusters$cluster == i, ]$ART_SCORE, na.rm=TRUE)
  print(im)
}





