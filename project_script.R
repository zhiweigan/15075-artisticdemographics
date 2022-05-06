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
data_factor_na_is_no = data.frame(data_factor)
data_factor_na_is_no[is.na(data_factor)] = "(2) No"
data_factor_na_is_no

# compute artistic score for several dataframes
dfList = list(data_factor_no_fill, data_factor_na_is_no)
dfList <- list(data_factor_no_fill=data_factor_no_fill, 
               data_factor_na_is_no=data_factor_na_is_no)
dfList = lapply(dfList, function(df) {
  df[artistic] = sapply(df[artistic],as.numeric)
  df[artistic] = df[artistic] - 1
  df$ART_SCORE = rowMeans(subset(df, select = artistic), na.rm = TRUE)
  df
})

# clustering algo
install.packages('klaR')
library(klaR)
two_clusters = kmodes(dfList$data_factor_na_is_no[demographics], 10, iter.max = 10, weighted = FALSE, fast = TRUE)

plot(jitter(dfList$data_factor_na_is_no[demographics]), col = two_clusters)
points(cl$modes, col = 1:5, pch = 8)
# }

mean(dfList$data_factor_na_is_no[unlist(two_clusters) == 1, ]$ART_SCORE, na.rm=TRUE)
mean(dfList$data_factor_na_is_no[unlist(two_clusters) == 2, ]$ART_SCORE, na.rm=TRUE)
mean(dfList$data_factor_na_is_no[unlist(two_clusters) == 3, ]$ART_SCORE, na.rm=TRUE)
mean(dfList$data_factor_na_is_no[unlist(two_clusters) == 4, ]$ART_SCORE, na.rm=TRUE)
mean(dfList$data_factor_na_is_no[unlist(two_clusters) == 5, ]$ART_SCORE, na.rm=TRUE)
mean(dfList$data_factor_na_is_no[unlist(two_clusters) == 6, ]$ART_SCORE, na.rm=TRUE)
mean(dfList$data_factor_na_is_no[unlist(two_clusters) == 7, ]$ART_SCORE, na.rm=TRUE)
mean(dfList$data_factor_na_is_no[unlist(two_clusters) == 8, ]$ART_SCORE, na.rm=TRUE)
mean(dfList$data_factor_na_is_no[unlist(two_clusters) == 9, ]$ART_SCORE, na.rm=TRUE)
mean(dfList$data_factor_na_is_no[unlist(two_clusters) == 10, ]$ART_SCORE, na.rm=TRUE)


