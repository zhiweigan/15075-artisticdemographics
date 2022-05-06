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
num_clusters = 10
clusters = kmodes(dfList$data_factor_na_is_no[demographics], num_clusters, iter.max = 1000, weighted = FALSE, fast = TRUE)
for(i in 1:num_clusters) {
  im = mean(dfList$data_factor_na_is_no[two_clusters$cluster == i, ]$ART_SCORE, na.rm=TRUE)
  print(im)
}





