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

data_factor = da37853.0001[c(demographics, artistic)]

# What are the types of each column?
str(data_factor)

# Missing Data Evaluation
head(rowSums(is.na(data_factor)))
colSums(is.na(data_factor))

# Make NA's 2's (No)
data_factor[is.na(data_factor)] = "(2) No"

data_factor
