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

da37853.0001[artistic]

# lbls <- sort(levels(da99999.0001$MYVAR))
# lbls <- (sub("^\\([0-9]+\\) +(.+$)", "\\1", lbls))
# da99999.0001$MYVAR <- as.numeric(sub("^\\(0*([0-9]+)\\).+$", "\\1", da99999.0001$MYVAR))
# da99999.0001$MYVAR <- add.value.labels(da99999.0001$MYVAR, lbls)