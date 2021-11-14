library(stringr)

steam <- read.csv(file = '/Users/pumpkin/Documents/Graduate_School/1st_Semester/ANLY_511/GU-ANLY511-PROJECT/raw_data/steam.csv')

steam$total_rating_count <- steam$positive_ratings + steam$negative_ratings
steam$positive_ratio <- steam$positive_ratings / steam$total_rating_count

a <- steam$owners
steam$owners <- gsub("(-).*", "\\1", a)
steam$owners_worst_case_scenario <- str_sub(steam$owners, end = -2)

cleanedData <- steam[steam$owners_worst_case_scenario != 0, ]

steamCorr <- lm(owners_worst_case_scenario ~ positive_ratio, data = cleanedData)

summary(steamCorr)