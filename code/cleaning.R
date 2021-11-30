library(stringr)

steam <- read.csv(file = '/Users/pumpkin/Documents/Graduate_School/1st_Semester/ANLY_511/GU-ANLY511-PROJECT/raw_data/steam.csv')

# calculate positive rating ratio
steam$total_rating_count <- steam$positive_ratings + steam$negative_ratings
steam$positive_ratio <- steam$positive_ratings / steam$total_rating_count

# calculate the amount of owners in the worst case possible just to be safe
a <- steam$owners
steam$owners <- gsub("(-).*", "\\1", a)
steam$owners_worst_case_scenario <- str_sub(steam$owners, end = -2)

# remove games that has lowest owners
cleanedData <- steam[steam$owners_worst_case_scenario != 0, ]

# linear regression over amount of owners vs positive ratio
steamCorr <- lm(owners_worst_case_scenario ~ positive_ratio, data = cleanedData)
summary(steamCorr)

# inspect  outliers
cleanedData$owners_worst_case_scenario = as.numeric(cleanedData$owners_worst_case_scenario)
boxplot(cleanedData$owners_worst_case_scenario)

# prepare to remove outliers
Q <- quantile(cleanedData$owners_worst_case_scenario, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(cleanedData$owners_worst_case_scenario)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range

# removal of outliers
eliminated <- subset(cleanedData, cleanedData$owners_worst_case_scenario > (Q[1] - 1.5*iqr) & cleanedData$owners_worst_case_scenario < (Q[2]+1.5*iqr))

# boxplot without outliers
boxplot(eliminated$owners_worst_case_scenario)

# stats for owners without outliers
steamCorr_no_Outliers <- lm(owners_worst_case_scenario ~ positive_ratio, data = eliminated)
summary(steamCorr_no_Outliers)

# plotting the linear regression 
plot(owners_worst_case_scenario ~ positive_ratio, data = eliminated)
abline(steamCorr_no_Outliers)

# chi square testing to see if sale is independent of rating
indep <- table(eliminated$positive_ratio, eliminated$owners_worst_case_scenario)
chisq.test(indep)

# the chi square test p value is lower than 0.05 so we reject the null hypothesis that one variable
# is indeed dependant on the other.

# t testing to see if the mean sale of games having positive rating more than 0.5 is different from
# those lower than 0.5
with(eliminated, shapiro.test(owners_worst_case_scenario[positive_ratio <= 0.5]))
with(eliminated, shapiro.test(owners_worst_case_scenario[positive_ratio > 0.7]))

# not statistically significant enough to be normally distributed so we would switch to non parametric 
# two-samples Wilcoxon rank test.

mean_t_1 <- eliminated[eliminated$positive_ratio <= 0.5,]
mean_t_2 <- eliminated[eliminated$positive_ratio > 0.5,]

mean(mean_t_2$owners_worst_case_scenario)
mean(mean_t_1$owners_worst_case_scenario)

res <- wilcox.test(mean_t_2$owners_worst_case_scenario, mean_t_1$owners_worst_case_scenario)

res