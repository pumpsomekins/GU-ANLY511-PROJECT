library(stringr)
library(dplyr)

# steam <- read.csv(file = '/Users/pumpkin/Documents/Graduate_School/1st_Semester/ANLY_511/GU-ANLY511-PROJECT/raw_data/steam.csv')
steam <- read.csv(file = 'D:/GU/School Work/Fall 21/ANLY 511/GU-ANLY511-PROJECT/raw_data/steam.csv')
glimpse(steam)
dim(steam)

# change data types
steam$positive_ratings <- as.numeric(steam$positive_ratings)
steam$negative_ratings <- as.numeric(steam$negative_ratings)
steam$price <- as.numeric(steam$price)

# drop NA
sapply(steam, function(x) sum(is.na(x))) # num of missing values
steam <- steam[!is.na(steam$positive_ratings),]
steam <- steam[!is.na(steam$negative_ratings),]
steam <- steam[!is.na(steam$price),]

# calculate positive rating ratio
steam$total_rating_count <- steam$positive_ratings + steam$negative_ratings
steam$positive_ratio <- steam$positive_ratings / steam$total_rating_count
# steam <- steam[!is.na(steam$positive_ratio),]

# calculate the amount of owners in the worst case possible just to be safe
a <- steam$owners
steam$owners <- gsub("(-).*", "\\1", a)
(steam$owners_worst_case_scenario <- str_sub(steam$owners, end = -2))
steam$owners_worst_case_scenario <- as.numeric(steam$owners_worst_case_scenario)

# remove games that has lowest owners
cleanedData <- steam[steam$owners_worst_case_scenario != 0, ]

# linear regression over amount of owners vs positive ratio
steamCorr <- lm(owners_worst_case_scenario ~ positive_ratio, data = cleanedData)
summary(steamCorr)

# vis owners_worst_case
t = steam$owners_worst_case_scenario
(brk = seq(min(t),max(t),1e5))
hist(steam$owners_worst_case_scenario, breaks = brk)

# inspect  outliers
# cleanedData$owners_worst_case_scenario = as.numeric(cleanedData$owners_worst_case_scenario)
boxplot(cleanedData$owners_worst_case_scenario)

# prepare to remove outliers
Q <- quantile(cleanedData$owners_worst_case_scenario, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(cleanedData$owners_worst_case_scenario)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range

# removal of outliers
eliminated <- subset(cleanedData, cleanedData$owners_worst_case_scenario > (Q[1] - 1.5*iqr) & cleanedData$owners_worst_case_scenario < (Q[2]+1.5*iqr))

# vis without outliers
boxplot(eliminated$owners_worst_case_scenario)
hist(eliminated$owners_worst_case_scenario)

# stats for owners without outliers
steamCorr_no_Outliers <- lm(owners_worst_case_scenario ~ positive_ratio, data = eliminated)
summary(steamCorr_no_Outliers)
# plotting the linear regression 
plot(owners_worst_case_scenario ~ positive_ratio, data = eliminated)
abline(steamCorr_no_Outliers)

# chi square testing to see if sale is independent of rating
indep <- table(eliminated$positive_ratio, eliminated$owners_worst_case_scenario)
(chisq.test(indep))

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

###### Permutation test to see if the mean sale of games having positive rating ratio more than 0.5 is different from
# those lower than 0.5

# first need to label the data with above_avg or below_avg according to rating
glimpse(eliminated)
(avg_rating = mean(eliminated$positive_ratio))
eliminated <- eliminated %>% 
  mutate(label = case_when(
    .$positive_ratio >= avg_rating ~ "above_avg",  
    .$positive_ratio < avg_rating ~ "below_avg"
  ))

table(eliminated$label)
boxplot(owners_worst_case_scenario ~ label, data = eliminated)

# Define a function to compute the test statistic: xbar_c - xbar_t
mytest.1 = function(mydf){
  agg = aggregate(owners_worst_case_scenario ~ label, data = mydf, FUN = mean)
  return(agg$owners_worst_case_scenario[1] - agg$owners_worst_case_scenario[2]) #xbar_c - xbar_t
}
mytest.1(eliminated)

# Create a function to permute the labels and compute the test statistic after the permutation
row_num = nrow(eliminated)
permute_sample = function(){
  eliminated$label = eliminated$label[sample(row_num,row_num,replace=F)]
  
  return(mytest.1(eliminated))
}
permute_sample()

M = 10000
test.1 = replicate(M,permute_sample())
hist(test.1, main = "Null distribution", prob=T, xlim=c(-6000,10000))
abline(v = mytest.1(eliminated), col = 2, lwd = 2)

# Estimate p-value
mean(test.1 > mytest.1(eliminated)) #p-value = 0: reject null hypothesis
