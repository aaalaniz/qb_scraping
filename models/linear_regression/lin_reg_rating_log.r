#+ setup, include=FALSE
library(knitr)
library(DAAG)
library(MASS)
library(leaps)
library(car)
opts_chunk$set(fig.path='figure/silk-')

#'
# Fetch Data
qb_stats <- read.csv("../data/qb_stats.csv")

# Grab the college predictors
predictors <- c('height', 'weight', 'age', 'c_avg_cmpp', 'c_rate', 'c_pct', 'c_avg_inter', 'c_avg_tds', 'c_avg_yds', 'c_numyrs', 'c_avg_att')
college_stats = qb_stats[,predictors]

# Set the resopnse variables
rating = qb_stats['rating']

# Generate clean data set
data.log.no_combine.for_rating = data.frame(log(na.omit(cbind(rating, college_stats)) + 0.1))

# Generate the linear model
lm.log.no_combine.rating <- lm(formula = rating ~. ,data = data.log.no_combine.for_rating)

# Find optimum linear regression model for rating
step_reg.log.no_combine.rating <- stepAIC(lm.log.no_combine.rating, direction="both")
summary(step_reg.log.no_combine.rating)
plot(step_reg.log.no_combine.rating)
leaps.log.no_combine.rating <- regsubsets(rating ~. ,data = data.log.no_combine.for_rating, nbest=10)
subsets(leaps.log.no_combine.rating, statistic="rsq")
cv.lm(df=data.log.no_combine.for_rating, step_reg.log.no_combine.rating, m=5) # 5 fold cross-validation