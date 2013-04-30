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
data.scaled.no_combine.for_rating = data.frame(scale(na.omit(cbind(rating, college_stats))))

# Generate the linear model
lm.scaled.no_combine.rating <- lm(formula = rating ~. ,data = data.scaled.no_combine.for_rating)

# Find optimum linear regression model for rating
step_reg.scaled.no_combine.rating <- stepAIC(lm.scaled.no_combine.rating, direction="both")
summary(step_reg.scaled.no_combine.rating)
plot(step_reg.scaled.no_combine.rating)
leaps.scaled.no_combine.rating <- regsubsets(rating ~. ,data = data.scaled.no_combine.for_rating, nbest=10)
subsets(leaps.scaled.no_combine.rating, statistic="rsq")
cv.lm(df=data.scaled.no_combine.for_rating, step_reg.scaled.no_combine.rating, m=5) # 5 fold cross-validation