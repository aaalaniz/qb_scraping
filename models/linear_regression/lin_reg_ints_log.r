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
ints = qb_stats['ints']

# Generate clean data set
data.log.no_combine.for_ints = data.frame(log(na.omit(cbind(ints, college_stats)) + 0.1))

# Generate the linear model
lm.log.no_combine.ints <- lm(formula = ints ~. ,data = data.log.no_combine.for_ints)

# Find optimum linear regression model for ints
step_reg.log.no_combine.ints <- stepAIC(lm.log.no_combine.ints, direction="both")
summary(step_reg.log.no_combine.ints)
plot(step_reg.log.no_combine.ints)
leaps.log.no_combine.ints <- regsubsets(ints ~. ,data = data.log.no_combine.for_ints, nbest=10)
subsets(leaps.log.no_combine.ints, statistic="rsq")
cv.lm(df=data.log.no_combine.for_ints, step_reg.log.no_combine.ints, m=5) # 5 fold cross-validation