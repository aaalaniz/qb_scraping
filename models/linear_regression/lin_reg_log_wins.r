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
wins = qb_stats['wins']

# Generate clean data set
data.log.no_combine.for_wins = data.frame(log(na.omit(cbind(wins, college_stats)) + 0.1))

# Generate the linear model
lm.log.no_combine.wins <- lm(formula = wins ~. ,data = data.log.no_combine.for_wins)

# Find optimum linear regression model for wins
step_reg.log.no_combine.wins <- stepAIC(lm.log.no_combine.wins, direction="both")
summary(step_reg.log.no_combine.wins)
plot(step_reg.log.no_combine.wins)
leaps.log.no_combine.wins <- regsubsets(wins ~. ,data = data.log.no_combine.for_wins, nbest=10)
subsets(leaps.log.no_combine.wins, statistic="rsq")
cv.lm(df=data.log.no_combine.for_wins, step_reg.log.no_combine.wins, m=5) # 5 fold cross-validation