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
data.scaled.no_combine.for_wins = data.frame(scale(na.omit(cbind(wins, college_stats))))

# Generate the linear model
lm.scaled.no_combine.wins <- lm(formula = wins ~. ,data = data.scaled.no_combine.for_wins)

# Find optimum linear regression model for wins
step_reg.scaled.no_combine.wins <- stepAIC(lm.scaled.no_combine.wins, direction="both")
summary(step_reg.scaled.no_combine.wins)
plot(step_reg.scaled.no_combine.wins)
leaps.scaled.no_combine.wins <- regsubsets(wins ~. ,data = data.scaled.no_combine.for_wins, nbest=10)
subsets(leaps.scaled.no_combine.wins, statistic="rsq")
cv.lm(df=data.scaled.no_combine.for_wins, step_reg.scaled.no_combine.wins, m=5) # 5 fold cross-validation