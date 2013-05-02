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
games_started = qb_stats['games_started']

# Generate clean data set
data.log.no_combine.for_games_started = data.frame(log(na.omit(cbind(games_started, college_stats)) + 0.1))

# Generate the linear model
lm.log.no_combine.games_started <- lm(formula = games_started ~. ,data = data.log.no_combine.for_games_started)

# Find optimum linear regression model for games_started
step_reg.log.no_combine.games_started <- stepAIC(lm.log.no_combine.games_started, direction="both")
summary(step_reg.log.no_combine.games_started)
plot(step_reg.log.no_combine.games_started)
leaps.log.no_combine.games_started <- regsubsets(games_started ~. ,data = data.log.no_combine.for_games_started, nbest=10)
subsets(leaps.log.no_combine.games_started, statistic="rsq")
cv.lm(df=data.log.no_combine.for_games_started, step_reg.log.no_combine.games_started, m=5) # 5 fold cross-validation