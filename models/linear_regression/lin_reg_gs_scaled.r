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
data.scaled.no_combine.for_games_started = data.frame(scale(na.omit(cbind(games_started, college_stats))))

# Generate the linear model
lm.scaled.no_combine.games_started <- lm(formula = games_started ~. ,data = data.scaled.no_combine.for_games_started)

# Find optimum linear regression model for games_started
step_reg.scaled.no_combine.games_started <- stepAIC(lm.scaled.no_combine.games_started, direction="both")
summary(step_reg.scaled.no_combine.games_started)
plot(step_reg.scaled.no_combine.games_started)
leaps.scaled.no_combine.games_started <- regsubsets(games_started ~. ,data = data.scaled.no_combine.for_games_started, nbest=10)
subsets(leaps.scaled.no_combine.games_started, statistic="rsq")
cv.lm(df=data.scaled.no_combine.for_games_started, step_reg.scaled.no_combine.games_started, m=5) # 5 fold cross-validation