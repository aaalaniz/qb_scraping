#+ setup, include=FALSE
library(knitr)
library(DAAG)
library(MASS)
library(leaps)
library(car)
opts_chunk$set(fig.path='figure/silk-')

#'
# Fetch Data
qb_stats_w_combine <- read.csv("../data/qb_stats_w_combine.csv")

# Grab the college predictors
predictors <- c('height', 'weight', 'age', 'c_avg_cmpp', 'c_rate', 'c_pct', 'c_avg_inter', 'c_avg_tds', 'c_avg_yds', 'c_numyrs', 'c_avg_att', 'X40', 'wonderlic', 'cone', 'shuttle', 'vert_leap', 'broad_jump')
college_stats = qb_stats_w_combine[,predictors]

# Set the resopnse variables
games_started = qb_stats_w_combine['games_started']

# Generate clean data set
data.log.w_combine.for_games_started = data.frame(log(na.omit(cbind(games_started, college_stats)) + 0.1))

# Generate the linear model
lm.log.w_combine.games_started <- lm(formula = games_started ~. ,data = data.log.w_combine.for_games_started)

# Find optimum linear regression model for games_started
step_reg.log.w_combine.games_started <- stepAIC(lm.log.w_combine.games_started, direction="both")
summary(step_reg.log.w_combine.games_started)
plot(step_reg.log.w_combine.games_started)
leaps.log.w_combine.games_started <- regsubsets(games_started ~. ,data = data.log.w_combine.for_games_started, nbest=10)
subsets(leaps.log.w_combine.games_started, statistic="rsq")
cv.lm(df=data.log.w_combine.for_games_started, step_reg.log.w_combine.games_started, m=5) # 5 fold cross-validation