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
wins = qb_stats_w_combine['wins']

# Generate clean data set
data.scaled.w_combine.for_wins = data.frame(scale(na.omit(cbind(wins, college_stats))))

# Generate the linear model
lm.scaled.w_combine.wins <- lm(formula = wins ~. ,data = data.scaled.w_combine.for_wins)

# Find optimum linear regression model for wins
step_reg.scaled.w_combine.wins <- stepAIC(lm.scaled.w_combine.wins, direction="both")
summary(step_reg.scaled.w_combine.wins)
plot(step_reg.scaled.w_combine.wins)
leaps.scaled.w_combine.wins <- regsubsets(wins ~. ,data = data.scaled.w_combine.for_wins, nbest=10)
subsets(leaps.scaled.w_combine.wins, statistic="rsq")
cv.lm(df=data.scaled.w_combine.for_wins, step_reg.scaled.w_combine.wins, m=5) # 5 fold cross-validation