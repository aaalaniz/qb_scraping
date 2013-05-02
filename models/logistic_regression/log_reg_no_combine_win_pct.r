#+ setup, include=FALSE
library(knitr)
library(boot)
opts_chunk$set(fig.path='figure/silk-')

#'
# Fetch Data
qb_stats <- read.csv("../data/qb_stats.csv")

# Grab the college predictors
predictors <- c('height', 'weight', 'age', 'c_avg_cmpp', 'c_rate', 'c_pct', 'c_avg_inter', 'c_avg_tds', 'c_avg_yds', 'c_numyrs', 'c_avg_att')
college_stats = qb_stats[,predictors]

# Set the resopnse variables
win_pct = qb_stats['wins'] / qb_stats['games_started']

# Establish the cost function
cost = function(r,pi=0) mean(abs(r-pi)>0.5)

# Generate clean data sets
bin_win_pct = ifelse(win_pct < 0.5, 0, 1)
data.no_combine.for_bin_win_pct = data.frame(na.omit(cbind(bin_win_pct, college_stats)))

# Logistic Regression
glm.no_combine.win_pct <- glm(formula = wins ~. ,data = data.no_combine.for_bin_win_pct, family=binomial())
exp(cbind(OR = coef(glm.no_combine.win_pct), confint(glm.no_combine.win_pct)))
win_pct.cv <- cv.glm(data = data.no_combine.for_bin_win_pct, glmfit = glm.no_combine.win_pct, cost, 5)
win_pct.cv.error <- win_pct.cv$delta[2]
cat("Cross Validation Error\n", win_pct.cv.error)