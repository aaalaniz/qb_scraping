#+ setup, include=FALSE
library(knitr)
library(boot)
opts_chunk$set(fig.path='figure/silk-')

#'
# Fetch Data
qb_stats_w_combine <- read.csv("../data/qb_stats_w_combine.csv")

# Grab the college predictors
predictors <- c('height', 'weight', 'age', 'c_avg_cmpp', 'c_rate', 'c_pct', 'c_avg_inter', 'c_avg_tds', 'c_avg_yds', 'c_numyrs', 'c_avg_att', 'X40', 'wonderlic', 'cone', 'shuttle', 'vert_leap', 'broad_jump')
college_stats = qb_stats_w_combine[,predictors]

# Set the resopnse variables
win_pct = qb_stats_w_combine['wins'] / qb_stats_w_combine['games_started']

# Establish the cost function
cost = function(r,pi=0) mean(abs(r-pi)>0.5)

# Generate clean data sets
bin_win_pct = ifelse(win_pct < 0.5, 0, 1)
data.w_combine.for_bin_win_pct = data.frame(na.omit(cbind(bin_win_pct, college_stats)))

# Logistic Regression
glm.w_combine.win_pct <- glm(formula = wins ~. ,data = data.w_combine.for_bin_win_pct, family=binomial())
exp(cbind(OR = coef(glm.w_combine.win_pct), confint(glm.w_combine.win_pct)))
win_pct.w_combine.cv <- cv.glm(data = data.w_combine.for_bin_win_pct, glmfit = glm.w_combine.win_pct, cost, 5)
win_pct.w_combine.cv.error <- win_pct.w_combine.cv$delta[2]
cat("Cross Validation Error\n", win_pct.w_combine.cv.error)