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

# Establish the cost function
cost = function(r,pi=0) mean(abs(r-pi)>0.5)

# Set the resopnse variables
td_int_ratio = qb_stats['tds'] / qb_stats['ints']
bin_td_int_ratio = ifelse(td_int_ratio < 2.0 , 0, 1)

# Generate clean data sets
data.no_combine.for_bin_td_int_ratio = data.frame(na.omit(cbind(bin_td_int_ratio, college_stats)))

# Logistic Regression
glm.no_combine.td_int_ratio <- glm(formula = tds ~. ,data = data.no_combine.for_bin_td_int_ratio, family='binomial')
exp(cbind(OR = coef(glm.no_combine.td_int_ratio), confint(glm.no_combine.td_int_ratio)))
td_int_ratio.cv <- cv.glm(data = data.no_combine.for_bin_td_int_ratio, glmfit = glm.no_combine.td_int_ratio, cost, 5)
td_int_ratio.cv.error <- td_int_ratio.cv$delta[2]
cat("Cross Validation Error\n", td_int_ratio.cv.error)