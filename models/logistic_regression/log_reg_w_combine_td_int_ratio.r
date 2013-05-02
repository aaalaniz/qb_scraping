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
td_int_ratio = qb_stats_w_combine['tds'] / qb_stats_w_combine['ints']

# Establish the cost function
cost = function(r,pi=0) mean(abs(r-pi)>0.5)

# Generate clean data sets
bin_td_int_ratio = ifelse(td_int_ratio < 2.0 , 0, 1)
data.w_combine.for_bin_td_int_ratio = data.frame(na.omit(cbind(bin_td_int_ratio, college_stats)))

# Logistic Regression
glm.w_combine.td_int_ratio <- glm(formula = tds ~. ,data = data.w_combine.for_bin_td_int_ratio, family='binomial')
exp(cbind(OR = coef(glm.w_combine.td_int_ratio), confint(glm.w_combine.td_int_ratio)))
td_int_ratio.w_combine.cv <- cv.glm(data = data.w_combine.for_bin_td_int_ratio, glmfit = glm.w_combine.td_int_ratio, cost, 5)
td_int_ratio.w_combine.cv.error <- td_int_ratio.w_combine.cv$delta[2]
cat("Cross Validation Error\n", td_int_ratio.w_combine.cv.error)