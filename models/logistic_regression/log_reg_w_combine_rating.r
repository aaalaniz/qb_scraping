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
bin_rating = ifelse(qb_stats_w_combine['rating'] < 85.0, 0, 1)
data.w_combine.for_bin_rating = data.frame(na.omit(cbind(bin_rating, college_stats)))

# Logistic Regression
glm.w_combine.rating <- glm(formula = rating ~. ,data = data.w_combine.for_bin_rating, family=binomial())
exp(cbind(OR = coef(glm.w_combine.rating), confint(glm.w_combine.rating)))
rating.w_combine.cv <- cv.glm(data = data.w_combine.for_bin_rating, glmfit = glm.w_combine.rating, cost, 5)
rating.w_combine.cv.error <- rating.w_combine.cv$delta[2]
cat("Cross Validation Error\n", rating.w_combine.cv.error)