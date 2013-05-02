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

# Establish the cost function
cost = function(r,pi=0) mean(abs(r-pi)>0.5)

# Generate clean data sets
bin_cpct = ifelse(qb_stats_w_combine['completion_percentage'] < 60.0, 0, 1)
data.w_combine.for_bin_cpct = data.frame(na.omit(cbind(bin_cpct, college_stats)))

# Logistic Regression
glm.w_combine.cpct <- glm(formula = completion_percentage ~. ,data = data.w_combine.for_bin_cpct, family=binomial())
exp(cbind(OR = coef(glm.w_combine.cpct), confint(glm.w_combine.cpct)))
cpct.w_combine.cv <- cv.glm(data = data.w_combine.for_bin_cpct, glmfit = glm.w_combine.cpct, cost, 5)
cpct.w_combine.cv.error <- cpct.w_combine.cv$delta[2]
cat("Cross Validation Error\n", cpct.w_combine.cv.error)