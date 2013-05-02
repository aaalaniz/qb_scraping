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
bin_cpct = ifelse(qb_stats['completion_percentage'] < 60.0, 0, 1)

# Generate clean data sets
data.no_combine.for_bin_cpct = data.frame(na.omit(cbind(bin_cpct, college_stats)))

# Logistic Regression
glm.no_combine.cpct <- glm(formula = completion_percentage ~. ,data = data.no_combine.for_bin_cpct, family=binomial())
exp(cbind(OR = coef(glm.no_combine.cpct), confint(glm.no_combine.cpct)))
cpct.cv <- cv.glm(data = data.no_combine.for_bin_cpct, glmfit = glm.no_combine.cpct, cost, 5)
cpct.cv.error <- cpct.cv$delta[2]
cat("Cross Validation Error\n", cpct.cv.error)