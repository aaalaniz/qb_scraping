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
bin_rating = ifelse(qb_stats['rating'] < 85.0, 0, 1)

# Generate clean data sets
data.no_combine.for_bin_rating = data.frame(na.omit(cbind(bin_rating, college_stats)))

# Logistic Regression
glm.no_combine.rating <- glm(formula = rating ~. ,data = data.no_combine.for_bin_rating, family=binomial())
exp(cbind(OR = coef(glm.no_combine.rating), confint(glm.no_combine.rating)))
rating.cv <- cv.glm(data = data.no_combine.for_bin_rating, glmfit = glm.no_combine.rating, cost, 5)
rating.cv.error <- rating.cv$delta[2]
cat("Cross Validation Error\n", rating.cv.error)