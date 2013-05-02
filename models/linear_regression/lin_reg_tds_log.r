#+ setup, include=FALSE
library(knitr)
library(DAAG)
library(MASS)
library(leaps)
library(car)
opts_chunk$set(fig.path='figure/silk-')

#'
# Fetch Data
qb_stats <- read.csv("../data/qb_stats.csv")

# Grab the college predictors
predictors <- c('height', 'weight', 'age', 'c_avg_cmpp', 'c_rate', 'c_pct', 'c_avg_inter', 'c_avg_tds', 'c_avg_yds', 'c_numyrs', 'c_avg_att')
college_stats = qb_stats[,predictors]

# Set the resopnse variables
tds = qb_stats['tds']

# Generate clean data set
data.log.no_combine.for_tds = data.frame(log(na.omit(cbind(tds, college_stats)) + 0.1))

# Generate the linear model
lm.log.no_combine.tds <- lm(formula = tds ~. ,data = data.log.no_combine.for_tds)

# Find optimum linear regression model for tds
step_reg.log.no_combine.tds <- stepAIC(lm.log.no_combine.tds, direction="both")
summary(step_reg.log.no_combine.tds)
plot(step_reg.log.no_combine.tds)
leaps.log.no_combine.tds <- regsubsets(tds ~. ,data = data.log.no_combine.for_tds, nbest=10)
subsets(leaps.log.no_combine.tds, statistic="rsq")
cv.lm(df=data.log.no_combine.for_tds, step_reg.log.no_combine.tds, m=5) # 5 fold cross-validation