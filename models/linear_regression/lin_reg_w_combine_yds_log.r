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
yds = qb_stats_w_combine['yds']

# Generate clean data set
data.log.w_combine.for_yds = data.frame(log(na.omit(cbind(yds, college_stats)) + 0.1))

# Generate the linear model
lm.log.w_combine.yds <- lm(formula = yds ~. ,data = data.log.w_combine.for_yds)

# Find optimum linear regression model for yds
step_reg.log.w_combine.yds <- stepAIC(lm.log.w_combine.yds, direction="both")
summary(step_reg.log.w_combine.yds)
plot(step_reg.log.w_combine.yds)
leaps.log.w_combine.yds <- regsubsets(yds ~. ,data = data.log.w_combine.for_yds, nbest=10)
subsets(leaps.log.w_combine.yds, statistic="rsq")
cv.lm(df=data.log.w_combine.for_yds, step_reg.log.w_combine.yds, m=5) # 5 fold cross-validation