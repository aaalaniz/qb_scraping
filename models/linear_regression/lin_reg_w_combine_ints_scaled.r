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
ints = qb_stats_w_combine['ints']

# Generate clean data set
data.scaled.w_combine.for_ints = data.frame(scale(na.omit(cbind(ints, college_stats))))

# Generate the linear model
lm.scaled.w_combine.ints <- lm(formula = ints ~. ,data = data.scaled.w_combine.for_ints)

# Find optimum linear regression model for ints
step_reg.scaled.w_combine.ints <- stepAIC(lm.scaled.w_combine.ints, direction="both")
summary(step_reg.scaled.w_combine.ints)
plot(step_reg.scaled.w_combine.ints)
leaps.scaled.w_combine.ints <- regsubsets(ints ~. ,data = data.scaled.w_combine.for_ints, nbest=10)
subsets(leaps.scaled.w_combine.ints, statistic="rsq")
cv.lm(df=data.scaled.w_combine.for_ints, step_reg.scaled.w_combine.ints, m=5) # 5 fold cross-validation