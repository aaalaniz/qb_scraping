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
yds = qb_stats['yds']

# Generate clean data set
data.scaled.no_combine.for_yds = data.frame(scale(na.omit(cbind(yds, college_stats))))

# Generate the linear model
lm.scaled.no_combine.yds <- lm(formula = yds ~. ,data = data.scaled.no_combine.for_yds)

# Find optimum linear regression model for yds
step_reg.scaled.no_combine.yds <- stepAIC(lm.scaled.no_combine.yds, direction="both")
summary(step_reg.scaled.no_combine.yds)
plot(step_reg.scaled.no_combine.yds)
leaps.scaled.no_combine.yds <- regsubsets(yds ~. ,data = data.scaled.no_combine.for_yds, nbest=10)
subsets(leaps.scaled.no_combine.yds, statistic="rsq")
cv.lm(df=data.scaled.no_combine.for_yds, step_reg.scaled.no_combine.yds, m=5) # 5 fold cross-validation