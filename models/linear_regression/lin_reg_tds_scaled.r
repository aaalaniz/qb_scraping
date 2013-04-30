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
data.scaled.no_combine.for_tds = data.frame(scale(na.omit(cbind(tds, college_stats))))

# Generate the linear model
lm.scaled.no_combine.tds <- lm(formula = tds ~. ,data = data.scaled.no_combine.for_tds)

# Find optimum linear regression model for tds
step_reg.scaled.no_combine.tds <- stepAIC(lm.scaled.no_combine.tds, direction="both")
summary(step_reg.scaled.no_combine.tds)
plot(step_reg.scaled.no_combine.tds)
leaps.scaled.no_combine.tds <- regsubsets(tds ~. ,data = data.scaled.no_combine.for_tds, nbest=10)
subsets(leaps.scaled.no_combine.tds, statistic="rsq")
cv.lm(df=data.scaled.no_combine.for_tds, step_reg.scaled.no_combine.tds, m=5) # 5 fold cross-validation