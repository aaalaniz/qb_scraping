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
cpct = qb_stats_w_combine['completion_percentage']

# Generate clean data set
data.scaled.w_combine.for_cpct = data.frame(scale(na.omit(cbind(cpct, college_stats))))

# Generate the linear model
lm.scaled.w_combine.cpct <- lm(formula = completion_percentage ~. ,data = data.scaled.w_combine.for_cpct)

# Find optimum linear regression model for cpct
step_reg.scaled.w_combine.cpct <- stepAIC(lm.scaled.w_combine.cpct, direction="both")
summary(step_reg.scaled.w_combine.cpct)
plot(step_reg.scaled.w_combine.cpct)
leaps.scaled.w_combine.cpct <- regsubsets(completion_percentage ~. ,data = data.scaled.w_combine.for_cpct, nbest=10)
subsets(leaps.scaled.w_combine.cpct, statistic="rsq")
cv.lm(df=data.scaled.w_combine.for_cpct, step_reg.scaled.w_combine.cpct, m=5) # 5 fold cross-validation