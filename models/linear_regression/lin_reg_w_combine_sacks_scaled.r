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
sacks = qb_stats_w_combine['sacked']

# Generate clean data set
data.scaled.w_combine.for_sacks = data.frame(scale(na.omit(cbind(sacks, college_stats))))

# Generate the linear model
lm.scaled.w_combine.sacks <- lm(formula = sacked ~. ,data = data.scaled.w_combine.for_sacks)

# Find optimum linear regression model for sacks
step_reg.scaled.w_combine.sacks <- stepAIC(lm.scaled.w_combine.sacks, direction="both")
summary(step_reg.scaled.w_combine.sacks)
plot(step_reg.scaled.w_combine.sacks)
leaps.scaled.w_combine.sacks <- regsubsets(sacked ~. ,data = data.scaled.w_combine.for_sacks, nbest=10)
subsets(leaps.scaled.w_combine.sacks, statistic="rsq")
cv.lm(df=data.scaled.w_combine.for_sacks, step_reg.scaled.w_combine.sacks, m=5) # 5 fold cross-validation