# Grab the college predictors
predictors <- c('height', 'weight', 'age', 'c_avg_cmpp', 'c_rate', 'c_pct', 'c_avg_inter', 'c_avg_tds', 'c_avg_yds', 'c_numyrs', 'c_avg_att')
college_stats = qb_stats[,predictors]

# Set the resopnse variables
win_pct = qb_stats['wins'] / qb_stats['games_started']
td_int_ratio = qb_stats['tds'] / qb_stats['ints']

bin_win_pct = ifelse(win_pct < 0.5, 0, 1)
bin_rating = ifelse(qb_stats['rating'] < 85.0, 0, 1)
bin_cpct = ifelse(qb_stats['completion_percentage'] < 60.0, 0, 1)
bin_td_int_ratio = ifelse(td_int_ratio < 2.0 , 0, 1)

# Establish the cost function
cost = function(r,pi=0) mean(abs(r-pi)>0.5)

# Generate clean data sets
data.no_combine.for_bin_win_pct = data.frame(na.omit(cbind(bin_win_pct, college_stats)))
data.no_combine.for_bin_rating = data.frame(na.omit(cbind(bin_rating, college_stats)))
data.no_combine.for_bin_cpct = data.frame(na.omit(cbind(bin_cpct, college_stats)))
data.no_combine.for_bin_td_int_ratio = data.frame(na.omit(cbind(bin_td_int_ratio, college_stats)))

# Logistic Regression
glm.no_combine.win_pct <- glm(formula = wins ~. ,data = data.no_combine.for_bin_win_pct, family=binomial())
exp(cbind(OR = coef(glm.no_combine.win_pct), confint(glm.no_combine.win_pct)))
win_pct.cv <- cv.glm(data = data.no_combine.for_bin_win_pct, glmfit = glm.no_combine.win_pct, cost, 5)
win_pct.cv.error <- win_pct.cv$delta[2]

glm.no_combine.rating <- glm(formula = rating ~. ,data = data.no_combine.for_bin_rating, family=binomial())
exp(cbind(OR = coef(glm.no_combine.rating), confint(glm.no_combine.rating)))
rating.cv <- cv.glm(data = data.no_combine.for_bin_rating, glmfit = glm.no_combine.rating, cost, 5)
rating.cv.error <- rating.cv$delta[2]

glm.no_combine.cpct <- glm(formula = completion_percentage ~. ,data = data.no_combine.for_bin_cpct, family=binomial())
exp(cbind(OR = coef(glm.no_combine.cpct), confint(glm.no_combine.cpct)))
cpct.cv <- cv.glm(data = data.no_combine.for_bin_cpct, glmfit = glm.no_combine.cpct, cost, 5)
cpct.cv.error <- cpct.cv$delta[2]

glm.no_combine.td_int_ratio <- glm(formula = tds ~. ,data = data.no_combine.for_bin_td_int_ratio, family='binomial')
exp(cbind(OR = coef(glm.no_combine.td_int_ratio), confint(glm.no_combine.td_int_ratio)))
td_int_ratio.cv <- cv.glm(data = data.no_combine.for_bin_td_int_ratio, glmfit = glm.no_combine.td_int_ratio, cost, 5)
td_int_ratio.cv.error <- td_int_ratio.cv$delta[2]