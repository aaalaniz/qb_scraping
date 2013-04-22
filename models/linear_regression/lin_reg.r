# Grab the college predictors
college_stats = qb_stats[,c('c_avg_cmpp','c_aya', 'c_rate', 'c_pct', 'c_avg_att', 'c_avg_inter', 'c_avg_tds', 'c_avg_yds')]

# Set the resopnse variables
wins = qb_stats['wins']
rating = qb_stats['rating']
cpct = qb_stats['completion_percentage']
games_started = qb_stats['games_started']
tds = qb_stats['tds']
yds = qb_stats['yds']

# Generate clean data sets
data_one = na.omit(cbind(wins, college_stats))
data_two = na.omit(cbind(rating, college_stats))
data_three = na.omit(cbind(cpct, college_stats))
data_four = na.omit(cbind(games_started, college_stats))
data_five = na.omit(cbind(tds, college_stats))
data_six = na.omit(cbind(yds, college_stats))

# Generate the linear models
glm.one <- glm(formula=data.matrix(data_one[,1])~data.matrix(data_one[,-1]))
glm.two <- glm(formula=data.matrix(data_two[,1])~data.matrix(data_two[,-1]))
glm.three <- glm(formula=data.matrix(data_three[,1])~data.matrix(data_three[,-1]))
glm.four <- glm(formula=data.matrix(data_four[,1])~data.matrix(data_four[,-1]))
glm.five <- glm(formula=data.matrix(data_five[,1])~data.matrix(data_five[,-1]))
glm.six <- glm(formula=data.matrix(data_six[,1])~data.matrix(data_six[,-1]))
