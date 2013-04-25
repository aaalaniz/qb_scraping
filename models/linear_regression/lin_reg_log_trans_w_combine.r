# Grab the college predictors
predictors <- c('height', 'weight', 'age', 'c_avg_cmpp', 'c_rate', 'c_pct', 'c_avg_inter', 'c_avg_tds', 'c_avg_yds', 'c_numyrs', 'c_avg_att', 'X40', 'wonderlic', 'cone', 'shuttle', 'vert_leap', 'broad_jump')
college_stats_w_combine = qb_stats_w_combine[, predictors]

# Set the resopnse variables
wins = qb_stats_w_combine['wins']
rating = qb_stats_w_combine['rating']
cpct = qb_stats_w_combine['completion_percentage']
games_started = qb_stats_w_combine['games_started']
tds = qb_stats_w_combine['tds']
yds = qb_stats_w_combine['yds']
ints = qb_stats_w_combine['ints']
sacks = qb_stats_w_combine['sacked']
game_winning_drives = qb_stats_w_combine['gwds']
comebacks = qb_stats_w_combine['comebacks']
yds_per_game = qb_stats_w_combine['yds_per_game']
qbr = qb_stats_w_combine['qbr']
pctsk = qb_stats_w_combine['perc_times_sk']
games_played = qb_stats_w_combine['games_played']

# Generate clean data sets
data.log.w_combine.for_wins = data.frame(log(na.omit(cbind(wins, college_stats_w_combine)) + 0.1))
data.log.w_combine.for_rating = data.frame(log(na.omit(cbind(rating, college_stats_w_combine)) + 0.1))
data.log.w_combine.for_cpct = data.frame(log(na.omit(cbind(cpct, college_stats_w_combine)) + 0.1))
data.log.w_combine.for_gs = data.frame(log(na.omit(cbind(games_started, college_stats_w_combine)) + 0.1))
data.log.w_combine.for_tds = data.frame(log(na.omit(cbind(tds, college_stats_w_combine)) + 0.1))
data.log.w_combine.for_yds = data.frame(log(na.omit(cbind(yds, college_stats_w_combine)) + 0.1))
data.log.w_combine.for_ints = data.frame(log(na.omit(cbind(ints, college_stats_w_combine)) + 0.1))
data.log.w_combine.for_sacks = data.frame(log(na.omit(cbind(sacks, college_stats_w_combine)) + 0.1))
data.log.w_combine.for_game_winning_drives = data.frame(log(na.omit(cbind(game_winning_drives, college_stats_w_combine)) + 0.1))
data.log.w_combine.for_comebacks = data.frame(log(na.omit(cbind(comebacks, college_stats_w_combine)) + 0.1))
data.log.w_combine.for_yds_per_game = data.frame(log(na.omit(cbind(yds_per_game, college_stats_w_combine)) + 0.1))
data.log.w_combine.for_qbr = data.frame(log(na.omit(cbind(qbr, college_stats_w_combine)) + 0.1))
data.log.w_combine.for_pctsk = data.frame(log(na.omit(cbind(pctsk, college_stats_w_combine)) + 0.1))
data.log.w_combine.for_games_played = data.frame(log(na.omit(cbind(games_played, college_stats_w_combine)) + 0.1))

# Generate the linear models
lm.log.w_combine.wins <- lm(formula = wins ~. ,data = data.log.w_combine.for_wins)
lm.log.w_combine.rating <- lm(formula = rating ~. ,data = data.log.w_combine.for_rating)
lm.log.w_combine.cpct <- lm(formula = completion_percentage ~. ,data = data.log.w_combine.for_cpct)
lm.log.w_combine.gs <- lm(formula = games_started ~. ,data = data.log.w_combine.for_gs)
lm.log.w_combine.tds <- lm(formula = tds ~. ,data = data.log.w_combine.for_tds)
lm.log.w_combine.yds <- lm(formula = yds ~. ,data = data.log.w_combine.for_yds)
lm.log.w_combine.ints <- lm(formula = ints ~. ,data = data.log.w_combine.for_ints)
lm.log.w_combine.sacks <- lm(formula = sacked ~. ,data = data.log.w_combine.for_sacks)
lm.log.w_combine.game_winning_drives <- lm(formula = gwds ~. ,data = data.log.w_combine.for_game_winning_drives)
lm.log.w_combine.comebacks <- lm(formula = comebacks ~. ,data = data.log.w_combine.for_comebacks)
lm.log.w_combine.yds_per_game <- lm(formula = yds_per_game ~. ,data = data.log.w_combine.for_yds_per_game)
lm.log.w_combine.qbr <- lm(formula = qbr ~. ,data = data.log.w_combine.for_qbr)
lm.log.w_combine.pctsk <- lm(formula = perc_times_sk ~. ,data = data.log.w_combine.for_pctsk)
lm.log.w_combine.games_played <- lm(formula = games_played ~. ,data = data.log.w_combine.for_games_played)

# Stepwise Regression
step_reg.log.w_combine.wins <- stepAIC(lm.log.w_combine.wins, direction="both")
step_reg.log.w_combine.rating <- stepAIC(lm.log.w_combine.rating, direction="both")
step_reg.log.w_combine.cpct <- stepAIC(lm.log.w_combine.cpct, direction="both")
step_reg.log.w_combine.gs <- stepAIC(lm.log.w_combine.gs, direction="both")
step_reg.log.w_combine.tds <- stepAIC(lm.log.w_combine.tds, direction="both")
step_reg.log.w_combine.yds <- stepAIC(lm.log.w_combine.yds, direction="both")
step_reg.log.w_combine.ints <- stepAIC(lm.log.w_combine.ints, direction="both")
step_reg.log.w_combine.sacks <- stepAIC(lm.log.w_combine.sacks, direction="both")
step_reg.log.w_combine.game_winning_drives <- stepAIC(lm.log.w_combine.game_winning_drives, direction="both")
step_reg.log.w_combine.comebacks <- stepAIC(lm.log.w_combine.comebacks, direction="both")
step_reg.log.w_combine.yds_per_game <- stepAIC(lm.log.w_combine.yds_per_game, direction="both")
#step_reg.log.w_combine.qbr <- stepAIC(lm.log.w_combine.qbr, direction="both")
step_reg.log.w_combine.pctsk <- stepAIC(lm.log.w_combine.pctsk, direction="both")
step_reg.log.w_combine.games_played <- stepAIC(lm.log.w_combine.games_played, direction="both")

# Subset Regression
leaps.log.w_combine.wins <- regsubsets(wins ~. ,data = data.log.w_combine.for_wins, nbest=30)
#subsets(leaps.log.w_combine.wins, statistic="rsq")
leaps.log.w_combine.rating <- regsubsets(rating ~. ,data = data.log.w_combine.for_rating, nbest=10)
#subsets(leaps.log.w_combine.rating, statistic="rsq")
leaps.log.w_combine.cpct <- regsubsets(completion_percentage ~. ,data = data.log.w_combine.for_cpct, nbest=30)
#subsets(leaps.log.w_combine.cpct, statistic="rsq")
leaps.log.w_combine.gs <- regsubsets(games_started ~. ,data = data.log.w_combine.for_gs, nbest=10)
#subsets(leaps.log.w_combine.gs, statistic="rsq")
leaps.log.w_combine.tds <- regsubsets(tds ~. ,data = data.log.w_combine.for_tds, nbest=10)
#subsets(leaps.log.w_combine.tds, statistic="rsq")
leaps.log.w_combine.yds <- regsubsets(yds ~. ,data = data.log.w_combine.for_yds, nbest=10)
#subsets(leaps.log.w_combine.yds, statistic="rsq")
leaps.log.w_combine.ints <- regsubsets(ints ~. ,data = data.log.w_combine.for_ints, nbest=10)
#subsets(leaps.log.w_combine.ints, statistic="rsq")
leaps.log.w_combine.sacks <- regsubsets(sacked ~. ,data = data.log.w_combine.for_sacks, nbest=10)
#subsets(leaps.log.w_combine.sacks, statistic="rsq")
leaps.log.w_combine.game_winning_drives <- regsubsets(gwds ~. ,data = data.log.w_combine.for_game_winning_drives, nbest=10)
#subsets(leaps.log.w_combine.game_winning_drives, statistic="rsq")
leaps.log.w_combine.comebacks <- regsubsets(comebacks ~. ,data = data.log.w_combine.for_comebacks, nbest=10)
#subsets(leaps.log.w_combine.comebacks, statistic="rsq")
leaps.log.w_combine.yds_per_game <- regsubsets(yds_per_game ~. ,data = data.log.w_combine.for_yds_per_game, nbest=10)
#subsets(leaps.log.w_combine.yds_per_game, statistic="rsq")
leaps.log.w_combine.qbr <- regsubsets(qbr ~. ,data = data.log.w_combine.for_qbr, nbest=10)
#subsets(leaps.log.w_combine.qbr, statistic="rsq")
leaps.log.w_combine.pctsk <- regsubsets(perc_times_sk ~. ,data = data.log.w_combine.for_pctsk, nbest=10)
#subsets(leaps.log.w_combine.pctsk, statistic="rsq")
leaps.log.w_combine.games_played <- regsubsets(games_played ~. ,data = data.log.w_combine.for_games_played, nbest=10)
#subsets(leaps.log.w_combine.games_played, statistic="rsq")