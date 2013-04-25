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
data.root.w_combine.for_wins = data.frame(sqrt(na.omit(cbind(wins, college_stats_w_combine))))
data.root.w_combine.for_rating = data.frame(sqrt(na.omit(cbind(rating, college_stats_w_combine))))
data.root.w_combine.for_cpct = data.frame(sqrt(na.omit(cbind(cpct, college_stats_w_combine))))
data.root.w_combine.for_gs = data.frame(sqrt(na.omit(cbind(games_started, college_stats_w_combine))))
data.root.w_combine.for_tds = data.frame(sqrt(na.omit(cbind(tds, college_stats_w_combine))))
data.root.w_combine.for_yds = data.frame(sqrt(na.omit(cbind(yds, college_stats_w_combine))))
data.root.w_combine.for_ints = data.frame(sqrt(na.omit(cbind(ints, college_stats_w_combine))))
data.root.w_combine.for_sacks = data.frame(sqrt(na.omit(cbind(sacks, college_stats_w_combine))))
data.root.w_combine.for_game_winning_drives = data.frame(sqrt(na.omit(cbind(game_winning_drives, college_stats_w_combine))))
data.root.w_combine.for_comebacks = data.frame(sqrt(na.omit(cbind(comebacks, college_stats_w_combine))))
data.root.w_combine.for_yds_per_game = data.frame(sqrt(na.omit(cbind(yds_per_game, college_stats_w_combine))))
data.root.w_combine.for_qbr = data.frame(sqrt(na.omit(cbind(qbr, college_stats_w_combine))))
data.root.w_combine.for_pctsk = data.frame(sqrt(na.omit(cbind(pctsk, college_stats_w_combine))))
data.root.w_combine.for_games_played = data.frame(sqrt(na.omit(cbind(games_played, college_stats_w_combine))))

# Generate the linear models
lm.root.w_combine.wins <- lm(formula = wins ~. ,data = data.root.w_combine.for_wins)
lm.root.w_combine.rating <- lm(formula = rating ~. ,data = data.root.w_combine.for_rating)
lm.root.w_combine.cpct <- lm(formula = completion_percentage ~. ,data = data.root.w_combine.for_cpct)
lm.root.w_combine.gs <- lm(formula = games_started ~. ,data = data.root.w_combine.for_gs)
lm.root.w_combine.tds <- lm(formula = tds ~. ,data = data.root.w_combine.for_tds)
lm.root.w_combine.yds <- lm(formula = yds ~. ,data = data.root.w_combine.for_yds)
lm.root.w_combine.ints <- lm(formula = ints ~. ,data = data.root.w_combine.for_ints)
lm.root.w_combine.sacks <- lm(formula = sacked ~. ,data = data.root.w_combine.for_sacks)
lm.root.w_combine.game_winning_drives <- lm(formula = gwds ~. ,data = data.root.w_combine.for_game_winning_drives)
lm.root.w_combine.comebacks <- lm(formula = comebacks ~. ,data = data.root.w_combine.for_comebacks)
lm.root.w_combine.yds_per_game <- lm(formula = yds_per_game ~. ,data = data.root.w_combine.for_yds_per_game)
lm.root.w_combine.qbr <- lm(formula = qbr ~. ,data = data.root.w_combine.for_qbr)
lm.root.w_combine.pctsk <- lm(formula = perc_times_sk ~. ,data = data.root.w_combine.for_pctsk)
lm.root.w_combine.games_played <- lm(formula = games_played ~. ,data = data.root.w_combine.for_games_played)

# Stepwise Regression
step_reg.root.w_combine.wins <- stepAIC(lm.root.w_combine.wins, direction="both")
step_reg.root.w_combine.rating <- stepAIC(lm.root.w_combine.rating, direction="both")
step_reg.root.w_combine.cpct <- stepAIC(lm.root.w_combine.cpct, direction="both")
step_reg.root.w_combine.gs <- stepAIC(lm.root.w_combine.gs, direction="both")
step_reg.root.w_combine.tds <- stepAIC(lm.root.w_combine.tds, direction="both")
step_reg.root.w_combine.yds <- stepAIC(lm.root.w_combine.yds, direction="both")
step_reg.root.w_combine.ints <- stepAIC(lm.root.w_combine.ints, direction="both")
step_reg.root.w_combine.sacks <- stepAIC(lm.root.w_combine.sacks, direction="both")
step_reg.root.w_combine.game_winning_drives <- stepAIC(lm.root.w_combine.game_winning_drives, direction="both")
step_reg.root.w_combine.comebacks <- stepAIC(lm.root.w_combine.comebacks, direction="both")
step_reg.root.w_combine.yds_per_game <- stepAIC(lm.root.w_combine.yds_per_game, direction="both")
#step_reg.root.w_combine.qbr <- stepAIC(lm.root.w_combine.qbr, direction="both")
step_reg.root.w_combine.pctsk <- stepAIC(lm.root.w_combine.pctsk, direction="both")
step_reg.root.w_combine.games_played <- stepAIC(lm.root.w_combine.games_played, direction="both")

# Subset Regression
leaps.root.w_combine.wins <- regsubsets(wins ~. ,data = data.root.w_combine.for_wins, nbest=30)
#subsets(leaps.root.w_combine.wins, statistic="rsq")
leaps.root.w_combine.rating <- regsubsets(rating ~. ,data = data.root.w_combine.for_rating, nbest=10)
#subsets(leaps.root.w_combine.rating, statistic="rsq")
leaps.root.w_combine.cpct <- regsubsets(completion_percentage ~. ,data = data.root.w_combine.for_cpct, nbest=30)
#subsets(leaps.root.w_combine.cpct, statistic="rsq")
leaps.root.w_combine.gs <- regsubsets(games_started ~. ,data = data.root.w_combine.for_gs, nbest=10)
#subsets(leaps.root.w_combine.gs, statistic="rsq")
leaps.root.w_combine.tds <- regsubsets(tds ~. ,data = data.root.w_combine.for_tds, nbest=10)
#subsets(leaps.root.w_combine.tds, statistic="rsq")
leaps.root.w_combine.yds <- regsubsets(yds ~. ,data = data.root.w_combine.for_yds, nbest=10)
#subsets(leaps.root.w_combine.yds, statistic="rsq")
leaps.root.w_combine.ints <- regsubsets(ints ~. ,data = data.root.w_combine.for_ints, nbest=10)
#subsets(leaps.root.w_combine.ints, statistic="rsq")
leaps.root.w_combine.sacks <- regsubsets(sacked ~. ,data = data.root.w_combine.for_sacks, nbest=10)
#subsets(leaps.root.w_combine.sacks, statistic="rsq")
leaps.root.w_combine.game_winning_drives <- regsubsets(gwds ~. ,data = data.root.w_combine.for_game_winning_drives, nbest=10)
#subsets(leaps.root.w_combine.game_winning_drives, statistic="rsq")
leaps.root.w_combine.comebacks <- regsubsets(comebacks ~. ,data = data.root.w_combine.for_comebacks, nbest=10)
#subsets(leaps.root.w_combine.comebacks, statistic="rsq")
leaps.root.w_combine.yds_per_game <- regsubsets(yds_per_game ~. ,data = data.root.w_combine.for_yds_per_game, nbest=10)
#subsets(leaps.root.w_combine.yds_per_game, statistic="rsq")
leaps.root.w_combine.qbr <- regsubsets(qbr ~. ,data = data.root.w_combine.for_qbr, nbest=10)
#subsets(leaps.root.w_combine.qbr, statistic="rsq")
leaps.root.w_combine.pctsk <- regsubsets(perc_times_sk ~. ,data = data.root.w_combine.for_pctsk, nbest=10)
#subsets(leaps.root.w_combine.pctsk, statistic="rsq")
leaps.root.w_combine.games_played <- regsubsets(games_played ~. ,data = data.root.w_combine.for_games_played, nbest=10)
#subsets(leaps.root.w_combine.games_played, statistic="rsq")