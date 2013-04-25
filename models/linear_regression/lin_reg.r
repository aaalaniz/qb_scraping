# Grab the college predictors
predictors <- c('height', 'weight', 'age', 'c_avg_cmpp', 'c_rate', 'c_pct', 'c_avg_inter', 'c_avg_tds', 'c_avg_yds', 'c_numyrs', 'c_avg_att')
college_stats = qb_stats[,predictors]

# Set the resopnse variables
wins = qb_stats['wins']
rating = qb_stats['rating']
cpct = qb_stats['completion_percentage']
games_started = qb_stats['games_started']
tds = qb_stats['tds']
yds = qb_stats['yds']
ints = qb_stats['ints']
sacks = qb_stats['sacked']
game_winning_drives = qb_stats['gwds']
comebacks = qb_stats['comebacks']
yds_per_game = qb_stats['yds_per_game']
qbr = qb_stats['qbr']
pctsk = qb_stats['perc_times_sk']
games_played = qb_stats['games_played']

# Generate clean data sets
data.scaled.no_combine.for_wins = data.frame(scale(na.omit(cbind(wins, college_stats))))
data.scaled.no_combine.for_rating = data.frame(scale(na.omit(cbind(rating, college_stats))))
data.scaled.no_combine.for_cpct = data.frame(scale(na.omit(cbind(cpct, college_stats))))
data.scaled.no_combine.for_gs = data.frame(scale(na.omit(cbind(games_started, college_stats))))
data.scaled.no_combine.for_tds = data.frame(scale(na.omit(cbind(tds, college_stats))))
data.scaled.no_combine.for_yds = data.frame(scale(na.omit(cbind(yds, college_stats))))
data.scaled.no_combine.for_ints = data.frame(scale(na.omit(cbind(ints, college_stats))))
data.scaled.no_combine.for_sacks = data.frame(scale(na.omit(cbind(sacks, college_stats))))
data.scaled.no_combine.for_game_winning_drives = data.frame(scale(na.omit(cbind(game_winning_drives, college_stats))))
data.scaled.no_combine.for_comebacks = data.frame(scale(na.omit(cbind(comebacks, college_stats))))
data.scaled.no_combine.for_yds_per_game = data.frame(scale(na.omit(cbind(yds_per_game, college_stats))))
data.scaled.no_combine.for_qbr = data.frame(scale(na.omit(cbind(qbr, college_stats))))
data.scaled.no_combine.for_pctsk = data.frame(scale(na.omit(cbind(pctsk, college_stats))))
data.scaled.no_combine.for_games_played = data.frame(scale(na.omit(cbind(games_played, college_stats))))

# Generate the linear models
lm.scaled.no_combine.wins <- lm(formula = wins ~. ,data = data.scaled.no_combine.for_wins)
lm.scaled.no_combine.rating <- lm(formula = rating ~. ,data = data.scaled.no_combine.for_rating)
lm.scaled.no_combine.cpct <- lm(formula = completion_percentage ~. ,data = data.scaled.no_combine.for_cpct)
lm.scaled.no_combine.gs <- lm(formula = games_started ~. ,data = data.scaled.no_combine.for_gs)
lm.scaled.no_combine.tds <- lm(formula = tds ~. ,data = data.scaled.no_combine.for_tds)
lm.scaled.no_combine.yds <- lm(formula = yds ~. ,data = data.scaled.no_combine.for_yds)
lm.scaled.no_combine.ints <- lm(formula = ints ~. ,data = data.scaled.no_combine.for_ints)
lm.scaled.no_combine.sacks <- lm(formula = sacked ~. ,data = data.scaled.no_combine.for_sacks)
lm.scaled.no_combine.game_winning_drives <- lm(formula = gwds ~. ,data = data.scaled.no_combine.for_game_winning_drives)
lm.scaled.no_combine.comebacks <- lm(formula = comebacks ~. ,data = data.scaled.no_combine.for_comebacks)
lm.scaled.no_combine.yds_per_game <- lm(formula = yds_per_game ~. ,data = data.scaled.no_combine.for_yds_per_game)
lm.scaled.no_combine.qbr <- lm(formula = qbr ~. ,data = data.scaled.no_combine.for_qbr)
lm.scaled.no_combine.pctsk <- lm(formula = perc_times_sk ~. ,data = data.scaled.no_combine.for_pctsk)
lm.scaled.no_combine.games_played <- lm(formula = games_played ~. ,data = data.scaled.no_combine.for_games_played)

# Stepwise Regression
step_reg.scaled.no_combine.wins <- stepAIC(lm.scaled.no_combine.wins, direction="both")
step_reg.scaled.no_combine.rating <- stepAIC(lm.scaled.no_combine.rating, direction="both")
step_reg.scaled.no_combine.cpct <- stepAIC(lm.scaled.no_combine.cpct, direction="both")
step_reg.scaled.no_combine.gs <- stepAIC(lm.scaled.no_combine.gs, direction="both")
step_reg.scaled.no_combine.tds <- stepAIC(lm.scaled.no_combine.tds, direction="both")
step_reg.scaled.no_combine.yds <- stepAIC(lm.scaled.no_combine.yds, direction="both")
step_reg.scaled.no_combine.ints <- stepAIC(lm.scaled.no_combine.ints, direction="both")
step_reg.scaled.no_combine.sacks <- stepAIC(lm.scaled.no_combine.sacks, direction="both")
step_reg.scaled.no_combine.game_winning_drives <- stepAIC(lm.scaled.no_combine.game_winning_drives, direction="both")
step_reg.scaled.no_combine.comebacks <- stepAIC(lm.scaled.no_combine.comebacks, direction="both")
step_reg.scaled.no_combine.yds_per_game <- stepAIC(lm.scaled.no_combine.yds_per_game, direction="both")
step_reg.scaled.no_combine.qbr <- stepAIC(lm.scaled.no_combine.qbr, direction="both")
step_reg.scaled.no_combine.pctsk <- stepAIC(lm.scaled.no_combine.pctsk, direction="both")
step_reg.scaled.no_combine.games_played <- stepAIC(lm.scaled.no_combine.games_played, direction="both")

# Subset Regression
leaps.scaled.no_combine.wins <- regsubsets(wins ~. ,data = data.scaled.no_combine.for_wins, nbest=10)
#subsets(leaps.scaled.no_combine.wins, statistic="rsq")
leaps.scaled.no_combine.rating <- regsubsets(rating ~. ,data = data.scaled.no_combine.for_rating, nbest=10)
#subsets(leaps.scaled.no_combine.rating, statistic="rsq")
leaps.scaled.no_combine.cpct <- regsubsets(completion_percentage ~. ,data = data.scaled.no_combine.for_cpct, nbest=10)
#subsets(leaps.scaled.no_combine.cpct, statistic="rsq")
leaps.scaled.no_combine.gs <- regsubsets(games_started ~. ,data = data.scaled.no_combine.for_gs, nbest=10)
#subsets(leaps.scaled.no_combine.gs, statistic="rsq")
leaps.scaled.no_combine.tds <- regsubsets(tds ~. ,data = data.scaled.no_combine.for_tds, nbest=10)
#subsets(leaps.scaled.no_combine.tds, statistic="rsq")
leaps.scaled.no_combine.yds <- regsubsets(yds ~. ,data = data.scaled.no_combine.for_yds, nbest=10)
#subsets(leaps.scaled.no_combine.yds, statistic="rsq")
leaps.scaled.no_combine.ints <- regsubsets(ints ~. ,data = data.scaled.no_combine.for_ints, nbest=10)
#subsets(leaps.scaled.no_combine.ints, statistic="rsq")
leaps.scaled.no_combine.sacks <- regsubsets(sacked ~. ,data = data.scaled.no_combine.for_sacks, nbest=10)
#subsets(leaps.scaled.no_combine.sacks, statistic="rsq")
leaps.scaled.no_combine.game_winning_drives <- regsubsets(gwds ~. ,data = data.scaled.no_combine.for_game_winning_drives, nbest=10)
#subsets(leaps.scaled.no_combine.game_winning_drives, statistic="rsq")
leaps.scaled.no_combine.comebacks <- regsubsets(comebacks ~. ,data = data.scaled.no_combine.for_comebacks, nbest=10)
#subsets(leaps.scaled.no_combine.comebacks, statistic="rsq")
leaps.scaled.no_combine.yds_per_game <- regsubsets(yds_per_game ~. ,data = data.scaled.no_combine.for_yds_per_game, nbest=10)
#subsets(leaps.scaled.no_combine.yds_per_game, statistic="rsq")
leaps.scaled.no_combine.qbr <- regsubsets(qbr ~. ,data = data.scaled.no_combine.for_qbr, nbest=10)
#subsets(leaps.scaled.no_combine.qbr, statistic="rsq")
leaps.scaled.no_combine.pctsk <- regsubsets(perc_times_sk ~. ,data = data.scaled.no_combine.for_pctsk, nbest=10)
#subsets(leaps.scaled.no_combine.pctsk, statistic="rsq")
leaps.scaled.no_combine.games_played <- regsubsets(games_played ~. ,data = data.scaled.no_combine.for_games_played, nbest=10)
#subsets(leaps.scaled.no_combine.games_played, statistic="rsq")