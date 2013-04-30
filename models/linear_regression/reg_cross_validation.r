library(DAAG)

# Cross validation on models for scaled data with no combine data
cv.lm(df=data.scaled.no_combine.for_wins, step_reg.scaled.no_combine.wins, m=5) # 5 fold cross-validation
cv.lm(df=data.scaled.no_combine.for_comebacks, step_reg.scaled.no_combine.comebacks, m=5) # 5 fold cross-validation
cv.lm(df=data.scaled.no_combine.for_cpct, step_reg.scaled.no_combine.cpct, m=5) # 5 fold cross-validation
cv.lm(df=data.scaled.no_combine.for_game_winning_drives, step_reg.scaled.no_combine.game_winning_drives, m=5) # 5 fold cross-validation
cv.lm(df=data.scaled.no_combine.for_games_played, step_reg.scaled.no_combine.games_played, m=5) # 5 fold cross-validation
cv.lm(df=data.scaled.no_combine.for_gs, step_reg.scaled.no_combine.gs, m=5) # 5 fold cross-validation
cv.lm(df=data.scaled.no_combine.for_ints, step_reg.scaled.no_combine.ints, m=5) # 5 fold cross-validation
cv.lm(df=data.scaled.no_combine.for_pctsk, step_reg.scaled.no_combine.pctsk, m=5) # 5 fold cross-validation
cv.lm(df=data.scaled.no_combine.for_qbr, step_reg.scaled.no_combine.qbr, m=5) # 5 fold cross-validation
cv.lm(df=data.scaled.no_combine.for_rating, step_reg.scaled.no_combine.rating, m=5) # 5 fold cross-validation
cv.lm(df=data.scaled.no_combine.for_sacks, step_reg.scaled.no_combine.sacks, m=5) # 5 fold cross-validation
cv.lm(df=data.scaled.no_combine.for_tds, step_reg.scaled.no_combine.tds, m=5) # 5 fold cross-validation
cv.lm(df=data.scaled.no_combine.for_yds, step_reg.scaled.no_combine.yds, m=5) # 5 fold cross-validation

# Cross validation on models for scaled data with combine data
cv.lm(df=data.scaled.w_combine.for_wins, step_reg.scaled.w_combine.wins, m=5) # 5 fold cross-validation
cv.lm(df=data.scaled.w_combine.for_comebacks, step_reg.scaled.w_combine.comebacks, m=5) # 5 fold cross-validation
cv.lm(df=data.scaled.w_combine.for_cpct, step_reg.scaled.w_combine.cpct, m=5) # 5 fold cross-validation
cv.lm(df=data.scaled.w_combine.for_game_winning_drives, step_reg.scaled.w_combine.game_winning_drives, m=5) # 5 fold cross-validation
cv.lm(df=data.scaled.w_combine.for_games_played, step_reg.scaled.w_combine.games_played, m=5) # 5 fold cross-validation
cv.lm(df=data.scaled.w_combine.for_gs, step_reg.scaled.w_combine.gs, m=5) # 5 fold cross-validation
cv.lm(df=data.scaled.w_combine.for_ints, step_reg.scaled.w_combine.ints, m=5) # 5 fold cross-validation
cv.lm(df=data.scaled.w_combine.for_pctsk, step_reg.scaled.w_combine.pctsk, m=5) # 5 fold cross-validation
cv.lm(df=data.scaled.w_combine.for_qbr, step_reg.scaled.w_combine.qbr, m=5) # 5 fold cross-validation
cv.lm(df=data.scaled.w_combine.for_rating, step_reg.scaled.w_combine.rating, m=5) # 5 fold cross-validation
cv.lm(df=data.scaled.w_combine.for_sacks, step_reg.scaled.w_combine.sacks, m=5) # 5 fold cross-validation
cv.lm(df=data.scaled.w_combine.for_tds, step_reg.scaled.w_combine.tds, m=5) # 5 fold cross-validation
cv.lm(df=data.scaled.w_combine.for_yds, step_reg.scaled.w_combine.yds, m=5) # 5 fold cross-validation

# Cross validation on models for log transformed data with no combine data
cv.lm(df=data.log.no_combine.for_wins, step_reg.log.no_combine.wins, m=5) # 5 fold cross-validation
cv.lm(df=data.log.no_combine.for_comebacks, step_reg.log.no_combine.comebacks, m=5) # 5 fold cross-validation
cv.lm(df=data.log.no_combine.for_cpct, step_reg.log.no_combine.cpct, m=5) # 5 fold cross-validation
cv.lm(df=data.log.no_combine.for_game_winning_drives, step_reg.log.no_combine.game_winning_drives, m=5) # 5 fold cross-validation
cv.lm(df=data.log.no_combine.for_games_played, step_reg.log.no_combine.games_played, m=5) # 5 fold cross-validation
cv.lm(df=data.log.no_combine.for_gs, step_reg.log.no_combine.gs, m=5) # 5 fold cross-validation
cv.lm(df=data.log.no_combine.for_ints, step_reg.log.no_combine.ints, m=5) # 5 fold cross-validation
cv.lm(df=data.log.no_combine.for_pctsk, step_reg.log.no_combine.pctsk, m=5) # 5 fold cross-validation
cv.lm(df=data.log.no_combine.for_qbr, step_reg.log.no_combine.qbr, m=5) # 5 fold cross-validation
cv.lm(df=data.log.no_combine.for_rating, step_reg.log.no_combine.rating, m=5) # 5 fold cross-validation
cv.lm(df=data.log.no_combine.for_sacks, step_reg.log.no_combine.sacks, m=5) # 5 fold cross-validation
cv.lm(df=data.log.no_combine.for_tds, step_reg.log.no_combine.tds, m=5) # 5 fold cross-validation
cv.lm(df=data.log.no_combine.for_yds, step_reg.log.no_combine.yds, m=5) # 5 fold cross-validation

# Cross validation on models for log transform with combine data
cv.lm(df=data.log.w_combine.for_wins, step_reg.log.w_combine.wins, m=5) # 5 fold cross-validation
cv.lm(df=data.log.w_combine.for_comebacks, step_reg.log.w_combine.comebacks, m=5) # 5 fold cross-validation
cv.lm(df=data.log.w_combine.for_cpct, step_reg.log.w_combine.cpct, m=5) # 5 fold cross-validation
cv.lm(df=data.log.w_combine.for_game_winning_drives, step_reg.log.w_combine.game_winning_drives, m=5) # 5 fold cross-validation
cv.lm(df=data.log.w_combine.for_games_played, step_reg.log.w_combine.games_played, m=5) # 5 fold cross-validation
cv.lm(df=data.log.w_combine.for_gs, step_reg.log.w_combine.gs, m=5) # 5 fold cross-validation
cv.lm(df=data.log.w_combine.for_ints, step_reg.log.w_combine.ints, m=5) # 5 fold cross-validation
cv.lm(df=data.log.w_combine.for_pctsk, step_reg.log.w_combine.pctsk, m=5) # 5 fold cross-validation
cv.lm(df=data.log.w_combine.for_qbr, step_reg.log.w_combine.qbr, m=5) # 5 fold cross-validation
cv.lm(df=data.log.w_combine.for_rating, step_reg.log.w_combine.rating, m=5) # 5 fold cross-validation
cv.lm(df=data.log.w_combine.for_sacks, step_reg.log.w_combine.sacks, m=5) # 5 fold cross-validation
cv.lm(df=data.log.w_combine.for_tds, step_reg.log.w_combine.tds, m=5) # 5 fold cross-validation
cv.lm(df=data.log.w_combine.for_yds, step_reg.log.w_combine.yds, m=5) # 5 fold cross-validation

# Cross validation on models for root transformed data with no combine data
cv.lm(df=data.root.no_combine.for_wins, step_reg.root.no_combine.wins, m=5) # 5 fold cross-validation
cv.lm(df=data.root.no_combine.for_comebacks, step_reg.root.no_combine.comebacks, m=5) # 5 fold cross-validation
cv.lm(df=data.root.no_combine.for_cpct, step_reg.root.no_combine.cpct, m=5) # 5 fold cross-validation
cv.lm(df=data.root.no_combine.for_game_winning_drives, step_reg.root.no_combine.game_winning_drives, m=5) # 5 fold cross-validation
cv.lm(df=data.root.no_combine.for_games_played, step_reg.root.no_combine.games_played, m=5) # 5 fold cross-validation
cv.lm(df=data.root.no_combine.for_gs, step_reg.root.no_combine.gs, m=5) # 5 fold cross-validation
cv.lm(df=data.root.no_combine.for_ints, step_reg.root.no_combine.ints, m=5) # 5 fold cross-validation
cv.lm(df=data.root.no_combine.for_pctsk, step_reg.root.no_combine.pctsk, m=5) # 5 fold cross-validation
cv.lm(df=data.root.no_combine.for_qbr, step_reg.root.no_combine.qbr, m=5) # 5 fold cross-validation
cv.lm(df=data.root.no_combine.for_rating, step_reg.root.no_combine.rating, m=5) # 5 fold cross-validation
cv.lm(df=data.root.no_combine.for_sacks, step_reg.root.no_combine.sacks, m=5) # 5 fold cross-validation
cv.lm(df=data.root.no_combine.for_tds, step_reg.root.no_combine.tds, m=5) # 5 fold cross-validation
cv.lm(df=data.root.no_combine.for_yds, step_reg.root.no_combine.yds, m=5) # 5 fold cross-validation

# Cross validation on models for root transform with combine data
cv.lm(df=data.root.w_combine.for_wins, step_reg.root.w_combine.wins, m=5) # 5 fold cross-validation
cv.lm(df=data.root.w_combine.for_comebacks, step_reg.root.w_combine.comebacks, m=5) # 5 fold cross-validation
cv.lm(df=data.root.w_combine.for_cpct, step_reg.root.w_combine.cpct, m=5) # 5 fold cross-validation
cv.lm(df=data.root.w_combine.for_game_winning_drives, step_reg.root.w_combine.game_winning_drives, m=5) # 5 fold cross-validation
cv.lm(df=data.root.w_combine.for_games_played, step_reg.root.w_combine.games_played, m=5) # 5 fold cross-validation
cv.lm(df=data.root.w_combine.for_gs, step_reg.root.w_combine.gs, m=5) # 5 fold cross-validation
cv.lm(df=data.root.w_combine.for_ints, step_reg.root.w_combine.ints, m=5) # 5 fold cross-validation
cv.lm(df=data.root.w_combine.for_pctsk, step_reg.root.w_combine.pctsk, m=5) # 5 fold cross-validation
cv.lm(df=data.root.w_combine.for_qbr, step_reg.root.w_combine.qbr, m=5) # 5 fold cross-validation
cv.lm(df=data.root.w_combine.for_rating, step_reg.root.w_combine.rating, m=5) # 5 fold cross-validation
cv.lm(df=data.root.w_combine.for_sacks, step_reg.root.w_combine.sacks, m=5) # 5 fold cross-validation
cv.lm(df=data.root.w_combine.for_tds, step_reg.root.w_combine.tds, m=5) # 5 fold cross-validation
cv.lm(df=data.root.w_combine.for_yds, step_reg.root.w_combine.yds, m=5) # 5 fold cross-validation