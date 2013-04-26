data = read.csv("F:/DataMining/project/qbStatsSubset.csv", header=TRUE)
library(plyr)
require(ggplot2)

# get rid of NAs
data = na.omit(data)
# avergage the predictor (height) value per outcome (wins)
mHeight <- tapply(data$height,data$wins,mean)
# get all the outcome values to match against predictor in new dataframe
wins <- as.factor(data$wins)
# make sure the things are in dataframe format
wins.df <- as.data.frame(as.numeric(levels(wins)))
avgH.df <- as.data.frame(mHeight)
# combine the predictor and outcome into the same dataframe
df.both <- wins.df
df.both[1]
df.both[2] <- avgH.df
df.both[2]
# rename the columns to something easier to deal with and meaningful
df <- rename(df.both, c("as.numeric(levels(wins))"="wins","mHeight"="avgHeight"))

# create the model and plot
lm.avg <- lm(as.numeric(df$wins) ~ as.numeric(df$avgHeight), data=df)
ggplot(df, aes(x=avgHeight , y=wins)) + theme_bw() + geom_point() + geom_abline(intercept=lm.avg$coef[1], slope=lm.avg$coef[2], colour="red") + xlab("height") + ylab("wins")

summary(lm.avg)

# create the model and plot, switching the axis, just to look at it differently
lm.avg <- lm(as.numeric(df$avgHeight) ~ as.numeric(df$wins), data=df)
ggplot(df, aes(x=wins, y=avgHeight)) + theme_bw() + geom_point() + geom_abline(intercept=lm.avg$coef[1], slope=lm.avg$coef[2], colour="red") + xlab("wins") + ylab("height")

summary(lm.avg)
# backup the original data frame to experiment with excluding some points that look like outliers
# df.bak <- df
# df <- df[-1,]
# put it back if you want.
# df <- df.bak

# useful for printing out everything you tried
# history(Inf)

