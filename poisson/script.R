library(gnm)

season2016 <- read.csv("data/pl_results_15_16.csv")
season2017 <- read.csv("data/pl_results_16_17.csv")
season2018 <- read.csv("data/pl_results_17_18.csv")
season2019 <- read.csv("data/pl_results_18_19.csv")

season2016 <- subset(season2016, select = c("HomeTeam", "AwayTeam", "FTR"))
season2017 <- subset(season2017, select = c("HomeTeam", "AwayTeam", "FTR"))
season2018 <- subset(season2018, select = c("HomeTeam", "AwayTeam", "FTR"))
season2019 <- subset(season2019, select = c("HomeTeam", "AwayTeam", "FTR"))

expanded2016 <- gnm::expandCategorical(season2016, "FTR", idvar = "match")
expanded2017 <- gnm::expandCategorical(season2017, "FTR", idvar = "match")
expanded2018 <- gnm::expandCategorical(season2018, "FTR", idvar = "match")
expanded2019 <- gnm::expandCategorical(season2019, "FTR", idvar = "match")

Davidson2016 <- fitDavidson(expanded2016, coefs = TRUE)
Davidson2016
Davidson2016_noadv <- fitDavidson_noadvantage(expanded2016, coefs = TRUE)
Davidson2016_noadv

Davidson2017 <- fitDavidson(expanded2017, coefs = TRUE)
Davidson2017
Davidson2017_noadv <- fitDavidson_noadvantage(expanded2017, coefs = TRUE)
Davidson2017_noadv

Davidson2018 <- fitDavidson(expanded2018, coefs = TRUE)
Davidson2018
Davidson2018_noadv <- fitDavidson_noadvantage(expanded2018, coefs = TRUE)
Davidson2018_noadv

Davidson2019 <- fitDavidson(expanded2019, coefs = TRUE)
Davidson2019
Davidson2019_noadv <- fitDavidson_noadvantage(expanded2019, coefs = TRUE)
Davidson2019_noadv

dim(season2016[season2016$FTR=="D",])[1]/380
dim(season2016[season2016$FTR=="A",])[1]/380
dim(season2016[season2016$FTR=="H",])[1]/380
Davidson2016[20] # home
Davidson2016[21] # draw


dim(season2017[season2017$FTR=="D",])[1]/380
dim(season2017[season2017$FTR=="A",])[1]/380
dim(season2017[season2017$FTR=="H",])[1]/380
Davidson2017[20] # home
Davidson2017[21] # draw

dim(season2018[season2018$FTR=="D",])[1]/380
dim(season2018[season2018$FTR=="A",])[1]/380
dim(season2018[season2018$FTR=="H",])[1]/380
Davidson2018[20] # home
Davidson2018[21] # draw

dim(season2019[season2019$FTR=="D",])[1]/120
dim(season2019[season2019$FTR=="A",])[1]/120
dim(season2019[season2019$FTR=="H",])[1]/120
Davidson2019[20] # home
Davidson2019[21] # draw
