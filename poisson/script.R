library(gnm)

season2016 <- read.csv("data/pl_results_15_16.csv")
season2017 <- read.csv("data/pl_results_16_17.csv")
season2018 <- read.csv("data/pl_results_17_18.csv")
season2019 <- read.csv("data/pl_results_18_19.csv")

season2016 <- subset(season2016, select = c("HomeTeam", "AwayTeam", "FTR"))
season2017 <- subset(season2017, select = c("HomeTeam", "AwayTeam", "FTR"))
season2018 <- subset(season2018, select = c("HomeTeam", "AwayTeam", "FTR"))
season2019 <- subset(season2019, select = c("HomeTeam", "AwayTeam", "FTR"))

expanded2019 <- gnm::expandCategorical(season2019, "FTR", idvar = "match")

## See the overview document for `gnm`, for the details of how to use
## a Poisson log-linear model to get the MLE for a multinomial response model
## (here, multinomial with 3 outcome categories)
## -- the so-called "Poisson trick".

Davidson2019 <- fitDavidson(expanded2019, coefs = TRUE)
Davidson2019

Davidson2019_noadv <- fitDavidson_noadvantage(expanded2019, coefs = TRUE)
Davidson2019_noadv
