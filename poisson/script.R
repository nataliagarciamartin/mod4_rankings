library(gnm)

season2015 <- read.csv("data/pl_results_15_16.csv")
season2016 <- read.csv("data/pl_results_16_17.csv")
season2017 <- read.csv("data/pl_results_17_18.csv")
season2018 <- read.csv("data/pl_results_18_19.csv")

season2015 <- subset(season2015, select = c("HomeTeam", "AwayTeam", "FTR"))
season2016 <- subset(season2015, select = c("HomeTeam", "AwayTeam", "FTR"))
season2017 <- subset(season2015, select = c("HomeTeam", "AwayTeam", "FTR"))
season2018 <- subset(season2015, select = c("HomeTeam", "AwayTeam", "FTR"))

expanded2018 <- gnm::expandCategorical(season2018, "FTR", idvar = "match")

## See the overview document for `gnm`, for the details of how to use
## a Poisson log-linear model to get the MLE for a multinomial response model
## (here, multinomial with 3 outcome categories)
## -- the so-called "Poisson trick".

Davidson2018 <- fitDavidson(expanded2018, coefs = TRUE)
Davidson2018


