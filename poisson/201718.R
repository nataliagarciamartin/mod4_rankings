season2018 <- read.csv("data/pl_results_17_18.csv")
season2018 <- subset(season2018, select = c("HomeTeam", "AwayTeam", "FTR"))

# First 100 matches

season2018_100 <- season2018[1:100,]
expanded2018_100 <- gnm::expandCategorical(season2018_100, "FTR", idvar = "match")
Davidson2018_100 <- fitDavidson(expanded2018_100, coefs = TRUE)
Davidson2018_100

# First 200

season2018_200 <- season2018[1:200,]
expanded2018_200 <- gnm::expandCategorical(season2018_200, "FTR", idvar = "match")
Davidson2018_200 <- fitDavidson(expanded2018_200, coefs = TRUE)
Davidson2018_200

# First 300

season2018_300 <- season2018[1:300,]
expanded2018_300 <- gnm::expandCategorical(season2018_300, "FTR", idvar = "match")
Davidson2018_300 <- fitDavidson(expanded2018_300, coefs = TRUE)
Davidson2018_300

# All data

Davidson2018 <- fitDavidson(expanded2018, coefs = TRUE)
Davidson2018
