variablepower <- function(data, power_vector){
  dev <- numeric(length(power_vector))
  data <- gnm::expandCategorical(data, "FTR", idvar = "match")
  for (i in 1:length(power_vector)){
    fit <- fitDavidson(data, coefs = F, power_vector[i])
    dev[i] <- fit$deviance
  }
  return(dev)
}

vector<- seq(from = 0.1, to = 0.6, by = 0.01)
deviance <- variablepower(season2018, vector)
min(deviance)
deviance[24]
vector[24]
dataframe<-data.frame(vector, deviance)
# minimum deviance at 0.33
library(ggplot2)
ggplot(dataframe,aes(vector,deviance))+geom_line()+geom_point()+geom_hline(yintercept = deviance[24], linetype="dashed")+ylim(691,700)

###########################################

season.data<-expanded2018
season.data$X <- makeX(season.data, power=1/3)
nteams <- ncol(season.data$X)
season.data$home <- as.numeric(season.data$FTR == "H")
season.data$draw <- as.numeric(season.data$FTR == "D")
thefit <- gnm(count ~ -1 + X + home + draw,
              eliminate = match,
              family = poisson,
              data = season.data, constrain = "West Ham")
prof<-profile(thefit)
plot(prof)
confint(prof)

