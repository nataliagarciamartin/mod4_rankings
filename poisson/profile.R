variablepower <- function(data, power_vector){
  dev <- numeric(length(power_vector))
  data <- gnm::expandCategorical(data, "FTR", idvar = "match")
  for (i in 1:length(power_vector)){
    fit <- fitDavidson(data, coefs = F, power_vector[i])
    dev[i] <- fit$deviance
  }
  return(dev)
}


library(ggplot2)
vector<- seq(from = 0.1, to = 0.9, by = 0.01)

deviance2016 <- variablepower(season2016, vector)
dataframe2016<-data.frame(vector, deviance2016)
plot2016<- ggplot(dataframe,aes(vector,deviance2016))+geom_line()+labs(x = "Power", y="Deviance (2016)")+theme(axis.title=element_text(size=10))#+geom_point()#+geom_hline(yintercept = deviance[which.min(deviance2016)], linetype="dashed")

deviance2017 <- variablepower(season2017, vector)
dataframe2017<-data.frame(vector, deviance2017)
plot2017<- ggplot(dataframe,aes(vector,deviance2017))+geom_line()+labs(x = "Power", y="Deviance (2017)")+theme(axis.title=element_text(size=10))#+geom_point()#+geom_hline(yintercept = deviance[which.min(deviance2017)], linetype="dashed")

deviance2018 <- variablepower(season2018, vector)
dataframe2018<-data.frame(vector, deviance2018)
plot2018<- ggplot(dataframe,aes(vector,deviance2018))+geom_line()+labs(x = "Power", y="Deviance (2018)")+theme(axis.title=element_text(size=10))#+geom_point()#+geom_hline(yintercept = deviance[which.min(deviance2018)], linetype="dashed")

deviance2019 <- variablepower(season2019, vector)
dataframe2019<-data.frame(vector, deviance2019)
plot2019<- ggplot(dataframe,aes(vector,deviance2019))+geom_line()+labs(x = "Power", y="Deviance (2019)")+theme(axis.title=element_text(size=10))#+#geom_point()#+geom_hline(yintercept = deviance[which.min(deviance2019)], linetype="dashed")

require(gridExtra)
grid.arrange(plot2016, plot2017, plot2018, plot2019, ncol=4)

