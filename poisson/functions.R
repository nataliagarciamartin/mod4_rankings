makeX <- function(season.data, power = 1/3){
  X <- matrix(0,
              nrow(season.data),
              length(levels(factor(as.character(season.data$AwayTeam)))))
  colnames(X) <- levels(factor(as.character(season.data$AwayTeam)))
  for (team in colnames(X)) {
    X[season.data$HomeTeam == team & season.data$FTR == "H", team] <- 1
    X[season.data$HomeTeam == team & season.data$FTR == "D", team] <- power
    X[season.data$AwayTeam == team & season.data$FTR == "A", team] <- 1
    X[season.data$AwayTeam == team & season.data$FTR == "D", team] <- power
  }
  return(X)
}

fitDavidson <- function(season.data, coefs = TRUE){
  season.data$X <- makeX(season.data, power = 1/3)
  nteams <- ncol(season.data$X)
  season.data$home <- as.numeric(season.data$FTR == "H")
  season.data$draw <- as.numeric(season.data$FTR == "D")
  
  thefit <- gnm(count ~ -1 + X + home + draw,
                eliminate = match,
                family = poisson,
                data = season.data, constrain = "West Ham", constrainTo = 1)
  
  thecoefs <- coef(thefit)
  names(thecoefs)[1:nteams] <- colnames(season.data$X)
  home <- thecoefs[nteams + 1]
  draw <- thecoefs[nteams + 2]
  abilities <- rev(sort(thecoefs[1:nteams]))
  abilities <- abilities - mean(abilities)
  if (coefs) return(c(abilities, home, draw))   ## all on the log scale
  else return(thefit)
}

fitDavidson_noadvantage <- function(season.data, coefs = TRUE){
  season.data$X <- makeX(season.data, power = 1/3)
  nteams <- ncol(season.data$X)
  season.data$home <- as.numeric(season.data$FTR == "H")
  season.data$draw <- as.numeric(season.data$FTR == "D")
  
  thefit <- gnm(count ~ -1 + X + draw,
                eliminate = match,
                family = poisson,
                data = season.data, constrain = "West Ham", constrainTo = 1)
  
  thecoefs <- coef(thefit)
  names(thecoefs)[1:nteams] <- colnames(season.data$X)
  draw <- thecoefs[nteams + 1]
  abilities <- rev(sort(thecoefs[1:nteams]))
  abilities <- abilities - mean(abilities)
  if (coefs) return(c(abilities, draw))   ## all on the log scale
  else return(thefit)
}

