# HEADERS
# Date
# HomeTeam 
# AwayTeam
# FTHG: Full Time Home Team Goals
# FTAG: Full Time Away Team Goals
# FTR: Full Time Result (H=Home Win, D=Draw, A=Away Win)
# HTHG: Half Time Home Team Goals
# HTAG: Half Time Away Team Goals
# HTR: Half Time Result (H=Home Win, D=Draw, A=Away Win)
# HS: Home Team Shots
# AS: Away Team Shots
# HST: Home Team Shots on Target
# AST: Away Team Shots on Target
# HF: Home Team Fouls Committed
# AF: Away Team Fouls Committed
# HC: Home Team Corners
# AC: Away Team Corners
# HY: Home Team Yellow Cards
# AY: Away Team Yellow Cards
# HR: Home Team Red Cards
# AR: Away Team Red Cards

rm(list=ls())

# Following stats are computed: Mean, std. dev., variance, maximum, minimum and median. for each feature.
computeStats <- function(x){
  stats <- data.frame(matrix(ncol = 16, nrow = 6))
  int_cols <- c("FTHG","FTAG","HTHG","HTAG","HS","AS","HST","AST","HF","AF","HC","AC","HY","AY","HR","AR") 
  colnames(stats) <- int_cols
  for (col in int_cols){
    mean <- mean(x[,col])
    std <- sd(x[,col])
    var <- var(x[,col])
    max <- max(x[,col])
    min <- min(x[,col])
    median <- median(x[,col])
    stats[,col] <- c(mean,std,var,max,min,median)  
  }
  return(stats)
}

#Calculate CI from a normal distribution
getCINormal <- function(mean,std,CI,n){
  interval = CI+(1-CI)/2
  error <- qnorm(interval)*std/sqrt(n)
  left <- mean-error
  right <- mean+error
  return(c(left,right))
}

#Calculate CI from an unknown distribution using bootstrap method
getCIUnknown <- function(data){
  iterations = 40
  #generate resamples
  estimated_means <- c(1:iterations)
  sample_mean <- mean(data)
  for (it in seq(1,iterations,by=1)){
    #resample
    it_samples = sample(data,size=length(data),replace=TRUE)
    mean_it_samples = mean(it_samples)
    #resample mean. add to estimations array
    estimation <- mean_it_samples - sample_mean
    estimated_means[it] <- estimation
  }
  
  #sort in descending order. get the 0.025 and 0.975 by index
  estimated_means[order(estimated_means)]
  lower = estimated_means[1]
  upper = estimated_means[iterations]
  return (c(sample_mean-lower,sample_mean-upper))
}

#Home
#Calculate Linear regression model of Home Shoots and Home Shoots on Target
linearModelsHome <- function(data_home){
  hist(as.matrix(football_data[,'HS']),ylab="", xlab="",main="Home Shoots")
  hist(as.matrix(football_data[,'HST']),ylab="", xlab="",main="Home S. on Target")
  hist(as.matrix(football_data[,'FTHG']),ylab="", xlab="",main="FT Home Goals")
  #Home shoots as function of Home shoots on target
  linear_mod_hs_hst=lm(data_home[,"HST"] ~ data_home[,"HS"], data=data_home) # build linear regression model on full data
  plot(data_home[,"HS"],data_home[,"HST"], xlab="Home Shoots", ylab="Home S. on Target")
  print(linear_mod_hs_hst)
  summary(linear_mod_hs_hst)
  abline(linear_mod_hs_hst,lwd=3,col=2)
  
  #Calculate the predictive conversion ratio of shoots on target and goals.
  #Ful time goals as function of Home team shoots
  linear_mod_fthg_hst=lm(data_home[,"FTHG"] ~ data_home[,"HST"], data=data_home) # build linear regression model on full data
  plot(data_home[,"HST"],data_home[,"FTHG"], ylab="FT Home Goals", xlab="Home S. on Target")
  print(linear_mod_fthg_hst)
  summary(linear_mod_fthg_hst)
  abline(linear_mod_fthg_hst,lwd=3,col=2)
}

#Away part
#Calculate Linear regression model of Home Shoots and Home Shoots on Target
linearModelsAway <- function(data_away){
  
  hist(as.matrix(football_data[,'AS']),ylab="", xlab="",main="Away Shoots")
  hist(as.matrix(football_data[,'AST']),ylab="", xlab="",main="Away S. on Target")
  hist(as.matrix(football_data[,'FTAG']),ylab="", xlab="",main="FT Away Goals")
  #Home shoots as function of Home shoots on target
  linear_mod_hs_hst=lm(data_away[,"AST"] ~ data_away[,"AS"], data=data_away) # build linear regression model on full data
  plot(data_away[,"AS"],data_away[,"AST"], xlab="Away Shoots", ylab="Away S. on Target")
  print(linear_mod_hs_hst)
  summary(linear_mod_hs_hst)
  abline(linear_mod_hs_hst,lwd=3,col=2)
  
  #Ful time goals as function of Home team shoots
  linear_mod_fthg_hst=lm(data_away[,"FTAG"] ~ data_away[,"AST"], data=data_away) # build linear regression model on full data
  plot(data_away[,"AST"],data_away[,"FTAG"], ylab="FT Away Goals", xlab="Away S. on Target")
  print(linear_mod_fthg_hst)
  summary(linear_mod_fthg_hst)
  abline(linear_mod_fthg_hst,lwd=3,col=2)
}


homeStatsAnalysis <- function(data_home,stats_home){
  # Check normality of data
  shapiro.test(data_home[,"HY"])
  shapiro.test(data_home[,"HF"])
  
  ci_hy_normal <- getCINormal(stats_home[1,"HY"],stats_home[2,"HY"],.95,nrow(stats_home))
  mean_test <- stats_home[1,"HY"]
  ci_hy_unknown <- getCIUnknown(data_home[,"HY"])
  home_yf <- cbind(data_home[,"HY"],data_home[,"HF"],data_home[,"AY"],data_home[,"AF"])
  cor_home_yf <- cor(home_yf)
  returnList <- list("CINormal" = ci_hy_normal, "CIUnk" = ci_hy_unknown, "Corr" = cor_home_yf)
  return(returnList)
}

awayStatsAnalysis <- function(data_away,stats_away){
  # Check normality of data
  shapiro.test(data_away[,"AY"])
  shapiro.test(data_away[,"AF"])
  
  ci_ay_normal <- getCINormal(stats_away[1,"AY"],stats_away[2,"AY"],.95,nrow(stats_away))
  mean_test <- stats_away[1,"AY"]
  ci_ay_unknown <- getCIUnknown(data_away[,"AY"])
  away_yf <- cbind(data_away[,"HY"],data_away[,"HF"],data_away[,"AY"],data_away[,"AF"])
  cor_away_yf <- cor(away_yf)
  returnList <- list("CINormal" = ci_ay_normal, "CIUnk" = ci_ay_unknown, "Corr" = cor_away_yf)
  return(returnList)
}

#Get mean of yellow cards
getYellows <- function(teams,data){
  mean_cards <- data.frame(matrix(ncol = 0, nrow = 2)) 
  for (team in teams){
    hy <- football_data[football_data$"HomeTeam" == team,][,'HY']
    ay <- football_data[football_data$"AwayTeam" == team,][,'AY']
    mean_hy <- mean(hy)
    mean_ay <- mean(ay)
    mean_cards[,team] <- c(mean_hy,mean_ay)  
  }
  return(mean_cards)
}

#Yellow cards montecarlo
montecarlo<- function(data){
  
  mc_data <- 1 * (data[,1] == data[,2])
  hc_values <- seq(range(data[,1])[1],range(data[,1])[2],by=1)
  ac_values <- seq(range(data[,2])[1],range(data[,2])[2],by=1)
  
  home_probs <- numeric()
  away_probs <- numeric()
  
  for (home_value in hc_values){
    prob_value <- sum(data[,1] == home_value) / (nrow(data))
    home_probs <- c(home_probs, prob_value)
  }
  for(away_value in ac_values){
    prob_value <- (sum(data[,2] == away_value)-1) / (nrow(data))
    away_probs <- c(home_probs, prob_value)
  }
  mean_vector <- numeric()
  vector_samples <- c(500,1000,5000,10000)
  for (nsamples in vector_samples) {
    for (num in 1:nsamples){
      hc_sample <- sample(max(hc_values)+1, size = nsamples, replace = TRUE, prob = home_probs)
      hc_sample <- hc_sample - 1 
      ac_sample <- sample(max(ac_values)+1, size = nsamples, replace = TRUE, prob = away_probs)
      ac_sample <- ac_sample - 1 
      joined_sample <- cbind(hc_sample,ac_sample)
      num_duplicates <- 1 * (joined_sample[,1] == joined_sample[,2])
    }
    mean_duplicates <- mean(as.numeric(num_duplicates))
    mean_vector <- c(mean_duplicates, mean_vector)
  }
  return(mean(mean_vector))
}


#Montecarlo CI 
getMontecarloCI95001 <- function(mean_montecarlo){
  
  n_samples_CI95_001 <- (1.96/.005) **2*(1-mean_montecarlo)*mean_montecarlo
  error1 <- qt(0.975,df=n_samples_CI95_001)*0.001/sqrt(n_samples_CI95_001)
  upper<- mean_montecarlo + error1
  lower <- mean_montecarlo - error1
  return(c(lower,upper))
}

data <- read.csv('~/data/datasets/spanish-laliga/archive/season-1718.csv')
football_data <- subset(data,select = c(Date,HomeTeam,AwayTeam,FTHG,FTAG,FTR,HTHG,HTAG,HTR,HS,AS,HST,AST,HF,AF,HC,AC,HY,AY,HR,AR))

teams <- as.character(unique(football_data[,"HomeTeam"]))

# Eibar Analysis
eibar_home <- subset(football_data, football_data[,"HomeTeam"] == "Eibar")
eibar_away <- subset(football_data, football_data[,"AwayTeam"] == "Eibar")

stats_eibar_home <- computeStats(eibar_home)
stats_eibar_away <- computeStats(eibar_away)

listHomeStats <- homeStatsAnalysis(eibar_home,stats_eibar_home)
listAwayStats <- awayStatsAnalysis(eibar_away,stats_eibar_away)

linearModelsHome(eibar_home)
linearModelsAway(eibar_away)


#General Anaylisis
stats <- computeStats(football_data)

linearModelsAway(football_data)
linearModelsHome(football_data)

# Hypothesis tests
yellow_cards <- getYellows(teams,football_data)
yc_test_welch <- t.test(yellow_cards[1,],yellow_cards[2,], conf.level=0.99)
barplot(as.matrix(yellow_cards), beside=TRUE,ylab="Home/Away Yellow Cards",xlab="Teams")


# Montecarlo if home corners equal to away corners 1 else 0
corner_data <- subset(football_data,select=c("HC", "AC"))
mean_montecarlo <- montecarlo(corner_data)
montecarlo_CI95_001 <- getMontecarloCI95001(mean_montecarlo)









