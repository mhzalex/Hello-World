setwd("D:/Downloads/ISU-MBA/MKT 445- Marketing Analytics/Group Project/data.csv")
rm(list = ls())

library(plyr)
library(dplyr)
#install.packages("MASS")
#install.packages("DAAG")
#install.packages("caret")
#install.packages("faraway")
library(MASS)
library(faraway)
library(caret)
library(sqldf)
library(car)
library(ggplot2)
library(corrplot)
View(old.fifa)



old.fifa <- read.csv("data.csv", stringsAsFactors = FALSE)

# Removing unnecessary columns like goal keeper skills and others.
old.fifa <- old.fifa[,-c(1,5,7,11,21,23,24,25,26,29:54,84:89)]

# Loading data set data frame
fifa <- as.data.frame(old.fifa)

# Removing "Euro" value to set column as numerical.
fifa$Value <- as.character(fifa$Value)
fifa$Value <- substr(fifa$Value, 4, 9)
fifa$Wage <- as.character(fifa$Wage)
fifa$Wage <- substr(fifa$Wage,4, 15)

# Removing text from wage and making it a numeric column.
fifa$Value <- gsub("M","",fifa$Value)
fifa$Wage <- gsub("K","",fifa$Wage) #%>% as.integer(fifa$Wage)
fifa$Wage <-  as.numeric(fifa$Wage)


# Removing all goal keepers from dataset
fifa <- sqldf(' select * from fifa where Position != "GK"')
#View(fifa)

# Converting empty wage to NA.
fifa$Wage[fifa$Wage == 0] <- NA

# Imputing wage with median
wage.median <- median(fifa$Wage, na.rm = TRUE)
fifa[is.na(fifa$Wage),"Wage"] <- wage.median
fifa$Wage <- fifa$Wage * 1000

# Selecting only attributes of players
fifa.summary <- as.matrix.data.frame(summary(fifa[,(20:48)]))
colnames(fifa.summary) <- colnames(fifa[,(20:48)])
fifa.summary <- as_tibble(fifa.summary)

# Creating a fifa sumary table to get descriptive stats of columns.
fifa.summary[] <- lapply(fifa.summary, gsub, pattern = "Min.   :" , replacement = "", fixed = TRUE)
fifa.summary[] <- lapply(fifa.summary, gsub, pattern = "1st Qu.:" , replacement = "", fixed = TRUE)
fifa.summary[] <- lapply(fifa.summary, gsub, pattern = "Median :" , replacement = "", fixed = TRUE)
fifa.summary[] <- lapply(fifa.summary, gsub, pattern = "Mean "    , replacement = "", fixed = TRUE)
fifa.summary[] <- lapply(fifa.summary, gsub, pattern = "3rd Qu.:" , replacement = "", fixed = TRUE)
fifa.summary[] <- lapply(fifa.summary, gsub, pattern = "Max. "    , replacement = "", fixed = TRUE)
fifa.summary[] <- lapply(fifa.summary, gsub, pattern = "NA's "    , replacement = "", fixed = TRUE)
fifa.summary[] <- lapply(fifa.summary, gsub, pattern = ":"        , replacement = "", fixed = TRUE)
fifa.summary[] <- lapply(fifa.summary, as.numeric)

# Creating a new summary table to view it better.
fifa.summary2 <- t(fifa.summary)
colnames(fifa.summary2) <-  c("Min","Q1","Median","Mean","Q3","Max","NA")
fifa.summary2 <- as.data.frame(fifa.summary2)

# Imputing all player attributes with mean.
fifa[is.na(fifa$Crossing),"Crossing"]                <- fifa.summary2[1,4]
fifa[is.na(fifa$Finishing),"Finishing"]              <- fifa.summary2[2,4]
fifa[is.na(fifa$HeadingAccuracy),"HeadingAccuracy"]  <- fifa.summary2[3,4]
fifa[is.na(fifa$ShortPassing),"ShortPassing"]        <- fifa.summary2[4,4]
fifa[is.na(fifa$Volleys),"Volleys"]                  <- fifa.summary2[5,4]
fifa[is.na(fifa$Dribbling),"Dribbling"]              <- fifa.summary2[6,4]
fifa[is.na(fifa$Curve),"Curve"]                      <- fifa.summary2[7,4]
fifa[is.na(fifa$FKAccuracy),"FKAccuracy"]            <- fifa.summary2[8,4]
fifa[is.na(fifa$LongPassing),"LongPassing"]          <- fifa.summary2[9,4]
fifa[is.na(fifa$BallControl),"BallControl"]          <- fifa.summary2[10,4]
fifa[is.na(fifa$Acceleration),"Acceleration"]        <- fifa.summary2[11,4]
fifa[is.na(fifa$SprintSpeed),"SprintSpeed"]          <- fifa.summary2[12,4]
fifa[is.na(fifa$Agility),"Agility"]                  <- fifa.summary2[13,4]
fifa[is.na(fifa$Reactions),"Reactions"]              <- fifa.summary2[14,4]
fifa[is.na(fifa$Balance),"Balance"]                  <- fifa.summary2[15,4]
fifa[is.na(fifa$ShotPower),"ShotPower"]              <- fifa.summary2[16,4]
fifa[is.na(fifa$Jumping),"Jumping"]                  <- fifa.summary2[17,4]
fifa[is.na(fifa$Stamina),"Stamina"]                  <- fifa.summary2[18,4]
fifa[is.na(fifa$Strength),"Strength"]                <- fifa.summary2[19,4]
fifa[is.na(fifa$LongShots),"LongShots"]              <- fifa.summary2[20,4]
fifa[is.na(fifa$Aggression),"Aggression"]            <- fifa.summary2[21,4]
fifa[is.na(fifa$Interceptions),"Interceptions"]      <- fifa.summary2[22,4]
fifa[is.na(fifa$Positioning),"Positioning"]          <- fifa.summary2[23,4]
fifa[is.na(fifa$Vision),"Vision"]                    <- fifa.summary2[24,4]
fifa[is.na(fifa$Penalties),"Penalties"]              <- fifa.summary2[25,4]
fifa[is.na(fifa$Composure),"Composure"]              <- fifa.summary2[26,4]
fifa[is.na(fifa$Marking),"Marking"]                  <- fifa.summary2[27,4]
fifa[is.na(fifa$StandingTackle),"StandingTackle"]    <- fifa.summary2[28,4]
fifa[is.na(fifa$SlidingTackle),"SlidingTackle"]      <- fifa.summary2[29,4]


summary(fifa$Wage)
# Plotting data
qplot(ShortPassing, log(Wage) ,data= fifa, geom = c("point","smooth"), 
      xlab = "Short Passing",
      ylab = " Log of Wage",
      main = " Scatter Plot of Short Passing against Log of Wage")



par(mfrow=c(1,2))
# Checking histogram of wage.
hist(fifa$Wage, breaks = 20, col = "blue",
     xlab = "Wage",
     ylab = "Frequency",
     main = " Histogram of Wage")

# Taking log of wage to make the distribution normal.
fifa$Salary.log <- log(fifa$Wage)
hist(fifa$Salary.log, breaks = 20, col = "red", freq = FALSE,
     xlab = "Log of Wage",
     ylab = " Frequency",
     main = " Histogram of Log of Wage")
lines(density(fifa$Salary.log, bw = .7), lwd = 2, col="blue")

# Creating a separate data frame with only the dependent and independent variables.
fifa.pairs <- fifa[,c(9,seq(20,49,1))]


# Creating a scatter plot. Do not run this code.
scatterplotMatrix(fifa.pairs)

par(mfrow=c(1,1))
# Checking correlation
fifa.cor <- as.matrix(round(cor(fifa.pairs),2))
colnames(fifa.cor)
fifa.p.mat <- cor.mtest(fifa.pairs)
fifa.p.mat$p
fifa.cor.value <- cbind(fifa.p.mat$p[,31], fifa.cor[,31])
colnames(fifa.p.mat$p) <- colnames(fifa.cor)
fifa.p.value <- as.matrix(fifa.p.mat$p[31,])
corrplot(cor=fifa.cor, method = "ellipse", type = "upper")
fifa.cor[fifa.cor > 0.7]

# Creating equation for model
fifa.equation <- Salary.log ~ Crossing + Finishing  + 
  HeadingAccuracy + 
  ShortPassing + Volleys + 
  Dribbling + Curve + 
  FKAccuracy + LongPassing + 
  BallControl + Acceleration + 
  SprintSpeed + Agility + 
  Reactions + Balance + 
  ShotPower + Jumping + 
  Stamina + Strength + 
  LongShots + Aggression + 
  Interceptions + Positioning + 
  Vision + Penalties + 
  Composure + Marking + 
  StandingTackle + SlidingTackle

# Model one with all variables.
fifa.model <- lm(fifa.equation, fifa)
summary(fifa.model)

library(car)
# Checking multicollinearity
library(faraway)

vif <- faraway::vif(fifa.pairs)
vif[vif<4]

multi.cor <-  vif(fifa.pairs)


sort(multi.cor[multi.cor > 4])
fifa.lowvif <- as.matrix(sort(multi.cor[multi.cor < 4]))
multi.cor[sqrt(multi.cor)>2]

# Equation for second model
fifa.equation2 <- Salary.log ~ Crossing + Finishing  + 
  HeadingAccuracy + 
  ShortPassing + Volleys + 
  Dribbling + Curve + 
  FKAccuracy + LongPassing + 
  BallControl + Acceleration + 
  SprintSpeed + Agility + 
  Reactions + Balance + 
  ShotPower + Jumping + 
  Stamina + Strength + 
  LongShots + Aggression + 
  Interceptions + Positioning + 
  Vision + Penalties + 
  Composure + Marking + 
  StandingTackle 

# Model with one less variable.
fifa.model2 <- lm(fifa.equation2, fifa)
summary(fifa.model2)

# Equation for model 3.
fifa.equation3 <- Salary.log ~  Reactions + Balance + 
   Jumping + 
  Stamina + Strength + 
 Aggression + 
  Composure + HeadingAccuracy + Penalties +
  Crossing + FKAccuracy + Agility + ShotPower

# Model with all variables except variables with vif greater than 4.
fifa.model3 <- lm(fifa.equation3, fifa)
summary(fifa.model3)


# Model four. Step-wise regression.
fifa.step.model <- stepAIC(fifa.model, data = fifa, direction = "both")#, trace = FALSE)
summary(fifa.step.model)


# Segmenting players on the basis of wage.
fifa.low <- sqldf('select * from fifa where Wage <= 3000') # Upto quartile one
fifa.medium <- sqldf('select * from fifa where Wage <= 9000 and Wage > 3000') # From quartile 1 to 3.
fifa.high <- sqldf('select * from fifa where wage >9000')# Quartile 3 onwards.


# Model for low income players
fifa.low.model <- lm(fifa.equation3,fifa.low)
fifa.low.step.model <- stepAIC(fifa.low.model, data = fifa.low, direction = "both", trace = FALSE)
summary(fifa.low.step.model)

# Model for medium income players
fifa.medium.model <- lm(fifa.equation3,fifa.medium)
fifa.medium.step.model <- stepAIC(fifa.medium.model, data = fifa.medium, direction = "both", trace = FALSE)
summary(fifa.medium.step.model)

# Model for high income players
fifa.high.model <- lm(fifa.equation3, fifa.high)
fifa.high.step.model <- stepAIC(fifa.high.model, data = fifa.high, direction = "both", trace = FALSE)
summary(fifa.high.step.model)

# Predicting the wage of chelsea
Chelsea <- sqldf('select Name from fifa where Club = "Chelsea" ')

Chelsea.players <- sqldf(' select * from Chelsea where
                         Name in ("E. Hazard" , "N. Kanté" , "Morata"
                         , "Pedro" , "G. Cahill" , "Marcos Alonso"
                          , "Azpilicueta" , "Jorginho" , "A. Rüdiger"
                          , "R. Barkley")
                         ')

#Actual Salary
input <- sqldf('select Name, Wage from fifa where Name in ("E. Hazard", "N. Kanté","Azpilicueta","Jorginho", "Marcos Alonso", "A. Rüdiger","Morata", "Pedro","G. Cahill","R. Barkley")')
#predicted Salary
chelsea_currentplayer<- sqldf('select * from fifa where Name in ("E. Hazard", "N. Kanté","Azpilicueta","Jorginho", "Marcos Alonso", "A. Rüdiger","Morata", "Pedro","G. Cahill","R. Barkley")')
predict(fifa.model3,chelsea_currentplayer)
chelsea_currentplayer
output <- exp(predict(fifa.model3,chelsea_currentplayer))

result <- cbind.data.frame(input,output)


sqldf('select Name from fifa where Reactions between 85 and 95
      and Balance between 84 and 94
      or Jumping between  46 and 66 and Stamina between 77 and 94 
      and Strength between 75 and 85
      and Aggression between 49 and 54 and  Composure between 80 and 95')

# Code to sort out players

# FOR HAZARD
sqldf('select * from fifa where Name = "E. Hazard"')
possible_list<- sqldf('select Name, Potential, Wage, Position from fifa 
where (Reactions between 85 and 90 or  Balance between 89 and 94 or Jumping between 46 and 56 or Stamina between 73 and 83 or Strength between 56 and 66 or Aggression between 44 and 54 or Composure between 81 and 91) and position
                      in ("Lw","RW", "CAM") and Potential >=87')
summary(possible_list)
subs_for_eden <-filter(possible_list,Wage< 340000)
summary(subs_for_eden)
Hazard <- head(arrange(subs_for_eden, desc(Potential)))

#For Kante
sqldf('select * from fifa where Name = "N. Kanté"')
possible_list_kante<- sqldf('select Name, Potential, Wage, Position from fifa where (Reactions between 88 and 93 or  Balance between 87 and 92 or Jumping between 72 and 77 or Stamina between 91 and 96 or Strength between 71 and 76 or Aggression between 85 and 90 or Composure between 80 and 85) and position
                            in ("LDM","RDM", "CDM") and Potential >=85')
head(possible_list_kante)
sqldf('select age from fifa where Name = "N. Kanté"')
subs_for_Kante <-filter(possible_list_kante,Wage< 225000)
summary(subs_for_Kante)
Kante <- arrange(subs_for_Kante, desc(Potential))

#For Morata
sqldf('select * from fifa where Club = "Chelsea" and Name = "Morata" ')
possible_list_Morata<- sqldf('select Name, Potential, Wage, Position from fifa where (Reactions between 74 and 79 or  Balance between 55 and 60 or Jumping between 79 and 84 or Stamina between 63 and 68 or Strength between 71 and 78 or Aggression between 51 and 56 or Composure between 71 and 76) and position
                             in ("ST","LF", "RF") and Potential >=80')
summary(possible_list_Morata)
subs_for_Morata <-filter(possible_list_Morata,Wage< 135000)
summary(subs_for_Morata)
Morate <- head(arrange(subs_for_Morata, desc(Potential)))

# FOR Pedro
sqldf('select * from fifa where Name = "Pedro"')
possible_list_Pedro<- sqldf('select Name, Potential, Wage, Position from fifa where (Reactions between 76 and 81 or  Balance between 75 and 80 or Jumping between 61 and 66 or Stamina between 65 and 70 or Strength between 42 and 47 or Aggression between 51 and 56 or Composure between 72 and 77) and position
                            in ("Lw","RW", "CAM") and Potential >=77')
summary(possible_list_Pedro)
subs_for_Pedro <-filter(possible_list_Pedro,Wage< 140000)
summary(subs_for_Pedro)

Pedro <- head(arrange(subs_for_Pedro, desc(Potential)))

# For G. Cahill
sqldf('select * from fifa where Name = "G. Cahill"')
possible_list_Cahill<- sqldf('select Name, Potential, Wage, Position from fifa where (Reactions between 76 and 81 or  Balance between 42 and 47 or Jumping between 76 and 81 or Stamina between 75 and 80 or Strength between 56 and 61 or Aggression between 51 and 56 or Composure between 72 and 77) and position
                             in ("CB") and Potential > 80')
summary(possible_list_Cahill)
subs_for_Cahill <-filter(possible_list_Cahill,Wage< 99000)

Cahill <- head(arrange(subs_for_Cahill, desc(Potential)))

# For alonso
sqldf('select * from fifa where Club="Chelsea" and Position ="LB"')
possible_list_Alonso<- sqldf('select Name, Potential, Wage, Position from fifa where (Reactions between 78 and 84 or  Balance between 53 and 58 or Jumping between 66 and 71 or Stamina between 85 and 90 or Strength between 71 and 76 or Aggression between 70 and 75 or Composure between 72 and 77) and position
                             in ("LB") and Potential >=78')
summary(possible_list_Alonso)
subs_for_Alonso <-filter(possible_list_Alonso,Wage< 130000)
summary(subs_for_Alonso)

Alonso <- head(arrange(subs_for_Alonso, desc(Potential)))

# For Azpi
sqldf('select * from fifa where Club="Chelsea" and Position ="RB"')
possible_list_Azpi<- sqldf('select Name, Potential, Wage, Position from fifa where (Reactions between 83 and 88 or  Balance between 68 and 73 or Jumping between 71 and 76 or Stamina between 85 and 90 or Strength between 71 and 76 or Aggression between 76 and 82 or Composure between 72 and 78) and position
                           in ("RB") and Potential >86')
summary(possible_list_Azpi)
subs_for_Azpi <-filter(possible_list_Azpi,Wage< 175000)
summary(subs_for_Azpi)

Azpi <- head(arrange(subs_for_Azpi, desc(Potential)))

# For Rudiger
sqldf('select * from fifa where Club="Chelsea" and Position ="CB"')
possible_list_Rudi<- sqldf('select Name, Potential, Wage, Position from fifa where (Reactions between 78 and 84 or  Balance between 48 and 53 or Jumping between 68 and 73 or Stamina between 55 and 60 or Strength between 76 and 82 or Aggression between 83 and 88 or Composure between 72 and 77) and position
                           in ("CB") and Potential >=84')
summary(possible_list_Rudi)
subs_for_Rudi <-filter(possible_list_Rudi,Wage< 110000)
summary(subs_for_Rudi)

Rudiger <- head(arrange(subs_for_Rudi, desc(Potential)))

# For Jorginho
sqldf('select * from fifa where Club="Chelsea" and Position in (" RCM", "LCM","CM")')
possible_list_Jorginho <- sqldf('select Name, Potential, Wage, Position from fifa where (Reactions between 78 and 84 or  Balance between 68 and 73 or Jumping between 53 and 58 or Stamina between 74 and 79 or Strength between 61 and 66 or Aggression between 74 and 79 or Composure between 72 and 79) and position
                                in ("CM") and Potential >87')
summary(possible_list_Jorginho)
subs_for_Jorginho <-filter(possible_list_Jorginho,Wage< 130000)
summary(subs_for_Jorginho)

Jorginho <- head(arrange(subs_for_Jorginho, desc(Potential)))

#FOr Barkley
sqldf('select * from fifa where Club="Chelsea" and Position = "CAM"')
possible_list_Barkley <- sqldf('select Name, Potential, Wage, Position from fifa where (Reactions between 68 and 74 or  Balance between 53 and 58 or Jumping between 57 and 62 or Stamina between 63 and 69 or Strength between 73 and 77 or Aggression between 69 and 74 or Composure between 75 and 80) and position
                               in ("CAM") and Potential >82')
summary(possible_list_Barkley)
subs_for_Barkley <-filter(possible_list_Barkley,Wage< 89000)
Barkley <- head(arrange(subs_for_Barkley, desc(Potential)))




# Plot betwen wage and variables
qplot(ShortPassing, log(Wage) ,data= fifa, geom = c("point","smooth"), 
      xlab = "Short Passing",xlim=c(20,80),
      ylab = " Log of Wage",
      main = " Scatter Plot of Short Passing against Log of Wage")
qplot(Crossing, log(Wage) ,data= fifa, geom = c("point","smooth"), 
      xlab = "Crossing",
      ylab = " Log of Wage",
      main = " Scatter Plot of crossing against Log of Wage")
qplot(Finishing, log(Wage) ,data= fifa, geom = c("point","smooth"), 
      xlab = "Finishing",
      ylab = " Log of Wage",
      main = " Scatter Plot of Finishing against Log of Wage")
qplot(HeadingAccuracy, log(Wage) ,data= fifa, geom = c("point","smooth"), 
      xlab = "Head Accuracy",
      ylab = " Log of Wage",
      main = " Scatter Plot of heading Acuuracy against Log of Wage")
qplot(Volleys, log(Wage) ,data= fifa, geom = c("point","smooth"), 
      xlab = "Volleys",
      ylab = " Log of Wage",
      main = " Scatter Plot of volleys against Log of Wage")
qplot(Dribbling, log(Wage) ,data= fifa, geom = c("point","smooth"), 
      xlab = "Dribbling",
      ylab = " Log of Wage",
      main = " Scatter Plot of Short Passing against Log of Wage")
qplot(Curve, log(Wage) ,data= fifa, geom = c("point","smooth"), 
      xlab = "Curve",
      ylab = " Log of Wage",
      main = " Scatter Plot of Short Passing against Log of Wage")
qplot(FKAccuracy, log(Wage) ,data= fifa, geom = c("point","smooth"), 
      xlab = "FKAccuracy",
      ylab = " Log of Wage",
      main = " Scatter Plot of Short Passing against Log of Wage")
qplot(LongPassing, log(Wage) ,data= fifa, geom = c("point","smooth"), 
      xlab = "long Passing",
      ylab = " Log of Wage",
      main = " Scatter Plot of Long Passing against Log of Wage")
qplot(BallControl, log(Wage) ,data= fifa, geom = c("point","smooth"), 
      xlab = "Ball control",
      ylab = " Log of Wage",
      main = " Scatter Plot of Ball control against Log of Wage")
qplot(Acceleration, log(Wage) ,data= fifa, geom = c("point","smooth"), 
      xlab = "Acceleration",
      ylab = " Log of Wage",
      main = " Scatter Plot of Short Passing against Log of Wage")
qplot(SprintSpeed, log(Wage) ,data= fifa, geom = c("point","smooth"), 
      xlab = "SprintSpeed",
      ylab = " Log of Wage",
      main = " Scatter Plot of Sprint Speed against Log of Wage")
qplot(Agility, log(Wage) ,data= fifa, geom = c("point","smooth"), 
      xlab = "Agility",
      ylab = " Log of Wage",
      main = " Scatter Plot of Agility against Log of Wage")
qplot(Reactions, log(Wage) ,data= fifa, geom = c("point","smooth"), 
      xlab = "Reactions",
      ylab = " Log of Wage",
      main = " Scatter Plot of Reactions against Log of Wage")
qplot(Balance, log(Wage) ,data= fifa, geom = c("point","smooth"), 
      xlab = "Balance",
      ylab = " Log of Wage",
      main = " Scatter Plot of Balance against Log of Wage")
qplot(ShotPower, log(Wage) ,data= fifa, geom = c("point","smooth"), 
      xlab = "ShotPower",
      ylab = " Log of Wage",
      main = " Scatter Plot of Shot Power against Log of Wage")
qplot(Jumping, log(Wage) ,data= fifa, geom = c("point","smooth"), 
      xlab = "Jumping",
      ylab = " Log of Wage",
      main = " Scatter Plot of Jumping against Log of Wage")
qplot(Stamina, log(Wage) ,data= fifa, geom = c("point","smooth"), 
      xlab = "Stamina",
      ylab = " Log of Wage",
      main = " Scatter Plot of Stamina against Log of Wage")
qplot(LongShots, log(Wage) ,data= fifa, geom = c("point","smooth"), 
      xlab = "LongShots",
      ylab = " Log of Wage",
      main = " Scatter Plot of longshots against Log of Wage")
qplot(Aggression, log(Wage) ,data= fifa, geom = c("point","smooth"), 
      xlab = "Aggression",
      ylab = " Log of Wage",
      main = " Scatter Plot of Aggression against Log of Wage")
qplot(Interceptions, log(Wage) ,data= fifa, geom = c("point","smooth"), 
      xlab = "Interceptions",ylab = " Log of Wage", main = " Scatter Plot of interceptions against Log of Wage")
qplot(Positioning, log(Wage) ,data= fifa, geom = c("point","smooth"), 
      xlab = "positioning",
      ylab = " Log of Wage",
      main = " Scatter Plot of positioning against Log of Wage")
qplot(Vision, log(Wage) ,data= fifa, geom = c("point","smooth"),xlab = "Vision",
      ylab = " Log of Wage",
      main = " Scatter Plot of vision against Log of Wage")
qplot(Penalties, log(Wage) ,data= fifa, geom = c("point","smooth"), 
      xlab = "Penalties",
      ylab = " Log of Wage",
      main = " Scatter Plot of penalties against Log of Wage")
qplot(Composure, log(Wage) ,data= fifa, geom = c("point","smooth"), 
      xlab = "Composure",
      ylab = " Log of Wage",
      main = " Scatter Plot of Composure against Log of Wage")
qplot(Marking, log(Wage) ,data= fifa, geom = c("point","smooth"), 
      xlab = "Marking",
      ylab = " Log of Wage",
      main = " Scatter Plot of Marking against Log of Wage")
qplot(StandingTackle, log(Wage) ,data= fifa, geom = c("point","smooth"), 
      xlab = "StandingTackle",
      ylab = " Log of Wage",
      main = " Scatter Plot of Standing Tackle against Log of Wage")
qplot(SlidingTackle, log(Wage) ,data= fifa, geom = c("point","smooth"), 
      xlab = "SlidingTackle",
      ylab = " Log of Wage",
      main = " Scatter Plot of Sliding Tackle against Log of Wage")
