#First, pull in the customer data we purchased from Trip Advisor and married with our CRM system data
setwd(your directory)
Trip_Data<-read.csv(file="Session 6 Data.csv", header = TRUE,sep=",")

#Probit model
Probit_BW<-glm(Chose.BW~Score+Reviews+Price+Position,family=binomial(link='probit'),data=Trip_Data)
summary(Probit_BW)

#Need to correct for selection bias in the Charges model. Create the IMR for selection bias problems
library(sampleSelection)
Trip_Data$IMR<-invMillsRatio(Probit_BW)$IMR1

#Estimate the Charges model with the addition of the selection bias correction
Charges_Bias_lm<-lm(Charge.Per.Night~Credit+Business+IMR,data=subset(Trip_Data,Chose.BW==1))
summary(Charges_Bias_lm)

#Now calculate CLV of our Customers
alpha<-.7 #retention
OTA_Fee<-.3
Price_BW<-231 #whatever price seen on TripAdvisor
Margin<-.5
Discount<-.1
Margin_Multiplier<-alpha/(1+Discount-alpha)
Program_Cost<-5000000

#Should BW pay Trip Advisor to put BW in the top OTA position 
#to capture more of the market? This is an analysis on the change in expected
#CLV from those currently not customers of BW.
Trip_DataNew<-Trip_Data
Trip_DataNew$Position<-1

#Calculate the new probability of being acquired
#First, the X*Beta
Trip_DataNew$XB<-as.matrix(cbind(1,Trip_DataNew$Score,Trip_DataNew$Reviews,Trip_DataNew$Price,Trip_DataNew$Position))%*%Probit_BW$coefficients

#Update the IMR
Trip_DataNew$IMRNew<-dnorm(Trip_DataNew$XB)/pnorm(Trip_DataNew$XB)
#Calculate the expected Charges, and CLV
Trip_DataNew$ECharge.Per.Night <- as.matrix(cbind(1,Trip_DataNew$Credit,Trip_DataNew$Business,Trip_DataNew$IMRNew))%*%Charges_Bias_lm$coefficients
#Account for the change in probability to derive the value
Trip_DataNew$EChangeCLV<-(pnorm(Trip_DataNew$XB)-Probit_BW$fitted.values)*((Price_BW*(1-OTA_Fee)+Trip_DataNew$ECharge.Per.Night)*2*Margin*Margin_Multiplier)
#Expected CLV of current non-customers
tapply(Trip_DataNew$EChangeCLV,Trip_DataNew$Chose.BW,FUN=sum)[1]-Program_Cost

#Students should find that moving BW to the top position will net an increase of CLV of ~$10M
#Therefore, given all the assumptions in the problem and data, this appears a good decision.
