Bed.Net<- read.csv("C:/Users/Genevieve/Documents/R/MalariaBedNetSubset.csv")
Bed.Net.High<- subset(Bed.Net, Bed.Net$High >0)
Bed.Net.Low<- subset(Bed.Net, Bed.Net$Low >0)
library(dplyr)
HHTotLow<-
  group_by(Bed.Net.Low, HHNo) %>% slice(which.max(HHMNo))
HHTotLow<- HHTotLow[,c("HHNo", "HHMNo")]
NetTotLow<-
  group_by(Bed.Net.Low, HHNo) %>% slice(which.max(NETNO))
#bad estimate of HHsize, replaced by LNNET+NoNet
HHNetsHi<-
  merge(HHTotHi, NetTotHi, by.x = "HHNo", by.y = "HHNo")
HHNetsHi$Netspp<- HHNetsHi$NETNO/HHNetsHi$HHMNo
NetuseHHLow<- 
  merge(LNNETLow, NoNetLow, by.x = "HHNo", by.y = "HHNo")
NetuseHHLow$HHtot<- 
  NetuseHHLow$`(sum(LNNET))` + NetuseHHLow$`(sum(NoNet))`
#New estimate of HHsize, takes greater of two methods
HHNetsLow$HHMNo<- pmax(HHNetsLow$HHMNo, HHNetsLow$HHtot)
HHNetsLow$pLNNET<- 
  (HHNetsLow$`(sum(LNNET))`/(HHNetsLow$`(sum(LNNET))`+HHNetsLow$`(sum(NoNet))`))
Bed.Net.High[c("HHMNo", "NETNO", "Netspp", "pLNNET")]<- NA
#IMPORTANT HHMNo, NETNO in by land data != OG, rep total
mlm.Hi<- 
  merge(Bed.Net.High, HHNetsHi, by.x= "HHNo", by.y= "HHNo")
mlm.Lo$MAL12<- sub("N", 0, mlm.Lo$MAL12)

  #below shows how R dummifies binomial vr (which is 0,1)
contrasts()

logit.Hi<-
  glm(LNNET ~ Age + Pregnant + Sex + NETDF + SHRSLP+ 
  SLPSTRUCT + MAL12 + HHMNo.y + Netspp.y, 
      data = mlm.Hi, family = "binomial")
summary(logit.Hi)
logit.Lo<-
  glm(LNNET ~ Age + Pregnant + Sex + NETDF + SHRSLP+ 
        SLPSTRUCT + MAL12 + HHMNo + Netspp, 
      data = mlm.Lo, family = "binomial", na.action= na.exclude)
summary(logit.Lo)
logit.sig.Hi<-
  glm(LNNET ~ Age + Sex + NETDF + 
        SLPSTRUCT + HHMNo.y + Netspp.y, 
      data = mlm.Hi, family = "binomial")
logit.sig.Lo<-
  glm(LNNET ~ Sex + NETDF + Netspp, 
      data = mlm.Lo, family = "binomial", na.action= na.exclude)
#assessment of model fit
with(logit.sig.Hi, null.deviance - deviance)
with(logit.sig.Hi, df.null - df.residual)
with(logit.sig.Hi, pchisq(null.deviance - deviance, 
                df.null - df.residual, lower.tail = FALSE))
#OR and CIs
confint.default(logit.sig.Hi)
exp(cbind(OR = coef(logit.sig.Hi), confint(logit.sig.Hi)))
#Predicted probabilities by age
predicted.Hi.F.ND1.M1<- with(mlm.Hi,
     data.frame(Age = as.numeric(factor(1:98)), Sex = factor("F"), NETDF = factor(1), SLPSTRUCT = factor(1), HHMNo.y = 6.366133, Netspp.y = 0.4632654))
predicted.Hi.M.ND1.M1<- with(mlm.Hi,
                             data.frame(Age = as.numeric(factor(1:98)), Sex = factor("M"), NETDF = factor(1), SLPSTRUCT = factor(1), HHMNo.y = 6.366133, Netspp.y = 0.4632654))
predicted.Hi.ND1.M1<- rbind(predicted.Hi.F.ND1.M1, predicted.Hi.M.ND1.M1)
predicted.Hi.F.ND1.M0<- with(mlm.Hi,
                             data.frame(Age = as.numeric(factor(1:98)), Sex = factor("F"), NETDF = factor(1), SLPSTRUCT = factor(0), HHMNo.y = 6.366133, Netspp.y = 0.4632654))
predicted.Hi.M.ND1.M0<- with(mlm.Hi,
                             data.frame(Age = as.numeric(factor(1:98)), Sex = factor("M"), NETDF = factor(1), SLPSTRUCT = factor(0), HHMNo.y = 6.366133, Netspp.y = 0.4632654))
predicted.Hi.ND1.M0<- rbind(predicted.Hi.F.ND1.M0, predicted.Hi.M.ND1.M0)
predicted.Hi.F.ND0.M1<- with(mlm.Hi,
                             data.frame(Age = as.numeric(factor(1:98)), Sex = factor("F"), NETDF = factor(0), SLPSTRUCT = factor(1), HHMNo.y = 6.366133, Netspp.y = 0.4632654))
predicted.Hi.M.ND0.M1<- with(mlm.Hi,
                             data.frame(Age = as.numeric(factor(1:98)), Sex = factor("M"), NETDF = factor(0), SLPSTRUCT = factor(1), HHMNo.y = 6.366133, Netspp.y = 0.4632654))
predicted.Hi.ND0.M1<- rbind(predicted.Hi.F.ND0.M1, predicted.Hi.M.ND0.M1)
predicted.Hi.F.ND0.M0<- with(mlm.Hi,
                                data.frame(Age = as.numeric(factor(1:98)), Sex = factor("F"), NETDF = factor(0), SLPSTRUCT = factor(0), HHMNo.y = 6.366133, Netspp.y = 0.4632654))
predicted.Hi.M.ND0.M0<- with(mlm.Hi,
                             data.frame(Age = as.numeric(factor(1:98)), Sex = factor("M"), NETDF = factor(0), SLPSTRUCT = factor(0), HHMNo.y = 6.366133, Netspp.y = 0.4632654))
predicted.Hi.ND0.M0<- rbind(predicted.Hi.F.ND0.M0, predicted.Hi.M.ND0.M0)
logit.sig.Lo.pp<-
  glm(LNNET ~ Age + Sex + NETDF + Netspp, 
      data = mlm.Lo, family = "binomial", na.action= na.exclude)
predicted.Lo.M.ND1<- with(mlm.Lo,
                      data.frame(Age = as.numeric(factor(1:98)), Sex = "M", NETDF = factor(1), Netspp =0.5425858))
predicted.Lo.F.ND1<- with(mlm.Lo,
                        data.frame(Age = as.numeric(factor(1:98)), Sex = "F", NETDF = factor(1), Netspp = 0.5425858))
predicted.Lo.ND1 <- rbind(predicted.Lo.F.ND1, predicted.Lo.M.ND1)
predicted.Lo.M.ND0<- with(mlm.Lo,
                     data.frame(Age = as.numeric(factor(1:98)), Sex = "M", NETDF = factor(0), Netspp =0.5425858))
predicted.Lo.F.ND0<- with(mlm.Lo,
                          data.frame(Age = as.numeric(factor(1:98)), Sex = "F", NETDF = factor(0), Netspp =0.5425858))
predicted.Lo.ND0 <- rbind(predicted.Lo.F.ND0, predicted.Lo.M.ND0)
predicted.Hi.ND1.M1$AgeP <- predict(logit.sig.Hi, newdata = predicted.Hi.ND1.M1, type = "response")
predicted.Hi.ND1.M0$AgeP <- predict(logit.sig.Hi, newdata = predicted.Hi.ND1.M0, type = "response")
predicted.Hi.ND0.M1$AgeP <- predict(logit.sig.Hi, newdata = predicted.Hi.ND0.M1, type = "response")
predicted.Hi.ND0.M0$AgeP <- predict(logit.sig.Hi, newdata = predicted.Hi.ND0.M0, type = "response")
predicted.Lo.ND1$AgeP <- predict(logit.sig.Lo.pp, newdata = predicted.Lo.ND1, type = "response")
predicted.Lo.ND0$AgeP <- predict(logit.sig.Lo.pp, newdata = predicted.Lo.ND0, type = "response")
#plotting
ggplot(predicted.Hi.ND1.M1, aes(x = Age, y = AgeP)) + geom_line(aes(colour = Sex), size=1)
#agevsLNNET
AgeppLNNET.Hi<- read.csv("C:/Users/Genevieve/Documents/R/AgeppLNNET.csv")
AgeppLNNET.Lo<- read.csv("C:/Users/Genevieve/Documents/R/AgeppLNNET.csv")

dummyppLNNET.Hi<- aggregate(mlm.Hi$LNNET, mlm.Hi[c("Age", "Age")], FUN=mean, na.rm=T)
dummyppLNNET.Lo<- aggregate(mlm.Lo$LNNET, mlm.Lo[c("Age", "Age")], FUN=mean, na.rm=T)
names(dummyppLNNET.Hi)[names(dummyppLNNET.Hi)=="x"] <- "ppLNNET"
names(dummyppLNNET.Lo)[names(dummyppLNNET.Lo)=="x"] <- "ppLNNET"

NetusebyAge.Hi<- ggplot(dummyppLNNET.Hi, aes(Age,ppLNNET)) + geom_point() + geom_smooth()
NetusebyAge.Lo<- ggplot(dummyppLNNET.Lo, aes(Age,ppLNNET)) + geom_point() + geom_smooth()
dev.copy(png,"NetusebyAgeLo.png",width=8,height=6,units="in",res=100)
dev.copy(png,"NetusebyAgeHi.png",width=8,height=6,units="in",res=100)

mlm.Hi$RDT<- as.numeric(as.character(mlm.Hi$RDT))
mlm.Lo$RDT<- as.numeric(as.character(mlm.Lo$RDT))
AgexRDT.Hi<- aggregate(mlm.Hi$RDT, mlm.Hi[c("Age")], FUN=mean, na.rm=T)
AgexRDT.Lo<- aggregate(mlm.Lo$RDT, mlm.Lo[c("Age")], FUN=mean, na.rm=T)
RDTbyAge.Hi<- ggplot(AgexRDT.Hi, aes(Age,x)) + geom_point() + geom_smooth()
dev.copy(png,"RDTbyAgeHi.png",width=8,height=6,units="in",res=100) dev.off()
RDTbyAge.Lo<- ggplot(AgexRDT.Lo, aes(Age,x)) + geom_point() + geom_smooth()
dev.copy(png,"RDTbyAgeLo.png",width=8,height=6,units="in",res=100)

#WHO BN Indicators
(mlm.Hi$`(sum(NoNet))`> 0 && mlm.Hi$`(sum(LNNET))` < mlm.Hi$NETNO.y)
(mlm.Lo$`(sum(NoNet))`> 0 && mlm.Lo$`(sum(LNNET))` < mlm.Lo$NETNO.y)

#AgexRDTxLNNET comparison
AgexRDTxNonuser_Hi <- 
  data.frame(
    subset(mlm.Hi$Age, mlm.Hi$LNNET == 0), 
   subset(mlm.Hi$RDT, mlm.Hi$LNNET == 0))

#Model reevaluation
#determining interaction terms
mean(mlm.Hi$Age[which(mlm.Hi$Sex == "M")])
mean(mlm.Hi$Age[which(mlm.Hi$Sex == "F")], na.rm = TRUE)
indepsexage.hi <- table(mlm.Hi$Sex, mlm.Hi$Age)
chisq.test(indepsexage.hi[c(2,3),])
fisher.test(indepsexage.hi,simulate.p.value=TRUE,B=1e7)
#re-made mlm.Lo and saved as mlm.Lo2 because Sex got changed to all M somehow
mean(mlm.Lo2$Age[which(mlm.Lo2$Sex == "F")], na.rm = TRUE)
mean(mlm.Lo2$Age[which(mlm.Lo2$Sex == "M")], na.rm = TRUE)
indepsexage.lo <- table(mlm.Lo2$Sex, mlm.Lo2$Age)
table(mlm.Hi$SLPSTRUCT, mlm.Hi$Sex)
chisq.test(indepslpsex.hi[c(2,3), c(2,3)])
#assigned mlm.Lo2$Sex to mlm.Lo$Sex
indepNetsHHM.hi <- table(mlm.Hi$Netspp.y, mlm.Hi$HHMNo.y)
chisq.test(indepNetsHHM.hi)
indepNetsHHM.lo <- table(mlm.Lo$Netspp, mlm.Lo$HHMNo)
chisq.test(indepNetsHHM.lo)
#new model with interaction terms
logit.sig.Hi.interaction<-
  glm(LNNET ~ Age + Sex + NETDF + 
        SLPSTRUCT + HHMNo.y + Netspp.y + 
        Netspp.y *HHMNo.y, 
      data = mlm.Hi, family = "binomial")
summary(logit.sig.Hi.interaction)

logit.sig.Lo.interaction<-
  glm(LNNET ~ Age + Sex + NETDF + HHMNo + Netspp + 
        Netspp *HHMNo, 
      data = mlm.Lo, family = "binomial")
summary(logit.sig.Lo.interaction)

#model validation and comparison
with(logit.sig.Hi.interaction, null.deviance - deviance)
with(logit.sig.Hi.interaction, df.null - df.residual)
with(logit.sig.Hi.interaction, pchisq(null.deviance - deviance, 
                          df.null - df.residual, lower.tail = FALSE))
#reworking highland model predictions to include interaction term
predicted.Hi.ND1.M1$AgeP.i <- predict(logit.sig.Hi.interaction, newdata = predicted.Hi.ND1.M1, type = "response")
predicted.Hi.ND1.M0$AgeP.i <- predict(logit.sig.Hi.interaction, newdata = predicted.Hi.ND1.M0, type = "response")
predicted.Hi.ND0.M1$AgeP.i <- predict(logit.sig.Hi.interaction, newdata = predicted.Hi.ND0.M1, type = "response")
predicted.Hi.ND0.M0$AgeP.i <- predict(logit.sig.Hi.interaction, newdata = predicted.Hi.ND0.M0, type = "response")

#Combining in a dataframe and graphing predictions
Predicted.Probabilities.Hi <- data.frame(predicted.Hi.ND0.M0$Age, predicted.Hi.ND0.M0$Sex, 
                                         predicted.Hi.ND0.M0$AgeP.i, predicted.Hi.ND0.M1$AgeP.i
                                         ,predicted.Hi.ND1.M0$AgeP.i, predicted.Hi.ND1.M1$AgeP.i)
Predicted.Probabilities.Lo <- data.frame(predicted.Lo.ND0$Age, predicted.Lo.ND0$Sex, 
                                         predicted.Lo.ND0$AgeP, predicted.Lo.ND1$AgeP)
colnames(Predicted.Probabilities.Hi)[1] <- "Age"
colnames(Predicted.Probabilities.Hi)[2] <- "Sex"
colnames(Predicted.Probabilities.Hi)[3] <- "No Diff, Not Main"
colnames(Predicted.Probabilities.Hi)[4] <- "No Diff, Main"
colnames(Predicted.Probabilities.Hi)[5] <- "Net Diff, Not Main"
colnames(Predicted.Probabilities.Hi)[6] <- "Net Diff, Main"
colnames(Predicted.Probabilities.Lo)[1] <- "Age"
colnames(Predicted.Probabilities.Lo)[2] <- "Sex"
colnames(Predicted.Probabilities.Lo)[3] <- "No Diff"
colnames(Predicted.Probabilities.Lo)[4] <- "Net Diff"

P.P.Hi.F <- Predicted.Probabilities.Hi[1:98,]
P.P.Hi.M <- Predicted.Probabilities.Hi[99:196,]
P.P.Lo.M <- Predicted.Probabilities.Lo[99:196,]
P.P.Lo.F <- Predicted.Probabilities.Lo[1:98,]

library(reshape2)
P.P.Hi.F.melted <- melt(P.P.Hi.F[,c(1,3,4,5,6)], id = "Age")
P.P.Hi.M.melted <- melt(P.P.Hi.M[,c(1,3,4,5,6)], id = "Age")
P.P.Lo.M.melted <- melt(P.P.Lo.M[,c(1,3,4)], id = "Age")
P.P.Lo.F.melted <- melt(P.P.Lo.F[,c(1,3,4)], id = "Age")

require(ggplot2)
Predicted.Prob.Hi.All.M <-ggplot(P.P.Hi.M.melted,
    aes(x = Age, y = value, color = variable)) +
      geom_line() + ylim(0.5, 1.0) + ylab("Probability Slept Under Net Last Night") 


Predicted.Prob.Hi.All.F <-ggplot(P.P.Hi.F.melted,
    aes(x = Age, y = value, color = variable)) +
  geom_line() + ylim(0.5, 1.0) + ylab("Probability Slept Under Net Last Night") 


Predicted.Prob.Lo.All.M <-ggplot(P.P.Lo.M.melted,
    aes(x = Age, y = value, color = variable)) +
  geom_line() + ylim(0.7,1.0) + ylab("Probability Slept Under Net Last Night") 


Predicted.Prob.Lo.All.F <-ggplot(P.P.Lo.F.melted,
   aes(x = Age, y = value, color = variable)) +
  geom_line() + ylim(0.7,1) + ylab("Probability Slept Under Net Last Night") 

#making mean values
land.mean.hi.f<- data.frame(P.P.Hi.F[,1], rowMeans(P.P.Hi.F[,3:6]))
land.mean.hi.m<- data.frame(P.P.Hi.M[,1], rowMeans(P.P.Hi.M[,3:6]))
land.mean.lo.m<- data.frame(P.P.Lo.M[,1], rowMeans(P.P.Lo.M[,3:4]))
land.mean.lo.f<- data.frame(P.P.Lo.F[,1], rowMeans(P.P.Lo.F[,3:4]))


AgeMeans<- read.csv("C:/Users/Genevieve/Documents/AgeGroupBedNetAnalysis/AgeMeans.csv")

P.P.Land.melted <- melt(AgeMeans[,c(1,6,7)], id = "Age")
P.P.Land.Gender.melted <- melt(AgeMeans[,c(1,2,3,4,5)], id = "Age")
P.P.Gender.melted <- melt(AgeMeans[,c(1,8,9)], id = "Age")

Predicted.Prob.Land <-ggplot(P.P.Land.melted,
                                 aes(x = Age, y = value, color = variable)) +
  geom_line() + ylim(0.7,1.0) + ylab("Probability Slept Under Net Last Night") 

Predicted.Prob.Land.Gender <-ggplot(P.P.Land.Gender.melted,
                             aes(x = Age, y = value, color = variable)) +
  geom_line() + ylim(0.7,1.0) + ylab("Probability Slept Under Net Last Night") 

Predicted.Prob.Gender <-ggplot(P.P.Gender.melted,
                             aes(x = Age, y = value, color = variable)) +
  geom_line() + ylim(0.7,1.0) + ylab("Probability Slept Under Net Last Night") 

#Age dummy variables
Bed.Net.High$Age0x1 <- ifelse(Bed.Net.High$Age == 1, 1,0)
Bed.Net.High$Age2x5 <- ifelse(Bed.Net.High$Age >= 2 & Bed.Net.High$Age <= 5, 1, 0)
Bed.Net.High$Age6x10 <- ifelse(Bed.Net.High$Age >= 6 & Bed.Net.High$Age <= 10, 1, 0)
Bed.Net.High$Age11x15 <- ifelse(Bed.Net.High$Age >= 11 & Bed.Net.High$Age <= 15, 1, 0)
Bed.Net.High$Age16x25 <- ifelse(Bed.Net.High$Age >= 16 & Bed.Net.High$Age <= 25, 1, 0)
Bed.Net.High$Age26x35 <- ifelse(Bed.Net.High$Age >= 26 & Bed.Net.High$Age <= 35, 1, 0)
Bed.Net.High$Age36x45 <- ifelse(Bed.Net.High$Age >= 36 & Bed.Net.High$Age <= 45, 1, 0)
Bed.Net.High$Age46x55 <- ifelse(Bed.Net.High$Age >= 46 & Bed.Net.High$Age <= 55, 1, 0)
Bed.Net.High$Age56x65 <- ifelse(Bed.Net.High$Age >= 56 & Bed.Net.High$Age <= 65, 1, 0)
Bed.Net.High$Age66 <- ifelse(Bed.Net.High$Age >= 66, 1, 0)

Bed.Net.Low$Age0x1 <- ifelse(Bed.Net.Low$Age == 1, 1,0)
Bed.Net.Low$Age2x5 <- ifelse(Bed.Net.Low$Age >= 2 & Bed.Net.Low$Age <= 5, 1, 0)
Bed.Net.Low$Age6x10 <- ifelse(Bed.Net.Low$Age >= 6 & Bed.Net.Low$Age <= 10, 1, 0)
Bed.Net.Low$Age11x15 <- ifelse(Bed.Net.Low$Age >= 11 & Bed.Net.Low$Age <= 15, 1, 0)
Bed.Net.Low$Age16x25 <- ifelse(Bed.Net.Low$Age >= 16 & Bed.Net.Low$Age <= 25, 1, 0)
Bed.Net.Low$Age26x35 <- ifelse(Bed.Net.Low$Age >= 26 & Bed.Net.Low$Age <= 35, 1, 0)
Bed.Net.Low$Age36x45 <- ifelse(Bed.Net.Low$Age >= 36 & Bed.Net.Low$Age <= 45, 1, 0)
Bed.Net.Low$Age46x55 <- ifelse(Bed.Net.Low$Age >= 46 & Bed.Net.Low$Age <= 55, 1, 0)
Bed.Net.Low$Age56x65 <- ifelse(Bed.Net.Low$Age >= 56 & Bed.Net.Low$Age <= 65, 1, 0)
Bed.Net.Low$Age66 <- ifelse(Bed.Net.Low$Age >= 66, 1, 0)

#New mlm model
mlm.Hi<- 
  merge(Bed.Net.High, HHNetsHi, by.x= "HHNo", by.y= "HHNo")

mlm.Lo<- 
  merge(Bed.Net.Low, HHNetsLow, by.x= "HHNo", by.y= "HHNo", na.rm = TRUE)

mlm.Lo$MAL12<- sub("N", 0, mlm.Lo$MAL12)

#redoing variables
mlm.Hi$SLPSTRUC <- ifelse(mlm.Hi$SLPSTRUCT == "main house", 1, 0)
mlm.Lo$SLPSTRUC <- ifelse(mlm.Lo$SLPSTRUCT == "main house", 1, 0)

mlm.Hi$Pregnant <- ifelse(mlm.Hi$Pregnant == 1, 1, 0)
mlm.Lo$Pregnant <- ifelse(mlm.Lo$Pregnant == 1, 1, 0)

mlm.Hi$SHRSLP <- ifelse(mlm.Hi$SHRSLP == "Y", 1, 0)
mlm.Lo$SHRSLP <- ifelse(mlm.Lo$SHRSLP == "Y", 1, 0)

mlm.Hi$MAL12 <- ifelse(mlm.Hi$MAL12 == 1, 1, 0)
mlm.Lo$MAL12 <- ifelse(mlm.Lo$MAL12 == 1, 1, 0)

mlm.Hi$NETDF <- ifelse(mlm.Hi$NETDF == 1, 1, 0)
mlm.Lo$NETDF <- ifelse(mlm.Lo$NETDF == 1, 1, 0)

mlm.Hi$Sex <- ifelse(mlm.Hi$Sex == "F", 1, 0)
mlm.Lo$Sex <- ifelse(mlm.Lo$Sex == "F", 1, 0)

mlm.Hi$RDT <- ifelse(mlm.Hi$RDT == 1, 1, 0)
mlm.Lo$RDT <- ifelse(mlm.Lo$RDT == 1, 1, 0)

mlm.Lo <- mlm.Lo[ ! mlm.Lo$Netspp %in% c("Inf"), ]

#DK coded as no, underestimation

logit.Hi<-
  glm(LNNET ~ Person.Under.5 + Pregnant + Sex + NETDF + SHRSLP+ 
        SLPSTRUC + MAL12 + HHMNo.y + Netspp.y + RDT + Age0x1 +
         Age2x5 + Age6x10 + Age11x15 + Age16x25 + Age26x35
      + Age36x45 + Age46x55 + Age56x65 + Age66, 
      data = mlm.Hi, family = "binomial", na.action = na.exclude)
summary(logit.Hi)

logit.Lo <-
  glm(LNNET ~ Sex + Pregnant + NETDF + SHRSLP + RDT +
        SLPSTRUC + MAL12 + HHMNo + Netspp + Age0x1 +
        Age2x5 + Age6x10 + Age11x15 + Age16x25 + Age26x35
      + Age36x45 + Age46x55 + Age56x65 + Age66, 
      data = mlm.Lo, family = "binomial", na.action = na.exclude)
summary(logit.Lo)

#excluded Netspp

#step-wise backwards elimination of nonsignificant variables, by least sig p value

#High(in order of elim): Person under five, age2x5, age66, age46x55, shrslp, age56x65, pregnant, 
#age26x35, age36x45, Age0x1, MAL12, RDT(p = 0.08)

#Low: age0x1, pregnant, age56x65, age26x35, mal12, slpstruc, shrslp, age66, age2x5, age46x55,
#RDT, HHMNo, age35x46

logit.Hi<-
  glm(LNNET ~ Sex + NETDF +
        SLPSTRUC + HHMNo.y + Netspp.y +
       Age6x10 + Age11x15 + Age16x25, 
      data = mlm.Hi, family = "binomial", na.action = na.exclude)
summary(logit.Hi)

logit.Lo <-
  glm(LNNET ~ Sex + NETDF + Netspp + 
        Age6x10 + Age11x15 + Age16x25, 
      data = mlm.Lo, family = "binomial", na.action = na.exclude)
summary(logit.Lo)

#CI and OR for reworked models
confint.default(logit.Hi)
exp(cbind(OR = coef(logit.Hi), confint(logit.Hi)))
confint.default(logit.Lo)
exp(cbind(OR = coef(logit.Lo), confint(logit.Lo)))


                              