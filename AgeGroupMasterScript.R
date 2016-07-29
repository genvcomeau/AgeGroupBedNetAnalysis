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

