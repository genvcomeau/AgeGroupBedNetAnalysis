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
predicted.Hi.F<- with(mlm.Hi,
     data.frame(Age = factor(1:98), Sex = factor("F"), NETDF = mean(NETDF), SLPSTRUCT = mean(SLPSTRUCT), HHMNo.y = mean(HHMNo.y), Netspp.y = mean(Netspp.y)))
predicted.Hi<- rbind(predicted.Hi.F, predicted.Hi.M)
predicted.Lo.M<- with(mlm.Lo,
                      data.frame(Age = factor(1:98), Sex = "M", Netspp = mean(Netspp, na.rm = TRUE)))
predicted.Lo.F<- with(mlm.Lo,
                      +      data.frame(Age = factor(1:98), Sex = "F", Netspp = mean(Netspp, na.rm = TRUE)))
predicted.Lo <- rbind(predicted.Lo.F, predicted.Lo.M)
