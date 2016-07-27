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