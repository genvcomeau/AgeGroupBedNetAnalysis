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
