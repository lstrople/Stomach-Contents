##########
#Library
##########
library(RInSp)
library(vegan)
################################
#Discrete - stickleback example
################################


data(Stickleback)

#Import dataset

Site_A = import.RInSp(Stickleback, row.names = 1, info.cols = c(2:13),subset.rows = c("Site", "A"))


WT = WTdMC(Site_A, replicates = 999)

SiteA_E = Emc(Site_A, popd.type = "average", replicates = 999) #Evalue 

Null.data = MCp.RInSp(Site_A, replicates = 999)


null.d.NODF = c() 
for (i in 1:(999 + 1)) {
  null.d.NODF = c(null.d.NODF, NODF(import.RInSp(Null.data[ , , i], print.messages=FALSE), print.results=FALSE)$NODF)}

null.d.NODF[1]


summary(null.d.NODF)
hist(null.d.NODF)
abline(v=null.d.NODF[1])

###########################
#Continuous trout example
##########################

data(Trout)
#import dataset
TroutRIS = import.RInSp(Trout, col.header=TRUE, row.names=1, info.cols=c(1:6), data.type="double")
#specialisation 0 is strong specialisation and 1 is generalisation
results <- WTcMC(TroutRIS, replicates = 999)
#weighted specialisation
results= WTcMC(TroutRIS, weight="N_items", replicates=999)
#E index 0 is all ind same resources in same proportion, 1 every ind relies on different resources
TroutE = Eindex(TroutRIS, index = "saramaki", jackknife = TRUE)

decomp = Hier2L(TroutRIS, factor=1)

#histogram
sumMC.RInSp(results)