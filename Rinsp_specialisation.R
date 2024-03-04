##########
#Library
##########
library(RInSp)

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


###########
#Chichocs
###########

#Haymard 
safoHayRIS = import.RInSp(safo, col.header=TRUE, row.names=1, info.cols=c(1:6), data.type = "integer", subset.rows = c("Lake", "Haymard"))

resultsHay <- WTcMC(safoHayRIS, replicates = 999)

resultsHayW= WTcMC(safoHayRIS, weight="N_items", replicates=999)

safohayE = Eindex(safoHayRIS, index = "saramaki", jackknife = TRUE)

#Schoner 0 niche overlap and 1 non niche over

sumMC.RInSp(resultsHayW)

#Paul
safopauRIS = import.RInSp(safo, col.header=TRUE, row.names=1, info.cols=c(1:6), data.type = "integer", subset.rows = c("Lake", "Paul"))

resultsPau <- WTcMC(safopauRIS, replicates = 999)

resultsPauW= WTcMC(safopauRIS, weight="N_items", replicates=999)

TroutE = Eindex(safopauRIS, index = "saramaki", jackknife = TRUE)

sumMC.RInSp(resultsPauW)

#Cascapedia
safocascRIS = import.RInSp(safo, col.header=TRUE, row.names=1, info.cols=c(1:6), data.type = "integer", subset.rows = c("Lake", "Cascapedia"))

resultsCasc <- WTcMC(safocascRIS, replicates = 999)

resultsCascW= WTcMC(safocascRIS, weight="N_items", replicates=999)

TroutE = Eindex(safocascRIS, index = "saramaki", jackknife = TRUE)

sumMC.RInSp(resultsCascW)

#Thibault
safothibRIS = import.RInSp(safo, col.header=TRUE, row.names=1, info.cols=c(1:6), data.type = "integer", subset.rows = c("Lake", "Thibault"))

resultsThib <- WTcMC(safothibRIS, replicates = 999)

resultsThibW = WTcMC(safothibRIS, weight="N_items", replicates=999)

TroutE = Eindex(safothibRIS, index = "saramaki", jackknife = TRUE)

sumMC.RInSp(resultsThibW)

#by season 
#schoener 
#Shannon diversity index 
#Figures nice 




