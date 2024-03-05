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

winter <- subset(safo, Season=="Winter")
summer <- subset(safo, Season=="Summer")

#################
#season combined
################

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

########
#winter
########


#Paul
safopauRISWi = import.RInSp(winter, col.header=TRUE, row.names=1, info.cols=c(1:6), data.type = "integer", subset.rows = c("Lake", "Paul"))

resultsPauWi <- WTcMC(safopauRISWi, replicates = 999)

resultsPauWWi= WTcMC(safopauRISWi, weight="N_items", replicates=999)

TroutEPauWi = Eindex(safopauRISWi, index = "saramaki", jackknife = TRUE)

sumMC.RInSp(resultsPauWi)

#Cascapedia
safocascRISWi = import.RInSp(winter, col.header=TRUE, row.names=1, info.cols=c(1:6), data.type = "integer", subset.rows = c("Lake", "Cascapedia"))

resultsCascWi <- WTcMC(safocascRISWi, replicates = 999)

resultsCascWWi= WTcMC(safocascRISWi, weight="N_items", replicates=999)

TroutECascWi = Eindex(safocascRISWi, index = "saramaki", jackknife = TRUE)

sumMC.RInSp(resultsCascWWi)


#Thibault
safothibRISWi = import.RInSp(winter, col.header=TRUE, row.names=1, info.cols=c(1:6), data.type = "integer", subset.rows = c("Lake", "Thibault"))

resultsThibWi <- WTcMC(safothibRISWi, replicates = 999)

resultsThibWWi = WTcMC(safothibRISWi, weight="N_items", replicates=999)

TroutEWi = Eindex(safothibRISWi, index = "saramaki", jackknife = TRUE)

sumMC.RInSp(resultsThibWWi)


########
#summer
########


#Paul
safopauRISSu = import.RInSp(summer, col.header=TRUE, row.names=1, info.cols=c(1:6), data.type = "integer", subset.rows = c("Lake", "Paul"))

resultsPauSu <- WTcMC(safopauRISSu, replicates = 999)

resultsPauWSu= WTcMC(safopauRISSu, weight="N_items", replicates=999)

TroutEPauSu = Eindex(safopauRISSu, index = "saramaki", jackknife = TRUE)

sumMC.RInSp(resultsPauSu)

#Cascapedia
safocascRISSu = import.RInSp(summer, col.header=TRUE, row.names=1, info.cols=c(1:6), data.type = "integer", subset.rows = c("Lake", "Cascapedia"))

resultsCascSu <- WTcMC(safocascRISSu, replicates = 999)

resultsCascWSu= WTcMC(safocascRISSu, weight="N_items", replicates=999)

TroutECascSu = Eindex(safocascRISSu, index = "saramaki", jackknife = TRUE)

sumMC.RInSp(resultsCascWSu)


#Thibault
safothibRISSu = import.RInSp(summer, col.header=TRUE, row.names=1, info.cols=c(1:6), data.type = "integer", subset.rows = c("Lake", "Thibault"))

resultsThibWi <- WTcMC(safothibRISSu, replicates = 999)

resultsThibWSu = WTcMC(safothibRISSu, weight="N_items", replicates=999)

TroutESu = Eindex(safothibRISSu, index = "saramaki", jackknife = TRUE)

sumMC.RInSp(resultsThibWSu)

#Haymard 
safoHayRISSu = import.RInSp(summer, col.header=TRUE, row.names=1, info.cols=c(1:6), data.type = "integer", subset.rows = c("Lake", "Haymard"))

resultsHay <- WTcMC(safoHayRISSu, replicates = 999)

resultsHayWSu= WTcMC(safoHayRISSu, weight="N_items", replicates=999)

safohayESu = Eindex(safoHayRISSu, index = "saramaki", jackknife = TRUE)

sumMC.RInSp(resultsHayWSu)

#schoener 
#Shannon diversity index 
#Figures nice 




