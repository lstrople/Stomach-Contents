

###########
#Chichocs
###########

setwd("C:/Users/lstrople/OneDrive - Norwegian University of Life Sciences/Winter_paper/KML files")

safo.df <- read.csv("C:/Users/lstrople/OneDrive - Norwegian University of Life Sciences/Mathias/stomach contents/stomachcontentsafo.csv")

winter <- subset(safo, Season=="Winter")
summer <- subset(safo, Season=="Summer")

#################
#season combined
################

#Haymard 
safoHayRIS = import.RInSp(stomachcontentsafo, col.header=TRUE, row.names=1, info.cols=c(1:6), data.type = "integer", subset.rows = c("Lake", "Haymard"))

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

#Shoener index 
PSiPauWi <- PSicalc(safopauRISWi, exclude = FALSE, replicates = 999)

#Cascapedia
safocascRISWi = import.RInSp(winter, col.header=TRUE, row.names=1, info.cols=c(1:6), data.type = "integer", subset.rows = c("Lake", "Cascapedia"))

resultsCascWi <- WTcMC(safocascRISWi, replicates = 999)

resultsCascWWi= WTcMC(safocascRISWi, weight="N_items", replicates=999)

TroutECascWi = Eindex(safocascRISWi, index = "saramaki", jackknife = TRUE)

sumMC.RInSp(resultsCascWWi)

PSiCascWi <- PSicalc(safocascRISWi, exclude = FALSE, replicates = 999)


#Thibault
safothibRISWi = import.RInSp(winter, col.header=TRUE, row.names=1, info.cols=c(1:6), data.type = "integer", subset.rows = c("Lake", "Thibault"))

resultsThibWi <- WTcMC(safothibRISWi, replicates = 999)

resultsThibWWi = WTcMC(safothibRISWi, weight="N_items", replicates=999)

TroutEWi = Eindex(safothibRISWi, index = "saramaki", jackknife = TRUE)

sumMC.RInSp(resultsThibWWi)

PSiThibWi <- PSicalc(safothibRISWi, exclude = FALSE, replicates = 999)


########
#summer
########


#Paul
safopauRISSu = import.RInSp(summer, col.header=TRUE, row.names=1, info.cols=c(1:6), data.type = "integer", subset.rows = c("Lake", "Paul"))

resultsPauSu <- WTcMC(safopauRISSu, replicates = 999)

resultsPauWSu= WTcMC(safopauRISSu, weight="N_items", replicates=999)

TroutEPauSu = Eindex(safopauRISSu, index = "saramaki", jackknife = TRUE)

sumMC.RInSp(resultsPauSu)

PSicascSu <- PSicalc(safocascRISSu, exclude = FALSE, replicates = 999)

#Cascapedia
safocascRISSu = import.RInSp(summer, col.header=TRUE, row.names=1, info.cols=c(1:6), data.type = "integer", subset.rows = c("Lake", "Cascapedia"))

resultsCascSu <- WTcMC(safocascRISSu, replicates = 999)

resultsCascWSu= WTcMC(safocascRISSu, weight="N_items", replicates=999)

TroutECascSu = Eindex(safocascRISSu, index = "saramaki", jackknife = TRUE)

sumMC.RInSp(resultsCascWSu)

PSicascSu <- PSicalc(safocascRISSu, exclude = FALSE, replicates = 999)


#Thibault
safothibRISSu = import.RInSp(summer, col.header=TRUE, row.names=1, info.cols=c(1:6), data.type = "integer", subset.rows = c("Lake", "Thibault"))

resultsThibWi <- WTcMC(safothibRISSu, replicates = 999)

resultsThibWSu = WTcMC(safothibRISSu, weight="N_items", replicates=999)

TroutESu = Eindex(safothibRISSu, index = "saramaki", jackknife = TRUE)

sumMC.RInSp(resultsThibWSu)

PSiThibSu <- PSicalc(safothibRISSu, exclude = FALSE, replicates = 999)

#Haymard

safoHayRISSu = import.RInSp(summer, col.header=TRUE, row.names=1, info.cols=c(1:6), data.type = "integer", subset.rows = c("Lake", "Haymard"))

resultsHay <- WTcMC(safoHayRISSu, replicates = 999)

resultsHayWSu= WTcMC(safoHayRISSu, weight="N_items", replicates=999)

safohayESu = Eindex(safoHayRISSu, index = "saramaki", jackknife = TRUE)

sumMC.RInSp(resultsHayWSu)

PSiHaySu <- PSicalc(safoHayRISSu, exclude = FALSE, replicates = 999)


############
#Shannon
###########

SummerSH <- subset(stomach_diversity, Season=="Summer")

HaySum <- subset(summer, Lake=="Haymard")

shannonHaySu <- (mean(diversity(HaySum, index = "shannon")))



#Shannon diversity index 
#Figures nice 




