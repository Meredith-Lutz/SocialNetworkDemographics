#################################################
##### Long term network demographics - KMNP #####
#####      Last updated by ML 12/1/2021     #####
#################################################
setwd("C:/Users/cecil/OneDrive/Desktop/SDC Work")
#setwd('G:/My Drive/Graduate School/Research/Projects/KMNPLongTermData/NSF Analyses')

library(stringr)
library(lme4)
library(lubridate)

source("C:/Users/cecil/OneDrive/Desktop/SDC Work/Github Work/NSFSocialNetwork2/NSFSocialNetwork/ObservationTimeFunctions.R")
#source('G:/My Drive/Graduate School/Research/Projects/KMNPLongTermData/NSF Analyses/NSFSocialNetwork/ObservationTimeFunctions.R')
source("C:/Users/cecil/OneDrive/Desktop/SDC Work/Github Work/SeasonalNetworkAnalyses/createNetworkFunction.R")
#source('G:/My Drive/Graduate School/Research/Projects/TemporalNets/SeasonalNetworkAnalyses/createNetworkFunction.R')

socialDataRaw		<- read.csv('All_nonSuppStudent_Social_Data_through_2019_Francis duplicates deleted_Jul262021_ML_2021_11_10.csv', stringsAsFactors = FALSE)
groups			<- read.csv('Compiled Group File with some data deleted for BL analysis_Nov 3 2021_ML Corrected11Nov2021.csv', stringsAsFactors = FALSE)
nnFocalList			<- read.csv('NearestNeighborIDs_TMM_ML_01Dec2021.csv', stringsAsFactors = FALSE)
actvFocalList		<- read.csv('FocalActivityIDs_TMM_ML_01Dec2021.csv', stringsAsFactors = FALSE)
filemakerFocalList	<- read.csv('FileMakerIDs_ML_06Dec2021.csv', stringsAsFactors = FALSE)
nn				<- read.csv('NearestNeighbor_TMM_ML_01Dec2021.csv', stringsAsFactors = FALSE)
actv				<- read.csv('FocalActivity_TMM_ML_11Nov2021.csv', stringsAsFactors = FALSE)
fm				<- read.csv('FileMaker_ML_01Dec2021.csv', stringsAsFactors = FALSE)


demo				<- read.csv('Copy of life.history.TMM with becca comments about conflicting info Feb10_2021_ML.csv', stringsAsFactors = FALSE)
demo$Name			<- str_to_title(demo$Name, locale = "en")
demo$Sex			<- ifelse(demo$Sex == '', 'unknown', as.character(demo$Sex))
sifakaNames			<- demo$Name

socialDataAllRaw		<- socialDataRaw[,c('OriginalFile', 'Observer', 'Obs.ID', 'Date', 'Month', 'Year', 'Focal', 'Start', 'Stop',
					'Duration', 'Duration.Seconds', 'Initiator', 'Receiver', 'Context', 'Behavior', 'Species',
					'Tree.number', 'Response', 'To', 'Win', 'Comments', 'Cleaning.Comments', 'StudentOb.YN')]

# Change baby names to match LH file
socialDataAllRaw$Initiator	<- gsub('Vanilla_baby_2011', 'Vanillababy2011', socialDataAllRaw$Initiator)
socialDataAllRaw$Initiator	<- gsub('Savannah_baby_2011', 'Savannahbaby2011', socialDataAllRaw$Initiator)
socialDataAllRaw$Receiver	<- gsub('Vanilla_baby_2011', 'Vanillababy2011', socialDataAllRaw$Receiver)
socialDataAllRaw$Receiver	<- gsub('Savannah_baby_2011', 'Savannahbaby2011', socialDataAllRaw$Receiver)

# Remove lines that have non-identified individuals, n=667 removed, n=137769 left
socialData		<- socialDataAllRaw[socialDataAllRaw$Initiator %in% sifakaNames & socialDataAllRaw$Receiver %in% sifakaNames,]
socialDataRemoved	<- socialDataAllRaw[!(socialDataAllRaw$Initiator %in% sifakaNames & socialDataAllRaw$Receiver %in% sifakaNames),]

#Merging groupNames back onto socialData
socialData	<- merge(socialDataRaw,groups[,1:3], by.x = c("Date", "Focal"),by.y = c("date", "animal"), all.x = TRUE)

socialData$monthNum	<- ifelse(socialData$Month == 'Jan', '01', 
					ifelse(socialData$Month == 'Feb', '02',
					ifelse(socialData$Month == 'Mar', '03',
					ifelse(socialData$Month == 'Apr', '04',
					ifelse(socialData$Month == 'May', '05',
					ifelse(socialData$Month == 'Jun', '06',
					ifelse(socialData$Month == 'Jul', '07',
					ifelse(socialData$Month == 'Aug', '08',
					ifelse(socialData$Month == 'Sep', '09',
					ifelse(socialData$Month == 'Oct', '10',
					ifelse(socialData$Month == 'Nov', '11', '12')))))))))))
socialData$yearMonth	<- paste(socialData$Year, socialData$monthNum, sep = '-')

######################################################
### Combine Focal Lists and Create Observation MAT ###
######################################################
fullFocalList	<- rbind.data.frame(filemakerFocalList, nnFocalList, actvFocalList, stringsAsFactors = FALSE)
fullFocalList	<- fullFocalList[order(fullFocalList$date, fullFocalList$start_time),]
fullFocalList$yearMonth	<- substr(fullFocalList$date,1,7)

#Pick focals so that they aren't overlapping
uniqueDates	<- unique(fullFocalList$date)
fullFocalListNonOverlapping	<- data.frame()
for (i in uniqueDates){
	print(i)
	groupsObserved	<- unique(fullFocalList[fullFocalList$date == i,"group"])
	for (j in groupsObserved){
		print(j)
		subset	<- fullFocalList[fullFocalList$group == j & fullFocalList$date == i,]
		#print(dim(subset))
		subset$start_time 	<- as.POSIXlt(subset$start_time, format = "%H:%M:%S")
		subset$stop_time 	<- as.POSIXlt(subset$stop_time, format = "%H:%M:%S")
		subset	<- subset[order(subset$start_time),]
		if (dim(subset)[1] >= 2){
			for (k in 1:(dim(subset)[1]-1)){
				for (m in (k+1):(dim(subset)[1])){
					#print(k)
					#print(m)
					#print(dim(subset))
					focal1	<- interval(start = subset[k,"start_time"],end = subset[k,"stop_time"])
					focal2	<- interval(start = subset[m,"start_time"],end = subset[m,"stop_time"])
					if (is.na(focal1)==FALSE & is.na(focal2)==FALSE){
						if (int_overlaps(focal1,focal2)){
							observers	<- unique(subset[c(k,m),"observer"])
							overlappingFocals	<- subset[c(k,m),]
							if (length(observers) > 1){
								observer1		<- observers[1]
								observerToKeep	<- ifelse(observer1 == "Becca","Becca",
												ifelse(observer1 == "Meredith","Meredith",
												ifelse(observer1 == "Max", "Max",
												ifelse(observer1 == "Max and Becca","Max and Becca",
												ifelse(observer1 == "Patrick","Patrick", 
								          			ifelse(observer1 == "Andry","Andry", 
												ifelse(observer1 == "Daniel","Daniel", 
												ifelse(observer1 == "Dessy","Dessy", 
												ifelse(observer1 == "Francis","Francis", 
												ifelse(observer1 == "Laura","Laura", 
												ifelse(observer1 == "Mampionona","Mampionona", 
												ifelse(observer1 == "Elvis","Elvis", "Felana"
												))))))))))))
								idToRemove	<- overlappingFocals[overlappingFocals$observer != observerToKeep,"focalid"]
								subset	<- subset[subset$focalid != idToRemove,]
							}
						}
					}
				}
			}
		}
		#print(dim(subset))
		#print(dim(fullFocalListNonOverlapping))
		fullFocalListNonOverlapping	<- rbind.data.frame(fullFocalListNonOverlapping,subset)
		#print(dim(fullFocalListNonOverlapping))
	}
}


##################################################
### Add Focal ID from Focal List to Social Data###
##################################################
socialData$focalID		<- NA
fullFocalList$adjStopTime	<- NA
socialData$Start		<- as.POSIXlt(socialData$Start, format = "%H:%M:%S")
socialData$Stop			<- as.POSIXlt(socialData$Stop, format = "%H:%M:%S")
fullFocalList$start_time	<- as.POSIXlt(fullFocalList$start_time, format = "%H:%M:%S")
fullFocalList$stop_time		<- as.POSIXlt(fullFocalList$stop_time, format = "%H:%M:%S")

for (i in 1:nrow(fullFocalList)){
	print(i)
	focalObserver	<- fullFocalList[i,"observer"]
	#print(focalObserver)
	focalAnimal	<- fullFocalList[i,"focal_animal"]
	#print(focalAnimal)
	focalDate	<- fullFocalList[i,"date"]
	#print(focalDate)
	focalStartTime	<- fullFocalList[i,"start_time"]
	#print(focalStartTime)
	focalStopTime	<- fullFocalList[i,"stop_time"]
	#print(focalStopTime)
	focalid		<- fullFocalList[i,"focalid"]
	#print(class(focalStartTime))
	socialData[socialData$Observer == focalObserver &
			socialData$Focal == focalAnimal & socialData$Date == focalDate & is.na(socialData$Start) == FALSE &
			socialData$Start >= focalStartTime & socialData$Start <= focalStopTime, "focalID"]	<- focalid 

	nLines	<- dim(socialData[socialData$focalID == focalid & is.na(socialData$focalid) == FALSE,])[1]
	
	if(nLines > 0){
		#Calculate actual stop time of focal
		maxBehaviorStopTime	<- max(socialData[socialData$focalID == focalid & is.na(socialData$focalID) == FALSE, "Stop"], na.rm = TRUE)
		actualFocalStopTime	<- ifelse(maxBehaviorStopTime > focalStopTime, maxBehaviorStopTime, focalStopTime)

		fullFocalList[i,]$adjStopTime	<- actualFocalStopTime

		#Add in the extra lines
		socialData[socialData$Observer == focalObserver &
			socialData$Focal == focalAnimal & socialData$Date == focalDate & 
			socialData$Start >= focalStartTime & socialData$Start <= actualFocalStopTime, "focalID"]	<- fullFocalList[i,"focalid"]  
	}
}

socialData$Start	<- format(socialData$Start, format = '%H:%M:%S')
socialData$Stop	<- format(socialData$Stop, format = '%H:%M:%S')

write.csv(socialData, "allSocialDataWithFocalIDs.csv", row.names = FALSE)

##############################################
### Identify Which Focals Need Social Data ###
##############################################
socialDataWithID		<- read.csv("allSocialDataWithFocalIDs.csv")

fullFocalList$start_time	<- format(fullFocalList$start_time, format = "%H:%M:%S")
fullFocalList$stop_time		<- format(fullFocalList$stop_time, format = "%H:%M:%S")

focalsNoSocialData		<- fullFocalList[!fullFocalList$focalid%in%unique(socialDataWithId$focalID),]

summarizeNNFocals		<- aggregate(nn$yes_socialdata,by=list(focalid = nn$focalid, date = nn$date, group = nn$group, focal = nn$focal_animal), FUN = sum)
trulyNoSocialDataNN		<- fullFocalList[fullFocalList$focalid %in% summarizeNNFocals[summarizeNNFocals$x == 0, "focalid"],]
socialDataNeedsEnteringNN	<- fullFocalList[fullFocalList$focalid %in% summarizeNNFocals[summarizeNNFocals$x > 0, "focalid"],]

summarizeActvFocals		<- aggregate(actv$yes_socialdata,by=list(focalid = actv$focalid, date = actv$date, group = actv$group, focal = actv$focal_animal), FUN = sum)
trulyNoSocialDataActv		<- fullFocalList[fullFocalList$focalid %in% summarizeActvFocals[summarizeActvFocals$x == 0, "focalid"],]
socialDataNeedsEnteringActv	<- fullFocalList[fullFocalList$focalid %in% summarizeActvFocals[summarizeActvFocals$x > 0, "focalid"],]

fileMakerTrulyNoSocialData	<- focalsNoSocialData[focalsNoSocialData$date < "2013-06-01",]
focalsWithSocialDataOrTrulyNone	<- rbind.data.frame(fullFocalList[fullFocalList$focalid %in% unique(socialDataWithID$focalID), ], trulyNoSocialDataActv, trulyNoSocialDataNN, fileMakerTrulyNoSocialData)
socialDataFinal			<- socialDataWithID[socialDataWithID$focalID %in% unique(focalsWithSocialDataOrTrulyNone$focalid), ]
socialDataScansNeedEntered	<- socialDataWithID[!socialDataWithID$focalID %in% unique(focalsWithSocialDataOrTrulyNone$focalid), ]

write.csv(socialDataFinal, 'socialDataFinalForBLAnalysis2021-12-13.csv', row.names = FALSE)
write.csv(focalsWithSocialDataOrTrulyNone[,1:9], 'focalListFinalForBLAnalysis2021-12-13.csv', row.names = FALSE)

write.csv(socialDataNeedsEnteringActv[,1:9], 'socialDataMissingFocalActivityEntered2021-12-13.csv', row.names = FALSE)
write.csv(socialDataNeedsEnteringNN[,1:9], 'socialDataMissingNearestNeighborEntered2021-12-13.csv', row.names = FALSE)
write.csv(trulyNoSocialDataNN[,1:9], 'NoSocialDataNearestNeighbor2021-12-13.csv', row.names = FALSE)
write.csv(trulyNoSocialDataActv[,1:9], 'NoSocialDataFocalActivity2021-12-13.csv', row.names = FALSE)
write.csv(socialDataScansNeedEntered, 'instantaneousDataMissingSocialEntered2021-12-13.csv', row.names = FALSE)

###############################
### Generate network slices ###
###############################
socialDataWithID	<- read.csv("allSocialDataWithFocalIDs.csv", stringsAsFactors = FALSE)
focalListsBL	<- read.csv("focalListFinalForBLAnalysis2021-12-13.csv", stringsAsFactors = FALSE)
socialDataBL	<- read.csv("socialDataFinalForBLAnalysis2021-12-13.csv", stringsAsFactors = FALSE)

#Need to change this to calculate full community every three months (look within group)
#Do analyses averaging every three months for each group, then average across each group
#Need to integrate the focal lists
#Need to switch it to continuous data

socialDataBL$season	<- ifelse(socialDataBL$monthNum <= 3, 'mating',
				ifelse(socialDataBL$monthNum >= 4 & socialDataBL$month <= 6, 'gestation',
				ifelse(socialDataBL$monthNum >= 7 & socialDataBL$month <= 9, 'birthing', 'lactation')))
focalListsBL$monthNum	<-  as.numeric(data.frame(strsplit(focalListsBL$yearMonth, split = "-"))[2,])
focalListsBL$year		<-  as.numeric(data.frame(strsplit(focalListsBL$yearMonth, split = "-"))[1,])
focalListsBL$season	<- ifelse(focalListsBL$monthNum <= 3, 'mating',
				ifelse(focalListsBL$monthNum >= 4 & focalListsBL$month <= 6, 'gestation',
				ifelse(focalListsBL$monthNum >= 7 & focalListsBL$month <= 9, 'birthing', 'lactation')))
socialDataBL$seasonID	<- factor(factor(socialDataBL$Year):factor(socialDataBL$season,levels=c("mating","gestation","birthing","lactation")))
focalListsBL$seasonID	<- factor(factor(focalListsBL$year):factor(focalListsBL$season,levels=c("mating","gestation","birthing","lactation")))

#duration is in minutes
focalListsBL$start_time 	<- as.POSIXlt(focalListsBL$start_time, format = "%H:%M:%S")
focalListsBL$stop_time 		<- as.POSIXlt(focalListsBL$stop_time, format = "%H:%M:%S")
focalListsBL$focal_duration	<- focalListsBL$stop_time - focalListsBL$start_time

focalDurationPerSeason		<- aggregate(focalListsBL$focal_duration, by = list(focalListsBL$seasonID), FUN = sum)
minimumTotalFocalDuration	<- 600
enoughData				<- focalDurationPerSeason[focalDurationPerSeason$x>=minimumTotalFocalDuration,]

#####################################################################
### Calculate three month networks across population average time ###
#####################################################################
seasonIDs	<- enoughData[,1]
listNets	<- list()

for(i in seasonIDs){
	print(paste('Working on', i))
	data		<- socialDataBL[socialDataBL$seasonID == i,]
	focals	<- as.character(unique(data[,c('Focal')]))
	init		<- as.character(unique(data[,c('Initiator')]))
	recip		<- as.character(unique(data[,c('Receiver')]))
	animals	<- sort(unique(c(focals, init, recip)))
	
	#Use network construction function to create grooming contact agonistic network 
	#Use obsMat function to create observation matrix
	#Divide out obsMat to create rates
	#Save rate to lists for behavior
}

#################################################
### Calculate Edge Differentiability and plot ###
#################################################

edgeDiffSummary	<- data.frame(group = character(), period = character(), year = character(), nAnimals = numeric(), edgeDiff = numeric())
for(i in 1:length(listNets)){
	#print(names(listNets)
	group		<- str_split(names(listNets)[i], '-')[[1]][1]
	#print(class(group))
	year		<- str_split(names(listNets)[i], '-')[[1]][2]
	#print(class(year))
	season	<- str_split(names(listNets)[i], '-')[[1]][3]
	noDiag	<- diag.remove(listNets[[i]])
	edgeDiff	<- sd(noDiag, na.rm = TRUE)/mean(noDiag, na.rm = TRUE)
	n		<- dim(noDiag)[1]
	#print(class(n))
	#print(class(edgeDiff))
	line		<- c(group, season, year, as.numeric(n), as.numeric(edgeDiff))
	edgeDiffSummary	<- rbind(edgeDiffSummary, line)
	colnames(edgeDiffSummary)	<- c('group', 'period', 'year', 'nAnimals', 'edgeDiff')
	edgeDiffSummary$group		<- as.character(edgeDiffSummary$group)	
	edgeDiffSummary$year		<- as.character(edgeDiffSummary$year)	
	edgeDiffSummary$period		<- as.character(edgeDiffSummary$period)	
	edgeDiffSummary$nAnimals	<- as.character(edgeDiffSummary$nAnimals)	
	edgeDiffSummary$edgeDiff	<- as.character(edgeDiffSummary$edgeDiff)	

}

