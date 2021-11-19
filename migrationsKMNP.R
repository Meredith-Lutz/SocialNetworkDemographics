############################################
##### Demographic changes - migrations #####
#####    Last updated 10/15/21 by ML   #####
############################################
setwd("C:/Users/cecil/OneDrive/Desktop/SDC Work")
library(stringr)
library(lme4)

source("C:/Users/cecil/OneDrive/Desktop/SDC Work/Github Work/NSFSocialNetwork2/NSFSocialNetwork/ObservationTimeFunctions.R")
source("C:/Users/cecil/OneDrive/Desktop/SDC Work/Github Work/SeasonalNetworkAnalyses/createNetworkFunction.R")

socialDataRaw		<- read.csv('All_nonSuppStudent_Social_Data_through_2019_Francis duplicates deleted_Jul262021_ML_2021_11_10 (1).csv', stringsAsFactors = FALSE)
matingSeasonStudent 	<- read.csv('studentMatingSeason_BL updates Jul232021_MLEdits.csv', stringsAsFactors = FALSE)
groups			<- read.csv('Compiled Group File with some data deleted for BL analysis_Nov 3 2021_ML Corrected11Nov2021.csv', stringsAsFactors = FALSE)
nnFocalList			<- read.csv('NearestNeighborIDs_TMM_ML_10Nov2021.csv', stringsAsFactors = FALSE)
actvFocalList		<- read.csv('FocalActivityIDs_TMM_ML_11Nov2021.csv', stringsAsFactors = FALSE)
filemakerFocalList	<- read.csv('FileMakerIDs_ML_11Oct2021.csv', stringsAsFactors = FALSE)

demo				<- read.csv('Copy of life.history.TMM with becca comments about conflicting info Feb10_2021_ML.csv', stringsAsFactors = FALSE)
demo$Name			<- str_to_title(demo$Name, locale = "en")
demo$Sex			<- ifelse(demo$Sex == '', 'unknown', as.character(demo$Sex))
sifakaNames			<- demo$Name

# Combine the mating season student data w/ other social data
matingSeasonStudent$StudentOb.YN	<- 'Y'
matingSeasonStudentSimp	<- matingSeasonStudent[,c('OriginalFile', 'Observer', 'Observation.ID', 'Date', 'Focal',
					'Start', 'Stop', 'Duration', 'Duration.Seconds', 'Initiator', 'Receiver', 'Context',
					'Behavior', 'Species', 'Tree.number', 'Response', 'Response.to', 'Win', 'Comments',
					'Cleaning.changes', 'StudentOb.YN')]
socialDataRawSimp		<- socialDataRaw[,c('OriginalFile', 'Observer', 'Obs.ID', 'Date', 'Focal', 'Start', 'Stop',
					'Duration', 'Duration.Seconds', 'Initiator', 'Receiver', 'Context', 'Behavior', 'Species',
					'Tree.number', 'Response', 'To', 'Win', 'Comments', 'Cleaning.Comments', 'StudentOb.YN')]
colnames(socialDataRawSimp)	<- colnames(matingSeasonStudentSimp)
socialDataAllRaw		<- rbind(socialDataRawSimp, matingSeasonStudentSimp)

# Change baby names to match LH file
socialDataAllRaw$Initiator	<- gsub('Vanilla_baby_2011', 'Vanillababy2011', socialDataAllRaw$Initiator)
socialDataAllRaw$Initiator	<- gsub('Savannah_baby_2011', 'Savannahbaby2011', socialDataAllRaw$Initiator)
socialDataAllRaw$Receiver	<- gsub('Vanilla_baby_2011', 'Vanillababy2011', socialDataAllRaw$Receiver)
socialDataAllRaw$Receiver	<- gsub('Savannah_baby_2011', 'Savannahbaby2011', socialDataAllRaw$Receiver)

# Remove lines that have non-identified individuals, n=667 removed, n=137769 left
socialData		<- socialDataAllRaw[socialDataAllRaw$Initiator %in% sifakaNames & socialDataAllRaw$Receiver %in% sifakaNames,]
socialDataRemoved	<- socialDataAllRaw[!(socialDataAllRaw$Initiator %in% sifakaNames & socialDataAllRaw$Receiver %in% sifakaNames),]

#Fixing errors in groupsFile
#groups$animal	<- gsub('Kelikely ', 'Kelikely', groups$animal)
#groups$sex		<- gsub('unknown', 'Unknown', groups$sex)
#groups$age 		<- gsub('subadult', 'Subadult', groups$age)
#groups$origin	<- gsub('Not Natal', 'Not natal', groups$origin)

#Fixes duplicates, but need to make sure that the comments stay if needed for Becca
#groups		<-groupsRaw[!duplicated(groupsRaw[,c("date","animal")]),]
#groupsLinesRemoved	<-groupsRaw[duplicated(groupsRaw[,c("date","animal")]),]
#write.csv(groups,"groupsEditedML2021-Oct-27.csv",row.names = FALSE)
#write.csv(groupsLinesRemoved,"groupsLinesRemovedML2021-Oct-27.csv",row.names = FALSE)

#Merging groupNames back onto socialData
socialData	<- merge(socialData,groups[,1:3], by.x = c("Date", "Focal"),by.y = c("date", "animal"), all.x = TRUE)

######################################################
### Combine Focal Lists and Create Observation MAT ###
######################################################
fullFocalList	<- rbind.data.frame(filemakerFocalList, nnFocalList, actvFocalList, stringsAsFactors = FALSE)
fullFocalList	<- fullFocalList[order(fullFocalList$date, fullFocalList$start_time),]
fullFocalList$yearMonth	<- substr(fullFocalList$date,1,7)
scansPerMonthPerGroup	<- aggregate(fullFocalList$number_scans,by = list(month=fullFocalList$yearMonth, group = fullFocalList$group),FUN = sum)

scansPerMonthPerGroup	<- scansPerMonthPerGroup[order(scansPerMonthPerGroup$month),]
scansPerMonthPerGroupNoSolitary	<- scansPerMonthPerGroup[scansPerMonthPerGroup$group%in%c("I","II","III","IV","V","VI","XI","XII"),]
plot.default(as.factor(scansPerMonthPerGroupNoSolitary$month), scansPerMonthPerGroupNoSolitary$x/6, col = as.factor(scansPerMonthPerGroupNoSolitary$group), pch = 16, ylab = "Hours of Observation")
legend("topleft", legend = c("I","II","III","IV","V","VI","XI","XII"),col = 1:8, pch = 16)


##Need to create this as a function that can be given to others

##############################################
### Write Functions for Migration Analysis ###
##############################################
pullTimePeriods	<- function(socialData, migrationEventDate, timeWindow,group) {
	migrationEventDate	<- as.Date(migrationEventDate)
	socialData$Date	<- as.Date(socialData$Date)
	socialDataSub	<- socialData[socialData$group == group,]
	startDate		<- migrationEventDate - timeWindow
	endDate		<- migrationEventDate + timeWindow
	preData		<- socialDataSub[socialDataSub$Date >= startDate & socialDataSub$Date <= migrationEventDate, ]
	postData		<- socialDataSub[socialDataSub$Date >= migrationEventDate & socialDataSub$Date <= endDate,]
	return(list(preData,postData))
}
pullTimePeriods(socialData, "2019-02-01", 10, "II")

groupSummary	<- function(groupsFile){
	groupStrings	<- data.frame()
	uniqueDays		<- unique(groupsFile$date)
	for (i in uniqueDays){
		print(i)
		groupsObserved	<- unique(groupsFile[groupsFile$date == i, "group"])
		for (j in groupsObserved){
			animals		<- groupsFile[groupsFile$date == i & groupsFile$group == j, "animal"]
			animalsCat		<- paste(animals, collapse = "")
			newLine		<- cbind.data.frame(i, j, animalsCat)
			print(newLine)
			groupStrings	<- rbind.data.frame(groupStrings, newLine)
		}
	}
	#groupStrings$i	<- as.Date(groupStrings$i, format = "%m/%d/%Y")
	groupStrings	<- groupStrings[order(groupStrings$j, groupStrings$i), ]
	return(groupStrings)
}
groupStrings		<-groupSummary(groups)

identifyGroupChanges	<- function(groupStrings,groupID){
	groupChangesSummary	<- data.frame()
	groupSubset	<- groupStrings[groupStrings$j == groupID,]	
	if (dim(groupSubset)[1] == 1){
		next
	}
	for (k in 2:dim(groupSubset)[1]){
		groupStringK	<- groupSubset[k-1, 3]	
		groupStringK_1	<- groupSubset[k, 3]
		dateK			<- groupSubset[k-1, 1]
		dateK_1		<- groupSubset[k, 1]
		if (groupStringK != groupStringK_1){
			groupChangeLine		<- cbind.data.frame(groupID, dateK, dateK_1, groupStringK, groupStringK_1)
			groupChangesSummary	<- rbind.data.frame(groupChangesSummary, groupChangeLine)	
		} 	
	}
	return(groupChangesSummary)
}

calculatePrePostNets	<- function(socialData, migrationEventDate, timeWindow, group, focalList, groupsFile, behavior, directional = TRUE){
	migrationEventDate	<- as.Date(migrationEventDate) 
	startDate			<- migrationEventDate - timeWindow
	stopDate			<- migrationEventDate + timeWindow
	
	groupsSubset	<- groupsFile[groupsFile$date >= startDate & groupsFile$date <= stopDate & groupsFile$group == group,]
	animals		<- sort(unique(groupsSubset$animal))
	
	prePostSocialData	<- pullTimePeriods(socialData, migrationEventDate, timeWindow, group)
	
	obsMatPre		<- calculateObservationMatrix(focalList = focalList,groupsFile = groupsSubset, startDate = startDate, endDate = migrationEventDate, animals = animals)
	obsMatPost		<- calculateObservationMatrix(focalList = focalList,groupsFile = groupsSubset, startDate = migrationEventDate, endDate = stopDate, animals = animals)

	preMat		<- createNet(prePostSocialData[[1]]$Initiator, prePostSocialData[[1]]$Receiver, prePostSocialData[[1]]$Behavior, behavior, subjects = animals, directional = directional, type = "duration", durs = prePostSocialData[[1]]$Duration.Seconds)
	postMat		<- createNet(prePostSocialData[[2]]$Initiator, prePostSocialData[[2]]$Receiver, prePostSocialData[[2]]$Behavior, behavior, subjects = animals, directional = directional, type = "duration", durs = prePostSocialData[[2]]$Duration.Seconds)
	preMatAdj		<- preMat/obsMatPre
	postMatAdj		<- postMat/obsMatPost
	
	return(list(preMat = preMatAdj, postMat = postMatAdj))
}

#pullTimePeriods(socialData, "2019-02-01", 10, "II")
calculatePrePostNets(socialData, "2019-02-01", 10, "II", focalList = fullFocalList, groupsFile = groups, behavior = "Groom", directional = TRUE) 
#Error in if (durs[1] != FALSE) { : missing value where TRUE/FALSE needed

####################################
### General Network Demographics ###
####################################
#How long is grooming?
socialData	<- merge(socialData,demo[,c(1,3,5:6)],by.x="Initiator",by.y="Name",all.x=TRUE)
colnames(socialData)[22:24]	<- c("Initiator.sex", "Initiator.Birth.Year","Initiator.Birth.Notes")
socialData	<- merge(socialData,demo[,c(1,3,5:6)],by.x="Receiver",by.y="Name",all.x=TRUE)
colnames(socialData)[25:27]	<- c("Receiver.sex", "Receiver.Birth.Year","Receiver.Birth.Notes")

socialData$dyadID	<- ifelse(socialData$Initiator < socialData$Receiver,
				paste(socialData$Initiator,socialData$Receiver,sep=""),
				paste(socialData$Receiver,socialData$Initiator,sep=""))

socialData$sexConcordance	<- ifelse(socialData$Initiator.sex < socialData$Receiver.sex,
				paste(socialData$Initiator.sex,socialData$Receiver.sex,sep=""),
				paste(socialData$Receiver.sex,socialData$Initiator.sex,sep=""))

groom		<- socialData[socialData$Behavior == "Groom" | socialData$Behavior == "Mutual_groom",]

#Malemale and malefemale grooming bouts are shorter than female female
model1	<- glmer(Duration.Seconds ~ sexConcordance + (1|dyadID), data = groom, family = "poisson")

#################################
### Generate migration events ###
#################################
groupsOfInterest			<- c("I", "II", "III", "IV", "V", "VI", "XI", "XII")
groupChangesAll			<- lapply(groupsOfInterest, identifyGroupChanges, groupStrings = groupStrings)
groupChangesAllDataFrame	<- as.data.frame(do.call(rbind,groupChangesAll))

write.csv(groupChangesAllDataFrame, "groupChangesSummary.csv", row.names=FALSE)