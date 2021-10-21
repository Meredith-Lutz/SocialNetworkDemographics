############################################
##### Demographic changes - migrations #####
#####    Last updated 10/15/21 by ML   #####
############################################
setwd("C:/Users/cecil/OneDrive/Desktop/SDC Work")

socialDataRaw		<- read.csv('All_nonSuppStudent_Social_Data_through_2019_2021_07_09_ML_BL edits for NSFanalysis_Francis duplicates deleted_Jul262021_MLEdits.csv', stringsAsFactors = FALSE)
matingSeasonStudent 	<- read.csv('studentMatingSeason_BL updates Jul232021_MLEdits.csv', stringsAsFactors = FALSE)
laura				<- read.csv('Master file of Laura focal activity data.csv', stringsAsFactors = FALSE)
groups			<- read.csv('Compiled Group File with some data deleted for BL analysis_Sept 2021.csv', stringsAsFactors = FALSE)
nnFocalList			<- read.csv('focal.ids.nn_tmm_25sep2021.csv', stringsAsFactors = FALSE)
actvFocalList		<- read.csv('focal.ids.actv_tmm_25sep2021.csv', stringsAsFactors = FALSE)
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

######################################################
### Combine Focal Lists and Create Observation MAT ###
######################################################
fullFocalList	<- rbind.data.frame(filemakerFocalList, nnFocalList, actvFocalList, stringsAsFactors = FALSE)
fullFocalList	<- fullFocalList[order(fullFocalList$date, fullFocalList$start_time),]

uniqueDays		<- unique(groups$date)
obsMat		<- 0*as.matrix(table(sifakaNames)%*%t(table(sifakaNames)))
for(i in uniqueDays) {
	print(paste("Starting Day", i))
	groupsObserved	<- unique(groups[groups$date == i, "group"])
	print(groupsObserved)
      for(j in groupsObserved) {
		subsetFocalList	<- fullFocalList[fullFocalList$date ==  i & fullFocalList$group == j,]
		print(paste(j, "Has", dim(subsetFocalList)[1], "Focals on", i))
		if(dim(subsetFocalList)[1] == 0){
			next
		}
		animalsPresent	<- groups[groups$date == i & groups$group == j, "animal"]
		for(k in animalsPresent){
 			for(m in animalsPresent){
				focals	<- subsetFocalList[subsetFocalList$focal_animal == k | subsetFocalList$focal_animal == m, ]
				obsTime	<- 10*sum(focals$number_scans,na.rm = TRUE)
				obsMat[rownames(obsMat) == k,colnames(obsMat) == m]	<- obsTime + obsMat[rownames(obsMat) == k,colnames(obsMat) == m]
				obsMat[rownames(obsMat) == m,colnames(obsMat) == k]	<- obsTime + obsMat[rownames(obsMat) == m,colnames(obsMat) == k]
			}
		}
	}
}

##############################################
### Write Functions for Migration Analysis ###
##############################################
pullTimePeriods	<- function(socialData, migrationEventDate, timeWindow,group) {
	migrationEventDate	<- as.Date(migrationEventDate)
	socialData$date	<- as.Date(socialData$date)
	socialDataSub	<- socialData[socialData$group == group,] 
	startDate		<- migrationEventDate - timeWindow
	endDate		<- migrationEventDate + timeWindow
	preData		<- socialDataSub[socialDataSub$date >= startDate & socialDataSub$date <= migrationEventDate, ]
	postData		<- socialDataSub[socialDataSub$date >= migrationEventDate & socialDataSub$date <= postData,]
	return(list(preData,postData)
}


groupStrings	<- data.frame()

for (i in uniqueDays){
	groupsObserved	<- unique(groups[groups$date == i, "group"])
	for (j in groupsObserved){
		animals	<- groups[groups$date == i & groups$group == j,"animal"]
		animalsCat	<- paste(animals,collapse = "")
		newLine	<- cbind.data.frame(i,j,animalsCat)
		groupStrings	<- rbind.data.frame(groupStrings,newLine)
	}
}


groupStrings	<- groupStrings[order(groupStrings$j,groupStrings$i),]
groupStrings$i	<- as.Date(groupStrings$i)
groupChangesSummary	<- data.frame()

groupStringsXII	<- groupStrings[groupStrings$j == "XII",]
groupStringsI	<- groupStrings[groupStrings$j == "I",]
groupStringsII	<- groupStrings[groupStrings$j == "II",]
groupStringsIII	<- groupStrings[groupStrings$j == "III",]
groupStringsIV	<- groupStrings[groupStrings$j == "IV",]
groupStringsV	<- groupStrings[groupStrings$j == "V",]
groupStringsVI	<- groupStrings[groupStrings$j == "VI",]
groupStringsXI	<- groupStrings[groupStrings$j == "XI",]

for (j in unique(groupStringsXI$j)){
	groupSubset	<- groupStringsXI[groupStringsXI$j == j,]
	if (dim(groupSubset)[1] == 1){
		next
	}
	for (k in 2:dim(groupSubset)[1]){
		groupStringK	<- groupStringsXI[k-1,3]	
		groupStringK_1	<- groupStringsXI[k,3]
		dateK			<- groupStringsXI[k-1,1]
		dateK_1		<- groupStringsXI[k,1]
		if (groupStringK != groupStringK_1){
			groupChangeLine	<- cbind.data.frame(j,dateK,dateK_1,groupStringK,groupStringK_1)
			groupChangesSummary	<- rbind.data.frame(groupChangesSummary,groupChangeLine)	
		} 	
	}
	
}

write.csv(groupChangesSummary, "groupChangesSummaryXI.csv", row.names=FALSE)

