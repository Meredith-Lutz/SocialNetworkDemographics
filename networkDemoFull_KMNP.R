#################################################
##### Long term network demographics - KMNP #####
#####      Last updated by ML 7/20/2022     #####
#################################################
#setwd('G:/My Drive/Graduate School/Research/Projects/KMNPLongTermData/NSF Analyses')
setwd('C:/Users/mclutz/Documents/KMNP_Demographics')

library(stringr)
library(lme4)
library(lubridate)
library(igraph)
library(sna)
library(asnipe)

#source('G:/My Drive/Graduate School/Research/Projects/KMNPLongTermData/NSF Analyses/NSFSocialNetwork/ObservationTimeFunctions.R')
source('C:/Users/mclutz/Documents/KMNP_Demographics/ObservationTimeFunctions.R')
source('C:/Users/mclutz/Documents/KMNP_Demographics/smallNetworkDoublePermuationsWeightedDirectional.R')

socialDataRaw		<- read.csv('allSocialDataWithFocalIDs2022-08-07_ML.csv', stringsAsFactors = FALSE)
groups			<- read.csv('Compiled Group File with some data deleted for BL analysis_Nov 3 2021_ML Corrected11Nov2021_NoBlanks.csv', stringsAsFactors = FALSE)
nnFocalList			<- read.csv('NearestNeighborIDs_TMM_ML_01Dec2021.csv', stringsAsFactors = FALSE)
actvFocalList		<- read.csv('FocalActivityIDs_TMM_ML_01Dec2021.csv', stringsAsFactors = FALSE)
filemakerFocalList	<- read.csv('FileMakerIDs_ML_06Dec2021.csv', stringsAsFactors = FALSE)
nn				<- read.csv('NearestNeighbor_TMM_ML_01Dec2021.csv', stringsAsFactors = FALSE)
actv				<- read.csv('FocalActivity_TMM_ML_11Nov2021.csv', stringsAsFactors = FALSE)
fm				<- read.csv('FileMaker_ML_01Dec2021.csv', stringsAsFactors = FALSE)
kinship			<- read.csv('Sifaka_relatedness_Kingroup_2015_12_01.csv')

demo				<- read.csv('Copy of life.history.TMM with becca comments about conflicting info Feb10_2021_ML.csv', stringsAsFactors = FALSE)
demo$Name			<- str_to_title(demo$Name, locale = "en")
demo$Sex			<- ifelse(demo$Sex == '', 'unknown', as.character(demo$Sex))
sifakaNames			<- demo$Name
demo[demo$Name == 'Barea', 'Birth.Year']	<- ''
demo[demo$Sex == '', 'Sex']	<- NA
demo[demo$Sex == 'unknown', 'Sex']	<- NA
demo[demo$Birth.Year == '', 'Birth.Year']	<- NA

kinship			<- kinship[,c(2:76)]
colnames(kinship)[29]	<- 'Camilla'
rownames(kinship)		<- colnames(kinship)
kinship			<- as.matrix(kinship)

socialDataAllRaw		<- socialDataRaw[,c('OriginalFile', 'Observer', 'Obs.ID', 'Date', 'Month', 'Year', 'Focal', 'Start', 'Stop',
					'Duration', 'Duration.Seconds', 'Initiator', 'Receiver', 'Context', 'Behavior', 'Species',
					'Tree.number', 'Response', 'To', 'Win', 'Comments', 'Cleaning.Comments', 'StudentOb.YN', 'focalID')]

# Change baby names to match LH file
socialDataAllRaw$Initiator	<- gsub('Vanilla_baby_2011', 'Vanillababy2011', socialDataAllRaw$Initiator)
socialDataAllRaw$Initiator	<- gsub('Savannah_baby_2011', 'Savannahbaby2011', socialDataAllRaw$Initiator)
socialDataAllRaw$Receiver	<- gsub('Vanilla_baby_2011', 'Vanillababy2011', socialDataAllRaw$Receiver)
socialDataAllRaw$Receiver	<- gsub('Savannah_baby_2011', 'Savannahbaby2011', socialDataAllRaw$Receiver)

# Remove lines that have non-identified individuals, n=667 removed, n=137769 left
socialData		<- socialDataAllRaw[socialDataAllRaw$Initiator %in% sifakaNames & socialDataAllRaw$Receiver %in% sifakaNames,]
socialDataRemoved	<- socialDataAllRaw[!(socialDataAllRaw$Initiator %in% sifakaNames & socialDataAllRaw$Receiver %in% sifakaNames),]

#Visits
groups$visit	<- 'N'
groups[groups$animal == 'Spirit' & groups$date == '2019-02-01' & groups$group == 'IV', 'visit']	<- 'Y'
groups[groups$animal == 'William' & groups$date == '2019-02-01' & groups$group == 'IV', 'visit']	<- 'Y'
groups[groups$animal == 'Spirit' & groups$date == '2019-02-04' & groups$group == 'XI', 'visit']	<- 'Y'
groups[groups$animal == 'Zavona' & groups$date == '2019-02-04' & groups$group == 'XI', 'visit']	<- 'Y'
groups[groups$animal == 'Spirit' & groups$date == '2019-03-10' & groups$group == 'XI', 'visit']	<- 'Y'
groups[groups$animal == 'Zavona' & groups$date == '2019-03-10' & groups$group == 'XI', 'visit']	<- 'Y'
groups[groups$animal == 'Valdes' & groups$date == '2017-10-16' & groups$group == 'III', 'visit']	<- 'Y'
groups[groups$animal == 'Zoma' & groups$date == '2018-06-02' & groups$group == 'II', 'visit']	<- 'Y'
groups[groups$animal == 'Spirit' & groups$date == '2019-07-26' & groups$group == 'III', 'visit']	<- 'Y'
groups[groups$animal == 'Spirit' & groups$date == '2019-08-11' & groups$group == 'Solitary', 'visit']	<- 'Y'
groups[groups$animal == 'Zavona' & groups$date == '2019-09-23' & groups$group == 'III', 'visit']	<- 'Y'
groupsNoVisits	<- groups[groups$visit == 'N',]

#Merging groupNames back onto socialData
socialData	<- merge(socialData, groupsNoVisits[,1:3], by.x = c("Date", "Focal"),by.y = c("date", "animal"), all.x = TRUE)

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

#Remove ad-lib/all occurances lines
socialData	<- socialData[socialData$Receiver == socialData$Focal | socialData$Initiator == socialData$Focal,]

#For avg group size
census	<- groups[groups$dataset == 'Census',]
animalPerGroup	<- aggregate(census$animal, by = list(date = census$date, group = census$group), FUN = length)
animalPerGroupStudyGroup	<- animalPerGroup[animalPerGroup$group %in% c('I', 'II', 'III', 'IV', 'V', 'VI', 'XI', 'XII') & animalPerGroup$x > 1,]
######################################################
### Combine Focal Lists and Create Observation MAT ###
######################################################
fullFocalList	<- rbind.data.frame(filemakerFocalList, nnFocalList, actvFocalList, stringsAsFactors = FALSE)
fullFocalList	<- fullFocalList[order(fullFocalList$date, fullFocalList$start_time),]
fullFocalList$yearMonth	<- substr(fullFocalList$date,1,7)

socialDataFullFocalList	<- socialData[socialData$focalID %in% fullFocalList$focalid,]

focalsNoSocialData		<- fullFocalList[!(fullFocalList$focalid %in% unique(socialDataFullFocalList$focalID)),]
summarizeNNFocals			<- aggregate(nn$yes_socialdata, by = list(focalid = nn$focalid, date = nn$date, group = nn$group, focal = nn$focal_animal), FUN = sum, na.rm = TRUE)
trulyNoSocialDataNN		<- fullFocalList[fullFocalList$focalid %in% summarizeNNFocals[summarizeNNFocals$x == 0, "focalid"],]
summarizeActvFocals		<- aggregate(actv$yes_socialdata, by = list(focalid = actv$focalid, date = actv$date, group = actv$group, focal = actv$focal_animal), FUN = sum)
trulyNoSocialDataActv		<- fullFocalList[fullFocalList$focalid %in% summarizeActvFocals[summarizeActvFocals$x == 0, "focalid"],]
fileMakerTrulyNoSocialData	<- focalsNoSocialData[focalsNoSocialData$date < "2013-06-01",]

focalsWithSocialDataOrTrulyNone	<- rbind.data.frame(fullFocalList[fullFocalList$focalid %in% unique(socialDataFullFocalList$focalID), ], 
							trulyNoSocialDataActv, trulyNoSocialDataNN, fileMakerTrulyNoSocialData)


#Pick focals so that they aren't overlapping
uniqueDates	<- unique(focalsWithSocialDataOrTrulyNone$date)
fullFocalListNonOverlapping	<- data.frame()
for(i in uniqueDates){
	print(i)
	groupsObserved	<- unique(focalsWithSocialDataOrTrulyNone[focalsWithSocialDataOrTrulyNone$date == i,"group"])
	for(j in groupsObserved){
		print(j)
		subset	<- focalsWithSocialDataOrTrulyNone[focalsWithSocialDataOrTrulyNone$group == j & focalsWithSocialDataOrTrulyNone$date == i,]
		#print(dim(subset))
		subset$start_time 	<- as.POSIXlt(subset$start_time, format = "%H:%M:%S")
		subset$stop_time 		<- as.POSIXlt(subset$stop_time, format = "%H:%M:%S")
		subset			<- subset[order(subset$start_time),]
		if(dim(subset)[1] >= 2){
			for(k in 1:(dim(subset)[1] - 1)){
				for(m in (k+1):(dim(subset)[1])){
					#print(k)
					#print(m)
					#print(dim(subset))
					focal1	<- interval(start = subset[k,"start_time"], end = subset[k,"stop_time"])
					focal2	<- interval(start = subset[m,"start_time"], end = subset[m,"stop_time"])
					if(is.na(focal1)== FALSE & is.na(focal2) == FALSE){
						if(int_overlaps(focal1,focal2)){
							observers		<- unique(subset[c(k, m), "observer"])
							overlappingFocals	<- subset[c(k, m),]
							if (length(observers) > 1){
								observer1		<- observers[1]
								observerToKeep	<- ifelse(observer1 == "Becca", "Becca",
												ifelse(observer1 == "Meredith", "Meredith",
												ifelse(observer1 == "Max", "Max",
												ifelse(observer1 == "Max and Becca", "Max and Becca",
												ifelse(observer1 == "Patrick", "Patrick", 
								          			ifelse(observer1 == "Andry", "Andry", 
												ifelse(observer1 == "Daniel", "Daniel", 
												ifelse(observer1 == "Dessy", "Dessy", 
												ifelse(observer1 == "Francis", "Francis", 
												ifelse(observer1 == "Laura", "Laura", 
												ifelse(observer1 == "Mampionona", "Mampionona", 
												ifelse(observer1 == "Elvis", "Elvis", "Felana"
												))))))))))))
								idToRemove	<- overlappingFocals[overlappingFocals$observer != observerToKeep, "focalid"]
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

socialDataMatching	<- socialData[socialData$focalID %in% fullFocalListNonOverlapping$focalid,]
focalListMatching		<- fullFocalListNonOverlapping[fullFocalListNonOverlapping$yearMonth <= '2019-12',]

###############################
### Generate network slices ###
###############################
socialDataMatching$monthNum	<- as.numeric(as.character(socialDataMatching$monthNum))
focalListMatching$monthNum	<- as.numeric(matrix(unlist(strsplit(as.character(focalListMatching$yearMonth), '-')), ncol = 2, byrow = TRUE)[,2])
socialDataMatching$season	<- ifelse(socialDataMatching$monthNum <= 3, 'mating',
					ifelse(socialDataMatching$monthNum >= 4 & socialDataMatching$month <= 6, 'gestation',
					ifelse(socialDataMatching$monthNum >= 7 & socialDataMatching$month <= 9, 'birthing', 'lactation')))
focalListMatching$monthNum	<- as.numeric(t(data.frame(strsplit(focalListMatching$yearMonth, split = "-"))[2,]))
focalListMatching$year		<- as.numeric(t(data.frame(strsplit(focalListMatching$yearMonth, split = "-"))[1,]))
focalListMatching$season	<- ifelse(focalListMatching$monthNum <= 3, 'mating',
					ifelse(focalListMatching$monthNum >= 4 & focalListMatching$month <= 6, 'gestation',
					ifelse(focalListMatching$monthNum >= 7 & focalListMatching$month <= 9, 'birthing', 'lactation')))
socialDataMatching$seasonID	<- factor(factor(socialDataMatching$Year):factor(socialDataMatching$season, levels = c("mating", "gestation", "birthing", "lactation")))
focalListMatching$seasonID	<- factor(factor(focalListMatching$year):factor(focalListMatching$season, levels = c("mating", "gestation", "birthing", "lactation")))

#duration is in minutes
focalListMatching$start_time 		<- as.POSIXlt(focalListMatching$start_time, format = "%H:%M:%S")
focalListMatching$stop_time 		<- as.POSIXlt(focalListMatching$stop_time, format = "%H:%M:%S")
focalListMatching$focal_duration	<- focalListMatching$stop_time - focalListMatching$start_time

focalDurationPerSeason				<- aggregate(focalListMatching$focal_duration, by = list(focalListMatching$seasonID), FUN = sum)
focalDurationPerGroup				<- aggregate(focalListMatching$focal_duration, by = list(focalListMatching$group), FUN = sum)
focalDurationPerSeasonPerGroup		<- aggregate(focalListMatching$focal_duration, by = list(season = focalListMatching$seasonID, group = focalListMatching$group), FUN = sum)

minimumTotalFocalDuration	<- 300

enoughDataPerSeasonPerGroup	<- focalDurationPerSeasonPerGroup[focalDurationPerSeasonPerGroup$x >= minimumTotalFocalDuration &
							focalDurationPerSeasonPerGroup$group %in% c('I', 'II', 'III', 'IV', 'V', 'VI', 'XI', 'XII'),]
enoughData				<- aggregate(enoughDataPerSeasonPerGroup$x, by = list(enoughDataPerSeasonPerGroup$season), FUN = sum)
nGroupsPerSeason			<- aggregate(enoughDataPerSeasonPerGroup$x, by = list(enoughDataPerSeasonPerGroup$season), FUN = length)
nSeasonsPerGroup			<- aggregate(enoughDataPerSeasonPerGroup$x, by = list(enoughDataPerSeasonPerGroup$group), FUN = length)

png('enoughDataPerGroup.png', width = 750, height = 750) 
hist(as.numeric(enoughDataPerSeasonPerGroup$x)/60, xlab = 'Hours of Observation Per Group Per Season', col = 'midnightblue', cex.axis = 1.5, cex.lab = 1.5, main = NA)
dev.off()

png('enoughDataPopulation.png', width = 750, height = 750) 
hist(as.numeric(enoughData$x)/60, xlab = 'Population Hours of Observation Per Season', col = 'midnightblue', cex.axis = 1.5, cex.lab = 1.5, main = NA)
dev.off()

###########################################
### Generate example dataset for Damien ###
###########################################

#2 individiuals: gp 4 2019 mating
gp4SocialData	<- socialDataMatching[socialDataMatching$group == 'IV' & socialDataMatching$seasonID == '2019:mating' 
				& socialDataMatching$Focal != 'William' & socialDataMatching$Receiver != 'William', ]
gp4FocalList	<- focalListMatching[focalListMatching$group == 'IV' & focalListMatching$seasonID == '2019:mating', ]

#3 individiuals: gp 12 2019 mating
gp12SocialData	<- socialDataMatching[socialDataMatching$group == 'XII' & socialDataMatching$seasonID == '2019:mating', ]
gp12FocalList	<- focalListMatching[focalListMatching$group == 'XII' & focalListMatching$seasonID == '2019:mating', ]

#6 individiuals: gp 6 2019 mating
gp6SocialData	<- socialDataMatching[socialDataMatching$group == 'VI' & socialDataMatching$seasonID == '2019:mating', ]
gp6FocalList	<- focalListMatching[focalListMatching$group == 'VI' & focalListMatching$seasonID == '2019:mating', ]

socialDataExample	<- rbind.data.frame(gp4SocialData, gp12SocialData, gp6SocialData)
focalListExample	<- rbind.data.frame(gp4FocalList, gp12FocalList, gp6FocalList)

#write.csv(socialDataExample, 'exampleSocialData.csv', row.names = FALSE)
#write.csv(focalListExample, 'exampleFocalList.csv', row.names = FALSE)


################################################
### Functions needed for double permutations ###
################################################

#socialData needs columns: date, group, focalID, actor, subject, focal_individual_id, seasonID
#groups needs columns: date, group, animal
populationDataPerSeason	<- function(socialData, groupsFile, seasonIDs, behav, behavColNum){
	dataList	<- list()
	for(i in 1:length(seasonIDs)){
		seasonID_i	<- seasonIDs[i]
		print(paste('Working on asssembling dataset from', seasonID_i))

		data		<- socialData[as.character(socialData$seasonID) == as.character(seasonID_i) &
					 socialData[, behavColNum] == behav,]
		
		if(nrow(data)> 0){
			groups	<- sort(unique(data$group))
			#print(groups)
			tempData_i	<- data.frame()
			for(j in groups){
				tempData_j		<- data.frame()
				#print(j)
				data_group_j	<- data[data$group == j,]
				data_group_j$focalID	<- as.character(data_group_j$focalID)
				focals		<- sort(as.character(unique(data_group_j$focalID)))

				for(k in focals){
					#print(k)
					#print(data_group_j)
					date_k	<- unique(data_group_j[data_group_j$focalID == k & is.na(data_group_j$focalID) == FALSE, 'date'])
					animals_k	<- unique(groupsFile[groupsFile$group == j & groupsFile$date == date_k, 'animal'])
					#print(date_k)
					#print(animals_k)
					data_k	<- data_group_j[(data_group_j$focalID == k & is.na(data_group_j$focalID) == FALSE), c('focal_individual_id', 'actor', 'subject', 'duration')]
					#print(data_k)
					focal_individual_id_k	<- unique(data_k$focal_individual_id)
					non_focals	<- animals_k[animals_k != focal_individual_id_k]
					edges_k	<- aggregate(data_k$duration, by = list(data_k$focal_individual_id, data_k$actor, data_k$subject), FUN = sum)	
					colnames(edges_k)	<- c('focal_individual_id', 'actor', 'subject', 'duration')
					
					edges_all_k_long	<- data.frame()
					for(m in non_focals){
						if(nrow(edges_k[edges_k$actor == focal_individual_id_k & edges_k$subject == m,]) > 0){
							outLine	<- cbind.data.frame(k, focal_individual_id_k, m,
								as.numeric(edges_k[edges_k$actor == focal_individual_id_k & edges_k$subject == m, 'duration']), 'out')
						}
						else{
							outLine	<- cbind.data.frame(k, focal_individual_id_k, m, 0, 'out')
						}
						colnames(outLine)	<- c('focalID', 'focal_individual_id', 'partner', 'duration', 'direction')
						edges_all_k_long	<- rbind.data.frame(edges_all_k_long, outLine)
					}

					for(m in non_focals){
						if(nrow(edges_k[edges_k$actor == m & edges_k$subject == focal_individual_id_k,]) > 0){
							inLine	<- cbind.data.frame(k, focal_individual_id_k, m,
								as.numeric(edges_k[edges_k$actor == m & edges_k$subject == focal_individual_id_k, 'duration']), 'in')
						}
						else{
							inLine	<- cbind.data.frame(k, focal_individual_id_k, m, 0, 'in')
						}
						colnames(inLine)	<- c('focalID', 'focal_individual_id', 'partner', 'duration', 'direction')
						edges_all_k_long	<- rbind.data.frame(edges_all_k_long, inLine)
					}
						
					edges_all_k_wide	 <- reshape(edges_all_k_long, idvar = c('focalID', 
									'focal_individual_id', 'partner'), timevar = 'direction', direction = 'wide')
					colnames(edges_all_k_wide)	<- c('focalID', 'focal_individual_id', 'partner', 'out', 'in')
					edges_all_k_wide$group	<- rep(j, nrow(edges_all_k_wide))
					tempData_j	<- rbind.data.frame(tempData_j, edges_all_k_wide)
				}
				tempData_i	<- rbind.data.frame(tempData_i, tempData_j)
			}
		}
		dataList[[i]]	<- tempData_i
		names(dataList)[i]	<- as.character(seasonID_i)
	}
	return(dataList)
}			

calculateNetworkFromDoubleEdgeList	<- function(dataWide, seasonID_i, focalList, groupsFile){
	year		<- as.numeric(strsplit(as.character(seasonID_i), split = ":")[[1]][1])
	seasonName	<- strsplit(as.character(seasonID_i), split = ":")[[1]][2]
	startDate	<- ifelse(seasonName == "mating", paste(year, "01-01", sep = "-"), 
				ifelse(seasonName == "gestation",  paste(year, "04-01", sep = "-"), 
				ifelse(seasonName == "birthing",  paste(year, "07-01", sep = "-"),
				paste(year, "10-01", sep = "-"))))
	stopDate	<- ifelse(seasonName == "mating", paste(year, "03-31", sep = "-"), 
				ifelse(seasonName == "gestation",  paste(year, "06-30", sep = "-"), 
				ifelse(seasonName == "birthing",  paste(year, "09-30", sep = "-"),
				paste(year, "12-31", sep = "-"))))

	dataLong	<- reshape(dataWide, idvar = c('focalID', 'focal_individual_id', 'partner', 'group'),
			varying = c('in', 'out'), times = c('in', 'out'), v.name = 'weight', new.row.names = 1:(2*nrow(dataWide)), direction = 'long')
	edgeListIn	<- dataLong[dataLong$time == 'in', c('partner', 'focal_individual_id', 'weight', 'group')]
	edgeListOut	<- dataLong[dataLong$time == 'out', c('focal_individual_id', 'partner', 'weight', 'group')]
	edgeListAll	<- rbind.data.frame(edgeListOut, edgeListIn)
	colnames(edgeListAll)[1:2]	<- c('actor', 'subject')
	animals	<- sort(unique(c(unique(edgeListAll$actor), unique(edgeListAll$subject))))
	edgeListAll	<- edgeListAll[order(edgeListAll$actor),]

	net		<- graph_from_data_frame(edgeListAll, vertices = animals)
	behavMat	<- as.matrix(as_adjacency_matrix(net, attr = 'weight'))

	obsMat	<- calculateObservationMatrix(focalList, groupsFile, startDate, stopDate, names(V(net)))
	behavMatAdj	<- behavMat/obsMat
	comment(behavMatAdj)	<- as.character(seasonID_i)
	return(behavMatAdj)
}

dyadID	<- function(animal1, animal2){
	blank_IDs	<- rep(NA, length(animal1))
	for(i in 1:length(animal1)){
		ID	<- paste(sort(c(animal1[i], animal2[i]))[1], sort(c(animal1[i], animal2[i]))[2], sep = '')
		blank_IDs[i]	<- ID
	}
	return(blank_IDs)
}

#Need columns in demo: name, sex, birthYear
calculateCovariateMatrices	<- function(net, seasonID_i, kinship, demo){
	year		<- as.numeric(strsplit(as.character(seasonID_i), split = ":")[[1]][1])
	seasonName	<- strsplit(as.character(seasonID_i), split = ":")[[1]][2]
	startDate	<- ifelse(seasonName == "mating", paste(year, "01-01", sep = "-"), 
				ifelse(seasonName == "gestation",  paste(year, "04-01", sep = "-"), 
				ifelse(seasonName == "birthing",  paste(year, "07-01", sep = "-"),
				paste(year, "10-01", sep = "-"))))
	stopDate	<- ifelse(seasonName == "mating", paste(year, "03-31", sep = "-"), 
				ifelse(seasonName == "gestation",  paste(year, "06-30", sep = "-"), 
				ifelse(seasonName == "birthing",  paste(year, "09-30", sep = "-"),
				paste(year, "12-31", sep = "-"))))

	lemurYear	<- as.numeric(ifelse(seasonName == 'Lactation', year, year - 1))

	animals	<- rownames(net)
	kinMat	<- matrix(, nrow = length(animals), ncol = length(animals), dimnames = list(animals, animals))
	ageMat	<- matrix(, nrow = length(animals), ncol = length(animals), dimnames = list(animals, animals))
	sexMat	<- matrix(, nrow = length(animals), ncol = length(animals), dimnames = list(animals, animals))

	for(i in 1:length(animals)){
		for(j in 1:length(animals)){
			animal_i	<- animals[i]
			#print(animal_i)
			animal_j	<- animals[j]
			#print(animal_j)
			sex_i		<- demo[demo$name == animal_i, 'sex']
			sex_j		<- demo[demo$name == animal_j, 'sex']
			birthYear_i	<- as.numeric(demo[demo$name == animal_i, 'birthYear'])
			birthYear_j	<- as.numeric(demo[demo$name == animal_j, 'birthYear'])
			age_i		<- lemurYear - birthYear_i
			age_j		<- lemurYear - birthYear_j
			ageCat_i	<- ifelse(age_i == 0, 1, ifelse(
						age_i == 1 | age_i == 2, 2, ifelse(
						age_i == 3 | age_i == 4, 3, ifelse(
						age_i >= 5, 4, NA))))
			ageCat_j	<- ifelse(age_j == 0, 1, ifelse(
						age_j == 1 | age_j == 2, 2, ifelse(
						age_j == 3 | age_j == 4, 3, ifelse(
						age_j >= 5, 4, NA))))

			if(!(animal_i %in% demo$name)){
				sex_i		<- NA
				ageCat_i	<- NA
			}
			if(!(animal_j %in% demo$name)){
				sex_j		<- NA
				ageCat_j	<- NA
			}

			if(animal_i %in% rownames(kinship) & animal_j %in% colnames(kinship)){
				kinMat[i, j]	<- kinship[rownames(kinship) == animal_i, colnames(kinship) == animal_j]
			}
			if(is.na(sex_i) == FALSE | is.na(sex_j) == FALSE){
				sexMat[i, j]	<- ifelse(sex_i == sex_j, 1, 0)
				#sexMat[i, j]	<- paste(sex_i, sex_j, sep = ":")
			}
			if(is.na(ageCat_i) == FALSE & is.na(ageCat_j) == FALSE){
				ageMat[i, j]	<- ageCat_i - ageCat_j
			}
		}
	}
	return(list(kinMat, ageMat, sexMat))
}

nodePermutationWithinGroup	<- function(network, seasonID, groupsFile){
	animals		<- names(V(graph_from_adjacency_matrix(network)))
	year			<- as.numeric(strsplit(as.character(seasonID), split = ":")[[1]][1])
	seasonName		<- strsplit(as.character(seasonID), split = ":")[[1]][2]
	startDate		<- ifelse(seasonName == "mating", paste(year, "01-01", sep = "-"), 
					ifelse(seasonName == "gestation",  paste(year, "04-01", sep = "-"), 
					ifelse(seasonName == "birthing",  paste(year, "07-01", sep = "-"),
					paste(year, "10-01", sep = "-"))))
	stopDate		<- ifelse(seasonName == "mating", paste(year, "03-31", sep = "-"), 
					ifelse(seasonName == "gestation",  paste(year, "06-30", sep = "-"), 
					ifelse(seasonName == "birthing",  paste(year, "09-30", sep = "-"),
					paste(year, "12-31", sep = "-"))))
	groupsFileSub	<- groupsFile[groupsFile$date >= startDate & groupsFile$date <= stopDate,]

	#convert NaN to NA
	network[is.nan(network)]	<- 0
	net					<- graph_from_adjacency_matrix(network, mode = 'directed', weighted = TRUE, diag = FALSE)
	
	groups	<- c()
	for(i in 1:length(animals)){
		animal_i_daily_groups	<- groupsFileSub[groupsFileSub$animal == animals[i],]
		animal_i_group		<- names(sort(table(animal_i_daily_groups$group), decreasing = TRUE)[1])
		groups[i]	<- animal_i_group
	}
	
	newGroups	<- rperm(groups)
	newNet	<- igraph::permute(net, newGroups)
	
	newMat	<- as_adjacency_matrix(newNet, attr = 'weight', sparse = FALSE, names = TRUE)
	return(newMat)
}

##########################################################################
### Calculate three month networks across population then average time ###
##########################################################################

#Calculate edge lists per season for permuatations
colnames(socialDataMatching)[c(1, 2, 11:13)]	<- c('date', 'focal_individual_id', 'duration', 'actor', 'subject')
realDataList	<- populationDataPerSeason(socialDataMatching, groupsNoVisits, enoughData[,1], 'Groom', 15)	

#Convert edge lists into networks adjusted for obs time 
realNetList	<- list()
for(i in 1:length(realDataList)){
	print(i)
	realNetList[[i]]	<- calculateNetworkFromDoubleEdgeList(realDataList[[i]], names(realDataList)[i], focalListMatching, groupsNoVisits)
	names(realNetList)[i]	<- names(realDataList)[i]
}

#Calculate covariate list
colnames(demo)[c(1, 3, 5)]	<- c('name', 'sex', 'birthYear')
covariateList	<- list()
for(i in 1:length(realNetList)){
	print(paste('Calculating covariates for', names(realNetList)[i]))
	covars	<- calculateCovariateMatrices(realNetList[[i]], names(realNetList)[i], kinship, demo)
	covariateList[[i]]	<- covars
	names(covariateList)[i]	<- names(realNetList)[i]
}

# Create pre-network randomized networks (step 1 double permutation)
nPerm	<- 1000
permutedNets	<- list()
for(i in 1:nPerm){
	print(paste('Working on permutation number', i))
	randNetList	<- list()
	for(j in 1:length(realNetList)){
		dataRand			<- preNetworkWeightedDirectionalRandomization(realDataList[[j]])
		netRand			<- calculateNetworkFromDoubleEdgeList(dataRand, names(realDataList)[j], focalListMatching, groupsNoVisits)
		randNetList[[j]]		<- netRand
		names(realNetList)[j]	<- names(realNetList)[j]
	}
	permutedNets[[i]]	<- randNetList
}
Sys.time()

##Read in objects from cluster
#realNetList	<- readRDS('G:/My Drive/Graduate School/Research/Projects/NetworkDemographics/SocialNetworkDemographics/realNetList.rds')
#realDataList	<- readRDS('G:/My Drive/Graduate School/Research/Projects/NetworkDemographics/SocialNetworkDemographics/realDataList.rds')
#permutedNets	<- readRDS('G:/My Drive/Graduate School/Research/Projects/NetworkDemographics/SocialNetworkDemographics/permutedNets.rds')

#Run second step of double permutation
workingNets		<- c(1:13, 15:16, 18:30)
perSeasonDoublePermResults	<- matrix(, nrow = length(realDataList), ncol = 6, dimnames = list(names(realDataList), c('Coef kin', 'P kin', 'Coef age', 'P age', 'Coef sex', 'P sex')))
for(i in workingNets){ #Run for each time point, then average at end
	print(paste('Now working on 2nd step of permutations for network', names(realNetList)[i]))

	# Force exchanges to happen within group only
	animals		<- names(V(graph_from_adjacency_matrix(realNetList[[i]])))
	rawRand	<- array(, dim = c(nPerm, length(animals), length(animals)))
	seasonID	<- names(realDataList[i])
	for(k in 1:nPerm){
		rawRand[k, ,]	<- nodePermutationWithinGroup(realNetList[[i]], seasonID, groupsNoVisits)
	}

	# Calculate effects
	modelReal		<- mrqap.custom.null(realNetList[[i]] ~ covariateList[[i]][[1]] + covariateList[[i]][[2]] + covariateList[[i]][[3]], rawRand)
	coef.raw.kin	<- modelReal$coefficients[2]
	coef.raw.age	<- modelReal$coefficients[3]
	coef.raw.sex	<- modelReal$coefficients[4]

	# coefficients based on data permutations
	out	<- matrix(, nrow = 6, ncol = nPerm)
	for(j in 1:nPerm){
		modelRand	<- mrqap.dsp(permutedNets[[j]][[i]] ~ covariateList[[i]][[1]] + covariateList[[i]][[2]] + covariateList[[i]][[3]], randomisations = 1, directed = 'directed')
		out[1, j]	<- modelRand$coefficients[2]		#coef kin
		out[2, j]	<- modelRand$test.statistic[2]	#t kin
		out[3, j]	<- modelRand$coefficients[3]		#coef age
		out[4, j]	<- modelRand$test.statistic[3]	#t age
		out[5, j]	<- modelRand$coefficients[4]		#coef sex
		out[6, j]	<- modelRand$test.statistic[4]	#t sex
	}
	coefs.rand.data.kin	<- out[1,]
	coefs.rand.data.age	<- out[3,]
	coefs.rand.data.sex	<- out[5,]
	
	print(paste('Finished calculating coefficients based on permutations for network', names(realNetList)[i]))

	# get controlled effect size
	effect.size.kin <- coef.raw.kin - median(coefs.rand.data.kin)
	effect.size.age <- coef.raw.age - median(coefs.rand.data.age)
	effect.size.sex <- coef.raw.sex - median(coefs.rand.data.sex)

	# control edge values by median of permuations
	nAnimals	<- ncol(as.matrix(permutedNets[[1]][[i]]))
	permutedNetsSeason_iList	<- list()
	for(j in 1:nPerm){
		permutedNetsSeason_iList[[j]]	<- as.matrix(permutedNets[[j]][[i]])
	}
	permutedNetsSeason_iArray	<- array(unlist(permutedNetsSeason_iList), dim = c(nAnimals, nAnimals, nPerm))
	
	network_median		<- apply(permutedNetsSeason_iArray, c(1, 2), median, na.rm = TRUE)
	network_controlled	<- as.matrix(realNetList[[i]]) - network_median

	controlRand			<- array(, dim = c(nPerm, length(animals), length(animals)))
	for(m in 1:nPerm){
		controlRand[m, ,]	<- nodePermutationWithinGroup(network_controlled, seasonID, groupsNoVisits)
	}

	# Calculate effect from controlled data (double permutation)
	modelControlled	<- mrqap.custom.null(network_controlled ~ covariateList[[i]][[1]] + covariateList[[i]][[2]] + covariateList[[i]][[3]], controlRand, directed = 'directed')
	coef.controlled.kin	<- modelControlled$coefficients[2]
	p.node.control.kin	<- modelControlled$P.greater[2]
	coef.controlled.age	<- modelControlled$coefficients[3]
	p.node.control.age	<- modelControlled$P.greater[3]
	coef.controlled.sex	<- modelControlled$coefficients[4]
	p.node.control.sex	<- modelControlled$P.greater[4]

	#Save in results matrix
	perSeasonDoublePermResults[i, 1]	<- coef.controlled.kin
	perSeasonDoublePermResults[i, 2]	<- p.node.control.kin
	perSeasonDoublePermResults[i, 3]	<- coef.controlled.age
	perSeasonDoublePermResults[i, 4]	<- p.node.control.age
	perSeasonDoublePermResults[i, 5]	<- coef.controlled.sex
	perSeasonDoublePermResults[i, 6]	<- p.node.control.sex

}

Sys.time()

meanResults	<- apply(perSeasonDoublePermResults, 2, mean, na.rm = TRUE)

perSeason	<- read.csv('G:/My Drive/Graduate School/Research/Projects/NetworkDemographics/nonDirectionalSex.csv')
perSeason$season	<- as.character(data.frame(strsplit(perSeason$X, ":"))[2,])
perSeason$year	<- as.numeric(data.frame(strsplit(perSeason$X, ":"))[1,])

perSeasonNoNA	<- perSeason[is.na(perSeason$P.kin) == FALSE,]

boxplot(perSeasonNoNA$P.kin ~ as.factor(perSeasonNoNA$season)) #No effect
boxplot(perSeasonNoNA$P.age ~ as.factor(perSeasonNoNA$season)) #increase distance, increase grm
boxplot(perSeasonNoNA$P.sex ~ as.factor(perSeasonNoNA$season)) #grm more w/ like sex