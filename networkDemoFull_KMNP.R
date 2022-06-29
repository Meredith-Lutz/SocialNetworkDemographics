#################################################
##### Long term network demographics - KMNP #####
#####      Last updated by ML 6/20/2022     #####
#################################################
setwd('G:/My Drive/Graduate School/Research/Projects/KMNPLongTermData/NSF Analyses')

library(stringr)
library(lme4)
library(lubridate)
library(igraph)
library(ANTs)

source('G:/My Drive/Graduate School/Research/Projects/KMNPLongTermData/NSF Analyses/NSFSocialNetwork/ObservationTimeFunctions.R')
source('G:/My Drive/Graduate School/Research/Projects/TemporalNets/SeasonalNetworkAnalyses/createNetworkFunction.R')

socialDataRaw		<- read.csv('All_nonSuppStudent_Social_Data_through_2019_2022_06_09_MLWithFocalID.csv', stringsAsFactors = FALSE)
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

kinship			<- kinship[,c(2:76)]
colnames(kinship)[29]	<- 'Camilla'
rownames(kinship)		<- colnames(kinship)

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

#Merging groupNames back onto socialData
socialData	<- merge(socialData, groups[,1:3], by.x = c("Date", "Focal"),by.y = c("date", "animal"), all.x = TRUE)

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

focalDurationPerSeason		<- aggregate(focalListMatching$focal_duration, by = list(focalListMatching$seasonID), FUN = sum)
minimumTotalFocalDuration	<- 600
enoughData				<- focalDurationPerSeason[focalDurationPerSeason$x>=minimumTotalFocalDuration,]

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

write.csv(socialDataExample, 'exampleSocialData.csv', row.names = FALSE)
write.csv(focalListExample, 'exampleFocalList.csv', row.names = FALSE)


##########################################################################
### Calculate three month networks across population then average time ###
##########################################################################
#First split into seasons, then calculate net for whole community
#Calculate metrics per season, average seasons

populationNetworksAvgTime	<- function(socialData, focalList, groupsFile, allAnimals, seasonIDs, behav, behavColNum, netList){
	for(i in 1:length(seasonIDs)){
		seasonID_i	<- seasonIDs[i]
		print(paste('Working on', seasonID_i))

		data		<- socialData[as.character(socialData$seasonID) == as.character(seasonID_i) &
					 socialData[, behavColNum] == behav,]

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
		
		if(nrow(data)> 0){
			groups	<- sort(unique(data$group))
			for(j in groups){
				for(k in focals){
					date_k	<- data[data$focalID == k, 'date']
					animals_group_k	<- unique(groupsFile[groupsFile$groups == j & groupsFile$date == k, 'animal']
					data_k	<- data[data$group == group_j & data$focalID == k, c('actor', 'subject', 'duration.seconds')]]
					edges_k	<- aggregate(data$duration.seconds, by = list(data$actor, data$subject), FUN = sum)	
			
					## Add 0's
					## Convert to wide
			

			obsMat	<- calculateObservationMatrix(focalList, groupsFile, startDate, stopDate, allAnimals)
			behavMatAdj	<- behavMat/obsMat
			netList[[i]]<- behavMatAdj
			names(netList)[i]	<- as.character(seasonID_i)
		}
	}
	netList	<- netList[which(!sapply(netList, is.null))]
	return(netList)
}

dyadID	<- function(animal1, animal2){
	blank_IDs	<- rep(NA, length(animal1))
	for(i in 1:length(animal1)){
		ID	<- paste(sort(c(animal1[i], animal2[i]))[1], sort(c(animal1[i], animal2[i]))[2], sep = '')
		blank_IDs[i]	<- ID
	}
	return(blank_IDs)
}

#Calculate the metrics for each net in list and then avg over time

##Need to model all demo together
##Need to use asnipe to do mqrap
calculateMetricsCommunityAvgTime	<- function(netList, demoFile, kinFile){
	metricPerSlice		<- cbind.data.frame(rep(NA, length(netList)), rep(NA, length(netList)), rep(NA, length(netList)))
	kinEdgeList			<- get.data.frame(graph.adjacency(as.matrix(kinFile), weighted = TRUE))
	kinEdgeList$dyadID	<- dyadID(kinEdgeList$from, kinEdgeList$to)
	kinEdgeList			<- kinEdgeList[!duplicated(kinEdgeList$dyadID) & is.na(kinEdgeList$weight) == FALSE,]
	kinEdgeList$weight	<- abs(kinEdgeList$weight)
	
	for(i in 1:length(netList)){
		print(paste('Working on network from', as.character(names(netList)[i])))
		#print(netList[[i]])
		edgeList			<- get.data.frame(graph.adjacency(netList[[i]], weighted = TRUE))
		edgeListNoNA		<- edgeList[is.nan(edgeList$weight) == FALSE,]
		
		edgeListDemo1		<- merge(edgeListNoNA, demoFile, by.x = 'from', by.y = 'Name', all.x = TRUE)
		colnames(edgeListDemo1)	<- c('from', 'to', 'weight', 'fromSex', 'fromBirthYear')
		edgeListDemo2		<- merge(edgeListDemo1, demoFile, by.x = 'to', by.y = 'Name', all.x = TRUE)
		colnames(edgeListDemo2)	<- c('to', 'from', 'edgeWeight', 'fromSex', 'fromBirthYear', 'toSex', 'toBirthYear')
		edgeListDemo2$dyadID	<- dyadID(edgeListDemo2$from, edgeListDemo2$to)

		edgeListDemo		<- merge(edgeListDemo2, kinEdgeList[,3:4], by.x = 'dyadID', by.y = 'dyadID', all.x = TRUE)
		
		edgeListDemo$sexCombo	<- dyadID(edgeListDemo$fromSex, edgeListDemo$toSex)
		edgeListDemo$ageDiff	<- abs(as.numeric(edgeListDemo$fromBirthYear) - as.numeric(edgeListDemo$toBirthYear))
		edgeListDemoSimp		<- edgeListDemo[,c(1, 9:11, 4)]
		colnames(edgeListDemoSimp)	<- c('dyadID', 'kin', 'sexCombo', 'ageDiff', 'edgeWeight')
		
		#edgeListDemoSimp[is.na(edgeListDemoSimp$ageDiff) == TRUE, 'ageDiff']	<- NA
		#edgeListDemoSimp[is.na(edgeListDemoSimp$sexCombo) == TRUE, 'sexCombo']	<- NA
		#edgeListDemoSimp[is.na(edgeListDemoSimp$kin) == TRUE, 'kin']		<- NA

		edgeListDemoDyadSum	<- aggregate(edgeListDemoSimp$edgeWeight, by = list(dyadID = edgeListDemoSimp$dyadID, 
							sexCombo = edgeListDemoSimp$sexCombo, ageDiff = edgeListDemoSimp$ageDiff, kin = edgeListDemoSimp$kin), FUN = sum)
		edgeListDemoDyadSum	<- edgeListDemoDyadSum[edgeListDemoDyadSum$x != Inf,]
		edgeListDemoDyadSum$sexMatch	<- ifelse(edgeListDemoDyadSum$sexCombo == 'femalemale', 0, 1)

		#print(edgeListDemoDyadSum)
		if(nrow(edgeListDemoDyadSum) > 0){
			model1	<- lm(x ~ ageDiff + sexMatch + kin, data = edgeListDemoDyadSum)
			metricPerSlice[i, 1]	<- model1$coef[2] #age
			metricPerSlice[i, 2]	<- model1$coef[3] #sex
			metricPerSlice[i, 3]	<- model1$coef[4] #kinship
		}

		#print(metricPerSlice)
	}
	colnames(metricPerSlice)	<- c('ageCoef', 'sexCoef', 'kinCoef')
	return(apply(metricPerSlice, 2, mean, na.rm = TRUE))
}

seasonIDs	<- enoughData[,1]
grmNets	<- list()
grmNets	<- populationNetworksAvgTime(socialDataMatching, focalListMatching, groups, sifakaNames, seasonIDs, "Groom", 15, grmNets)
grmArray	<- array(unlist(grmNets), c(length(sifakaNames), length(sifakaNames), length(seasonIDs)))
realGrmNetAvgAcrossTime	<- apply(grmArray, 1:2, mean, na.rm = TRUE)
colnames(realGrmNetAvgAcrossTime)	<- rownames(realGrmNetAvgAcrossTime)	<- sifakaNames

##Not sure why happened in 2018 and beyond
# Calculate dyadic effects
obsMetricsGrm	<- calculateMetricsCommunityAvgTime(grmNets, demo[,c(1, 3, 5)], abs(kinship))

coef.trait 		<- realNetModel$coefficients[2]
t.trait		<- realNetModel2$test.statistic[2]
p.node 		<- realNetModel$P.greater[2]

# Calculate data stream permutations for first step of double permutations
networksRand 	<- dataStreamRandomizationsValued(socialData, subjects, obsmat, n.rand = 1000)

# coefficients based on data stream permutations
out <- apply(networksRand, 1, function(x) { 
		model <- mrqap.dsp(x ~ kinship.matrix, randomisations = 1); 
		return(c(model$coefficients[2], model$test.statistic[2])) 
	})
coefs.rand.data	<- out[1,]
t.rand.data 	<- out[2,]
p.rand.data 	<- sum(coef.trait <= coefs.rand.data)/n.perm
p.t 			<- sum(t.trait <= t.rand.data)/n.perm

# get controlled effect size
effect.size <- coef.trait - median(coefs.rand.data)

# control edge values by median of permuations
network_median 		<- apply(networksRand, c(2,3), median)
network_controlled 	<- realNetwork - network_median

# Calculate effect from controlled data (double permutation)
controlledNetModel 	<- mrqap.dsp(network_controlled ~ kinship.matrix, randomisations = n.perm2)
coef.controlled	 	<- controlledNetModel$coefficients[2]
p.node.control	 	<- controlledNetModel$P.greater[2]

##################################################################
### Calculate all time nets for each group then average groups ###
##################################################################
#Do analyses averaging every three months for each group, then average across group
#netList needs to be an array of n length, m for gps
netList	<- list()
groupsObserved	<- unique(focalList$group)
allTimeNetsAvgGp	<- function(socialData, focalList, groupsFile, behav, behavColNum, netList, groups, minPerGroup){
	for(i in 1:length(groups)){
		group_i	<- as.character(groups[i])
		print(paste('Working on group', group_i))

		groupFocalList	<- focalList[focalList$group == group_i,]

		groupFocalDurationPerSeason		<- aggregate(groupFocalList$focal_duration, by = list(groupFocalList$seasonID), FUN = sum)
		enoughData					<- groupFocalDurationPerSeason[groupFocalDurationPerSeason$x >= minPerGroup, ]
		seasonIDs					<- as.character(unique(enoughData[,1]))
		groupData					<- socialData[socialData$group == group_i &
					 					socialData[, behavColNum] == behav & 
										as.character(socialData$seasonID) %in% seasonIDs,]

		focals	<- as.character(unique(groupData[,c('Focal')]))
		init		<- as.character(unique(groupData[,c('Initiator')]))
		recip		<- as.character(unique(groupData[,c('Receiver')]))
		animals	<- sort(unique(c(focals, init, recip)))
		
		tempList	<- list()
		for(j in 1:length(seasonIDs)){
			seasonID_j			<- as.character(seasonIDs[j])
			seasonGroupDataSubset	<- groupData[groupData$seasonID == seasonID_j & is.na(groupData$seasonID) == FALSE,]
			seasonGroupFocalListSub	<- groupFocalList[groupFocalList$seasonID == seasonID_j,]

			if(nrow(seasonGroupDataSubset) > 0){
				behavMat	<- createNet(seasonGroupDataSubset$Initiator, seasonGroupDataSubset$Receiver, seasonGroupDataSubset[, behavColNum], behav,
							subjects = animals, directional = TRUE, type = "duration", durs = seasonGroupDataSubset$Duration.Seconds)
				
				year		<- as.numeric(strsplit(as.character(seasonID_j), split = ":")[[1]][1])
				seasonName	<- strsplit(as.character(seasonID_j), split = ":")[[1]][2]
				startDate	<- ifelse(seasonName == "mating", paste(year, "01-01", sep = "-"), 
							ifelse(seasonName == "gestation",  paste(year, "04-01", sep = "-"), 
							ifelse(seasonName == "birthing",  paste(year, "07-01", sep = "-"),
							paste(year, "10-01", sep = "-"))))
				stopDate	<- ifelse(seasonName == "mating", paste(year, "03-31", sep = "-"), 
							ifelse(seasonName == "gestation",  paste(year, "06-30", sep = "-"), 
							ifelse(seasonName == "birthing",  paste(year, "09-30", sep = "-"),
							paste(year, "12-31", sep = "-"))))

				obsMat			<- calculateObservationMatrix(seasonGroupFocalListSub, groupsFile, startDate, stopDate, animals)
				behavMatAdj			<- behavMat/obsMat
				tempList[[j]]		<- as.matrix(behavMatAdj)
				names(tempList)[j]	<- seasonID_j
			}
		}
		netList[[i]]		<- tempList
		names(netList)[i]		<- group_i
	}
	netList	<- netList[which(!sapply(netList, is.null))]
	return(netList)
}

grmAllTimeAvgGrp	<- allTimeNetsAvgGp(socialDataMatching, focalListMatching, groups, "Groom", 15, netList, groupsObserved, 50)

calculateMetricsAllTimeAvgGrp	<- function(netList, demoFile, kinFile){
	metricPerSlice		<- cbind.data.frame(rep(NA, length(netList)), 3)
	kinEdgeList			<- get.data.frame(graph.adjacency(as.matrix(kinFile), weighted = TRUE))
	kinEdgeList$dyadID	<- dyadID(kinEdgeList$from, kinEdgeList$to)
	kinEdgeList			<- kinEdgeList[!duplicated(kinEdgeList$dyadID) & is.na(kinEdgeList$weight) == FALSE,]
	kinEdgeList$weight	<- abs(kinEdgeList$weight)
	
	for(i in 1:length(netList)){
		group_i	<- names(netList)[i]
		print(paste('Working on group', group_i))
		
		netList	<- netList[which(!sapply(netList, is.null))]

		nSeasons	<- length(netList[[i]])
		tempList	<- cbind.data.frame(rep(NA, nSeasons), 3)

		for(j in 1:nSeasons){
			print(j)

			if(is.null(netList[[i]][[j]]) == FALSE){
				edgeList			<- get.data.frame(graph.adjacency(as.matrix(netList[[i]][[j]]), weighted = TRUE))
				edgeListNoNA		<- edgeList[is.nan(edgeList$weight) == FALSE,]
		
				edgeListDemo1		<- merge(edgeListNoNA, demoFile, by.x = 'from', by.y = 'Name', all.x = TRUE)
				colnames(edgeListDemo1)	<- c('from', 'to', 'weight', 'fromSex', 'fromBirthYear')
				edgeListDemo2		<- merge(edgeListDemo1, demoFile, by.x = 'to', by.y = 'Name', all.x = TRUE)
				colnames(edgeListDemo2)	<- c('to', 'from', 'edgeWeight', 'fromSex', 'fromBirthYear', 'toSex', 'toBirthYear')
				edgeListDemo2$dyadID	<- dyadID(edgeListDemo2$from, edgeListDemo2$to)

				edgeListDemo		<- merge(edgeListDemo2, kinEdgeList[,3:4], by.x = 'dyadID', by.y = 'dyadID', all.x = TRUE)
			
				edgeListDemo$sexCombo	<- dyadID(edgeListDemo$fromSex, edgeListDemo$toSex)
				edgeListDemo$ageDiff	<- abs(as.numeric(edgeListDemo$fromBirthYear) - as.numeric(edgeListDemo$toBirthYear))
				edgeListDemoSimp		<- edgeListDemo[,c(1, 9:11, 4)]
				colnames(edgeListDemoSimp)	<- c('dyadID', 'kin', 'sexCombo', 'ageDiff', 'edgeWeight')
		
				edgeListDemoSimp[is.na(edgeListDemoSimp$ageDiff) == TRUE, 'ageDiff']	<- 'unk'
				edgeListDemoSimp[is.na(edgeListDemoSimp$sexCombo) == TRUE, 'sexCombo']	<- 'unk'
				edgeListDemoSimp[is.na(edgeListDemoSimp$kin) == TRUE, 'kin']		<- 'unk'
			
				edgeListDemoDyadSum	<- aggregate(edgeListDemoSimp$edgeWeight, by = list(dyadID = edgeListDemoSimp$dyadID, 
									sexCombo = edgeListDemoSimp$sexCombo, ageDiff = edgeListDemoSimp$ageDiff, kin = edgeListDemoSimp$kin), FUN = sum)
				edgeListDemoDyadSum	<- edgeListDemoDyadSum[edgeListDemoDyadSum$x != Inf,]
				edgeListDemoDyadSum$sexMatch	<- ifelse(edgeListDemoDyadSum$sexCombo == 'femalemale', 0, 1)

				model1	<- lm(x ~ ageDiff, data = edgeListDemoDyadSum)
				tempList[j, 1]	<- model1$coef[2] #age
				tempList[j, 2]	<- model1$coef[3] #sex
				tempList[j, 3]	<- model1$coef[4] #kinship
			}
		}
		metricPerSlice[i, 1]	<- mean(tempList[,1])
		metricPerSlice[i, 2]	<- mean(tempList[,2])
		metricPerSlice[i, 3]	<- mean(tempList[,3])
	}
	colnames(metricPerSlice)	<- c('ageCoef', 'sexCoef', 'kinCoef')
	return(apply(metricPerSlice, 2, mean, na.rm = TRUE))
}

obsMetricsGrm	<- calculateMetricsAllTimeAvgGrp(grmAllTimeAvgGrp, demo[,c(1, 3, 5)], abs(kinship))


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

