#################################################
#################################################
##### Long term network demographics - KMNP #####
#################################################
#################################################

library(RPostgreSQL)
library(chron)
library(stringr)

## Change for Meredith's directory
setwd('C:/Users/amichel1/Documents/Lemur210105/')

socialData	<- read.csv('All_Social_Data_8-25-20.csv')
scan		<- read.csv('Focal Activity NN combined clean 11-16-20_ML.csv')
demo		<- read.csv('lifeHistoryYOB.csv')

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

scan$monthNum	<- ifelse(scan$Month == 'Jan', '01', 
					ifelse(scan$Month == 'Feb', '02',
					ifelse(scan$Month == 'Mar', '03',
					ifelse(scan$Month == 'Apr', '04',
					ifelse(scan$Month == 'May', '05',
					ifelse(scan$Month == 'Jun', '06',
					ifelse(scan$Month == 'Jul', '07',
					ifelse(scan$Month == 'Aug', '08',
					ifelse(scan$Month == 'Sep', '09',
					ifelse(scan$Month == 'Oct', '10',
					ifelse(scan$Month == 'Nov', '11', '12')))))))))))

socialData$yearMonth	<- paste(socialData$Year, socialData$monthNum, sep = '-')
scan$yearMonth		<- paste(scan$Year, scan$monthNum, sep = '-')

###################################
### Prepping data for Homophily ###
###################################
socialDataSub		<- socialData[socialData$yearMonth >= '2018-06' & socialData$yearMonth <= '2019-05',]
scanDataSub			<- scan[scan$yearMonth >= '2018-06' & scan$yearMonth <= '2019-05',]

gp2	<- sort(c('Savannah', 'William', 'Sadakely', 'Zoma', 'Zavona', 'Spirit'))
gp3	<- sort(c('Velo', 'Thor', 'Venus', 'Vervet'))
gp4	<- sort(c('Polina', 'Valdes'))
gp6	<- sort(c('Emily', 'Nancy', 'Nectar', 'Egret', 'Neptune', 'Mafia', 'Eclair', 'Nyx'))
gp1	<- sort(c('Louise', 'Khaleb', 'Leopard', 'Lojy', 'Lion', 'Lexar'))
gp12	<- sort(c('Chloe', 'Walrus', 'Quentin'))

socialDataSub$group	<- ifelse(socialDataSub$Focal %in% gp2, 2,
					ifelse(socialDataSub$Focal %in% gp3, 3,
					ifelse(socialDataSub$Focal %in% gp4, 4,
					ifelse(socialDataSub$Focal %in% gp6, 6,
					ifelse(socialDataSub$Focal %in% gp12, 12,
					ifelse(socialDataSub$Focal %in% gp1, 1, NA))))))

#Fix the weird Elvis line
scanDataSub[scanDataSub$Observer == 'Elvis', 'Observer']	<- 'Daniel'
#Fix the weird Zena line
socialDataSub[socialDataSub$Initiator == 'Zena', 'Initiator']	<- 'Zavona'
#Fix the weird group III lines
scanDataSub[scanDataSub$Focal %in% gp3, 'Group']	<- 'III'
#Fix the weird group VI lines
scanDataSub[scanDataSub$Focal %in% gp6, 'Group']	<- 'VI'
scanDataSub[scanDataSub$Focal %in% gp1, 'Group']	<- 'I'
scanDataSub[scanDataSub$Focal %in% gp2, 'Group']	<- 'II'
scanDataSub[scanDataSub$Focal %in% gp4, 'Group']	<- 'IV'
scanDataSub[scanDataSub$Focal %in% gp12, 'Group']	<- 'XII'
scanDataSub[scanDataSub$Focal == 'Syrup', 'Group']	<- 'XI'
scanDataSub[scanDataSub$Focal == 'Albert' & scanDataSub$Group == 'VI', 'Group']	<- 'XI'

#################################################################
### Trying to figure out who has which datasets on which days ###
#################################################################
socialDayObserverSum	<- aggregate(socialDataSub$Date, by = list(observer = socialDataSub$Observer, date = socialDataSub$Date), FUN = length)
scanDayObserverSum	<- aggregate(scanDataSub$Date, by = list(observer = scanDataSub$Observer, date = scanDataSub$Date), FUN = length)
andryScan			<- scanDataSub[scanDataSub$Observer == 'Andry',]
danielScan			<- scanDataSub[scanDataSub$Observer == 'Daniel',]
maxScan			<- scanDataSub[scanDataSub$Observer == 'Max',]
francisScan			<- scanDataSub[scanDataSub$Observer == 'Francis',]
patrickScan			<- scanDataSub[scanDataSub$Observer == 'Patrick',]

days		<- sort(unique(c(socialDayObserverSum$date, scanDayObserverSum$date)))
observers	<- sort(unique(c(socialDayObserverSum$observer, scanDayObserverSum$observer)))
daysToRemoveFromScan	<- data.frame(observer = character(), date = character())
daysToRemoveFromSocial	<- data.frame(observer = character(), date = character())
for(i in 1:length(observers)){
	obsNam	<- observers[i]
	print(paste('Checking', obsNam, "'s data for errors"))
	tempDaysToRemoveFromSocial	<- c()
	tempDaysToRemoveFromScan	<- c()
	for(j in days){ #0 means that data wasn't collected
		nScans	<- scanDayObserverSum[scanDayObserverSum$observer == obsNam & scanDayObserverSum$date == j, 3]
		#print(nScans)
		nBehav	<- socialDayObserverSum[socialDayObserverSum$observer == obsNam & socialDayObserverSum$date == j, 3]
		#print(nBehav)
		if(length(nScans) == 0 & length(nBehav) != 0){
			print(paste(obsNam, 'has social data but no scan data for', j))
			tempDaysToRemoveFromSocial	<- c(tempDaysToRemoveFromSocial, j)
		}
		if(length(nScans) != 0 & length(nBehav) == 0){
			print(paste(obsNam, 'has scan data but no social data for', j))
			tempDaysToRemoveFromScan	<- c(tempDaysToRemoveFromScan, j)
		}
	}
	print(paste('Finished checking', i, "'s data for errors."))
	daysToRemoveFromScan		<- rbind(daysToRemoveFromScan, cbind(rep(obsNam, length(tempDaysToRemoveFromScan)), tempDaysToRemoveFromScan))
	daysToRemoveFromSocial		<- rbind(daysToRemoveFromSocial, cbind(rep(obsNam, length(tempDaysToRemoveFromSocial)), tempDaysToRemoveFromSocial))

}

#Need to add Meredith's scan totals from the focal lengths since Scans are occasionally skipped in AO, but doesn't reflect obstime
scanFinal	<- scanDataSub[(scanDataSub$Observer == 'Patrick' & ! (scanDataSub$Date %in% daysToRemoveFromScan[daysToRemoveFromScan[, 1] == 'Patrick', 2])) | 
				(scanDataSub$Observer == 'Andry' & ! (scanDataSub$Date %in% daysToRemoveFromScan[daysToRemoveFromScan[, 1] == 'Andry', 2])) |
				(scanDataSub$Observer == 'Daniel' & ! (scanDataSub$Date %in% daysToRemoveFromScan[daysToRemoveFromScan[, 1] == 'Daniel', 2])) |
				(scanDataSub$Observer == 'Francis' & ! (scanDataSub$Date %in% daysToRemoveFromScan[daysToRemoveFromScan[, 1] == 'Francis', 2])) |
				(scanDataSub$Observer == 'Max' & ! (scanDataSub$Date %in% daysToRemoveFromScan[daysToRemoveFromScan[,1 ] == 'Max', 2])),]

socialFinal	<- socialDataSub[(socialDataSub$Observer == 'Patrick' & ! (socialDataSub$Date %in% daysToRemoveFromSocial[daysToRemoveFromSocial[, 1] == 'Patrick', 2])) | 
				(socialDataSub$Observer == 'Andry' & ! (socialDataSub$Date %in% daysToRemoveFromSocial[daysToRemoveFromSocial[, 1] == 'Andry', 2])) |
				(socialDataSub$Observer == 'Daniel' & ! (socialDataSub$Date %in% daysToRemoveFromSocial[daysToRemoveFromSocial[, 1] == 'Daniel', 2])) |
				(socialDataSub$Observer == 'Francis' & ! (socialDataSub$Date %in% daysToRemoveFromSocial[daysToRemoveFromSocial[, 1] == 'Francis', 2])) |
				(socialDataSub$Observer == 'Meredith') | 
				(socialDataSub$Observer == 'Max'),]

obsTimePerAnimalnoML		<- aggregate(scanFinal[,1], by = list(focal_individual_id = scanFinal$Focal), FUN = length)
colnames(obsTimePerAnimalnoML)	<- c('focal_individual_id', 'numScans')
obsTimePerAnimalnoML$numHours	<- obsTimePerAnimalnoML$numScans/6

###########################################
### Get Meredith's focal hour durations ###
###########################################
## read in obsTimePerAnimalML.csv

obsTimePerAnimalML <- read.csv("obsTimePerAnimalML.csv")

##Final obs counts
obsTimeTotal	<- rbind(obsTimePerAnimalnoML[, c(1, 3)], obsTimePerAnimalML[, c(4, 3)])

obsTimeByAnimal	<- aggregate(obsTimeTotal$numHours, by = list(focal_individual_id = obsTimeTotal$focal_individual_id), FUN = sum)

obsTimeByAnimal$group_id <- ifelse(obsTimeByAnimal$focal_individual_id %in% gp2, 'Verreauxi 2',
                                   ifelse(obsTimeByAnimal$focal_individual_id %in% gp3, 'Verreauxi 3',
                                          ifelse(obsTimeByAnimal$focal_individual_id %in% gp6, 'Verreauxi 6',
                                                 'other')))
obsTimeByAnimal$group_id <- ifelse(obsTimeByAnimal$focal_individual_id %in% c('Nyx','Eclair'), 'other',
                                   obsTimeByAnimal$group_id)
  
#######################################
##### Social Network Demographics #####
#######################################
library(network)
library(chron)
library(stringr)
library(EloRating)
library(statnet)

## Source functions
source('C:/Users/amichel1/Documents/LEMUR/createNetworkFunction.R') #Edge weights are either counts or duration
source('C:/Users/amichel1/Documents/LEMUR/createObsMatrix.R')


###########################################
### Generating monthly overlap matrices ###
###########################################

#First assume that all animals in the group are able to overlap with everyone, then remove the appropriate amounts
verreauxi2	<- sort(c('Savannah', 'William', 'Sadakely', 'Zoma', 'Zavona', 'Spirit'))
verreauxi3	<- sort(c('Velo', 'Thor', 'Venus', 'Vervet'))
verreauxi6	<- sort(c('Emily', 'Nancy', 'Nectar', 'Egret', 'Neptune', 'Mafia'))

listAnimalsByGroup	<- list(verreauxi2, verreauxi3, verreauxi6)
listGroups		<- c('Verreauxi 2', 'Verreauxi 3', 'Verreauxi 6')
listFocalByGroup	<- list(obsTimeByAnimal[obsTimeByAnimal$group_id == listGroups[1],], 
                         obsTimeByAnimal[obsTimeByAnimal$group_id == listGroups[2],],
                         obsTimeByAnimal[obsTimeByAnimal$group_id == listGroups[3],])
allFocalDurations <- listFocalByGroup

allObsMatList	<- lapply(allFocalDurations, createObsMatrix) #Need to adjust for Vo, Rk, and Af


################################################################################
### Correct for times when one individual wasn't in the group the whole time ###
################################################################################
scanFinal$Date <- as.character(scanFinal$Date)
#Sadakely disappears on 9/25/2018
scanSadakelyRemove		<- scanFinal[scanFinal$Date >= '2018-09-26' & scanFinal$Group == 'II', ]
scanSadakelySummaryRemovenoML	<- aggregate(scanSadakelyRemove$Focal, by = list(focal_individual_id = scanSadakelyRemove$Focal), FUN = length)
scanSadakelySummaryRemovenoML$numHours	<- scanSadakelySummaryRemovenoML$x/6
focalListSadakelyRemove		<- obsTimePerAnimalML[obsTimePerAnimalML$focal_individual_id %in% gp2,]
obsTimeToRemoveSadakely		<- rbind(scanSadakelySummaryRemovenoML[, c(1, 3)], focalListSadakelyRemove[, c(4, 3)])
obsTimeFinalToRemoveSadakely	<- aggregate(obsTimeToRemoveSadakely$numHours, by = list(focal_individual_id = obsTimeToRemoveSadakely$focal_individual_id), FUN = sum)

#Zoma disappears on 9/28/2018
scanZomaRemove		<- scanFinal[scanFinal$Date >= '2018-09-29' & scanFinal$Group == 'II', ]
scanZomaSummaryRemovenoML	<- aggregate(scanZomaRemove$Focal, by = list(focal_individual_id = scanZomaRemove$Focal), FUN = length)
scanZomaSummaryRemovenoML$numHours	<- scanZomaSummaryRemovenoML$x/6
focalListZomaRemove		<- obsTimePerAnimalML[obsTimePerAnimalML$focal_individual_id %in% gp2,]
obsTimeToRemoveZoma		<- rbind(scanZomaSummaryRemovenoML[, c(1, 3)], focalListZomaRemove[, c(4, 3)])
obsTimeFinalToRemoveZoma	<- aggregate(obsTimeToRemoveZoma$numHours, by = list(focal_individual_id = obsTimeToRemoveZoma$focal_individual_id), FUN = sum)

#Egret disappears on 8/12/2018
scanEgretRemove		<- scanFinal[scanFinal$Date >= '2018-08-13' & scanFinal$Group == 'VI', ]
scanEgretSummaryRemovenoML	<- aggregate(scanEgretRemove$Focal, by = list(focal_individual_id = scanEgretRemove$Focal), FUN = length)
scanEgretSummaryRemovenoML$numHours	<- scanEgretSummaryRemovenoML$x/6
focalListEgretRemove		<- obsTimePerAnimalML[obsTimePerAnimalML$focal_individual_id %in% gp6,]
obsTimeToRemoveEgret		<- rbind(scanEgretSummaryRemovenoML[, c(1, 3)], focalListEgretRemove[, c(4, 3)])
obsTimeFinalToRemoveEgret	<- aggregate(obsTimeToRemoveEgret$numHours, by = list(focal_individual_id = obsTimeToRemoveEgret$focal_individual_id), FUN = sum)

obsTimesToRemove	<- list(obsTimeFinalToRemoveSadakely, obsTimeFinalToRemoveZoma, obsTimeFinalToRemoveEgret)
problemAnimals	<- c('Sadakely', 'Zoma', 'Egret')

## Post Sadakely
postSa		<- obsTimesToRemove[[1]]
focalTableV2	<- postSa
postSaFocalTable	<- data.frame(focal_individual_id = listAnimalsByGroup[[1]], duration = c(0, focalTableV2[2:6,2]))
postSaObsMat	<- createObsMatrix(postSaFocalTable)
nonSa			<- c(1,0,0,0,0,0)
Sa			<- rep(1, 6)
zeroingMatV2	<- matrix(c(Sa, rep(nonSa, 5)), nrow = 6, ncol = 6)
subMatSaV2		<- postSaObsMat * zeroingMatV2

## Post Zoma
postZo		<- obsTimesToRemove[[2]]
focalTableV2	<- postZo
postZoFocalTable	<- data.frame(focal_individual_id = listAnimalsByGroup[[1]], duration = c(focalTableV2[1:5,2], 0))
postZoObsMat	<- createObsMatrix(postZoFocalTable)
nonZo			<- c(0,0,0,0,0,1)
Zo			<- rep(1, 6)
zeroingMatV2	<- matrix(c(rep(nonZo, 5), Zo), nrow = 6, ncol = 6)
subMatZoV2		<- postZoObsMat * zeroingMatV2

## Post Egret
postEg		<- obsTimesToRemove[[3]]
focalTableV6	<- postEg
postEgFocalTable	<- data.frame(focal_individual_id = verreauxi6, duration = c(0, focalTableV6[2:6,2]))
postEgObsMat	<- createObsMatrix(postEgFocalTable)
nonEg			<- c(1, rep(0, 5))
Eg			<- rep(1, 6)
zeroingMatV6	<- matrix(c(Eg, rep(nonEg, 5)), nrow = 6, ncol = 6)
subMatV6		<- postEgObsMat * zeroingMatV6

allObsMatList	<- list(allObsMatList[[1]] - subMatSaV2 - subMatZoV2, 
                      allObsMatList[[2]], allObsMatList[[3]] - subMatV6)

################################
### Removing problem animals ###
################################
#socialData1	<- socialData[socialData$actor != 'Ml' & socialData$subject != 'Ml' & socialData$actor != 'UNK' & socialData$subject != 'UNK',]
#socialData2	<- socialData1[!((socialData1$actor == 'Vo' | socialData1$subject == 'Vo') & socialData1$startTime >= '2019-09-28'),]

socialData2 <- socialFinal

##########################################
### Organizing demographic information ###
##########################################
v2demo	<- demo[demo$Individual.ID %in% verreauxi2, ]
v3demo	<- demo[demo$Individual.ID %in% verreauxi3, ]
v6demo	<- demo[demo$Individual.ID %in% verreauxi6, ]

###########################################
### Calculate covariate matrices ##########
###########################################
v2AgeDiff	<- 0*table(verreauxi2)%*%t(table(verreauxi2))
for(i in verreauxi2){
  for(j in verreauxi2){
    actorAge		<- v2demo[v2demo$Individual.ID == i,]$birthYear
    recipAge		<- v2demo[v2demo$Individual.ID == j,]$birthYear
    v2AgeDiff[i, j]	<- abs(actorAge - recipAge)
  }
}

v3AgeDiff	<- 0*table(verreauxi3)%*%t(table(verreauxi3))
for(i in verreauxi3){
  for(j in verreauxi3){
    actorAge		<- v3demo[v3demo$Individual.ID == i,]$birthYear
    recipAge		<- v3demo[v3demo$Individual.ID == j,]$birthYear
    v3AgeDiff[i, j]	<- abs(actorAge - recipAge)
  }
}

v6AgeDiff	<- 0*table(verreauxi6)%*%t(table(verreauxi6))
for(i in verreauxi6){
  for(j in verreauxi6){
    actorAge		<- v6demo[v6demo$Individual.ID == i,]$birthYear
    recipAge		<- v6demo[v6demo$Individual.ID == j,]$birthYear
    v6AgeDiff[i, j]	<- abs(actorAge - recipAge)
  }
}



#############################
## Fix behavior categories ##
#############################

#note - excludes categories that had 0 obs in this dataset, such as "Groom_out_of_sight"
socialData2$behavior <- ifelse(socialData2$Behavior %in% c("Play", "Play_out_of_sight"), "Play",
                               ifelse(socialData2$Behavior %in% c("Groom","Mutual_groom"), "Groom",
                                      "not-play-groom"))
socialData2$duration <- socialData2$Duration.Seconds
socialData2$actor <- socialData2$Initiator
socialData2$subject <- socialData2$Receiver

######################
### Create network ###
######################
v2plymat	<- createNet(socialData2$actor, socialData2$subject, socialData2$behavior, 'Play', subjects = verreauxi2, type = 'duration', durs = socialData2$duration)
v3plymat	<- createNet(socialData2$actor, socialData2$subject, socialData2$behavior, 'Play', subjects = verreauxi3, type = 'duration', durs = socialData2$duration)
v6plymat	<- createNet(socialData2$actor, socialData2$subject, socialData2$behavior, 'Play', subjects = verreauxi6, type = 'duration', durs = socialData2$duration)

v2grmmat	<- createNet(socialData2$actor, socialData2$subject, socialData2$behavior, 'Groom', subjects = verreauxi2, type = 'duration', durs = socialData2$duration)
v3grmmat	<- createNet(socialData2$actor, socialData2$subject, socialData2$behavior, 'Groom', subjects = verreauxi3, type = 'duration', durs = socialData2$duration)
v6grmmat	<- createNet(socialData2$actor, socialData2$subject, socialData2$behavior, 'Groom', subjects = verreauxi6, type = 'duration', durs = socialData2$duration)


v2plynet	<- as.network.matrix(v2plymat, directed = TRUE, names.eval = 'numply', ignore.eval = FALSE)
v3plynet	<- as.network.matrix(v3plymat, directed = TRUE, names.eval = 'numply', ignore.eval = FALSE)
v6plynet	<- as.network.matrix(v6plymat, directed = TRUE, names.eval = 'numply', ignore.eval = FALSE)

v2grmnet	<- as.network.matrix(v2grmmat, directed = TRUE, names.eval = 'numply', ignore.eval = FALSE)
v3grmnet	<- as.network.matrix(v3grmmat, directed = TRUE, names.eval = 'numply', ignore.eval = FALSE)
v6grmnet	<- as.network.matrix(v6grmmat, directed = TRUE, names.eval = 'numply', ignore.eval = FALSE)

set.vertex.attribute(v2grmnet, 'sex', as.character(v2demo$sex))
set.vertex.attribute(v2grmnet, 'obstime', as.numeric(allFocalDurations[[1]]$x))
set.vertex.attribute(v2grmnet, 'age', 2019 - v2demo$birthYear)
set.edge.value(v2grmnet, 'grm', v2grmmat) 
set.edge.value(v2grmnet, 'agediff', v2AgeDiff)
set.edge.value(v2grmnet, 'dyadObs', allObsMatList[[1]])
set.edge.value(v2grmnet, 'logObs', log(allObsMatList[[1]]))

set.vertex.attribute(v3plynet, 'sex', as.character(v3demo$sex))
set.vertex.attribute(v3plynet, 'obstime', as.numeric(allFocalDurations[[2]]$x))
set.vertex.attribute(v3plynet, 'age', 2019 - v3demo$birthYear)
set.edge.value(v3plynet, 'grm', v3grmmat) 
set.edge.value(v3plynet, 'agediff', v3AgeDiff)
set.edge.value(v3plynet, 'dyadObs', allObsMatList[[2]])
set.edge.value(v3plynet, 'logObs', log(allObsMatList[[2]]))

set.vertex.attribute(v6plynet, 'sex', as.character(v6demo$sex))
set.vertex.attribute(v6plynet, 'obstime', as.numeric(allFocalDurations[[3]]$x))
set.vertex.attribute(v6plynet, 'age', 2019 - v6demo$birthYear)
set.edge.value(v6plynet, 'ply', v6plymat) 
set.edge.value(v6plynet, 'agediff', v6AgeDiff)
set.edge.value(v6plynet, 'dyadObs', allObsMatList[[3]])
set.edge.value(v6plynet, 'logObs', log(allObsMatList[[3]]))


##############
### ERGM's ###
##############

startTimeModel1	<- Sys.time()

modelv6	<- ergm(v6plynet ~ sum + nodematch('sex', form = 'sum') + edgecov(v6plynet, 'agediff', form = 'sum') + nodecov('age', form = 'sum') + 
                 edgecov(v6plynet, 'logObs', form = 'sum'), response = 'numply', reference = ~Poisson,
               control = control.ergm(MCMC.interval = 5000, MCMLE.maxit = 300, init.method = 'CD', CD.maxit=200, MCMC.samplesize = 1000, seed = 32164))

modelv2	<- ergm(v2plynet ~ sum + nodematch('sex', form = 'sum') + edgecov(v2plynet, 'agediff', form = 'sum') + nodecov('age', form = 'sum') +
                 edgecov(v2plynet, 'logObs', form = 'sum'), response = 'numply', reference = ~Poisson, 
                control = control.ergm(MCMC.interval = 5000, MCMLE.maxit = 300, init.method = 'CD', CD.maxit=200, MCMC.samplesize = 1000, seed = 32164))

modelv3	<- ergm(v3plynet ~ sum + nodematch('sex', form = 'sum') + edgecov(v3plynet, 'agediff', form = 'sum') + nodecov('age', form = 'sum') + 
                 edgecov(v3plynet, 'logObs', form = 'sum'), response = 'numply', reference = ~Poisson, 
                control = control.ergm(MCMC.interval = 5000, MCMLE.maxit = 300, init.method = 'CD', CD.maxit=200, MCMC.samplesize = 1000, seed = 32164))

#saveRDS(modelv3, "modelv3.rds") #save each model to go to other groups

endTimeModel1	<- Sys.time()
print('Run time for model 1 was', endTimeModel1 - startTimeModel1)

summary(model1)
mcmc.diagnostics(modelv6)

startTimeModel2	<- Sys.time()

simulatedModelsv6 <- simulate(modelv6, nsim = 1000, statsonly = TRUE)
obsStatsv6		<- summary(v6plynet ~ sum + #mutual(form = 'min') + transitiveweights('min', 'max', 'min') + nodefactor('sex', form = 'sum') +
                         nodematch('sex', form = 'sum') + edgecov(v6plynet, 'agediff', form = 'sum') + nodecov('age', form = 'sum') + 
                         #edgecov(v6plynet, 'grm', form = 'sum') + 
                         edgecov(v6plynet, 'logObs', form = 'sum'), response = 'numply')

png('modelv6MCMC1.png', width = 4.41, height = 2.5, unit = 'in', res = 300)
par(mar = c(2.5, 2.5, 0.5, 0.5), oma = c(1, 1, 1, 1))
plot(modelv6$sample[,1:3], main = '', xlab = '', cex.axis = 0.75)
dev.off()

png('modelv6MCMC2.png', width = 4.41, height = 2.5, unit = 'in', res = 300)
par(mar = c(2.5, 2.5, 0.5, 0.5), oma = c(1, 1, 1, 1))
plot(modelv6$sample[,4:5], main = '', xlab = '', cex.axis = 0.75)
dev.off()

png('modelv6GOF.png', width = 4.41, height = 2.5, unit = 'in', res = 300)
par(mfrow = c(2, 3), mar = c(2.5, 2.5, 0.5, 0.5), oma = c(1, 1, 1, 1))
for(i in 1:5){
  plot(density(simulatedModelsv6[,i]), xlab = '', main = '', cex.axis = 0.75)
  abline(v = obsStatsv6[i])
}
dev.off()



####################################################
### Calculate CVs and plot effect sizes ############
####################################################

# read in saved models as necessary
modelv3 <- readRDS("modelv3.rds")
modelv6 <- readRDS("modelv6.rds")
modeld2 <- readRDS("modeld2.rds")
modeld3 <- readRDS("modeld3.rds")
modelf2 <- readRDS("modelf2ply.rds")
#modelf3 <- readRDS("modelf3.rds") #used wrong obstime

# extract effect sizes and standard error from model summary
modv6tab <- summary(modelv6)$coefs[2:3,1:2]
modv3tab <- summary(modelv3)$coefs[2:3,1:2]
modd2tab <- summary(modeld2)$coefs[2:3,1:2]
modd3tab <- summary(modeld3)$coefs[2:3,1:2]
modf2tab <- summary(modelf2)$coefs[2:3,1:2]
#modf3tab <- summary(modelf3)$coefs[2:3,1:2]

# for diadema1, got model results from Meredith's paper. Here, write them into a table
modd1tab <- modd2tab
modd1tab[1,1] = 0.0684
modd1tab[2,1] = -0.265
modd1tab[1,2] = 0.0343
modd1tab[2,2] = 0.0116


#plot order = fulvus, diadema, verreauxi
modtab <- rbind(modv3tab, modv6tab, modf2tab, modd1tab, modd2tab, modd3tab)
modtab$var <- c(1,1.4,1.05,1.45,1.1,1.5,1.15,1.55,1.2,1.6,1.25,1.65)


#option to use log instead, but this causes huge variation and it's hard to determine how to do negatives
#modtab$logEst <- ifelse(modtab$Estimate < 0, log(abs(modtab$Estimate)),
#                        0 - log(abs(modtab$Estimate)))
#modtab$logSE <- abs(log(modtab$`Std. Error`))
#modtab$lower <- modtab$logEst - modtab$logSE
#modtab$upper <- modtab$logEst + modtab$logSE

#calculate estimate +/- 2SE
modtab$lower <- modtab$Estimate - 2*modtab$`Std. Error`
modtab$upper <- modtab$Estimate + 2*modtab$`Std. Error`

#make nice colors
lemurcols <- c("midnightblue","midnightblue","midnightblue","midnightblue","midnightblue","midnightblue",
               "#8b5a00ff","#8b5a00ff",
               "#00688bff","#00688bff","#00688bff","#00688bff")
lemurcols1 <- c("midnightblue",
                "#8b5a00ff",
                "#00688bff")

#make the plot
plot(modtab$Estimate, modtab$var, pch = 20, cex = 1.4, 
     col = rev(lemurcols), 
     xlim = c(-1.1,1.1),
     xlab = "Estimate +/- 2SE", 
     ylab = "Sex concordance               Age difference",
     yaxt = 'n')
abline(v = 0, lty = 2, col = "darkgrey")
arrows(modtab$lower, modtab$var, modtab$upper, modtab$var, 
       length=0, angle=90, code=3, lwd = 3,
       col = rev(lemurcols))
legend("bottomleft", bty = "n", pch=16, cex = 0.7,
       col = rev(lemurcols1),
       legend = c("d","f","v"))
       
#make it pretty in Inkscape!

