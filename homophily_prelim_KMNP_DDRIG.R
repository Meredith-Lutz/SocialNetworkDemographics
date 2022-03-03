#################################################
#################################################
##### Long term network demographics - KMNP #####
#################################################
#################################################

library(RPostgreSQL)
library(chron)
library(stringr)


setwd('G:/My Drive/Graduate School/Research/Projects/KMNPLongTermData/Meredith Corrected KMNP Long Term Data')
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
## Connect to database
drv	<- dbDriver('PostgreSQL') ##Be sure to use real database name
con	<- dbConnect(drv, dbname = 'verreauxi_2019_all_data', host = 'localhost', port = 5432,
								 user = 'postgres', password = 'Animalbehavior1#')

focalList	<- dbGetQuery(con, 'select main_tables.list_focals.session_start_time, 
				main_tables.list_focals.focal_start_time, main_tables.list_focals.focal_end_time,
				main_tables.list_focals.focal_individual_id, main_tables.list_sessions.group_id,
				main_tables.list_sessions.pin_code_name from main_tables.list_focals
				left join main_tables.list_sessions
				on main_tables.list_focals.session_start_time = main_tables.list_sessions.session_start_time;')

focal_start_str			<- data.frame(str_split_fixed(as.character(focalList$focal_start_time), ' ', n = 2))
colnames(focal_start_str)	<- c('focal_date', 'focal_start_time')
focal_start_chron			<- chron(dates. = as.character(focal_start_str$focal_date), times. = as.character(focal_start_str$focal_start_time),
						format = c(dates = 'y-m-d', times = 'h:m:s'))
focal_end_str			<- data.frame(str_split_fixed(as.character(focalList$focal_end_time), ' ', n = 2))
colnames(focal_end_str)		<- c('focal_date', 'focal_end_time')
focal_end_chron			<- chron(dates. = as.character(focal_end_str$focal_date), times. = as.character(focal_end_str$focal_end_time),
						format = c(dates = 'y-m-d', times = 'h:m:s'))
focalList$focal_start_chron		<- focal_start_chron
focalList$focal_end_chron		<- focal_end_chron
focalList$duration			<- focalList$focal_end_chron - focalList$focal_start_chron

obsTimePerAnimalML		<- aggregate(focalList$duration*24, by = list(focal_individual_id = focalList$focal_individual_id), FUN = sum)
obsTimePerAnimalML$Focal	<- c('Chloe', 'Eclair', 'Emily', 'Leopard', 'Mafia', 'Nancy', 'Neptune', 'Nectar', 'Nyx', 'Quentin',
						'Spirit', 'Savannah', 'Thor', 'Valdes', 'Velo', 'Venus', 'Vervet', 'William', 'Walrus', 'Zavona')
colnames(obsTimePerAnimalML)	<- c('focal_abb', 'numHours', 'focal_individual_id')

##Final obs counts
obsTimeTotal	<- rbind(obsTimePerAnimalnoML[, c(1, 3)], obsTimePerAnimalML[, c(3, 2)])

obsTimeByAnimal	<- aggregate(obsTimeTotal$numHours, by = list(focal_individual_id = obsTimeTotal$focal_individual_id), FUN = sum)

#################################################
### Generate dataframe of aff amount, obstime ###
#################################################

#Just using gps 1, 2, 3, 6, 12 since 4 and 11 only have 1 animal
socialRates	<- data.frame(actor = character(), recip = character(), group = character(), dyadID = character(), numHours = numeric(), 
		durGrm = numeric(), durPly = numeric(), ageDiff = numeric(), sexMatch = character())

listAnimals	<- list(gp1, gp2, gp3, gp6, gp12)
for(i in 1:length(listAnimals)){
	group	<- listAnimals[[i]]
	for(j in 1:length(group)){
		actor	<- group[[j]]
		for(k in 1:length(group)){
			recip	<- group[[k]]
			group_id	<- i
			dyadID	<- paste(sort(c(actor, recip))[1], sort(c(actor, recip))[2], sep = '')
			numHours	<- obsTimeByAnimal[obsTimeByAnimal$focal_individual_id == actor, 2] + obsTimeByAnimal[obsTimeByAnimal$focal_individual_id == recip, 2]
			groom		<- socialFinal[socialFinal$Initiator == actor & socialFinal$Receiver == recip & (socialFinal$Behavior == 'Groom' | socialFinal$Behavior == 'Mututal_groom'), ]
			groomMin	<- sum(groom$Duration.Seconds)/60
			play		<- socialFinal[socialFinal$Initiator == actor & socialFinal$Receiver == recip & (socialFinal$Behavior == 'Play' | socialFinal$Behavior == 'Play_out_of_sight'), ]
			playMin	<- sum(play$Duration.Seconds)/60
			ageDiff	<- demo[demo$Individual.ID == actor, 'birthYear'] - demo[demo$Individual.ID == recip, 'birthYear']
			sexMatch	<- ifelse(demo[demo$Individual.ID == actor, 'sex'] == 'M' & demo[demo$Individual.ID == recip, 'sex'] == 'M', 'MM',
						ifelse(demo[demo$Individual.ID == actor, 'sex'] == 'F' & demo[demo$Individual.ID == recip, 'sex'] == 'F', 'FF', 'MF'))
			line	<- c(actor, recip, group_id, dyadID, numHours, groomMin, playMin, ageDiff, sexMatch)
			socialRates	<- rbind(socialRates, line)
		}
	}
}

colnames(socialRates)	<- c('actor', 'recip', 'group', 'dyadID', 'numHours', 'durGrm', 'durPly', 'ageDiff', 'sexMatch')

socialRates	<- socialRates[socialRates$actor != socialRates$recip, ]

#Nyx and Eclair aren't consistently recorded so remove them
socialRates	<- socialRates[socialRates$actor != 'Nyx' & socialRates$recip != 'Nyx', ]
socialRates	<- socialRates[socialRates$actor != 'Eclair' & socialRates$recip != 'Eclair', ]

################################################################################
### Correct for times when one individual wasn't in the group the whole time ###
################################################################################
#Lion disappears on 10/23/2018
scanLionRemove			<- scanFinal[scanFinal$Date >= '2018-10-24' & scanFinal$Group == 'I', ]
scanLionSummaryRemovenoML	<- aggregate(scanLionRemove$Focal, by = list(focal_individual_id = scanLionRemove$Focal), FUN = length)
scanLionSummaryRemovenoML$numHours	<- scanLionSummaryRemovenoML$x/6
focalListLionRemove		<- obsTimePerAnimalML[obsTimePerAnimalML$focal_individual_id %in% gp1,]
obsTimeToRemoveLion		<- rbind(scanLionSummaryRemovenoML[, c(1, 3)], focalListLionRemove[, c(3, 2)])
obsTimeFinalToRemoveLion	<- aggregate(obsTimeToRemoveLion$numHours, by = list(focal_individual_id = obsTimeToRemoveLion$focal_individual_id), FUN = sum)
	
#Sadakely disappears on 9/25/2018
scanSadakelyRemove		<- scanFinal[scanFinal$Date >= '2018-09-26' & scanFinal$Group == 'II', ]
scanSadakelySummaryRemovenoML	<- aggregate(scanSadakelyRemove$Focal, by = list(focal_individual_id = scanSadakelyRemove$Focal), FUN = length)
scanSadakelySummaryRemovenoML$numHours	<- scanSadakelySummaryRemovenoML$x/6
focalListSadakelyRemove		<- obsTimePerAnimalML[obsTimePerAnimalML$focal_individual_id %in% gp2,]
obsTimeToRemoveSadakely		<- rbind(scanSadakelySummaryRemovenoML[, c(1, 3)], focalListSadakelyRemove[, c(3, 2)])
obsTimeFinalToRemoveSadakely	<- aggregate(obsTimeToRemoveSadakely$numHours, by = list(focal_individual_id = obsTimeToRemoveSadakely$focal_individual_id), FUN = sum)

#Zoma disappears on 9/28/2018
scanZomaRemove		<- scanFinal[scanFinal$Date >= '2018-09-29' & scanFinal$Group == 'II', ]
scanZomaSummaryRemovenoML	<- aggregate(scanZomaRemove$Focal, by = list(focal_individual_id = scanZomaRemove$Focal), FUN = length)
scanZomaSummaryRemovenoML$numHours	<- scanZomaSummaryRemovenoML$x/6
focalListZomaRemove		<- obsTimePerAnimalML[obsTimePerAnimalML$focal_individual_id %in% gp2,]
obsTimeToRemoveZoma		<- rbind(scanZomaSummaryRemovenoML[, c(1, 3)], focalListZomaRemove[, c(3, 2)])
obsTimeFinalToRemoveZoma	<- aggregate(obsTimeToRemoveZoma$numHours, by = list(focal_individual_id = obsTimeToRemoveZoma$focal_individual_id), FUN = sum)

#Egret disappears on 8/12/2018
scanEgretRemove		<- scanFinal[scanFinal$Date >= '2018-08-13' & scanFinal$Group == 'VI', ]
scanEgretSummaryRemovenoML	<- aggregate(scanEgretRemove$Focal, by = list(focal_individual_id = scanEgretRemove$Focal), FUN = length)
scanEgretSummaryRemovenoML$numHours	<- scanEgretSummaryRemovenoML$x/6
focalListEgretRemove		<- obsTimePerAnimalML[obsTimePerAnimalML$focal_individual_id %in% gp6,]
obsTimeToRemoveEgret		<- rbind(scanEgretSummaryRemovenoML[, c(1, 3)], focalListEgretRemove[, c(3, 2)])
obsTimeFinalToRemoveEgret	<- aggregate(obsTimeToRemoveEgret$numHours, by = list(focal_individual_id = obsTimeToRemoveEgret$focal_individual_id), FUN = sum)

obsTimesToRemove	<- list(obsTimeFinalToRemoveLion, obsTimeFinalToRemoveSadakely, obsTimeFinalToRemoveZoma, obsTimeFinalToRemoveEgret)
problemAnimals	<- c('Lion', 'Sadakely', 'Zoma', 'Egret')
socialRates$adjObsTime	<- NA
socialRates$numHours	<- as.numeric(socialRates$numHours)
for(i in 1:length(obsTimesToRemove)){
	problemID	<- problemAnimals[i]
	for(j in 1:dim(obsTimesToRemove[[i]])[1]){
		recip	<- obsTimesToRemove[[i]][j, 1]
		socialRates[socialRates$actor == problemID & socialRates$recip == recip, ]$adjObsTime	<- socialRates[socialRates$actor == problemID & socialRates$recip == recip, 'numHours'] - obsTimesToRemove[[i]][j, 2]
		socialRates[socialRates$actor == recip & socialRates$recip == problemID, ]$adjObsTime	<- socialRates[socialRates$actor == recip & socialRates$recip == problemID, 'numHours'] - obsTimesToRemove[[i]][j, 2]
	}
}

###Finalized obs times
socialRates[is.na(socialRates$adjObsTime) == TRUE, 'adjObsTime']	<- socialRates[is.na(socialRates$adjObsTime) == TRUE, 'numHours']

#######################
### Actual analysis ###
#######################
socialRates$durPly	<- as.numeric(socialRates$durPly)
socialRates$durGrm	<- as.numeric(socialRates$durGrm)
socialRates$ageDiff	<- as.numeric(socialRates$ageDiff)

socialRates$plyRate	<- socialRates$durPly/socialRates$adjObsTime
socialRates$grmRate	<- socialRates$durGrm/socialRates$adjObsTime
socialRates$affRate	<- (socialRates$durPly + socialRates$durGrm)/socialRates$adjObsTime

#Directional-ish
#Age effect on affiliation (closer in age affiliate more), MM affiliate the most, followed by MF, then FF
plot(jitter(abs(socialRates$ageDiff)), socialRates$affRate, pch = 16)
boxplot(socialRates$affRate ~ socialRates$sexMatch, pch = 16)

#No Age or sex effect on grooming 
plot(jitter(abs(socialRates$ageDiff)), socialRates$grmRate, pch = 16)
boxplot(socialRates$grmRate ~ socialRates$sexMatch, pch = 16)

#No Age or sex effect on grooming 
plot(jitter(abs(socialRates$ageDiff)), socialRates$plyRate, pch = 16)
boxplot(socialRates$plyRate ~ socialRates$sexMatch, pch = 16)

socialRatesUndirected	<- aggregate(socialRates[,c('adjObsTime', 'durGrm', 'durPly')], by = list(dyadID = socialRates$dyadID, group = socialRates$group, sexMatch = socialRates$sexMatch, ageDiff = abs(socialRates$ageDiff)), FUN = sum)
socialRatesUndirected$plyRate	<- socialRatesUndirected$durPly/socialRatesUndirected$adjObsTime
socialRatesUndirected$grmRate	<- socialRatesUndirected$durGrm/socialRatesUndirected$adjObsTime
socialRatesUndirected$affRate	<- (socialRatesUndirected$durPly + socialRatesUndirected$durGrm)/socialRatesUndirected$adjObsTime

#Some age effect, some sex effect, no obvious group effect
par(mfrow = c(1, 2))
plot(socialRatesUndirected$ageDiff, socialRatesUndirected$affRate, pch = 16, xlab = 'Age Difference (years)', ylab = 'Affiliation Rate (min/hr)')
#legend('topright', col = 1:5, pch = 16, legend = c('Group I', 'Group II', 'Group III', 'Group VI', 'Group XII'))
boxplot(socialRatesUndirected$affRate ~ socialRatesUndirected$sexMatch, pch = 16, xlab = 'Sex Combination', ylab = 'Affiliation Rate (min/hr)')

#No Age or sex effect on grooming
png('verreauxiDemograhpics.png', width = 8, height = 6.5, units = 'in', res = 300)
par(mfrow = c(2, 2), mar = c(5.1, 4.1, 1.1, 2.1))
plot(socialRatesUndirected$ageDiff, socialRatesUndirected$grmRate,  pch = 16, xlab = 'Age Difference (years)', ylab = 'Grooming Rate (min/hr)')
#legend('topleft', col = 1:5, pch = 16, legend = c('Group I', 'Group II', 'Group III', 'Group VI', 'Group XII'))
boxplot(socialRatesUndirected$grmRate ~ socialRatesUndirected$sexMatch, pch = 16, xlab = 'Sex Combination', ylab = 'Grooming Rate (min/hr)')

#Some age effect, some sex effect
#par(mfrow = c(1, 2))
plot(socialRatesUndirected$ageDiff, socialRatesUndirected$plyRate, pch = 16, ylim = c(0, 0.2), xlab = 'Age Difference (years)', ylab = 'Play Rate (min/hr)')
boxplot(socialRatesUndirected$plyRate ~ socialRatesUndirected$sexMatch, pch = 16, ylim = c(0, 0.2), xlab = 'Sex Combination', ylab = 'Play Rate (min/hr)')
dev.off()
