##########################################################
##########################################################
##### Group size effects network demographics - KMNP #####
##########################################################
##########################################################

library(RPostgreSQL)
library(chron)
library(stringr)
library(sna)

setwd('G:/My Drive/Graduate School/Research/Projects/KMNPLongTermData/Meredith Corrected KMNP Long Term Data/Original Data')
nn		<- read.csv('Nearest_Neighbor_All_Data_11-16-20.csv')

setwd('G:/My Drive/Graduate School/Research/Projects/KMNPLongTermData/Meredith Corrected KMNP Long Term Data')
demo		<- read.csv('lifeHistoryYOB.csv')

#groups	<- read.csv('Groups-Final-xls_ML.csv')
#socialData	<- read.csv('All_Social_Data_8-25-20.csv')
#scan		<- read.csv('Focal Activity NN combined clean 11-16-20_ML.csv')

#socialData$monthNum	<- ifelse(socialData$Month == 'Jan', '01', 
#					ifelse(socialData$Month == 'Feb', '02',
#					ifelse(socialData$Month == 'Mar', '03',
#					ifelse(socialData$Month == 'Apr', '04',
#					ifelse(socialData$Month == 'May', '05',
#					ifelse(socialData$Month == 'Jun', '06',
#					ifelse(socialData$Month == 'Jul', '07',
#					ifelse(socialData$Month == 'Aug', '08',
#					ifelse(socialData$Month == 'Sep', '09',
#					ifelse(socialData$Month == 'Oct', '10',
#					ifelse(socialData$Month == 'Nov', '11', '12')))))))))))

nn$monthNum	<- ifelse(nn$Month == 'Jan', '01', 
					ifelse(nn$Month == 'Feb', '02',
					ifelse(nn$Month == '2', '02',
					ifelse(nn$Month == 'Mar', '03',
					ifelse(nn$Month == '3', '03',
					ifelse(nn$Month == 'Apr', '04',
					ifelse(nn$Month == '4', '04',
					ifelse(nn$Month == 'May', '05',
					ifelse(nn$Month == '5', '05',
					ifelse(nn$Month == 'Jun', '06',
					ifelse(nn$Month == '6', '06',
					ifelse(nn$Month == 'Jul', '07',
					ifelse(nn$Month == '7', '07',
					ifelse(nn$Month == 'Aug', '08',
					ifelse(nn$Month == 'Sep', '09',
					ifelse(nn$Month == 'Oct', '10',
					ifelse(nn$Month == 'Nov', '11', '12')))))))))))))))))

#scan$monthNum	<- ifelse(scan$Month == 'Jan', '01', 
#					ifelse(scan$Month == 'Feb', '02',
#					ifelse(scan$Month == 'Mar', '03',
#					ifelse(scan$Month == 'Apr', '04',
#					ifelse(scan$Month == 'May', '05',
#					ifelse(scan$Month == 'Jun', '06',
#					ifelse(scan$Month == 'Jul', '07',
#					ifelse(scan$Month == 'Aug', '08',
#					ifelse(scan$Month == 'Sep', '09',
#					ifelse(scan$Month == 'Oct', '10',
#					ifelse(scan$Month == 'Nov', '11', '12')))))))))))

#socialData$yearMonth	<- paste(socialData$Year, socialData$monthNum, sep = '-')
#scan$yearMonth		<- paste(scan$Year, scan$monthNum, sep = '-')
nn$yearMonth		<- paste(nn$Year, nn$monthNum, sep = '-')

#######################################################
### Fixing all the wrong groups data for Group Size ###
#######################################################
nn[nn$Observer == 'Elvis', 'Observer']	<- 'Daniel'
nn[nn$Observer == 'Becca', 'Observer']	<- 'Patrick'
nn[nn$Focal == "Éclair", 'Focal']		<- 'Eclair'
nn[nn$Focal == "Arrow ", 'Focal']		<- 'Arrow' 
nn[nn$Focal == "Colugo ", 'Focal']		<- 'Colugo'
nn[nn$Focal == " Colugo ", 'Focal']		<- 'Colugo' 
nn[nn$Focal == "Pumpkin ", 'Focal']		<- 'Pumpkin' 
nn[nn$Nearest.neighbor == "Syrup ", 'Nearest.neighbor']		<- 'Syrup' 
nn[nn$Nearest.neighbor == 'Pumpkin ', 'Nearest.neighbor']	<- 'Pumpkin'
nn[nn$Nearest.neighbor == 'Colugo ', 'Nearest.neighbor']	<- 'Colugo'

nn[nn$Focal == 'Thor' & nn$Group == 'II', 'Group']	<- 'III'
nn[nn$Focal == 'Thor' & nn$Group == 'IV', 'Group']	<- 'III'
nn[nn$Focal == 'Thor' & nn$Group == 'VI', 'Group']	<- 'III'
nn[nn$Focal == 'Valdes' & nn$Group == 'VI', 'Group']	<- 'IV'
nn[nn$Focal == 'Valdes' & nn$Group == 'XII', 'Group']	<- 'IV'
nn[nn$Focal == 'Valdes' & nn$Group == 'II', 'Group']	<- 'IV'
nn[nn$Focal == 'Venus' & nn$Group == 'II', 'Group']	<- 'III'
nn[nn$Focal == 'Venus' & nn$Group == 'VI', 'Group']	<- 'III'
nn[nn$Focal == 'Vervet' & nn$Group == 'VI', 'Group']	<- 'III'
nn[nn$Focal == 'Vervet' & nn$Group == 'IV', 'Group']	<- 'III'
nn[nn$Focal == 'Vervet' & nn$Group == 'II', 'Group']	<- 'III'
nn[nn$Focal == 'Velo' & nn$Group == 'II', 'Group']	<- 'III'
nn[nn$Focal == 'Velo' & nn$Group == 'IV', 'Group']	<- 'III'
nn[nn$Focal == 'Velo' & nn$Group == 'VI', 'Group']	<- 'III'
nn[nn$Focal == 'Polina' & nn$Group == 'VI', 'Group']	<- 'IV'
nn[nn$Focal == 'Polina' & nn$Group == 'II', 'Group']	<- 'IV'
nn[nn$Focal == 'Polina' & nn$Group == 'XII', 'Group']	<- 'IV'
nn[nn$Focal == 'Quentin' & nn$Group == 'XI', 'Group']	<- 'XII'
nn[nn$Focal == 'William' & nn$Group == 'VI', 'Group']	<- 'II'
nn[nn$Focal == 'William' & nn$Group == 'IV', 'Group']	<- 'II'
nn[nn$Focal == 'William' & nn$Group == 'III', 'Group']	<- 'II'
nn[nn$Focal == 'Savannah' & nn$Group == 'III', 'Group']	<- 'II'
nn[nn$Focal == 'Sadakely' & nn$Group == 'III', 'Group']	<- 'II'
nn[nn$Focal == 'Savannah' & nn$Group == 'IV', 'Group']	<- 'II'
nn[nn$Focal == 'Zoma' & nn$Group == 'IV', 'Group']	<- 'II'
nn[nn$Focal == 'Zoma' & nn$Group == 'III', 'Group']	<- 'II'
nn[nn$Focal == 'Zavona' & nn$Group == 'III' & nn$Year == '2017', 'Group']	<- 'II'
nn[nn$Focal == 'Spirit' & nn$Group == 'III' & nn$Year == '2017', 'Group']	<- 'II'
nn[nn$Focal == 'Spirit' & nn$Group == 'VI', 'Group']	<- 'II'
nn[nn$Focal == 'Syrup' & nn$Group == 'VI', 'Group']	<- 'XI'
nn[nn$Focal == 'Syrup' & nn$Group == 'IV', 'Group']	<- 'XI'
nn[nn$Focal == 'Albert' & nn$Group == 'VI', 'Group']	<- 'XI'
nn[nn$Focal == 'Abby' & nn$Group == 'III', 'Focal']	<- 'Velo'
nn[nn$Focal == 'Tonic' & nn$Group == 'III', 'Focal']	<- 'Thor'
nn[nn$Focal == 'Khaleb' & nn$Group == 'XII', 'Group']	<- 'I'
nn[nn$Focal == 'Louise' & nn$Group == 'XII', 'Group']	<- 'I'
nn[nn$Focal == 'Lexar' & nn$Group == 'XII', 'Group']	<- 'I'
nn[nn$Focal == 'Lojy' & nn$Group == 'XII', 'Group']	<- 'I'
nn[nn$Focal == 'Leopard' & nn$Group == 'XII', 'Group']	<- 'I'
nn[nn$Focal == 'Albert' & nn$Group == 'IV' & nn$Year == '2018', 'Group']	<- 'I'

nn$nnWithinGroup	<- nn$Nearest.neighbor
nn[nn$Nearest.neighbor %in% c('Vanilla', 'Zacky', 'Zafiry', 'Rija', 'Velo', 'Rajako', 'Thor') & nn$Group == 'II', 'nnWithinGroup']	<- ''
nn[nn$Nearest.neighbor %in% c('Abby', 'Hira', 'Rajako', 'Savannah', 'Vanilla', 'Sunny', 'Victor', 'William') & nn$Group == 'III', 'nnWithinGroup']	<- ''
nn[nn$Nearest.neighbor %in% c('Albert', 'Khaleb', 'Louise', 'Mafia', 'Petunia', 'Quicksilver', 'Rich', 'Rija', 'Savannah', 'Thor', 'Velo', 'Zacky') & nn$Group == 'IV', 'nnWithinGroup']	<- ''
nn[nn$Nearest.neighbor %in% c('Bella', 'Lojy', 'Sadakely', 'Savannah') & nn$Group == 'V', 'nnWithinGroup']	<- ''
nn[nn$Nearest.neighbor %in% c('Bob', 'Marsu') & nn$Group == 'VI', 'nnWithinGroup']	<- ''
nn[nn$Nearest.neighbor %in% c('Abby', 'Spirit', 'Zavona') & nn$Group == 'XI', 'nnWithinGroup']	<- ''

notLegitNNDist	<- c('/', '???', '110', '813', 'A', 'B9', 'G37', 'H17', 'O')
nn$cleanedNNDist	<- nn$NN.distance
nn[nn$cleanedNNDist %in% notLegitNNDist, 'cleanedNNDist']	<- ''
nn$cleanedNNDist	<- as.numeric(nn$cleanedNNDist)

#write.csv(nn, 'Nearest_Neighbor_All_Data_11-16-20_MLCleaned.csv')

###############################
### Generate network slices ###
###############################
nn$monthNum	<- as.numeric(nn$monthNum)
nn$season	<- ifelse(nn$monthNum <= 3, 'mating',
			ifelse(nn$monthNum >= 4 & nn$month <= 6, 'gestation',
			ifelse(nn$monthNum >= 7 & nn$month <= 9, 'birthing', 'lactation')))

nnNoSol		<- nn[nn$Group %in% c('I', 'II', 'III', 'IV', 'V', 'VI', 'XI', 'XII') & nn$Exclude_for_Analysis != 'Yes' & nn$Focal != nn$Nearest.neighbor,]
nnNoSol$sliceID	<- paste(nnNoSol$Group, nnNoSol$Year, nnNoSol$season, sep = '-')

nScansForEnoughData	<- 250
enoughData		<- aggregate(nnNoSol$sliceID, by = list(nnNoSol$sliceID), FUN = length)[aggregate(nnNoSol$sliceID, by = list(nnNoSol$sliceID), FUN = length)$x >= nScansForEnoughData,]

#Get rid of unmarked animals
nnNoUnmarked	<- nnNoSol[nnNoSol$Focal != 'Unmarked female' & nnNoSol$Focal != 'Unmarked male' & nnNoSol$nnWithinGroup != 'Unmarked male',]

#########################################
### Calculate networks for each slice ###
#########################################
distForContact	<- 0.1 #This is arbitrary and fixes the divide by 0 problem
sliceIDs	<- enoughData[,1]
listNets	<- list()

for(i in sliceIDs){
	print(paste('Working on', i))
	data		<- nnNoUnmarked[nnNoUnmarked$sliceID == i & is.na(nnNoUnmarked$cleanedNNDist) == FALSE & nnNoUnmarked$nnWithinGroup != '',]
	animals	<- sort(unique(c(data[,c('Focal')], data[,c('nnWithinGroup')])))
	if(i == 'XI-birthing-2019'){ #This deals with the weird IGE/wrong focal name issue
		animals	<- c('Albert', 'Barea', 'Syrup')
	}
	if(length(animals) > 2){
		mat		<- matrix(, length(animals), length(animals), dimnames = list(animals, animals))
		for(j in animals){
			for(k in animals){
				dataDyad	<- rbind(data[data$Focal == j & data$nnWithinGroup == k,], data[data$Focal == k & data$nnWithinGroup == j,])
				dataDyad[dataDyad$cleanedNNDist == 0, 'cleanedNNDist']	<- distForContact
				if(dim(dataDyad)[1] == 0){
					mat[j, k]	<- 0
				}
				else{
					dataDyad$weight	<- 1/dataDyad$cleanedNNDist
					weight		<- mean(dataDyad$weight)
					mat[j, k]		<- weight
				}	
			}
		}
		listNets[[i]]	<- mat
	}
}

#################################################
### Calculate Edge Differentiability and plot ###
#################################################

edgeDiffSummary	<- data.frame(group = character(), period = character(), year = character(), nAnimals = numeric(), edgeDiff = numeric())
for(i in 1:length(listNets)){
	group		<- str_split(names(listNets)[i], '-')[[1]][1]
	year		<- str_split(names(listNets)[i], '-')[[1]][2]
	season	<- str_split(names(listNets)[i], '-')[[1]][3]
	noDiag	<- diag.remove(listNets[[i]])
	edgeDiff	<- sd(noDiag, na.rm = TRUE)/mean(noDiag, na.rm = TRUE)
	n		<- dim(noDiag)[1]
	line		<- c(group, season, year, as.numeric(n), as.numeric(edgeDiff))
	edgeDiffSummary	<- rbind(edgeDiffSummary, line)
}

colnames(edgeDiffSummary)	<- c('group', 'season', 'year', 'n', 'edgeDiff')
edgeDiffSummary$n			<- as.numeric(edgeDiffSummary$n)
edgeDiffSummary$edgeDiff	<- as.numeric(edgeDiffSummary$edgeDiff)	

png('edgeDiffGraph.png', height = 6, width = 6, units = 'in', res = 300)
plot(edgeDiffSummary$n, edgeDiffSummary$edgeDiff, pch = 16, xlab = 'Number of Animals in Group', ylab = 'Coefficient of Variation in Edge Weights')
dev.off()

means	<- aggregate(edgeDiffSummary[, 'edgeDiff'], by = list(edgeDiffSummary[,'n']), FUN = mean)