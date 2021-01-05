setwd('G:/My Drive/Graduate School/Research/Projects/TemporalNets/KMNP Data')

groups	<- read.csv('Groups_Updated_Aug_2020 copy_ML.csv')
post2013	<- groups[groups$censusDate >= '2013-07' & groups$Month_Count == 1 & groups$Group %in% c('I', 'II', 'III', 'IV', 'V', 'VI', 'XII'),]
post2013	<- post2013[is.na(post2013$Month_Count) == FALSE,]
post2013$censusGroupID	<- paste(post2013$censusDate, post2013$Group)
groupCensuses	<- unique(post2013$censusGroupID)

for(i in groupCensuses){
	if(length(unique(post2013[post2013$censusGroupID == i, 'Date'])) > 1){
		print(paste(i, 'has more than 1 census'))
	}
}

#Fix census #'ing
post2013[post2013$Date == '6/30/2013' & post2013$Group == 'I' & is.na(post2013$Group) == FALSE, 'censusGroupID'] <- '2013-06 I'
post2013[post2013$Date == '6/30/2013' & post2013$Group == 'II' & is.na(post2013$Group) == FALSE, 'censusGroupID'] <- '2013-06 II'
post2013[post2013$Date == '6/30/2013' & post2013$Group == 'III' & is.na(post2013$Group) == FALSE, 'censusGroupID'] <- '2013-06 III'
post2013[post2013$Date == '6/30/2013' & post2013$Group == 'IV' & is.na(post2013$Group) == FALSE, 'censusGroupID'] <- '2013-06 IV'
post2013[post2013$Date == '6/30/2013' & post2013$Group == 'V' & is.na(post2013$Group) == FALSE, 'censusGroupID'] <- '2013-06 V'
post2013[post2013$Date == '8/8/2019' & post2013$Group == 'I' & is.na(post2013$Group) == FALSE, 'censusGroupID'] <- '2019-07 I'

post2013$ageSex	<- paste(post2013$Age_Class, post2013$Sex)
groupSize	<- aggregate(post2013[,'censusGroupID'], by = list(post2013[,'censusGroupID']), FUN = length)

adultFemales	<- table(post2013$censusGroupID, post2013$ageSex)[,1]
adultMales		<- table(post2013$censusGroupID, post2013$ageSex)[,2]

