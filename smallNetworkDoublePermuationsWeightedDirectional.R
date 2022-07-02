################################################
################################################
##### Double Permutations for Small Groups #####
################################################
################################################

#Start with dataset of focal ID, focal_individual_id, partner, in weight, and out weight
#W/in focal, randomize WOR partner ID and in/out

preNetworkWeightedDirectionalRandomization	<- function(data){
	focals	<- unique(data$focalID)
	temp		<- data.frame()
	for(i in focals){
		print(paste('Working on randomizing focal', i))
		subset	<- data[data$focalID == i,]
		nPartners	<- nrow(subset)
		partners	<- unique(subset$partner)
		newPartners	<- sample(partners, nPartners, replace = FALSE)
		subset$partner	<- newPartners
		for(j in 1:nPartners){
			weights		<- c(subset[j, 'in'], subset[j, 'out'])
			randomWeights	<- sample(weights, 2, replace = FALSE)
			subset[j, 'in']	<- randomWeights[1]
			subset[j, 'out']	<- randomWeights[2]
		}
		temp	<- rbind.data.frame(temp, subset)
	}
	colnames(temp)	<- c('focalID', 'focal_individual_id', 'partner', 'in', 'out', 'group')
	return(temp)
}