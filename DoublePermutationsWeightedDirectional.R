###############################
###############################
##### Double Permutations #####
###############################
###############################

# Use Reggie's function to get network and do data stream randomization
# Control factor are individual focals
# Dataframe needs actor receipient focal Id in duration
# Scan is a variable relating to each focal that you want to constrain swaps w/in
# For example, morning focals only swapping with morning focals
# Focal and alters are actor and receipient
# OBS in double permutation function is df in data stream function
# In double permutaion function index is an association index
# Test function runs all tests in a package(median)
# This seems to be set up for node level metrics, not edge level
# The code we want is simulation 3
# Looks like can use code from simulation 2 once randomized networks are populated from Reggie's code

socialData	<- socialDataMatching
subjects	<- sifakaNames

colnames(socialData)[12]	<- 'actor'
colnames(socialData)[13]	<- 'subject'
colnames(socialData)[1]		<- 'date'
colnames(socialData)[2]		<- 'focal_individual_id'
colnames(socialData)[11]	<- 'duration'

#need to have columns for actor, subject, focalID, focal_individual_id, duration

dataStreamRandomizationsValued <- function(socialData, subjects, obsmat, n.rand = 1000, behav, behavColNum) {
	data.rand 		<- socialData
	data.rand$index 	<- 1:nrow(data.rand)

	socmat.rand <- array(NA, c(length(subjects), length(subjects), n.rand))
	
	for(i in 1:n.rand){
		repeat{
			# select two observations from the same focal
			a 			<- sample(data.rand$index, 1)
			data.rand.tmp 	<- data.rand[which(data.rand$index != data.rand$index[a]), , drop = FALSE]
			
			# if there are further observations on this focal
			if(nrow(data.rand.tmp[which(data.rand.tmp$focalID == data.rand$focalID[a]), ]) > 0){
				b 	<- sample(data.rand.tmp$index[which(data.rand.tmp$focalID == data.rand$focalID[a])], 1)

				# check that these represent 4 unique individuals
				rows 	<- which(data.rand$index %in% c(a, b))
				if(length(unique(c(data.rand$actor[rows], data.rand$subject[rows]))) == 4){
				
					# Extract which ones aren't a focal (if neither, select one at random)
					inds.a 	<- c(data.rand$actor[rows[1]], data.rand$subject[rows[1]])
					i.a 		<- which(!(inds.a %in% data.rand$focal_individual_id[rows[1]]))
					if(length(i.a) > 1){
						i.a <- sample(i.a, 1)
					}
					inds.b 	<- c(data.rand$actor[rows[2]], data.rand$subject[rows[2]])
					i.b 		<- which(!(inds.b %in% data.rand$focal_individual_id[rows[2]]))
					if(length(i.b) > 1){
						i.b <- sample(i.b, 1)
					}

					# Then swap them in
					data.rand[rows[1], c('actor', 'subject')][i.a] <- inds.b[i.b]
					data.rand[rows[1], c('actor', 'subject')][i.b] <- inds.a[i.a]

					# then break out of this loop
					break();
				}
			}
		}	
		# recalculate network
		behavMat	<- createNet(data.rand$actor, data.rand$subject, data.rand[, behavColNum], behav,
						subjects = subjects, directional = TRUE, type = "duration", durs = data.rand$duration)
		socmat.rand[, , i] <- behavMat/obsMat

	}
	return(socmat.rand)
}



