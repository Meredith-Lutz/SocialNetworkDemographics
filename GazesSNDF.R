
library(sna)
library(reshape2)
rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# read in data (note: StringsAsFactors=FALSE saves many downstream problems)

  grm<-as.data.frame(read.csv("GazesRawGrmDataFull.csv", header=T, stringsAsFactors=FALSE))
  subjects <- c('De', 'Dv', 'Mt', 'Nb', 'Ng', 'Nk',  'Nm',  'Nt','Nv', 'Nw', 'Ny', 'Sd', 'Sg', 'Sn', 'St', 'Sv')
  n<-sum(lengths(subjects))
  grmmat	<- matrix(, nrow = n, ncol = n, dimnames = list(subjects, subjects))
  focaldurmat<-as.matrix(read.csv("GazesFocalDurMat.csv", sep=",", row.names=1))

## Note "Nl" occurs in data but not in subjects or focaldurmat
grm <- grm[which(grm$Actor %in% subjects & grm$Subject %in% subjects),]

# turn the network code into a function
get_network <- function(grm, subjects, focaldurmat) {

	grmmat<-matrix(0, nrow = n, ncol = n, dimnames = list(subjects, subjects))
	a <- dcast(grm, Actor ~ Subject, value.var= "Duration", fun.aggregate=sum,drop=FALSE)
	names.a <- a[,1]
	a <- as.matrix(a[,2:ncol(a)])
	rownames(a) <- names.a
	grmmat[which(subjects %in% rownames(a)),which(subjects %in% colnames(a))] <- a
	grmmat <- grmmat/focaldurmat

	return(grmmat)
}


# randomisation function
net_rand <- function(grm, subjects, focaldurmat, n.rand=1000) {

	grm.rand <- grm
	grm.rand$index <- 1:nrow(grm.rand)

	grmmat.rand <- array(NA, c(n.rand,nrow(focaldurmat),ncol(focaldurmat)))
	
	for (i in 1:n.rand) {

		repeat {
			# select two observations from the same date
			a <- sample(grm.rand$index,1)
			grm.rand.tmp <- grm.rand[which(grm.rand$index != grm.rand$index[a]),,drop=FALSE]
			
			# if there are further observations on this date
			if (nrow(grm.rand.tmp[which(grm.rand.tmp$Date == grm.rand$Date[a]),]) > 0) {
				b <- sample(grm.rand.tmp$index[which(grm.rand.tmp$Date == grm.rand$Date[a])],1)

				# check that these represent 4 unique individuals
				rows <- which(grm.rand$index %in% c(a,b))
				if (length(unique(c(grm.rand$Actor[rows],grm.rand$Subject[rows])))==4) {
				
					# Extract which ones aren't a focal (if neither, select one at random)
					inds.a <- c(grm.rand$Actor[rows[1]],grm.rand$Subject[rows[1]])
					i.a <- which(!(inds.a %in% grm.rand$FocalAnimal[rows[1]]))
					if (length(i.a) > 1) i.a <- sample(i.a,1)
					inds.b <- c(grm.rand$Actor[rows[2]],grm.rand$Subject[rows[2]])
					i.b <- which(!(inds.b %in% grm.rand$FocalAnimal[rows[2]]))
					if (length(i.b) > 1) i.b <- sample(i.b,1)

					# Then swap them in
					grm.rand[rows[1],c("Actor","Subject")][i.a] <- inds.b[i.b]
					grm.rand[rows[1],c("Actor","Subject")][i.b] <- inds.a[i.a]

					# then break out of this loop
					break();
				}
			}
		}
		
		# recalculate network
		grmmat.rand[i,,] <- get_network(grm.rand, subjects, focaldurmat)
	}

	return(grmmat.rand)
}
		


# calculate observed network
grmmat.obs <- get_network(grm, subjects, focaldurmat)
infocent(grmmat.obs)

# some network statistic (degree of De for example)  #Figure out what to put here for the WHOLE network analysis
stat.obs <- sd((grmmat.obs))/mean((grmmat.obs))*100

#stat.obs <- sum(grmmat.obs[1,])
#stat.obs <- sum((grmmat.obs))

#deg_weighted <- infocent(grmasnipenet)

## Randomisation
n.rand <- 1000
grmmat.rand <- net_rand(grm, subjects, focaldurmat, n.rand)

# distribution of randomise values-reggie edits
stat.rand <- rep(NA, n.rand)
for (i in 1:n.rand) {
  stat.rand[i] <- sd((grmmat.rand[i,,]))/mean((grmmat.rand[i,,]))*100 # use exactly the same code as above, but on the stack of networks
}


# distribution of randomise values- DF version
#stat.rand <- rep(NA, n.rand)
#for (i in 1:n.rand) {
#	stat.rand[i] <- sum(grmmat.rand[i,1,]) # use exactly the same code as above, but on the stack of networks
#}

# P value
sum(stat.obs <= stat.rand)/(n.rand)


