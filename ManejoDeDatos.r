standardization <- function(dataset){
	data <- dataset

	for(i in 1:length(dataset[,1])) {
		for(k in 1:length(dataset[1,])) { 
			data[i,k] <- 2 * ( dataset[i,k] - min(dataset[,k]) ) / ( max(dataset[,k]) - min(dataset[,k]) ) - 2
		}
	}
	return(data)
}

normalization <-function(dataset){
	data <- dataset
	for(i in 1:length(dataset[,1])) {
		for(k in 1:length(dataset[1,])) { 
			data[i,k] <- ( dataset[i,k] - mean(dataset[,k]) ) / ( sd(dataset[,k]))
		}
	}
	
	return(data)
}

crossfolder <- function(dataset, partition){
	last = 1
	for(i in 1:partition){
		filename = paste("cross", i, ".data" , sep="")
		write.table(dataset[ last : (length(dataset[,1]) * (i/partition)), 1:13], filename, row.names = FALSE, col.names = FALSE)
		last <- (length(dataset[,1]) * (i/partition) + 1)
	}
}

 prediction <- function(data, beta){
 	return(cbind(1,as.matrix(data[1:12])) %*% beta )
 }


TrainBatch <- function(data, beta, alpha){
	betab <- c(5,5,5,5,5,5,5,5,5,5,5,5,5)
	repeat{
		beta <- Batch(data, beta, alpha)
		if(abs(sum(beta) - sum(betab)) < 0.005){
			break
		}
		betab <- beta
	}
	return(beta)
}

TrainStocha <- function(data, beta, alpha){
	betab <- c(5,5,5,5,5,5,5,5,5,5,5,5,5)
	repeat{
		beta <- Stochastic(data, beta, alpha)
		if(abs(sum(beta) - sum(betab)) < 0.005){
			break
		}
		betab <- beta
	}
	return(beta)
}


TrainBatch <- function(data, beta, iterations, alpha){
	for(i in 1:iterations){
		beta <- Stochastic(data, beta, alpha)	
	}
	return(beta)
}

Error <- function(data, beta){
	return( (data[,13] - cbind(1,as.matrix(data[1:12])) %*% beta )^2 )
}

