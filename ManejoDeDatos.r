standardization <- function(dataset){
	data <- dataset

	for(i in 1:length(dataset[,1])) {
		for(k in 1:length(dataset[1,])) { 
			data[i,k] <- 2 * (( dataset[i,k] - min(dataset[,k]) ) / ( max(dataset[,k]) - min(dataset[,k]) ))  -1
		}
	}
	return(data)
}

Specialstandardization <- function(dataset, dataset2){
	data <- dataset2
	for(i in 1:length(dataset2[,1])) {
		for(k in 1:length(dataset2[1,])) { 
			data[i,k] <-  (2* ( data[i,k] - min(dataset[,k]) ) / ( max(dataset[,k]) - min(dataset[,k]) )) - 1
		}
	}
	return(data)
}

DeStandar <- function(data, dataset){
	for(i in 1:length(data[,1])) {
		data[i,1] <-  ((data[i,1] -1 ) * ( (max(dataset[,k]) - min(dataset[,k]))/ 2) + min(dataset) ) 
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

Specialnormalization <- function(dataset, dataset2){
	data <- dataset2
	for(i in 1:length(dataset2[,1])) {
		for(k in 1:length(dataset2[1,])) { 
			data[i,k] <- ( dataset2[i,k] - mean(dataset[,k]) ) / ( sd(dataset[,k]))
		}
	}
	
	return(data)
}

DeNormalization <- function(data, dataset){
	for(i in 1:length(data[,1])) {
		data[i,1] <-  (data[i,1] * (sd(dataset)) + mean(dataset)) 
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
		if( (abs( sum(betab) - sum(beta)) < 0.001) ) {
			break
		}
		betab <- beta
	}
	return(beta)
}

TrainRaphson <- function(data, beta, iteration, alpha){
    for(i in 1:iteration){
    	beta <- NewtonRaphson(data, beta, alpha)
    }
    return(beta)
}

TrainStocha <- function(data, beta, alpha){
	betab <- c(5,5,5,5,5,5,5,5,5,5,5,5,5)
	repeat{
		beta <- Stochastic(data, beta, alpha)
		if( (abs( sum(t(betab)) - sum(t(beta)) )  < 0.01) ) {
			break
		}
		betab <- beta
	}
	return(beta)
}

Error <- function(data, beta){
	return( (data[,13] - cbind(1,as.matrix(data[1:12])) %*% beta )^2 )
}

ErrorN <- function(data, dataset2, beta){
	return( (data[,13] - DeNormalization((cbind(1,as.matrix(Specialnormalization(dataset2, data[1:12]))) %*% beta), dataset2[,13]) )^2 )
}

ErrorS <- function(data, dataset2, beta){
	return( (data[,13] - DeStandar((cbind(1,as.matrix(Specialstandardization(dataset2, data[1:12]))) %*% beta),dataset2[,13]) )^2  ) 
}


