normalization <- function(dataset){
	data <- dataset

	for(i in 1:length(dataset[,1])) {
		for(k in 1:length(dataset[1,])) { 
			data[i,k] <-(( dataset[i,k] - min(dataset[,k]) ) / ( max(dataset[,k]) - min(dataset[,k]) )) 
		}
	}
	return(data)
}

Specialnormalization <- function(dataset, dataset2){
	data <- dataset2
	for(i in 1:length(dataset2[,1])) {
		for(k in 1:length(dataset2[1,])) { 
			data[i,k] <-  ( data[i,k] - min(dataset[,k]) ) / ( max(dataset[,k]) - min(dataset[,k]) ) 
		}
	}
	return(data)
}


standardization <-function(dataset){
	data <- dataset
	for(i in 1:length(dataset[,1])) {
		for(k in 1:length(dataset[1,])) { 
			data[i,k] <- ( dataset[i,k] - mean(dataset[,k]) ) / ( sd(dataset[,k]))
		}
	}
	
	return(data)
}

Specialstandardization <- function(dataset, dataset2){
	data <- dataset2
	for(i in 1:length(dataset2[,1])) {
		for(k in 1:length(dataset2[1,])) { 
			data[i,k] <- ( dataset2[i,k] - mean(dataset[,k]) ) / ( sd(dataset[,k]))
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
		if( (abs( sum(betab) - sum(beta)) < 0.001) ) {
			break
		}
		betab <- beta
	}
	return(beta)
}

trainAscent <- function(data, beta, alpha){
	betab <- c(5,5,5,5,5,5,5,5,5,5,5,5,5)
	repeat{
		beta <- Ascent(data, beta, alpha)
		if( (abs( sum(t(betab)) - sum(t(beta)) )  < 0.05) ) {
			break
		}
		betab <- beta
	}
	return(beta)
}

TrainRaphson <- function(data, beta, iteration){
    for(i in 1:iteration){
    	beta <- LogisticNewtonRaphson(data, beta)
    }
    return(beta)
}

TrainStocha <- function(data, beta, alpha){
	betab <- c(5,5,5,5,5,5,5,5,5,5,5,5,5)
	repeat{
		beta <- Stochastic(data, beta, alpha)
		if( (abs( sum(t(betab)) - sum(t(beta)) )  < 0.05) ) {
			break
		}
		betab <- beta
	}
	return(beta)
}

Error <- function(data, beta){
	return( (data[,13] - cbind(1,as.matrix(data[1:12])) %*% beta )^2 )
}

Errorate <- function(data, beta){

	numbers <- cbind(1,as.matrix(data[1:6])) %*% beta
	numbers <- numbers > 0
	target  <- data[,7] >0
	numbers <- numbers == target
	contador = 0
	for(i in 1:length(numbers[,1])) {
		if( numbers[i,1] == TRUE){
			contador = contador +1
		}
	}
	return(1 - (contador/length(numbers[,1])))
}


ErrorN <- function(test, train, beta){
	acumulacion<- c()
	evaluation <- cbind(1,Specialnormalization(train,test[1:12]))
	for(i in 1:length(test[,1])){
		acumulacion[i] <- (test[i,13] - DeNormalization(sum(evaluation[i,]*beta),train[,13]))^2
	}
	return(acumulacion)
}

ErrorS <- function(test, train, beta){ 
	acumulacion<- c()
	evaluation <- cbind(1,Specialstandardization(train,test[1:12]))
	for(i in 1:length(test[,1])){
		acumulacion[i] <- ( test[i,13] - DeStandar(sum(evaluation[i,]*beta),train[,13]) )^2
	}
	return(acumulacion)
}

DeNormalization <- function(dato, dataset){
		dato <-  ((dato) * ( (max(dataset) - min(dataset)))) + min(dataset)   
	return(dato)
}

DeStandar <- function(dato, dataset){
		dato <-  dato * (sd(dataset)) + mean(dataset) 
	return(dato)
}

