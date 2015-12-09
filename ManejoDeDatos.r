library(infotheo)
library(discretization)

normalization <- function(dataset){ #### Funcion para normalizar ###
	data <- dataset

	for(i in 1:length(dataset[,1])) {
		for(k in 1:length(dataset[1,])) { 
			data[i,k] <- (( dataset[i,k] - min(dataset[,k]) ) / ( max(dataset[,k]) - min(dataset[,k]) ))
		}
	}
	return(data)
}

Specialnormalization <- function(dataset, dataset2){ #### Función para normalizar dependiendo de otro data set ###
	data <- dataset2
	for(i in 1:length(dataset2[,1])) {
		for(k in 1:length(dataset2[1,])) { 
			data[i,k] <-  ( data[i,k] - min(dataset[,k]) ) / ( max(dataset[,k]) - min(dataset[,k]) ) 
		}
	}
	return(data)
}


standardization <-function(dataset){ ##### Funcion para standarizar
	data <- dataset
	for(i in 1:length(dataset[,1])) {
		for(k in 1:length(dataset[1,])) { 
			data[i,k] <- ( dataset[i,k] - mean(dataset[,k]) ) / ( sd(dataset[,k]))
		}
	}
	
	return(data)
}

Specialstandardization <- function(dataset, dataset2){ #### Funcion para standarizar dependiendo de otra data
	data <- dataset2
	for(i in 1:length(dataset2[,1])) {
		for(k in 1:length(dataset2[1,])) { 
			data[i,k] <- ( dataset2[i,k] - mean(dataset[,k]) ) / ( sd(dataset[,k]))
		}
	}
	
	return(data)
}


crossfolder <- function(dataset, partition){ #### Funcion para particionar data en diferentes archivos
	last = 1
	for(i in 1:partition){
		filename = paste("cross", i, ".data" , sep="")
		write.table(dataset[ last : (length(dataset[,1]) * (i/partition)), 1:13], filename, row.names = FALSE, col.names = FALSE)
		last <- (length(dataset[,1]) * (i/partition) + 1)
	}
}

 prediction <- function(data, beta){ ### Funcion para predecir
 	return(cbind(1,as.matrix(data[1:12])) %*% beta )
 }


TrainBatch <- function(data, beta, alpha){ ### Funcion para entrenar a gradiante descendente batch
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

trainAscent <- function(data, beta, alpha){ ### Funcion para entrenar a grandiante descendente ascent
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

TrainRaphson <- function(data, beta, iteration){ ## Funcion para entrenar a newton raphson
    for(i in 1:iteration){
    	beta <- LogisticNewtonRaphson(data, beta)
    }
    return(beta)
}

TrainStocha <- function(data, beta, alpha){ ## funcion para enrenar a grandante descenten stochastic o online
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

Error <- function(data, beta){ ### funcion para calcular el error cuadratio medio
	return( (data[,13] - cbind(1,as.matrix(data[1:12])) %*% beta )^2 )
}

Errorate <- function(data, beta){ ### Funcion para calcular el error rate, dado unos betas

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


ErrorN <- function(test, train, beta){ ## Funcion para calcular el error cuadratico medio dado data normalizada
	acumulacion<- c()
	evaluation <- cbind(1,Specialnormalization(train,test[1:12]))
	for(i in 1:length(test[,1])){
		acumulacion[i] <- (test[i,13] - DeNormalization(sum(evaluation[i,]*beta),train[,13]))^2
	}
	return(acumulacion)
}

ErrorS <- function(test, train, beta){  ## funcion para calculcar el error cuadratico medio dada data standarizada
	acumulacion<- c()
	evaluation <- cbind(1,Specialstandardization(train,test[1:12]))
	for(i in 1:length(test[,1])){
		acumulacion[i] <- ( test[i,13] - DeStandar(sum(evaluation[i,]*beta),train[,13]) )^2
	}
	return(acumulacion)
}

DeNormalization <- function(dato, dataset){ ### Funcion para des normalizar, es necesario la data original
		dato <-  ((dato) * ( (max(dataset) - min(dataset)))) + min(dataset)   
	return(dato)
}

DeStandar <- function(dato, dataset){ ## funcion para des standarizar, es necesario la data original
		dato <-  dato * (sd(dataset)) + mean(dataset) 
	return(dato)
}

sortColumn<- function(data){ ##### Funcion que ordena las columnas de una matriz respecto a la variable con mayor desviación standart #####
	data <- data[order(data$V3),]
	rownames(data) <- NULL
	return(data)
}

 EqualFreq2 <- function(x,n){ #### Funcion para aplicar discretar una columna
    nx <- length(x)
    nrepl <- floor(nx/n)
    nplus <- sample(1:n,nx - nrepl*n)
    nrep <- rep(nrepl,n)
    nrep[nplus] <- nrepl+1
    x[order(x)] <- rep(seq.int(n),nrep)
    return(x)
}
 
Discretization1 <- function(data,n){ ##Aplica la función anterior a todas las columnas de interez, equal frecu
	for(i in 1:(ncol(data)-1)){
		h <- EqualFreq2(data[,i],n)
		data[,i] <- h
	}
	return(data)
}

SpecialDiscretization1 <- function(data, dataset,k){ ## Con esta función se discretiza el testing set usando la misma disretizacion que el training
	DiscretData <- Discretization1(data,k)
	for(i in 1:(ncol(data)-1)){
		sortA <- sort(data[,i])
		sortB <- sort(DiscretData[,i])
		for(k in 1:nrow(dataset)){
			for(w in 1:nrow(data)){
				if((dataset[k,i] <= sortA[w])){ ### Se va revisando en que casilla corresponde la data
					if((w == 1)){
						dataset[k,i] = sortB[1]
						break
					}
					dataset[k,i] = sortB[w-1]
					break
				}
				if((w == nrow(data))){
					if((dataset[k,i]>sortA[w])){
						dataset[k,i] = sortB[w-1]
						break
					}
				}
			}
		}
	}
	return(dataset)
}

Discretization2 <- function(data, k){
	dis <- discretize( data[,1:6], disc="equalwidth", nbins=k) ### Funcion para discretizar equal width
	dis <- cbind(dis,data[,7])
	colnames(dis) <- c("V1","V2","V3","V4","V5","V6","V7")
	return(dis)
}

SpecialDiscretization2 <- function(data, dataset,k){ ## Funcion que discretiza el testing set
	DiscretData <- discretize( data, disc="equalwidth", nbins=k)
	for(i in 1:(ncol(data)-1)){
		sortA <- sort(data[,i])
		sortB <- sort(DiscretData[,i])
		for(k in 1:nrow(dataset)){
			for(w in 1:nrow(data)){
				if((dataset[k,i] <= sortA[w])){
					if((w == 1)){
						dataset[k,i] = sortB[1]
						break
					}
					dataset[k,i] = sortB[w-1]
					break
				}
				if((w == nrow(data))){
					if((dataset[k,i]>sortA[w])){
						dataset[k,i] = sortB[w-1]
						break
					}
				}
			}
		}
	}
	return(dataset)
}

Discretization3 <-function(dataset,alpha){ ## Funcion para discretizar utilizando ChiMerge algorithm
	data <- chiM(dataset, alpha = alpha)
	return(data$Disc.data)
}

SpecialDiscretization3 <-function(data,dataset,alpha){ ## Funcion para discretizar utilizando chiMerger el testingSet
	DiscretData <- Discretization3(data,alpha)
	for(i in 1:(ncol(data)-1)){
		sortA <- sort(data[,i])
		sortB <- sort(DiscretData[,i])
		for(k in 1:nrow(dataset)){
			for(w in 1:nrow(data)){
				if((dataset[k,i] <= sortA[w])){
					if((w == 1)){
						dataset[k,i] = sortB[1]
						break
					}
					dataset[k,i] = sortB[w-1]
					break
				}
				if((w == nrow(data))){
					if((dataset[k,i]>sortA[w])){
						dataset[k,i] = sortB[w-1]
						break
					}
				}
			}
		}
	}
	return(dataset)
}


### Error rate adaptado a problema LDA ###
Errorate <- function(data, dataset){ ### Funcion para calcular el error rate, dado un resultado
	numbers <- data
	numbers <- numbers > 0.5
	target  <- dataset[,9] >0.5
	numbers <- numbers == target
	numbers <- sum(numbers)
	return(1 - (numbers/length(data)))
}

ErrorateB <- function(data, dataset){ ### Funcion para calcular el error rate, usado para el perceptron
	numbers <- data
	numbers <- numbers > 0
	target  <- dataset[,7] > 0
	numbers <- numbers == target
	numbers <- sum(numbers)
	return(1 - (numbers/length(data)))
}
ErrorateC <- function(data, dataset){ ### Funcion para calcular el error rate, usado para el perceptron dual
	numbers <- data
	numbers <- numbers > 0
	target  <- dataset[,9] > 0
	numbers <- numbers == target
	numbers <- sum(numbers)
	return(1 - (numbers/length(data)))
}


Errorate <- function(data, dataset){ ### Funcion para calcular el error rate para diabetes
	numbers <- data
	numbers <- numbers > 0.5
	target  <- dataset[,9] >0.5
	numbers <- numbers == target
	numbers <- sum(numbers)
	return(1 - (numbers/length(data)))
}

#####Kernels

Gauss <- function(x,y,sigma){
	result <- exp(-sqrt(sum((y - x)^2))/2*sigma^2)
	return(result)
}

rbfKernel <- function(dataset, data){
	matrix <- matrix(data = 1, nrow = nrow(data), ncol = nrow(dataset))
	for(i in 1:nrow(dataset)){
		for(k in 1:nrow(data)){
			matrix[k,i] <- Gauss(dataset[k,],data[i,],1)
		}
	}
	return(matrix)
}


evaluation <- function(data,target,alpha){
	data <- as.matrix(data)
	target <- as.matrix(target)
	matriz <- matrix(data = target, ncol = nrow(data), nrow = ncol(data) - 1 )
	result <- rowSums(data[,1:8]*t(matriz))
	result <- sum(alpha[1:(length(alpha)-1)]*data[,9]*result) + alpha[length(alpha)]
	return(result)
	
}

getResults <-function(data,dataset,alpha){
	result <- c()
	for(i in 1:nrow(dataset)){
		result[i] <- evaluation(data,dataset[i,1:8],alpha)
	}
	return(result)
}


