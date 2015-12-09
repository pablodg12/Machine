## Algoritmos Tarea 2 ##

library(e1071)

## Calculo de la matriz de varianza y covarianza###

cov(x)

## Calculo de P_k ##

LabelProbability <- function(data, label){
	dimc <- ncol(data)
	if(label == 0){
		return( 1 - sum(data[,length(data[1,])])/length(data[,1]))
	}
	return(sum(data[,length(data[1,])])/length(data[,1]))
}

## Calculo de U_k ##

TotalMeanLabel <- function(data, label){
	dimr <- nrow(data)
	dimc <- ncol(data)  #### Se calcular las dimensions
	if(label == 1){ #Si buscamos identficar las medias de los label 1, creamos una matriz con 1 y 0 igual a los target de nuestra data, y luego lo multiplicamos
		total <- sum(data[,dimc])
		data <- data * matrix(data[,dimc], nrow = dimr, ncol = dimc)
	}
	if(label == 0){#Si buscamos identifcar las de los label 0, hacemos lo mismo, peor esta vez la matriz creada sera la resta de una matriz de puros uno con los targets.
		total <- sum(matrix(data=1, ncol = length(data[,1])) - data[,length(data[1,])])
		data <- data * (matrix(data = 1, nrow = dimr, ncol = dimc) - matrix(data[,dimc], nrow = dimr, ncol = dimc))
	}

	return((colSums(data)/total)[1:dimc-1])
}

## Calculo de la Media intercuartilica ##

TotalMeanLabelIRQ <- function(data, label){
	dimr <- nrow(data)	
	dimc <- ncol(data)  #### Se calcular las dimensions
	if(label == 1){ #Si buscamos identficar las medias de los label 1, creamos una matriz con 1 y 0 igual a los target de nuestra data, y luego lo multiplicamos
		total <- sum(data[,dimc])
		data <- sortColumn(data * matrix(data[,dimc], nrow = dimr, ncol = dimc))
		number <- length(data[,dimc]) - sum(data[,dimc])### Se obtiene la matriz sin los 0, para poder calcular el rango
		data <- data[(number +1):(length(data[,dimc])),]
		rownames(data) <- NULL
	}
	if(label == 0){#Si buscamos identifcar las de los label 0, hacemos lo mismo, peor esta vez la matriz creada sera la resta de una matriz de puros uno con los targets.
		number <- sum(data[,dimc])
		total <- sum(matrix(data=1, ncol = length(data[,1])) - data[,length(data[1,])])
		data <- sortColumn(data * (matrix(data = 1, nrow = dimr, ncol = dimc) - matrix(data[,dimc], nrow = dimr, ncol = dimc)))
		data <- data[(number + 1):(length(data[,dimc])),]
		rownames(data) <- NULL
	}
	return((colSums(data[(0.25*nrow(data)):(0.75*nrow(data)),])/length(data[,1]))[1:dimc-1])
}
#Usando las funciones de arriba, implementamos LDA#
	LDA <- function(data, data2){
		dimc <- ncol(data)
		Prob0 <- LabelProbability(data,0)
		Prob1 <- LabelProbability(data,1)
		result <- c()
		for(i in 1:length(data2[,1])){ ##Basicamente es calcular las partes de la funcion que nos entregan las ppts, usando las funciones antesriores, y luego discriminar
			A <- as.matrix(data2[i,1:dimc-1]) %*% cov(data[,1:dimc-1]) %*% (as.matrix(TotalMeanLabel(data,1)) - as.matrix(TotalMeanLabel(data,0)))
			B <- 1/2 * (t(as.matrix(TotalMeanLabel(data,1))) %*% cov(data[,1:dimc-1]) %*% as.matrix(TotalMeanLabel(data,1)))
			C <- 1/2 * (t(as.matrix(TotalMeanLabel(data,0))) %*% cov(data[,1:dimc-1]) %*% as.matrix(TotalMeanLabel(data,0)))
			
			if(A > B - C + Prob1 - Prob0){
				result[i] = 1
			}
			else{
				result[i] = 0
			}
		}
		return(result)
	}

##Lda usando media intercuartilica
	LDA <- function(data, data2){	
		dimc <- ncol(data)
		Prob0 <- LabelProbability(data,0)
		Prob1 <- LabelProbability(data,1)
		result <- c()
		for(i in 1:length(data2[,1])){
			A <- as.matrix(data2[i,1:dimc-1]) %*% cov(data[,1:dimc-1]) %*% (as.matrix(TotalMeanLabelIRQ(data,1)) - as.matrix(TotalMeanLabelIRQ(data,0)))
			B <- 1/2 * (t(as.matrix(TotalMeanLabelIRQ(data,1))) %*% cov(data[,1:dimc-1]) %*% as.matrix(TotalMeanLabelIRQ(data,1)))
			C <- 1/2 * (t(as.matrix(TotalMeanLabelIRQ(data,0))) %*% cov(data[,1:dimc-1]) %*% as.matrix(TotalMeanLabelIRQ(data,0)))
			
			if(A > B - C + Prob1 - Prob0){
				result[i] = 1
			}
			else{
				result[i] = 0
			}
		}
		return(result)
	}


#Para endenter un poco mas el algoritmo, observe el video https://www.youtube.com/watch?v=XcwH9JGfZOU, de la cual me dio una idea de como calcular la probabilidad.

NaiveBayes <- function(data, dataset){

	Prob0 <- LabelProbability(data,0) ## Se inicializan algunas variables, como la probabilidad y las dimensiones
	Prob1 <- LabelProbability(data,1)
	dimdata <- ncol(data)
	dimdataset <- nrow(dataset)
	totalA <- c(0,0,0,0,0,0,0)
	totalB <- c(0,0,0,0,0,0,0)
	result <- c()

	number <- length(data[,dimdata]) - sum(data[,dimdata]) ##Se calcula este numero con el fin de dividir el data set
	data <- data[order(data$V7),] ##Se hace un sort respecto a las etiquetas, con el fin de dividir el data set en 2, los resultados con 0 y los con 1
	rownames(data) <- NULL

	A <- data[1:number,] ##Se completa la division
	B <- data[(number+1):(length(data[,dimdata])),]

	for(i in 1:dimdataset){

		matrixA <- matrix(data = dataset[i,], nrow = 7, ncol = nrow(A)) ##Se forman matrices con el x_m que deseamos saber
		matrixB <- matrix(data = dataset[i,], nrow = 7, ncol = nrow(B))

		matrixA <- t(matrixA)
		matrixB <- t(matrixB)

		totalA <- colSums(matrixA == as.matrix(A))
		totalB <- colSums(matrixB == as.matrix(B))	
		totalA <- (prod(totalA[1:6])*Prob0 + 2) # Se calcula la probabilidad, usando la coreccion de laplace.
		totalB <- (prod(totalB[1:6])*Prob1 + 2)

		result[i] <- totalB/(totalA + totalB) # Se calcula la probabilidad de que x_m sea parte de la etiqueta del 1.
	}	

	return(result) # Se retorna un vector con las probabilidades, para ser comparado afuera.
}	


PerceptronPrimal <- function(data, rate, max){ ## Perceptron primal, con condicion de paradas igual a un % de datos bien calificados

	data$V7[data$V7 == 0] <- -1
	dimr <- nrow(data)
	b <- 0
	w <- c(0,0,0,0,0,0)
	R <- max(rowSums(data[,1:6]))
	count <- 0
	count2 <- 0
	tolerance <- 1

	while((count < dimr*tolerance)){
		count2 <- count2 + 1
		for(i in 1:dimr){
			if( (data[i,7]*(sum(w * data[i,1:6]) + b) <= 0) ){
				w <- w + rate*data[i,7]*data[i,1:6]
				b <- b + rate*data[i,7]*R
				count <- 0
			}
			count <- count + 1
		}
		if(count2 == max){ ### reinicio
			w <- c(0,0,0,0,0,0)
			b <- 0
			count  <-0
			count2 <- 0
			tolerance <- tolerance - 0.05
			message = paste("Tolerancia cambio a ", tolerance, sep="")
			print(message)
		}
	}
	print(count2)	
	print(tolerance)
	return(cbind(w,b))
}

## SVm ** https://cran.r-project.org/web/packages/e1071/e1071.pdf 
	
SVM <- function(data, C ){
	algoritmo <- svm(V7~. ,data, scale = TRUE, type = "C-classification", kernel = "linear", cost = C)
	return(algoritmo)
}


#Funcion que nos devuelve, con cual parametros existe uel mejor desemempeño
tune(svm, V7~., data = training, ranges = list( gamma =
 seq(.5, .9, by = .1), kernel = "radial"), tunecontrol = tune.control(sampling = "cross", cross = 5, performance = TRUE, best.model = TRUE))




