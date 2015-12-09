
library(e1071)

#Funcion para calcular la probabilidad de etiquietas###
LabelProbability <- function(data, label){
	dimc <- ncol(data)
	if(label == 0){
		return( 1 - sum(data[,length(data[1,])])/length(data[,1]))
	}
	return(sum(data[,length(data[1,])])/length(data[,1]))
}


NaiveBayes <- function(data, dataset){

	Prob0 <- LabelProbability(data,0) ## Se inicializan algunas variables, como la probabilidad y las dimensiones
	Prob1 <- LabelProbability(data,1)
	dimdata <- ncol(data)
	dimdataset <- nrow(dataset)
	totalA <- c(0,0,0,0,0,0,0,0,0)
	totalB <- c(0,0,0,0,0,0,0,0,0)
	result <- c()

	number <- length(data[,dimdata]) - sum(data[,dimdata]) ##Se calcula este numero con el fin de dividir el data set
	data <- data[order(data$V9),] ##Se hace un sort respecto a las etiquetas, con el fin de dividir el data set en 2, los resultados con 0 y los con 1
	rownames(data) <- NULL

	A <- data[1:number,] ##Se completa la division
	B <- data[(number+1):(length(data[,dimdata])),]

	for(i in 1:dimdataset){

		matrixA <- matrix(data = dataset[i,], nrow = 9, ncol = nrow(A)) ##Se forman matrices con el x_m que deseamos saber
		matrixB <- matrix(data = dataset[i,], nrow = 9, ncol = nrow(B))

		matrixA <- t(matrixA)
		matrixB <- t(matrixB)

		totalA <- colSums(matrixA == as.matrix(A))
		totalB <- colSums(matrixB == as.matrix(B))

		totalA <- (prod(totalA[1:8])*Prob0 + 2) # Se calcula la probabilidad, usando la coreccion de laplace.
		totalB <- (prod(totalB[1:8])*Prob1 + 2)

		result[i] <- totalB/(totalA + totalB) # Se calcula la probabilidad de que x_m sea parte de la etiqueta del 1.
	}	

	return(result) # Se retorna un vector con las probabilidades, para ser comparado afuera.
}	


PerceptronDual <- function(data, max, kernel,rate){ ## Perceptron primal, con condicion de paradas igual a un % de datos bien calificados

	data$V9[data$V9 == 0] <- -1 ### Se cambian todos los 0 por -1
	dimr <- nrow(data)
	b <- 0 ## Se inicia beta
	w <- matrix(data = 0, ncol = nrow(data), nrow = 1) ## Se inician los alpha iguala a la cantidad de datos
	R <- max(rowSums(data[,1:8]^2)) ##### Se encuentra el R
	count <- 0
	count2 <- 0 ### Contadores
	tolerance <- 1 ## Nivel de tolerancia inicial
	while((count < dimr*tolerance)){ ### While con condicion de parada igual a un porcentaje de datos bien clasificados
		count2 <- count2 + 1
		for(i in 1:dimr){
			if(( data[i,9]*(sum(w[1,] * data[,9] * kernel[,i]) + b) <= 0)) { ## Se aplica
				w[1,i] <- w[1,i] + 1
				b <- b + rate*data[i,9]*R
				count <- 0
			}
			count <- count + 1
		}
		if(count2 == max){ ### reinicio
			w <- matrix(data = 0, ncol = nrow(data), nrow = 1)
			b <- 0
			count  <-0
			count2 <- 0
			tolerance <- tolerance - 0.05
			message = paste("Tolerancia cambio a ", tolerance, sep="")
			print(message)
		}
	}
	#print(count2)	
	#print(tolerance)
	return(cbind(w,b))
}

	
SVM <- function(data, C , gamma){
	algoritmo <- svm(V9~. ,data, scale = TRUE, type = "C-classification", kernel = "radial", cost = C, gamme = gamma)
	return(algoritmo)
}

tune(svm, V9~., data = training, ranges = list( gamma =
 seq(.5, .9, by = .1), kernel = "radial"), tunecontrol = tune.control(sampling = "cross", cross = 5, performance = TRUE, best.model = TRUE))

