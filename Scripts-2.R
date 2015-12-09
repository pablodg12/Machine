

#Funcion para generar 20 data sets
#Ingresar el nombre con >>" "<< // Ejemplo "Cereales", "Credit"
#Para que esta funcion cumpla su objetivo, se debe estar en la carpeta donde se encuentra el archivo 
#con la funcion setwd("Direccion"), ejemplo setwd("/Users/pabloibarra/MachineLearning")
GenerateSamples <-function(dataname){
	if(dataname == "cereales"){
		orig_name <- "cereales.data"
	}
	if(dataname == "credit"){
		orig_name <- "credit.data"
	}
	if(dataname == "diabetes"){
		orig_name <- "diabetes.data"
	}
	data <-read.table(orig_name, sep = ",")
	for(k in 1:20){
		for(i in 1:100000){
			data <- swap( data, sample(1:length(data[,1]),1) , sample(1:length(data[,1]),1) )
		}
		filename = paste("Training", k,  orig_name, sep="")
		filenameB = paste("Test", k,  orig_name, sep="")		
		write.table(data[1:(0.75*length(data[,1])),], filename, row.names = FALSE, col.names = FALSE)
		write.table(data[(0.75* length(data[,1]) + 1):length(data[,1]),], filenameB, row.names = FALSE, col.names = FALSE)
	}
}


swap <- function(a,m,n) {
tmp <- a[m,]
a[m,] <- a[n,]
a[n,] <- tmp
return(a)
}
