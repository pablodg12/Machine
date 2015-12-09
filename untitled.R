package(nnet)



# dataset: https://archive.ics.uci.edu/ml/datasets/SUSY#
#Para cargar el data set

Data <- read.table("SUSY.csv")

#Luego el data set se le sacaron solo las low Features, correspondientes a las 9 primeras variables.

write.table(Data[,1:9], "SUSYLOW.csv")

#Luego se lee solo SUSYLOW, de esta manera la memoria almacena 700 mega en vez de 2 gigas que es el tamaño de SUSY original

Data <- read.table("SUSYLOW.csv")


#Para entrenar las redes neuronales con funcion Softmax se utilizo el siguiente codigo, es necesario dividir las etiquetas en 0 y 1, de manera binaria
#usando lo siguiente.

Data2 <- Data 
Data2[,1] <- class.ind(data[,1])

net <- nnet(label~. , data = Data2[?:?,], softmax = TRUE, maxit = 10000, size = 25) # Donde sale ?, ingresar desde donde a hasta donde se tomara la data

#Para entrenar las redes neuronales con funcion Entropy se utilizo el siguiente codigo, aca no es necesario dividir las etiquetas

net <- nnet(label~. , data =Data[?:?,], entropy = TRUE, maxit = 10000, size = 25) #Donde sale ?, ingresar desde donde a hasta donde se tomara la data,
#Aca se usa la Data, Data 2 no se usa debido a que estan dividias las etiquetas.


#Para predecir Data utilizamos las siguientes funcion

predict(net, data[?:?,], type = c("raw", "class"))

#Para mas información del paquete: https://cran.r-project.org/web/packages/nnet/nnet.pdf

#Para hacer el crossfold se utilizo.

crossfold <- function(net, inicio, final, dataset) { #Usar para softmax, Usar Data2.
	for(i in 1:5){
		Train <- dataset[1:(0.8*length(dataset[inicio:final,1])), ]
        Test <- dataset[ ((0.8*length(dataset[inicio:final,1])) + 1): length(dataset[inicio:final,1]) , ]
        net <- nnet(label~. , data = Train, softmax= TRUE, maxit = 10000, size = 25)
        x <- predict(net,Test[,2:9], type = c("raw", "class"))[,2] > 0.5
        y <- Test[,1] > 0.5
        h <- x == y
        print(h/length(Test[1,]))
        dataset <- rbind(Test, Train)
        rownames(dataset) <- NULL 
	}
}

crossfold <- function(net, inicio, final, dataset) { #Usar para entropy, Usar Data
	for(i in 1:5){
		Train <- dataset[1:(0.8*length(dataset[inicio:final,1])), ]
        Test <- dataset[ ((0.8*length(dataset[inicio:final,1])) + 1): length(dataset[inicio:final,1]) , ]
        net <- nnet(label~. , data = Train, entropy= TRUE, maxit = 10000, size = 25)
        x <- predict(net,Test[,2:9], type = c("raw", "class"))[,1] > 0.5
        y <- Test[,1] > 0.5
        h <- x == y
        print(h/length(Test[1,]))
        dataset <- rbind(Test, Train)
        rownames(dataset) <- NULL 
	}
}

#Para graficar se uso

library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(net)



