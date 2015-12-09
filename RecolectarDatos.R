#Regresion Lineal##

library(matrixStats)
library(MASS)

General <-function(){
    for(i in 1:20){
        name = paste("Training", i, "cereales.data", sep="")
        data <- read.table(name)
        print(GetData(data, i))
    }
}

General2 <- function(){ ####Para la parte de generalizacion####
    acumulacion <- matrix(1:1, nrow = 20, ncol = 2)
    for(i in 1:20){
        name = paste("Training", i, "cereales.data", sep="")
        name2 = paste("Test", i, "cereales.data", sep="")
        data <- read.table(name)
        data2 <- read.table(name2)
        acumulacion[i,1] <- GetData2(data, data2, i)[1]
        acumulacion[i,2] <- GetData2(data, data2, i)[2]

    }
    colnames(acumulacion) <- c("Training", "Test")
    boxplot(acumulacion, main = "General Batch Standart") ####Cambiar segun el algoritmo########
    print(colSds(acumulacion))
    print(colSds(acumulacion)/colMeans(acumulacion))
    return(colMeans(acumulacion))
}

GetData <- function(dataset, number){
    alphaBatch <- c(10^(-8) , 3*10^(-8), 6* 10^(-8) , 9*10^(-8)  , 10^(-7))                  #Alpha Batch crudo
    alpha <- c(10^(-4), 2*10^(-4), 10^(-3), 2*10^(-3), 10^(-5))                              #Alpha Normal y standar batch, normal y satandar online
    bandwidth <- c(100, 1000, 10000, 100000, 1000000)

    aError <- matrix(1:1, nrow = 5, ncol = 5)
    aErrorRapson<- matrix(1:1, nrow = 5, ncol = 1)
    acumulacion <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

    for(i in 1:5){
        Train <- dataset[1:(0.8*length(dataset[,1])), ]
        Test <- dataset[ ((0.8*length(dataset[,1])) + 1): length(dataset[,1]) , ]
        for(k in 1:5){
            beta <- c(0,0,0,0,0,0,0,0,0,0,0,0,0)                                               #Beta inicial Cereales
            #beta <- c(0,0,0,0,0,0,0)                                                          #Beta incial Creditos
            #betaBatch <-TrainBatch(Train, beta, alphaBatch[k])                                #Batch, usar alphabatch para convergencia
            #betaBatch <- TrainBatch(normalization(Train), beta , alpha[k])                    #Batch, normalizacion entre 0 y 1 
            #betaBatch <-TrainBatch(standardization(Train), beta, alpha[k])                    #Batch, standarizacion con norma 0 y desviacion 1
            #betaStocha <-TrainStocha(Train, beta, alpha[k])                                   #Online, usar alpha, datos crudos
            #betaStocha <- TrainStocha(normalization(Train), beta, alpha[k])                   #Online, normalizacion entre 0 y 1
            betaStocha <- TrainStocha(standardization(Train), beta, alpha[k])                 #Online, standarizacion con norma 0 y desviacion 1
            #betaNewton <-LinealNewtonRaphson(Train)                                           #Newton Raphson, datos sin intervencion.
            #betaNewton <-LinealNewtonRaphson(standardization(Train))                          #Newton Raphson, datos standarizados.
            #betaNewton <-LinealNewtonRaphson(normalization(Train))                            #Newton Raphson, datos normalizados
            #Target <- cbind(1,Test[1:12])
            #for(z in 1:length(Test[,1])){                                                     #Calculo de Locally
            #    betaLocal <- Locally(standardization(Train), Test[z,], bandwidth[k])
            #    acumulacion[z] <- mean(Error(Test, betaLocal))
            #} 
            #aError[i,k] <- mean(acumulacion)                                                  #Error del Locally
            #betaAscent <- trainAscent(Train, beta, alphaStoch[i])
            #aError[i,k] <- mean(Error(Test, betaBatch))                                        #Calcular el error cuadradito Batch.
            #aError[i,k] <- mean(ErrorS(Test, Train , betaBatch))                               #Calcular el error cuadratico Batch standarizado norma 0 y desvicion 1
            #aError[i,k] <- mean(ErrorN(Test, Train , betaBatch))                               #Calcular el error cuadratico Batch normalizado 0 y 1
            #aError[i,k] <- mean(Error(Test, t(as.matrix(betaStocha))))                         #Calcular el error cuadratico online.
            aError[i,k] <- mean(ErrorS(Test,Train, t(as.matrix(betaStocha))))                  #Calcular el error cuadratico online standarizado norma 0 y devisacion 1
            #aError[i,k] <- mean(ErrorN(Test,Train, t(as.matrix(betaStocha))))                  #Calcular el error cuadratico online normalizado 0 y 1
            #aErrorRapson[i,1] <- mean(Error(Test, betaNewton))                                 #Calcular el error cuadratico NewtonLineal.
            #aErrorRapson[i,1] <- mean(ErrorS(Test,Train, betaNewton))                          #Calcular el error cuadradico NewtonLineal standarizado.
            #aErrorRapson[i,1] <- mean(ErrorN(Test,Train, betaNewton))                          #Calcular el error cuadratico NewtonLineal Normalizado.     
        }
        dataset <- rbind(Test, Train)
        rownames(dataset) <- NULL   
    }
    name = paste("Training", number, "Set", sep="")
    print(colSds(aError))
    #print(colSds(aErrorRapson))
    print(colMeans(aError))
    #print(colMeans(aErrorRapson))
    colnames(aError) <- c("10^(-4)", "2*10^(-4)", "10^(-3)", "2*10^(-3)", "10^(-5)")
    #colnames(aError) <- c("100", "1000", "10000", "100000", "1000000")
    boxplot(aError, main = name)
    #boxplot(aErrorRapson, main = name)
    #return(colSds(aError)/colMeans(aError))
    return(colSds(aErrorRapson)/colMeans(aErrorRapson))
}


    GetData2 <- function(dataset, dataset2, number){
        aError <- matrix(1:1, nrow = 1, ncol = 2)
        #aErrorRapson<- matrix(1:1, nrow = 1, ncol = 2)
        #datasetb <- cbind(1,as.matrix(dataset2[1:12]))
        #acumulacion <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
        #acumulacionb <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
        beta <- c(0,0,0,0,0,0,0,0,0,0,0,0,0)                                               #Beta inicial Cereales
        #betaR<- c(0,0,0,0,0,0,0)                                                          #Beta incial Creditos
        #betaR <-TrainBatch(dataset, beta, 10^-7)
        #betaR <- TrainBatch(normalization(dataset), beta , 2*10^-3)                    #Batch, normalizacion entre 0 y 1 
        betaR <-TrainBatch(standardization(dataset), beta, 2*10^-3)                    #Batch, standarizacion con norma 0 y desviacion 1
        #betaR <-TrainStocha(dataset, beta, 2*10^-6)                                   #Online, usar alpha, datos crudos
        #betaR <- TrainStocha(normalization(dataset), beta, 2*10^-3)                   #Online, normalizacion entre 0 y 1
        #betaR <- TrainStocha(standardization(dataset), beta,  2*10^-3)                 #Online, standarizacion con norma 0 y desviacion 1
        #betaR <-LinealNewtonRaphson(dataset)                                           #Newton Raphson, datos sin intervencion.
        #betaR <-LinealNewtonRaphson(standardization(dataset))                          #Newton Raphson, datos standarizados.
        #betaR <-LinealNewtonRaphson(normalization(dataset))                            #Newton Raphson, datos normalizados
        #for(z in 1:length(dataset2[,1])){                                                     #Calculo de Locally
        #    betaLocal <- Locally(dataset, dataset[z,], 1000000)
        #    #acumulacion[z] <- mean(ErrorN(dataset, dataset, betaLocal))
        #    #acumulacionb[z] <- mean(ErrorN(dataset2, dataset2, betaLocal))
        #    acumulacion[z] <- sum( datasetb[z,]* betaLocal)
        #} 
        #aError[1,1] <- mean(acumulacion)  
        #aError[1,2] <- mean(acumulacionb)                                                
        #aError[1,1] <- mean(Error(dataset, betaR))
        #aError[1,2] <- mean(Error(dataset2, betaR))                                     
        #aError[1,1] <- mean(ErrorN(dataset, dataset , betaR))                          
        #aError[1,2] <- mean(ErrorN(dataset2, dataset2 , betaR))                         
        aError[1,1] <- mean(ErrorS(dataset, dataset , betaR))                           
        aError[1,2] <- mean(ErrorS(dataset2, dataset2 , betaR))                  
        #aError[i,k] <- mean(ErrorN(Test,Train, t(as.matrix(betaStocha))))                  
        #aErrorRapson[i,1] <- mean(Error(Test, betaNewton))                                 
        #aErrorRapson[i,1] <- mean(ErrorS(Test,Train, betaNewton))                          
        #aErrorRapson[i,1] <- mean(ErrorN(Test,Train, betaNewton))                               
        return(aError)
    }


Lector <- function(){
    x <- read.table("Training1cereales.data")
    y <- read.table("Test1cereales.data")
    print(GetData2(x,y,1))
}

### Regresion Logistica ###


GetData <- function(dataset, number){                #Alpha Batch crudo
    #alpha <- c(10^(-4), 2*10^(-4), 10^(-3), 2*10^(-3), 10^(-5))                              #Alpha Normal y standar batch, normal y satandar online

    #aError <- matrix(1:1, nrow = 5, ncol = 5)
    aErrorRapson<- matrix(1:1, nrow = 5, ncol = 1)

    for(i in 1:5){
        Train <- dataset[1:(0.8*length(dataset[,1])), ]
        Test <- dataset[ ((0.8*length(dataset[,1])) + 1): length(dataset[,1]) , ]

        for(k in 1:5){   
        beta <- c(0,0,0,0,0,0,0)  
        betaAscent <- LogisticNewtonRaphson(normalization(Train), beta)
        #aError[i,k] <- Errorate(cbind(Test,Test[,7]), betaAscent)
        aErrorRapson[i,1] <- Errorate(cbind(normalization(Test[1:6]),Test[,7]), betaAscent)

        }

        dataset <- rbind(Test, Train)
        rownames(dataset) <- NULL   
    }

    name = paste("Training", number, "Set", sep="")
    
    print(colSds(aErrorRapson))
    #print(colSds(aErrorRapson))
    print(colMeans(aErrorRapson))
    #print(colMeans(aErrorRapson))
    #colnames(aError) <- c("10^(-4)", "2*10^(-4)", "10^(-3)", "2*10^(-3)", "10^(-5)")
    #colnames(aError) <- c("100", "1000", "10000", "100000", "1000000")
    boxplot(aErrorRapson, main = name)
    #boxplot(aErrorRapson, main = name)
    return(colSds(aErrorRapson)/colMeans(aErrorRapson))
}

General <-function(){
    for(i in 1:20){
        name = paste("Training", i, "credit.data", sep="")
        data <- read.table(name)
        print(GetData(data, i))
    }
}

General2 <- function(){ ####Para la parte de generalizacion####
    acumulacion <- matrix(1:1, nrow = 20, ncol = 2)
    for(i in 1:20){
        name = paste("Training", i, "credit.data", sep="")
        name2 = paste("Test", i, "credit.data", sep="")
        data <- read.table(name)
        data2 <- read.table(name2)
        acumulacion[i,1] <- GetData2(data, data2, i)[1]
        acumulacion[i,2] <- GetData2(data, data2, i)[2]

    }
    colnames(acumulacion) <- c("Training", "Test")
    boxplot(acumulacion, main = "General Ascent Normalizado ") ####Cambiar segun el algoritmo########
    print(colSds(acumulacion))
    print(colSds(acumulacion)/colMeans(acumulacion))
    return(colMeans(acumulacion))
}

GetData2 <- function(dataset, dataset2, number){
    aError <- matrix(1:1, nrow = 1, ncol = 2)
    #aErrorRapson<- matrix(1:1, nrow = 1, ncol = 2)                   
    beta <- c(0,0,0,0,0,0,0)  
    betaAscent <- Ascent(dataset, beta, 2*10^-4)
    aError[1,1] <- Errorate(dataset, betaAscent)
    aError[1,2] <- Errorate(dataset2, betaAscent)                            
    return(aError)
}



