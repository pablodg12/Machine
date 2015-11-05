library(matrixStats)
library(MASS)

GetData <- function(dataset, number){
    #alphaBatch <- c(10^(-8) , 3*10^(-8), 6* 10^(-8) , 9*10^(-8)  , 10^(-7))                  #Alpha Batch crudo
    #alpha <- c(10^(-4), 2*10^(-4), 10^(-3), 2*10^(-3), 10^(-5))                              #Alpha Normal y standar batch, normal y satandar online
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
            #betaStocha <- TrainStocha(standardization(Train), beta, alpha[k])                 #Online, standarizacion con norma 0 y desviacion 1
            #betaNewton <-LinealNewtonRaphson(Train)                                           #Newton Raphson, datos sin intervencion.
            #betaNewton <-LinealNewtonRaphson(standardization(Train))                          #Newton Raphson, datos standarizados.
            #betaNewton <-LinealNewtonRaphson(normalization(Train))                             #Newton Raphson, datos normalizados
            #betaLocal <- Locally(standardization(Train), bandwidth[k]) 
            #betaLocal <- Locally(normalization(Train), bandwidth[k])
            #Target <- cbind(1,Test[1:12])
            for(z in 1:length(Test[,1])){
                betaLocal <- Locally(Train, Test[z,], bandwidth[k])
                acumulacion[z] <- mean(Error(Test, betaLocal))
            } 
            aError[i,k] <- mean(acumulacion)
            #betaStocha <-TrainStocha(standardization(Train), beta, alphaStoch[k])
            #betaNewton <-NewtonRaphson(Train, beta)
            #betaNewton <-LinealNewtonRaphson(normalization(Train))
            #betaNewton <- TrainRaphson(cbind(standardization(Train[1:6]), Train[,7]), beta, 2)
            #betaAscent <- trainAscent(Train, beta, alphaStoch[i])
            #aError[i,k] <- mean(Error(Test, betaBatch))                                        #Calcular el error cuadradito Batch.
            #aError[i,k] <- mean(ErrorS(Test, Train , betaBatch))                               #Calcular el error cuadratico Batch standarizado norma 0 y desvicion 1
            #aError[i,k] <- mean(ErrorN(Test, Train , betaBatch))                               #Calcular el error cuadratico Batch normalizado 0 y 1
            #aError[i,k] <- mean(Error(Test, t(as.matrix(betaStocha))))                         #Calcular el error cuadratico online.
            #aError[i,k] <- mean(ErrorS(Test,Train, t(as.matrix(betaStocha))))                  #Calcular el error cuadratico online standarizado norma 0 y devisacion 1
            #aError[i,k] <- mean(ErrorN(Test,Train, t(as.matrix(betaStocha))))                  #Calcular el error cuadratico online normalizado 0 y 1
            #aErrorRapson[i,1] <- mean(Error(Test, betaNewton))                                 #Calcular el error cuadratico NewtonLineal.
            #aErrorRapson[i,1] <- mean(ErrorS(Test,Train, betaNewton))                          #Calcular el error cuadradico NewtonLineal standarizado.
            #aErrorRapson[i,1] <- mean(ErrorN(Test,Train, betaNewton))                          #Calcular el error cuadratico NewtonLineal Normalizado.
            #aError[i,k] <- mean(Error(Test, betaLocal))
            #aError[i,k] <- mean(ErrorS(Train,Test, betaLocal))
            #aError[i,k] <- mean(ErrorN(Train,Test, betaLocal))  
            #aError[i,k] <-  Errorate(Test, betaAscent)       
        }
        dataset <- rbind(Test, Train)
        rownames(dataset) <- NULL   
    }
    name = paste("Training", number, "Set", sep="")
    print(colSds(aError))
    #print(colSds(aErrorRapson))
    print(colMeans(aError))
    #print(colMeans(aErrorRapson))
    #colnames(aError) <- c("10^(-4)", "2*10^(-4)", "10^(-3)", "2*10^(-3)", "10^(-5)")
    colnames(aError) <- c("100", "1000", "10000", "100000", "1000000")
    boxplot(aError, main = name)
    #boxplot(aErrorRapson, main = name)
    return(colSds(aError)/colMeans(aError))
    #return(colSds(aErrorRapson)/colMeans(aErrorRapson))
}


General <-function(){
    for(i in 1:20){
        name = paste("Training", i, "cereales.data", sep="")
        #name = paste("Training", i, "credit.data", sep="")
        data <- read.table(name)
        print(GetData(data, i))
    }
}

