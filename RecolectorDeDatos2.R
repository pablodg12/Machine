library(matrixStats)
library(MASS)


General <- function(){ ## Funcion general, la idea es solo remplazar la función general
	par(mfrow=c(2,2))
    acumulacion1 <- matrix(1:1, nrow = 20, ncol = 2)
    acumulacion2 <- matrix(1:1, nrow = 20, ncol = 2)
    acumulacion3 <- matrix(1:1, nrow = 20, ncol = 2)
    acumulacion4 <- matrix(1:1, nrow = 20, ncol = 2)
    for(i in 1:20){
        name = paste("Training", i, "diabetes.data", sep="")
        name2 = paste("Test", i, "diabetes.data", sep="")
        data <- read.table(name)
        data2 <- read.table(name2)

        testing1 <- rbind(data[101:nrow(data),],data2)
        testing2 <- rbind(data[101:nrow(data),],data2)
        testing3 <- rbind(data[101:nrow(data),],data2)

        rownames(testing1) <- NULL   
        rownames(testing2) <- NULL   
        rownames(testing3) <- NULL   

        acumulacion1[i,1] <- GetDataSVM(data, data)
        acumulacion1[i,2] <- GetDataSVM(data, data2)
        acumulacion2[i,1] <- GetDataSVM(data[1:100,], data[1:100,])
        acumulacion2[i,2] <- GetDataSVM(data[1:100,], testing1)
        acumulacion3[i,1] <- GetDataSVM(data[1:200,], data[1:200,])
        acumulacion3[i,2] <- GetDataSVM(data[1:200,], testing2)
        acumulacion4[i,1] <- GetDataSVM(data[1:300,], data[1:300,])
        acumulacion4[i,2] <- GetDataSVM(data[1:300,], testing3)

    }
    print(acumulacion1)
    print(acumulacion2)
    print(acumulacion3)
    print(acumulacion4)
    colnames(acumulacion1) <- c("Training", "Test")
    colnames(acumulacion2) <- c("Training", "Test")
    colnames(acumulacion3) <- c("Training", "Test")
    colnames(acumulacion4) <- c("Training", "Test")
    boxplot(acumulacion1, main = "General SVM Gausssian Kernel")
    boxplot(acumulacion2, main = "General SVM Gausssian Kernel 100")
    boxplot(acumulacion3, main = "General SVM Gausssian Kernel 200")
    boxplot(acumulacion4, main = "General SVM Gausssian Kernel 300")
    print(colSds(acumulacion1))
    print(colSds(acumulacion2))
    print(colSds(acumulacion3))
    print(colSds(acumulacion4))
    print(colMeans(acumulacion1))
    print(colMeans(acumulacion2))
    print(colMeans(acumulacion3))
    return(colMeans(acumulacion4))
}

General <- function(){ ## Funcion general, la idea es solo remplazar la función general
	par(mfrow=c(2,2))
    acumulacion1 <- matrix(1:1, nrow = 20, ncol = 2)
    acumulacion2 <- matrix(1:1, nrow = 20, ncol = 2)
    acumulacion3 <- matrix(1:1, nrow = 20, ncol = 2)
    acumulacion4 <- matrix(1:1, nrow = 20, ncol = 2)
    for(i in 1:20){
        name = paste("Training", i, "diabetes.data", sep="")
        name2 = paste("Test", i, "diabetes.data", sep="")
        data <- read.table(name)
        data2 <- read.table(name2)

        testing1 <- rbind(data[101:nrow(data),],data2)
        testing2 <- rbind(data[201:nrow(data),],data2)
        testing3 <- rbind(data[301:nrow(data),],data2)

        rownames(testing1) <- NULL   
        rownames(testing2) <- NULL   
        rownames(testing3) <- NULL   

        algoritmo1 <- GetDataPerceptronDual(data,data,2000,1,0.2)
        algoritmo2 <- GetDataPerceptronDual(data,data2,2000,1,0.2)
        algoritmo3 <- GetDataPerceptronDual(data[1:100,],data[1:100,],2000,1,0.2)
        algoritmo4 <- GetDataPerceptronDual(data[1:100,],testing1,2000,1,0.2)
        algoritmo5 <- GetDataPerceptronDual(data[1:200,],data[1:200,],2000,1,0.2)
        algoritmo6 <- GetDataPerceptronDual(data[1:200,],testing2,2000,1,0.2)
        algoritmo7 <- GetDataPerceptronDual(data[1:300,],data[1:300,],2000,1,0.2)
        algoritmo8 <- GetDataPerceptronDual(data[1:300,],testing3,2000,1,0.2)

        acumulacion1[i,1] <- ErrorateC(algoritmo1,data)
        acumulacion1[i,2] <- ErrorateC(algoritmo2,data2)
        acumulacion2[i,1] <- ErrorateC(algoritmo3,data[1:100,])
        acumulacion2[i,2] <- ErrorateC(algoritmo4,testing1)
        acumulacion3[i,1] <- ErrorateC(algoritmo5,data[1:200,])
        acumulacion3[i,2] <- ErrorateC(algoritmo6,testing2)
        acumulacion4[i,1] <- ErrorateC(algoritmo7,data[1:300,])
        acumulacion4[i,2] <- ErrorateC(algoritmo8,testing3)

    }
    print(acumulacion1)
    print(acumulacion2)
    print(acumulacion3)
    print(acumulacion4)
    colnames(acumulacion1) <- c("Training", "Test")
    colnames(acumulacion2) <- c("Training", "Test")
    colnames(acumulacion3) <- c("Training", "Test")
    colnames(acumulacion4) <- c("Training", "Test")
    boxplot(acumulacion1, main = "General Perceptron Dual ")
    boxplot(acumulacion2, main = "General Perceptron Dual 100")
    boxplot(acumulacion3, main = "General Perceptron Dual 200")
    boxplot(acumulacion4, main = "General Perceptron Dual 300")
    print(colSds(acumulacion1))
    print(colSds(acumulacion2))
    print(colSds(acumulacion3))
    print(colSds(acumulacion4))
    print(colMeans(acumulacion1))
    print(colMeans(acumulacion2))
    print(colMeans(acumulacion3))
    return(colMeans(acumulacion4))
}

generalCross <-function(){
    for(i in 1:20){
        name = paste("Training", i, "diabetes.data", sep="")
        data <- read.table(name)
        for(k in 1:5){
			Train <- data[1:(0.8*length(data[,1])), ]
	        Test <- data[ ((0.8*length(data[,1])) + 1): length(data[,1]) , ]
	        x <- fiveCross(Train,Test)
	        dataset <- rbind(Test, Train)
        	rownames(dataset) <- NULL           	
        }

    }
}


fiveCross <- function(Train,Test){
    aError <- matrix(1:1, nrow = 5, ncol =5)
    for(i in 1:5){
        for(k in 1:5){
            result <- GetDataPerceptronDual(Train,Test,2000,i/5,k/5)
            aError[i,k] <- ErrorateC(result,Test)
        }
    }
    print(aError)
    return(aError)
}

GetDataNB <- function(dataset,dataset2){
    training <- Discretization1(dataset, 2)
    testing <- SpecialDiscretization1(dataset,dataset2, 2)
    total<- NaiveBayes(training,testing)
    aError <- Errorate(total,testing)
    return(aError)
}

GetDataSVM <- function(dataset,dataset2){
	dataset1 <- cbind(standardization(dataset[1:8]),dataset[,9])
    dataset2 <- cbind(Specialstandardization(dataset,dataset2),dataset2[,9])
    colnames(dataset2) <- c("V1","V2","V3","V4","V5","V6","V7","V8","V9")
    colnames(dataset1) <- c("V1","V2","V3","V4","V5","V6","V7","V8","V9")
    algoritmo <- SVM(dataset1, 1, 0.5)
    total <- predict(algoritmo, dataset2[,1:8])
    aError <- Errorate(as.matrix(total),dataset2)
    return(aError)
}


GetDataPerceptronDual <- function(dataset,dataset2,max,rate,sigma){
    dataset1 <- cbind(standardization(dataset[1:8]),dataset[,9])
    dataset2 <- cbind(Specialstandardization(dataset,dataset2),dataset2[,9])
	colnames(dataset2) <- c("V1","V2","V3","V4","V5","V6","V7","V8","V9")
    colnames(dataset1) <- c("V1","V2","V3","V4","V5","V6","V7","V8","V9")
    kernel <- gausskernel(dataset1[,1:8],sigma = sigma)
    algoritmo <- PerceptronDual(dataset1,max,kernel,rate)
    result1 <- getResults(dataset1,dataset2,algoritmo)
    return(result1)
}

