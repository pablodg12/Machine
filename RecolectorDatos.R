library(matrixStats)
library(MASS)


General <- function(){ ## Funcion general, la idea es solo remplazar la función general
    acumulacion <- matrix(1:1, nrow = 20, ncol = 2)
    for(i in 1:20){
        name = paste("Training", i, "credit.data", sep="")
        name2 = paste("Test", i, "credit.data", sep="")
        data <- read.table(name)
        data2 <- read.table(name2)
        acumulacion[i,1] <- GetDataNB(data, data)
        acumulacion[i,2] <- GetDataNB(data, data2)

    }
    print(acumulacion)
    colnames(acumulacion) <- c("Training", "Test")
    boxplot(acumulacion, main = "General NaiveBayes")
    print(colSds(acumulacion))
    return(colMeans(acumulacion))
}


GetDataLDA <- function(dataset, dataset2){
    dataset <- cbind(standardization(dataset),dataset[,7])
    dataset2 <- cbind(Specialstandardization(dataset,dataset2),dataset2[,7])
    total <- LDA(dataset,dataset2)
    aError <- Errorate(total,dataset2)
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
    dataset1 <- cbind(standardization(dataset[1:6,]),dataset[,7])
    dataset2 <- cbind(Specialstandardization(dataset[1:6,],dataset2[1:6,]),dataset2[,7])
    colnames(dataset1) <- c("V1","V2","V3","V4","V5","V6","V7")
    colnames(dataset2) <- c("V1","V2","V3","V4","V5","V6","V7")
    algoritmo <- SVM(dataset1, 1)
    total <- predict(algoritmo, dataset2[,1:8])
    aError <- Errorate(as.matrix(total),dataset2)
    return(aError)
}

GetDataPerceptronPrimal <- function(dataset,dataset2,rate,max){
    dataset1 <- cbind(standardization(dataset[1:6,]),dataset[,7])
    dataset2 <- cbind(Specialstandardization(dataset[1:6,],dataset2[1:6,]),dataset2[,7])
    colnames(dataset1) <- c("V1","V2","V3","V4","V5","V6","V7")
    colnames(dataset2) <- c("V1","V2","V3","V4","V5","V6","V7")
    algoritmo <- PerceptronPrimal(dataset1,rate,max)
    evaluation <- (as.matrix(dataset2[,1:6]) %*% t(as.matrix(algoritmo[1:6]))) + algoritmo[,7]
    aError <- ErrorateB(evaluation, dataset2)
    return(aError)
}




generalCross <-function(){
    result <- c(0,0,0,0,0,0,0,0,0,0)
    for(i in 1:20){
        name = paste("Training", i, "credit.data", sep="")
        data <- read.table(name)
        x <- fiveCross(data, i)
        for(i in 1:length(x)){
            result[x[i]] <- result[x[i]] + 1
        }
    }
    print(result)
}

fiveCross <- function(dataset, number){
    aError <- matrix(1:1, nrow = 5, ncol =10)
    for(i in 1:5){
        Train <- dataset[1:(0.8*length(dataset[,1])), ]
        Test <- dataset[ ((0.8*length(dataset[,1])) + 1): length(dataset[,1]) , ]
        for(k in 1:10){
            training <- Discretization3(Train, k/10)
            testing <- SpecialDiscretization3(Train,Test,k/10)
            result <- NaiveBayes(training,testing)
            aError[i,k] <- Errorate(result,testing)
        }
        dataset <- rbind(Test, Train)
        rownames(dataset) <- NULL   
    }
    name = paste("Training", number, "Set", sep="")
    print(colMeans(aError))
    colnames(aError) <- c("0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1")
    boxplot(aError, main = name)
    return(which(colMeans(aError) %in% min(colMeans(aError))))
}
