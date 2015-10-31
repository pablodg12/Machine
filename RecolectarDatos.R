library(matrixStats)

GetData <- function(dataset, number){
    alphaBatch <- c(10^(-8) , 3*10^(-8), 6* 10^(-8) , 9*10^(-8)  , 10^(-7))
    #alphaBatch <- c(10^(-4), 3*10^(-4), 6* 10^(-4), 9*10^(-4), 10^(-5))
    #alphaStoch <- c(2*10^(-6), 4*10^(-6), 6*10^(-6), 8*10^(-6), 10^(-5))
    #bandwidth <- c(10^(-3), 4*10^(-3), 6*10^(-3), 8*10^(-3), 10^(-4))

    aErrorBatch <- matrix(1:1, nrow = 5, ncol = 5)
    #aErrorStocha <- matrix(1:1, nrow = 5, ncol = 5)

    for(i in 1:5){
        Train <- dataset[1:(0.8*length(dataset[,1])), ]
        Test <- dataset[ ((0.8*length(dataset[,1])) + 1): length(dataset[,1]) , ]
        for(k in 1:5){
            beta <- c(0,0,0,0,0,0,0,0,0,0,0,0,0)
            betaBatch <-TrainBatch(Train, beta, alphaBatch[k])
            #betaLocal <- Locally(Train, bandwidth[k])
            #betaStocha <-TrainStocha(Train, beta, alphaStoch[k])
            #betaNewton <-NewtonRaphson(Train, beta)
            #aErrorStocha[i,k] = mean(Error(Test, betaLocal))
            aErrorBatch[i,k] <- mean(Error(Test, betaBatch))        
        }
        dataset <- rbind(Test, Train)   
    }
    name = paste("Training", number, "Set", sep="")
    #print(colMeans(aErrorStocha))
    print(aErrorBatch)
    print(colSds(aErrorBatch))
    print(colMeans(aErrorBatch))
    colnames(aErrorBatch) <- c("10^(-8)" , "3*10^(-8)", "6* 10^(-8)" , "9*10^(-8)"  , "10^(-7)")
    boxplot(aErrorBatch, main = name) 
    return(colSds(aErrorBatch)/colMeans(aErrorBatch))

}


General <-function(){
    for(i in 1:20){
        name = paste("Training", i, "cereales.data", sep="")
        data <- read.table(name)
        print(GetData(data, i))
    }
}