GetData <- function(dataset){

    alphaBatch <- c(10^(-8), 3*10^(-8), 6* 10^(-8), 9*10^(-8), 10^(-7))
    alphaStoch <- c(2*10^(-6), 4*10^(-6), 6*10^(-6), 8*10^(-6), 10^(-5))
    beta <- c(0,0,0,0,0,0,0,0,0,0,0,0,0)
    aErrorBatch <- c()
    aErrorStocha <- c()

    for(i in 1:5){
    	Train <- dataset[1:(0.75*length(dataset)), 1:13]
    	Test <- dataset[(0.75*length(dataset) + 1):length(dataset), 1:13]
    	for(k in 1:5){
	        betaBatch <-TrainBatch(Train, beta, alphaBatch[k] )
	        betaStocha <-TrainStocha(Train, beta, alphaStoch[k] )
	        aErrorBatch <- rbind(aErrorBatch, Error(Test, betaBatch) )
	        aErrorStocha <- rbind(aErrorStocha, Error(Test, betaStocha) ) 
    	}
    	dataset <- rbind(Test, Train)	
    }

    print(colMeans(aErrorStocha))
    print(colMeans(aErrorBatch))

}