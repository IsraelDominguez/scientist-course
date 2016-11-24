pollutantmean <- function(directory, pollutant, id = 1:332) {
    #select files to be imported based on the user input or default
    selected.files = readFilesDirectory(directory, id)
    #import data
    Data = lapply(file.path(directory,selected.files),read.csv)
    #convert into data frame
    Data = do.call(rbind.data.frame,Data)
    #calculate mean
    mean(Data[,pollutant],na.rm=TRUE)
}

complete <- function(directory="specdata", id = 1:332) {
    #select files to be imported based on the user input or default
    selected.files = readFilesDirectory(directory, id)
    #read complete data in all selected files
    data <- lapply(file.path(directory,selected.files),readComplete)
    #create frame with data
    data.frame(id,nobs = unlist(data))
}

corr <- function(directory, threshold = 0) {
    checkThreshold <- subset(complete(), nobs > threshold)

    # --- Assert create an empty numeric vector
    corrsNum <- numeric(0)
    
    for (cid in checkThreshold[,"id"]) {
        fileStr <- paste(directory, "/", sprintf("%03d", as.numeric(cid)), ".csv", sep = "")
        rawDfr <- read.csv(fileStr)
        corrsNum <- c(corrsNum, cor(rawDfr$sulfate, rawDfr$nitrate, use = "pairwise.complete.obs"))
    }
    corrsNum
}


readComplete <- function(csv_file) {
    my_csv = read.csv(csv_file)
    cases<-complete.cases(my_csv)
    sum(cases)
}

readFilesDirectory <- function(directory = "specdata", id = 1:332) {
    #get the file List in that directory
    fileList<-list.files(directory)
    
    #extract the file names and store as numeric for comparison
    file.names<-as.numeric(sub("\\.csv$","",fileList))
    
    #select files to be imported based on the user input or default
    selected.files<-fileList[match(id,file.names)]
}




