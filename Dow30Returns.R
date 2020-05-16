## Calcualte return for each of 30 Dow stocks and put them in one csv file

#Read in 1st data and create list of data (to be merged with return ratio)
setwd("C:/Users/sharo/OneDrive/Documents/Research Assistant/Tast1_DowJones/DowJonesTask")
aapl_raw <- read.csv(file = 'Raw Data/DowJonesData_Downloaded/AAPL.csv')
init_date_list <- aapl_raw[2:row_num,1]
Output <- as.data.frame(init_date_list)

#loop through 30 Dow price files and calculate returns, and merge with date 
listofcsv_raw <- list.files("Raw Data/DowJonesData_Downloaded/")

for (i in seq_along(listofcsv_raw)) {
  filename <- listofcsv_raw[i]
  colname <- paste(strsplit(filename,"[.]")[[1]][1],"Return")
  file_location <- paste("Raw Data/DowJonesData_Downloaded/",filename, sep = "")
  raw_data <- read.csv(file = file_location)
  
  row_num <- nrow(raw_data)
  raw_adjclosing <- raw_data[, "Adj.Close", drop = FALSE]
  raw_return <- (raw_adjclosing[2:row_num,1]/raw_adjclosing[1:(row_num-1),1]-1)
  date_list <- raw_data[2:row_num,1]
  #names(raw_return) <- raw_data[2:row_num,1]
  raw_return_df <- data.frame(Date = date_list, stock_return = raw_return)
  names(raw_return_df)[names(raw_return_df) == "stock_return"] <- colname
  
  Output = merge(Output, raw_return_df, by.x = 'init_date_list', by.y='Date', all = TRUE)
  
  #print(row_num)
}

write.csv(Output,"Dow30Returns.csv", row.names = FALSE)


