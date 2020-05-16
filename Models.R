##Read in data -- returns and fama parameters
setwd("C:/Users/sharo/OneDrive/Documents/Research Assistant/Tast1_DowJones/DowJonesTask")
model_raw <- read.csv(file = 'Dow30_ModelInput.csv')
head(model_raw)

## Cut data for 160 Year&Month and loop through each month

#Create a list of Dow 30 stocks
col_name = colnames(model_raw)
col_name_selected = col_name[3:32]
Dow30_names = vector()
for (i in seq_along(col_name_selected)){
  Dow30_name <- strsplit(col_name_selected,"[.]")[[i]][1]
  Dow30_names[i] <- Dow30_name
}
Dow30_names

#Select data by year&month (159 sets of data)
yearmonth_list <- unique(model_raw["Year_Month"])
yearmonth_num <- nrow(yearmonth_list)
#Read in frame for coeff output
Coef_frame <- read.csv(file = 'ModelCoeff_frame.csv')
fama3_rmkrf_Coefframe <- read.csv(file = 'ModelCoeff_frame.csv')
fama3_SMB_Coefframe <- read.csv(file = 'ModelCoeff_frame.csv')
fama3_HML_Coefframe <- read.csv(file = 'ModelCoeff_frame.csv')
fama4_rmkrf_Coefframe <- read.csv(file = 'ModelCoeff_frame.csv')
fama4_SMB_Coefframe <- read.csv(file = 'ModelCoeff_frame.csv')
fama4_HML_Coefframe <- read.csv(file = 'ModelCoeff_frame.csv')
fama4_RMW_Coefframe <- read.csv(file = 'ModelCoeff_frame.csv')
fama5_rmkrf_Coefframe <- read.csv(file = 'ModelCoeff_frame.csv')
fama5_SMB_Coefframe <- read.csv(file = 'ModelCoeff_frame.csv')
fama5_HML_Coefframe <- read.csv(file = 'ModelCoeff_frame.csv')
fama5_RMW_Coefframe <- read.csv(file = 'ModelCoeff_frame.csv')
fama5_CMA_Coefframe <- read.csv(file = 'ModelCoeff_frame.csv')



#Apply Models
write_row = 1
for (i in 1:yearmonth_num){
  #select data by year&month (end up with 159 subsets)
  Dow30_permonth <- subset(model_raw, Year_Month == yearmonth_list[i,1])
  Dow30_currmonth = Dow30_permonth['Year_Month'][1,1]
  #print(Dow30_currmonth)
  
  
  for (j in seq_along(Dow30_names)) {
    #Apply CAPM for each subset of data
    yvar_colname = paste(Dow30_names[j],".Return", sep="")
    camp_name = paste(Dow30_names[j],"_capm", sep="")
    yvar_colname
    camp_name
    capm_name <- lm(paste(yvar_colname," ~ RF + Mkt.RF",sep = ""),data = Dow30_permonth)
    capm_coef = capm_name$coefficients[3]
    capm_coef
    summary(capm_name)
    Coef_frame[write_row,j+1] = capm_coef
    
    
    
    #Apply Fama French 3 Factors Model for each subset of data
    fama3_name = paste(Dow30_names[j],"_fama3", sep="")
    fama3_name
    fama3_name <- lm(paste(yvar_colname," ~ RF + Mkt.RF + SMB + HML",sep = ""),data = Dow30_permonth)
    fama3_coef_rmkrf = fama3_name$coefficients[3]
    fama3_coef_rmkrf
    fama3_coef_smb = fama3_name$coefficients[4]
    fama3_coef_hml = fama3_name$coefficients[5]
    fama3_coef_smb
    fama3_coef_hml
    #summary(fama3_name)
    
    fama3_rmkrf_Coefframe[write_row,j+1] = fama3_coef_rmkrf
    fama3_SMB_Coefframe[write_row,j+1] = fama3_coef_smb
    fama3_HML_Coefframe[write_row,j+1] = fama3_coef_hml
    
    
    
    
    #Apply Fama Cohort 4 Factors Model for each subset of data
    fama4_name = paste(Dow30_names[j],"_fama3", sep="")
    fama4_name
    fama4_name <- lm(paste(yvar_colname," ~ RF + Mkt.RF + SMB + HML + RMW",sep = ""),data = Dow30_permonth)
    fama4_coef_rmkrf = fama4_name$coefficients[3]
    summary(fama4_name)
    fama4_coef_smb = fama4_name$coefficients[4]
    fama4_coef_hml = fama4_name$coefficients[5]
    fama4_coef_rmw = fama4_name$coefficients[6]
    
    fama4_coef_smb
    fama4_coef_hml
    fama4_coef_rmw
    
    fama4_rmkrf_Coefframe[write_row,j+1] = fama4_coef_rmkrf
    fama4_SMB_Coefframe[write_row,j+1] = fama4_coef_smb
    fama4_HML_Coefframe[write_row,j+1] = fama4_coef_hml
    fama4_RMW_Coefframe[write_row,j+1] = fama4_coef_rmw
    
    
    
    
    #Apply Fama French 5 Factors Model for each subset of data
    fama5_name = paste(Dow30_names[j],"_fama3", sep="")
    fama5_name
    fama5_name <- lm(paste(yvar_colname," ~ RF + Mkt.RF + SMB + HML + RMW + CMA",sep = ""),data = Dow30_permonth)
    fama5_coef_rmkrf = fama5_name$coefficients[3]
    summary(fama5_name)
    fama5_coef_smb = fama5_name$coefficients[4]
    fama5_coef_hml = fama5_name$coefficients[5]
    fama5_coef_rmw = fama5_name$coefficients[6]
    fama5_coef_cma = fama5_name$coefficients[7]
    
    fama5_coef_smb
    fama5_coef_hml
    fama5_coef_rmw
    fama5_coef_cma
    
    fama5_rmkrf_Coefframe[write_row,j+1] = fama5_coef_rmkrf
    fama5_SMB_Coefframe[write_row,j+1] = fama5_coef_smb
    fama5_HML_Coefframe[write_row,j+1] = fama5_coef_hml
    fama5_RMW_Coefframe[write_row,j+1] = fama5_coef_rmw
    fama5_CMA_Coefframe[write_row,j+1] = fama5_coef_cma
    
    
  } 
  write_row = write_row+1
}

write.csv(Coef_frame,"Model_Coeff/CAMP_Coeff.csv", row.names = FALSE)
write.csv(fama3_rmkrf_Coefframe,"Model_Coeff/Fama3_RmkRf_Coeff.csv", row.names = FALSE)
write.csv(fama3_SMB_Coefframe,"Model_Coeff/Fama3_SMB_Coeff.csv", row.names = FALSE)
write.csv(fama3_HML_Coefframe,"Model_Coeff/Fama3_HML_Coeff.csv", row.names = FALSE)

write.csv(fama4_rmkrf_Coefframe,"Model_Coeff/Fama4_RmkRf_Coeff.csv", row.names = FALSE)
write.csv(fama4_SMB_Coefframe,"Model_Coeff/Fama4_SMB_Coeff.csv", row.names = FALSE)
write.csv(fama4_HML_Coefframe,"Model_Coeff/Fama4_HML_Coeff.csv", row.names = FALSE)
write.csv(fama4_RMW_Coefframe,"Model_Coeff/Fama4_RMW_Coeff.csv", row.names = FALSE)

write.csv(fama5_rmkrf_Coefframe,"Model_Coeff/Fama5_RmkRf_Coeff.csv", row.names = FALSE)
write.csv(fama5_SMB_Coefframe,"Model_Coeff/Fama5_SMB_Coeff.csv", row.names = FALSE)
write.csv(fama5_HML_Coefframe,"Model_Coeff/Fama5_HML_Coeff.csv", row.names = FALSE)
write.csv(fama5_RMW_Coefframe,"Model_Coeff/Fama5_RMW_Coeff.csv", row.names = FALSE)
write.csv(fama5_CMA_Coefframe,"Model_Coeff/Fama5_CMA_Coeff.csv", row.names = FALSE)













