# required packages
library(readr)
library(ggplot2)
library(grid)
library(gridExtra)
library(reshape2)
library(ggpubr)

# import data
setwd("/Volumes/wd_elements_5tb/postdoc/1_sps_health_environment/data/all_subjects_by_questionnaire/questionnaires_separated")
data_files <- c("ASR.csv","ATQ_recoded.csv","BIG.csv","CTQ.csv",'DHU.csv', 'HEC.csv', "IDS.csv", 'LIE_3.csv', 'LIS.csv', 'MDP.csv', "OCM.csv","PSS.csv","SLS.csv","SPS-24.csv","STS.csv","UBO.csv")

# read all questionnaire files into separate data frames
for(i in 1:length(data_files)) {
  assign(paste0("df", i), read_csv(paste0(data_files[i])))
}
df_list <- list(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12, df13, df14, df15, df16)

# merge all questionnaire data frames
all_questionnaires <- Reduce(function(x, y) merge(x, y, by=c("IID"), all=TRUE), df_list)
names(all_questionnaires)[names(all_questionnaires) == "sex"] <- "sex_M1_F2" # rename column
setwd("/Volumes/wd_elements_5tb/postdoc/1_sps_health_environment/data/all_subjects_by_questionnaire")
names(all_questionnaires) 

# subset only columns of interest 
sum_questionnaires<-all_questionnaires[,c('IID','ASR_sum','ATQ_sum', 'BIG_neuroticism_sum', 'BIG_openness_sum', 'CTQ_new_sum', 'DDU_severity', 'DDH_severity', 'HEC8_sum', "IDS_sum", 'LIE_3_sum', 'age', 'sex_M1_F2', 'LIS07_education_completed', 'LIS09_country_of_birth', 'LIS23_net_household_income', 'MDP_sum', 'OCM_sum', 'PSS_sum', 'SLS_sum', 'sps_sum', 'STS_sum', 'UBO_sum')] # create dataframe for only the columns of interest
names(sum_questionnaires)
names(sum_questionnaires)[names(sum_questionnaires) == "sps_sum"] <- "SPS_sum" # rename columns
names(sum_questionnaires)[names(sum_questionnaires) == "HEC8_sum"] <- "HEC_sum"

# after visually checking distributions, log transform non-normal dependent variables
sum_questionnaires$SLS_sum_log = log(sum_questionnaires$SLS_sum)
sum_questionnaires$IDS_sum_log = log(sum_questionnaires$IDS_sum) 
sum_questionnaires$HEC_sum_log = log(sum_questionnaires$HEC_sum) 

# replace Inf in data with NA
sum_questionnaires <- do.call(data.frame, lapply(sum_questionnaires, function(x) replace(x, is.infinite(x), NA)))
data <- sum_questionnaires
names(data)

# remove all participants that did not complete the SPS questionnaire
data <- data[!is.na(data$SPS_sum),]

# remove outliers more than 3 standard deviations away from the mean, standardize, and replace with NA
data[,-1] <- lapply(data[, -1], function(x) replace(x, abs(scale(x))>3, NA))

# mean center (demean) all variables
names(data) # check column number
data_mean_centered <- sapply(data[c(2:27)], function(x) scale(x, scale=TRUE))
sum_questionnaires_clean_mean <- data_mean_centered

# export dataframe into working directory
write.csv(sum_questionnaires_clean_mean, file = "sum_questionnaires_clean_mean.csv", quote=FALSE, row.names = FALSE)  # export .csv to working dir
