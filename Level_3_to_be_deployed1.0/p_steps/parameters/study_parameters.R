####Study Parameters####
parameter_file_fl<-list.files(paste0(projectFolder,"/p_parameters/"),"study_parameters")
parameter_file<-as.data.table(read_excel(paste0(projectFolder,"/p_parameters/",parameter_file_fl),col_types = "text", sheet = "study_parameters"))

min_age_preg<-as.numeric(parameter_file[variable=="min_age_preg", value])
max_age_preg<-as.numeric(parameter_file[variable=="max_age_preg", value])
#Set parameters basic parameters
start_study_date <- parameter_file[variable=="start_study_date", value]
end_study_date <- parameter_file[variable=="end_study_date", value]
population_distribution_date <- parameter_file[variable=="population_distribution_date", value]

#start_study_date <- "20140101"
#end_study_date <- "20181231"
lookback_period <- as.numeric(parameter_file[variable=="lookback_period", value])
Age_max <- as.numeric(parameter_file[variable=="Age_max", value])
Age_min <- as.numeric(parameter_file[variable=="Age_min", value])