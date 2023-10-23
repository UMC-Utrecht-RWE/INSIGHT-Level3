if(!require(data.table)){install.packages("data.table")}
library(data.table)
if(!require(readxl)){install.packages("readxl")}
library(readxl)


#Save info about running scripts
parameter_file_fl<-list.files(paste0(projectFolder,"/p_parameters/"),"study_parameters")
file_to_run<-as.data.table(read_excel(paste0(projectFolder,"/p_parameters/",parameter_file_fl),col_types = "text", sheet = "scripts_to_run"))
run_study_source_population<-file_to_run[Indicator=="study_source_population",required]
run_medicines<-file_to_run[Indicator=="medicines",required]
run_vaccines<-file_to_run[Indicator=="vaccines",required]
run_diagnoses<-file_to_run[Indicator=="diagnoses",required]
run_pregnancy<-file_to_run[Indicator=="pregnancy",required]
run_poi<-file_to_run[Indicator=="poi",required]
