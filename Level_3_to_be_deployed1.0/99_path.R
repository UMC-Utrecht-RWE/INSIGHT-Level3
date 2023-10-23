#Directory
setwd('..') #in Data Characterisation
setwd('..') #in ConcePTION/STUDY NAME
dir_base<-getwd()
# set the name of the study
#StudyName <- "ConcePTION"

#List files in dir_base
folders_present<-list.files(dir_base)
if("CDMInstances" %in% folders_present & "Data characterisation" %in% folders_present){

studies_present<-list.files(paste0(dir_base, "/CDMInstances/"))
  
  ####Read excel file####
  parameter_file_fl<-list.files(paste0(projectFolder,"/p_parameters/"),"study_parameters")
  parameter_file<-as.data.table(read_excel(paste0(projectFolder,"/p_parameters/",parameter_file_fl),col_types = "text", sheet = "study_parameters"))
  #Set parameters basic parameters
  StudyName <- parameter_file[variable=="StudyName",value]

  if(StudyName %in% studies_present){
  path_dir<-paste0(dir_base,"/CDMInstances/",StudyName,"/")
  path<-path_dir
  
#Set the path to where you want your report to be saved(make sure that the output folder already exists)
output_dir<-paste0(projectFolder,"/g_output/")
path_output<-output_dir

pre_dir<-paste0(projectFolder,"/p_steps/")

g_intermediate<-paste0(projectFolder,"/g_intermediate/")
tmp<-paste0(g_intermediate,"tmp/")
populations_dir<-paste0(g_intermediate,"populations/")

#Extra paths
#g_output folder
med_dir<-paste0(output_dir,"MEDICINES/")
vacc_dir<-paste0(output_dir,"VACCINES/")
diag_dir<-paste0(output_dir, "DIAGNOSES/")
preg_dir<-paste0(output_dir, "PREGNANCY/")
poi_dir<-paste0(output_dir, "POI/")

eurocat_dir<-paste0(output_dir,"EUROCAT/")

  }else{
  stop(print("The study name in the `study_parameters.xlsx` doesn't match with the folder name inside CDMInstances.This folder is used to store all .csv files of the ConcePTION CDM."))
}
}else{
  stop(print("The folder structure is wrong. In the main project folder both folders 'CDMInstances' and 'Data characterisation' should be present."))
}