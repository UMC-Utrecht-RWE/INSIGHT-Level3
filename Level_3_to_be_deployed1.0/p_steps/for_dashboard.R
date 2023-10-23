#Script to collect and organize all the csv output files

source(paste0(pre_dir,"/functions/create_folder.R"))
create_folder(projectFolder,"g_output/", "ForDashboard")

####METADATA, CDM SOURCE and INSTANCE tables####
METADATA_files<-list.files(path_dir, "^METADATA")
METADATA<-lapply(paste0(path_dir, METADATA_files), fread)
METADATA<-as.data.table(do.call(rbind, METADATA))

CDM_SOURCE_files<-list.files(path_dir, "^CDM_SOURCE")
CDM_SOURCE<-lapply(paste0(path_dir, CDM_SOURCE_files), fread)
CDM_SOURCE<-as.data.table(do.call(rbind, CDM_SOURCE))

INSTANCE_files<-list.files(path_dir, "^INSTANCE")
INSTANCE<-lapply(paste0(path_dir, INSTANCE_files), fread)
INSTANCE<-as.data.table(do.call(rbind, INSTANCE))

date_creation<-paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"))
instance_number<-CDM_SOURCE[,instance_number]
data_access_provider_name<-CDM_SOURCE[,data_access_provider_name]

date_DAP_name_part<-paste0("_",data_access_provider_name,"_",date_creation,"_")
create_folder(projectFolder,"g_output/ForDashboard/", paste0("Level3",date_DAP_name_part,instance_number))

outputs_path<-paste0(projectFolder,"/g_output/ForDashboard/","Level3",date_DAP_name_part,instance_number,"/")
create_folder(projectFolder,paste0("/g_output/ForDashboard/","Level3",date_DAP_name_part,instance_number,"/"), "Reports")
reports_path<-paste0(outputs_path,"Reports/")

#export to ForDashboard folder
fwrite(METADATA, paste0(outputs_path, "METADATA.csv"), row.names = F)
fwrite(CDM_SOURCE, paste0(outputs_path, "CDM_SOURCE.csv"), row.names = F)
fwrite(INSTANCE, paste0(outputs_path, "INSTANCE.csv"), row.names = F)

files<-list.files(paste0(projectFolder,"/g_output/"))
####Study source population###
if("STUDY_SOURCE_POPULATION" %in% files){
  csv_files<-list.files(paste0(projectFolder,"/g_output/STUDY_SOURCE_POPULATION/"), "\\.csv$", full.names = T)
  file.copy(from = csv_files, to = outputs_path, overwrite = TRUE)
  reports<-list.files(paste0(projectFolder,"/g_output/STUDY_SOURCE_POPULATION/Reports/"), "\\.html$", full.names = T)
  file.copy(from = reports, to = paste0(outputs_path, "Reports/"), overwrite = TRUE)
  rm(csv_files, reports)
}
####MEDICINES###
if("MEDICINES" %in% files){
  csv_files<-list.files(paste0(projectFolder,"/g_output/MEDICINES/"), "\\.csv$", full.names = T)
  file.copy(from = csv_files, to = outputs_path, overwrite = TRUE)
  reports<-list.files(paste0(projectFolder,"/g_output/MEDICINES/Reports/"), "\\.html$", full.names = T)
  file.copy(from = reports, to = paste0(outputs_path, "Reports/"), overwrite = TRUE)
  rm(csv_files, reports)
}
####VACCINES###
if("VACCINES" %in% files){
  csv_files<-list.files(paste0(projectFolder,"/g_output/VACCINES/"), "\\.csv$", full.names = T)
  file.copy(from = csv_files, to = outputs_path, overwrite = TRUE)
  reports<-list.files(paste0(projectFolder,"/g_output/VACCINES/Reports/"), "\\.html$", full.names = T)
  file.copy(from = reports, to = paste0(outputs_path, "Reports/"), overwrite = TRUE)
  rm(csv_files, reports)
}
####DIAGNOSES###
if("DIAGNOSES" %in% files){
  csv_files<-list.files(paste0(projectFolder,"/g_output/DIAGNOSES/"), "\\.csv$", full.names = T)
  file.copy(from = csv_files, to = outputs_path, overwrite = TRUE)
  reports<-list.files(paste0(projectFolder,"/g_output/DIAGNOSES/Reports/"), "\\.html$", full.names = T)
  file.copy(from = reports, to = paste0(outputs_path, "Reports/"), overwrite = TRUE)
  rm(csv_files, reports)
}
####PREGNANCY###
if("PREGNANCY" %in% files){
  csv_files<-list.files(paste0(projectFolder,"/g_output/PREGNANCY/"), "\\.csv$", full.names = T)
  file.copy(from = csv_files, to = outputs_path, overwrite = TRUE)
  reports<-list.files(paste0(projectFolder,"/g_output/PREGNANCY/Reports/"), "\\.html$", full.names = T)
  file.copy(from = reports, to = paste0(outputs_path, "Reports/"), overwrite = TRUE)
  rm(csv_files, reports)
}
####POI###
if("POI" %in% files){
  csv_files<-list.files(paste0(projectFolder,"/g_output/POI/"), "\\.csv$", full.names = T)
  file.copy(from = csv_files, to = outputs_path, overwrite = TRUE)
  reports<-list.files(paste0(projectFolder,"/g_output/POI/Reports/"), "\\.html$", full.names = T)
  file.copy(from = reports, to = paste0(outputs_path, "Reports/"), overwrite = TRUE)
  rm(csv_files, reports)
}
####EUROCAT###
# if("EUROCAT" %in% files){
#   csv_files<-list.files(paste0(projectFolder,"/g_output/EUROCAT/"), "\\.csv$", full.names = T)
#   file.copy(from = csv_files, to = outputs_path, overwrite = TRUE)
#   reports<-list.files(paste0(projectFolder,"/g_output/EUROCAT/Reports/"), "\\.html$", full.names = T)
#   file.copy(from = reports, to = paste0(outputs_path, "Reports/"), overwrite = TRUE)
#   rm(csv_files, reports)
# }