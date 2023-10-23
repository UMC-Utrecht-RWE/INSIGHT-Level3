#Author: Vjola Hoxhaj Drs.
#email: v.hoxhaj@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 09/07/2022

initial_time<-Sys.time()
date_running_start<-Sys.Date()

#####Source needed parameters#####
source(paste0(pre_dir, "parameters/DAP_info.R")) 
source(paste0(pre_dir,"parameters/info.R"))
source(paste0(pre_dir, "parameters/date_parameters.R"))
source(paste0(pre_dir,"/functions/create_folder.R"))
source(paste0(pre_dir,"/functions/create_categories.R"))
source(paste0(pre_dir,"/parameters/create_categories_masking.R"))


####Read excel file####
parameter_file_fl<-list.files(paste0(projectFolder,"/p_parameters/"),"study_parameters")
parameter_file<-as.data.table(read_excel(paste0(projectFolder,"/p_parameters/",parameter_file_fl),col_types = "text", sheet = "study_parameters"))
#Set parameters basic parameters
diagnoses_of_interest <- parameter_file[variable=="diagnoses_of_interest",value]
diagnoses_of_interest<-c(unlist(str_split(diagnoses_of_interest,",")))
diagnoses_of_interest<-trimws(diagnoses_of_interest, "left")
diagnoses_of_interest<-data.table(condition=diagnoses_of_interest)

#Set parameters basic parameters
vaccine_var <- parameter_file[variable=="vaccine_analysis",value]

if(vaccine_var=="ATC"){
  var_to_keep<-"vx_atc"
  indicator_to_run<-"vx_atc"}
if(vaccine_var=="type"){
  var_to_keep<-"vx_type"
  indicator_to_run<-"vx_type"}
if(vaccine_var=="both"){
  var_to_keep<-c("vx_type","vx_atc")
  indicator_to_run<-"both"}

#####Create output folders in g_output#####
if(subpopulations_present=="No"){
  #Create output folders
  create_folder(projectFolder,"g_output/", "POI")
  create_folder(projectFolder,"g_output/POI/", "EVENTS_MEDICINES")
  create_folder(projectFolder,"g_output/POI/", "EVENTS_VACCINES")
  create_folder(projectFolder,"g_output/POI/", "EVENTS_PREGNANCY")
  create_folder(projectFolder,"g_output/POI/", "PREGNANCY_MEDICINES")
  create_folder(projectFolder,"g_output/POI/", "PREGNANCY_VACCINES")
  create_folder(projectFolder,"g_output/POI/", "Time_log")
  create_folder(projectFolder,"g_output/POI/", "Reports")
  create_folder(projectFolder,"g_output/POI/", "GDPR")
  create_folder(projectFolder,"g_output/POI/", "Masked")
  create_folder(projectFolder,"g_output/POI/EVENTS_MEDICINES/", "Masked")
  create_folder(projectFolder,"g_output/POI/EVENTS_VACCINES/", "Masked")
  create_folder(projectFolder,"g_output/POI/EVENTS_PREGNANCY/", "Masked")
  create_folder(projectFolder,"g_output/POI/PREGNANCY_MEDICINES/", "Masked")
  create_folder(projectFolder,"g_output/POI/PREGNANCY_VACCINES/", "Masked")
  
  create_folder(projectFolder,"g_intermediate/tmp/", "POI")
  create_folder(projectFolder,"g_intermediate/populations/", "EVENTS_PREGNANCY")
  create_folder(projectFolder,"g_intermediate/populations/", "PREGNANCY_MEDICINES")
  create_folder(projectFolder,"g_intermediate/populations/", "PREGNANCY_VACCINES")
  
  #set up some paths
  populations_dir<-paste0(projectFolder,"/g_intermediate/populations/")
  tmp<-paste0(projectFolder,"/g_intermediate/tmp/")
  poi_tmp<-paste0(tmp,"POI/")
}


####Main script####
if(sum(c("MEDICINES","VACCINES", "DIAGNOSES", "PREGNANCY") %in% list.files(populations_dir))>0){
  if(sum(length(list.files(paste0(populations_dir,"MEDICINES/"))), 
         length(list.files(paste0(populations_dir,"VACCINES/"))), 
         length(list.files(paste0(populations_dir,"DIAGNOSES/"))), 
         length(list.files(paste0(populations_dir,"PREGNANCY/"))))>0){

  source(paste0(pre_dir,"Step_12_01_POI_L3_diagnoses_primary.R"))
    source(paste0(pre_dir,"Step_12_02_POI_L3_pregnancy_primary.R"))
  }
}

date_running_end<-Sys.Date()
end_time<-Sys.time()

time_log<-data.table(DAP=data_access_provider_name,
                     Script="POI", 
                     Start_date=date_running_start, 
                     End_date=date_running_end,
                     Time_elaspsed=format(end_time-initial_time, digits=2))
fwrite(time_log,paste0(output_dir,"/POI/Time_log/time_poi.csv"),row.names = F)

#Delete folders poi from tmp
unlink(paste0(tmp,"POI"), recursive = T)

keep_environment<-c("StudyName", "data_access_provider_name", "data_source_name", "subpopulations_names", "subpopulations_present", "subpopulations","SUBP",
                    "Age_max","Age_min", "min_age_preg","max_age_preg",
                    "start_study_date","end_study_date", "lookback_period", "intv","recommended_end_date", "date_creation","start_study_date2","end_study_date2",
                    "METADATA_subp", "actual_tables", "tmp", "s", "meanings_birth_registry","CountPersonTime2",
                    "diagnoses","pregnancies","diagnoses_pregnancy_med_vacc","diagnoses_pregnancy_med","diagnoses_pregnancy_vacc","pregnancy_only_med_vacc","pregnancy_only_med","pregnancy_only_vacc",
                    "dir_base", "populations_dir", "output_dir", "pre_dir", "study_population_dir", "g_intermediate", "path_dir","projectFolder","path","path_output",
                    "med_dir","medicines_tmp","medicines_pop", "Rmd_MEDICINES",
                    "vacc_dir", "vaccines_tmp", "vaccines_pop","Rmd_VACCINES",
                    "diag_dir", "events_tmp","mo_tmp","so_tmp","diag_tmp", "diag_pop","Rmd_DIAGNOSES", "conditions","diagnoses","duplicated_event_dates","remove_subj",
                    "preg_dir","preg_ev_tmp","preg_m_tmp","preg_s_tmp","preg_si_tmp","preg_tmp", "preg_pop","Rmd_PREGNANCY","pregnancies",
                    "poi_dir","ev_preg_dir","med_preg_dir","vacc_preg_dir","ev_med_preg_dir","ev_vacc_preg_dir","ev_med_dir","ev_vacc_dir","ev_preg_pop","med_preg_pop","vacc_preg_pop","ev_med_preg_pop","ev_vacc_preg_pop","ev_med_pop","ev_vacc_pop","poi_tmp","Rmd_POI",
                    "records_categories", "users_categories", "followup_categories")

list_rm<-ls()[ls() %!in% keep_environment]
rm(list = list_rm)
rm(list_rm)
`%!in%` = Negate(`%in%`)


