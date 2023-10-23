#Author: Vjola Hoxhaj Drs.
#email: v.hoxhaj@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 06/07/2021

initial_time<-Sys.time()
date_running_start<-Sys.Date()
date_DAP_name_part<-paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_")

####Load parameters####
source(paste0(pre_dir, "parameters/DAP_info.R")) 
source(paste0(pre_dir, "parameters/info.R")) 
source(paste0(pre_dir, "parameters/date_parameters.R"))
source(paste0(pre_dir,"/functions/create_folder.R"))
source(paste0(pre_dir, "Codelists/create_conceptsets.R")) #for diagnoses
source(paste0(pre_dir,"/functions/create_categories.R"))
source(paste0(pre_dir,"/parameters/create_categories_masking.R"))

####Create output folders####
if(subpopulations_present=="No"){
  #Create output folders
  create_folder(projectFolder,"g_output/", "DIAGNOSES")
  create_folder(projectFolder,"g_output/DIAGNOSES/", "Time_log")
  create_folder(projectFolder,"g_output/DIAGNOSES/", "Reports")
  create_folder(projectFolder,"g_output/DIAGNOSES/", "GDPR")
  create_folder(projectFolder,"g_output/DIAGNOSES/", "Masked")
  
  create_folder(projectFolder,"g_intermediate/tmp/", "EVENTS")
  create_folder(projectFolder,"g_intermediate/tmp/", "MO")
  create_folder(projectFolder,"g_intermediate/tmp/", "SO")
  create_folder(projectFolder,"g_intermediate/tmp/", "DIAGNOSES")
  create_folder(projectFolder,"g_intermediate/populations/", "DIAGNOSES")
  
  #set up some paths
  populations_dir<-paste0(projectFolder,"/g_intermediate/populations/")
  tmp<-paste0(projectFolder,"/g_intermediate/tmp/")
  diag_tmp<-paste0(tmp,"DIAGNOSES/")
  diag_pop<-paste0(populations_dir,"DIAGNOSES/")
  diag_dir<-paste0(projectFolder,"/g_output/", "DIAGNOSES/")
  events_tmp<-paste0(tmp,"EVENTS/")
  mo_tmp<-paste0(tmp,"MO/")
  so_tmp<-paste0(tmp,"SO/")
  
  
}
####Main script####
if(sum(length(actual_tables$EVENTS),length(actual_tables$MEDICAL_OBSERVATIONS),length(actual_tables$SURVEY_OBSERVATIONS))>0){
if (subpopulations_present=="Yes"){
for (s in 1: length(subpopulations_names)){
  #Load study population
  study_sub_population<-study_population_dir[grepl(study_population_dir, pattern=paste0(subpopulations_names[s],"_study_population"), fixed=T)]
  study_sub_population<-study_sub_population[grepl(study_sub_population, pattern=paste0("^", subpopulations_names[s]))]
  study_population<-readRDS(paste0(g_intermediate, "populations/", study_sub_population))[,c("person_id","sex_at_instance_creation","birth_date","end_follow_up","start_follow_up","age_start_follow_up")]
  study_population<-study_population[,person_id:=as.character(person_id)]
  study_population<-study_population[sex_at_instance_creation %in% c("F","M")]
  nr_std<-study_population[,.N]

    #MEANINGS TO BE EXCLUDED
  meanings_exclude_events<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="EVENTS" & other==subpopulations_names[s],values], pattern = " "))
  meanings_exclude_mo<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="MEDICAL_OBSERVATIONS" & other==subpopulations_names[s],values], pattern = " "))
  meanings_exclude_so<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="SURVEY_OBSERVATIONS" & other==subpopulations_names[s],values], pattern = " "))

  source(paste0(pre_dir, "Step_10_01_DIAGNOSES_L3_pre_script_EVENTS.R"))
  source(paste0(pre_dir, "Step_10_02_DIAGNOSES_L3_pre_script_MO.R"))
  source(paste0(pre_dir, "Step_10_03_DIAGNOSES_L3_pre_script_SO.R"))
  source(paste0(pre_dir, "Step_10_04_DIAGNOSES_L3_pre_script_Results.R"))
  
  do.call(file.remove, list(list.files(events_tmp, full.names = T)))
  do.call(file.remove, list(list.files(mo_tmp, full.names = T)))
  do.call(file.remove, list(list.files(so_tmp, full.names = T)))
  do.call(file.remove, list(list.files(diag_tmp, full.names = T)))

  rm(nr_std)
  }
} else {
  #Load study population
  study_population_dir<-study_population_dir[grepl(study_population_dir, pattern="ALL_study_population", fixed=T)]
  study_population<-readRDS(paste0(g_intermediate, "populations/", study_population_dir))[,c("person_id","sex_at_instance_creation","birth_date","end_follow_up","start_follow_up","age_start_follow_up")]
  study_population<-study_population[,person_id:=as.character(person_id)]
  study_population<-study_population[sex_at_instance_creation %in% c("F","M")]
  nr_std<-study_population[,.N]

  #MEANINGS TO BE EXCLUDED
  meanings_exclude_events<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="EVENTS",values], pattern = " "))
  meanings_exclude_mo<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="MEDICAL_OBSERVATIONS", values], pattern = " "))
  meanings_exclude_so<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="SURVEY_OBSERVATIONS", values], pattern = " "))
  
  source(paste0(pre_dir, "Step_10_01_DIAGNOSES_L3_pre_script_EVENTS.R"))
  source(paste0(pre_dir, "Step_10_02_DIAGNOSES_L3_pre_script_MO.R"))
  source(paste0(pre_dir, "Step_10_03_DIAGNOSES_L3_pre_script_SO.R"))
  source(paste0(pre_dir, "Step_10_04_DIAGNOSES_L3_pre_script_Results.R"))
 
  do.call(file.remove, list(list.files(events_tmp, full.names = T)))
  do.call(file.remove, list(list.files(mo_tmp, full.names = T)))
  do.call(file.remove, list(list.files(so_tmp, full.names = T)))
  do.call(file.remove, list(list.files(diag_tmp, full.names = T)))
  
  rm(nr_std)
  
}
}

date_running_end<-Sys.Date()
end_time<-Sys.time()

time_log<-data.table(DAP=data_access_provider_name,
                     Script="DIAGNOSES", 
                     Start_date=date_running_start, 
                     End_date=date_running_end,
                     Time_elaspsed=format(end_time-initial_time, digits=2))
fwrite(time_log,paste0(output_dir,"/DIAGNOSES/Time_log/time_diagnoses.csv"),row.names = F)

####Clean environment####
#Delete folders events, so, mo from tmp
unlink(paste0(tmp,"EVENTS"), recursive = T)
unlink(paste0(tmp,"MO"), recursive = T)
unlink(paste0(tmp,"SO"), recursive = T)
unlink(paste0(tmp,"DIAGNOSES"), recursive = T)


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


