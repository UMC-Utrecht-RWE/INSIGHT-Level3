#Author: Vjola Hoxhaj Drs.
#email: v.hoxhaj@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 06/07/2021

initial_time<-Sys.time()
date_running_start<-Sys.Date()
date_DAP_name_part<-paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_")

#####Source needed parameters#####
source(paste0(pre_dir, "parameters/DAP_info.R")) 
source(paste0(pre_dir,"parameters/info.R"))
source(paste0(pre_dir, "parameters/date_parameters.R"))
source(paste0(pre_dir,"/functions/create_folder.R"))
source(paste0(pre_dir, "Codelists/create_conceptsets_pregnancy.R")) #for diagnoses
source(paste0(pre_dir,"/functions/create_categories.R"))
source(paste0(pre_dir,"/parameters/create_categories_masking.R"))


####Create output folders#####
if(subpopulations_present=="No"){
  #Create output folders
  create_folder(projectFolder,"g_output/", "PREGNANCY")
  create_folder(projectFolder,"g_output/PREGNANCY/", "Time_log")
  create_folder(projectFolder,"g_output/PREGNANCY/", "Reports")
  create_folder(projectFolder,"g_output/PREGNANCY/", "GDPR")
  create_folder(projectFolder,"g_output/PREGNANCY/", "Masked")
  
  create_folder(projectFolder,"g_intermediate/tmp/", "PREGNANCY_EV")
  create_folder(projectFolder,"g_intermediate/tmp/", "PREGNANCY_M")
  create_folder(projectFolder,"g_intermediate/tmp/", "PREGNANCY_S")
  create_folder(projectFolder,"g_intermediate/tmp/", "PREGNANCY_SI")
  create_folder(projectFolder,"g_intermediate/tmp/", "PREG")
  create_folder(projectFolder,"g_intermediate/populations/", "PREGNANCY")
  
  #set up some paths
  populations_dir<-paste0(projectFolder,"/g_intermediate/populations/")
  tmp<-paste0(projectFolder,"/g_intermediate/tmp/")
  preg_tmp<-paste0(tmp,"PREG/")
  preg_pop<-paste0(populations_dir,"PREGNANCY/")
  preg_ev_tmp<-paste0(tmp,"PREGNANCY_EV/")
  preg_m_tmp<-paste0(tmp,"PREGNANCY_M/")
  preg_s_tmp<-paste0(tmp,"PREGNANCY_S/")
  preg_si_tmp<-paste0(tmp,"PREGNANCY_SI/")
}
###Meanings of importance####
####Read excel file####
parameter_file_fl<-list.files(paste0(projectFolder,"/p_parameters/"),"study_parameters")
parameter_file<-as.data.table(read_excel(paste0(projectFolder,"/p_parameters/",parameter_file_fl),col_types = "text", sheet = "study_parameters"))
#Set parameters basic parameters
meanings_start_pregnancy <- c(unlist(str_split(parameter_file[variable=="meanings_start_pregnancy",value]," ")))
meanings_interruption_pregnancy<-c(unlist(str_split(parameter_file[variable=="meanings_interruption_pregnancy",value]," ")))
meanings_ongoing_pregnancy<-c(unlist(str_split(parameter_file[variable=="meanings_ongoing_pregnancy",value]," ")))
meanings_end_pregnancy<-c(unlist(str_split(parameter_file[variable=="meanings_end_pregnancy",value]," ")))

####Main script####
if(sum(length(actual_tables$EVENTS),length(actual_tables$MEDICAL_OBSERVATIONS),length(actual_tables$SURVEY_OBSERVATIONS), length(actual_tables$SURVEY_ID))>0){
  if (subpopulations_present=="Yes"){
    for (s in 1: length(subpopulations_names)){
      study_sub_population<-study_population_dir[grepl(study_population_dir, pattern=paste0(subpopulations_names[s],"_study_population"))]
      study_sub_population<-study_sub_population[grepl(study_sub_population, pattern=paste0("^", subpopulations_names[s]))]
      study_population<-readRDS(paste0(g_intermediate, "populations/", study_sub_population))[,c("person_id","sex_at_instance_creation","birth_date","end_follow_up","start_follow_up","age_start_follow_up")]
      study_population<-study_population[sex_at_instance_creation=="F"]
      
      study_population[,start_follow_up_min:=birth_date + min_age_preg*365.25]
      study_population[,start_follow_up_min:=as.IDate(start_follow_up_min)]
      study_population[, start_follow_up_preg:=fifelse(start_follow_up_min>start_follow_up, start_follow_up_min, start_follow_up)]
      study_population[,start_follow_up_min:=NULL][,start_follow_up:=NULL]
      
      study_population[,end_follow_up_max:=birth_date + max_age_preg*365.25]
      study_population[,end_follow_up_max:=as.IDate(end_follow_up_max)]
      study_population[, end_follow_up_preg:=fifelse(end_follow_up_max>end_follow_up, end_follow_up, end_follow_up_max)]
      study_population[,end_follow_up_max:=NULL][,end_follow_up:=NULL]
      
      study_population[start_follow_up_preg>end_follow_up_preg, filter:=1]
      study_population<-study_population[is.na(filter)]
      study_population[,filter:=NULL]
      
      nr_std<-study_population[,.N]
      #MEANINGS TO BE EXCLUDED
      meanings_exclude_events<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="EVENTS" & other==subpopulations_names[s],values], pattern = " "))
      meanings_exclude_mo<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="MEDICAL_OBSERVATIONS" & other==subpopulations_names[s],values], pattern = " "))
      meanings_exclude_so<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="SURVEY_OBSERVATIONS" & other==subpopulations_names[s],values], pattern = " "))
      meanings_exclude_si<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="SURVEY_ID" & other==subpopulations_names[s],values], pattern = " "))
      
      meanings_birth_registry<-c(meanings_start_pregnancy,meanings_interruption_pregnancy,meanings_ongoing_pregnancy,meanings_end_pregnancy)
      
      source(paste0(pre_dir,"Step_11_01_PREGNANCY_L3_pre_script_EVENTS.R"))
      source(paste0(pre_dir,"Step_11_02_PREGNANCY_L3_pre_script_MO.R"))
      source(paste0(pre_dir,"Step_11_03_PREGNANCY_L3_pre_script_SO.R"))
      source(paste0(pre_dir,"Step_11_04_PREGNANCY_L3_pre_script_SI.R"))
      source(paste0(pre_dir,"Step_11_05_PREGNANCY_L3_pre_script_Results.R"))
      
      
      do.call(file.remove, list(list.files(preg_ev_tmp, full.names = T)))
      do.call(file.remove, list(list.files(preg_m_tmp, full.names = T)))
      do.call(file.remove, list(list.files(preg_s_tmp, full.names = T)))
      do.call(file.remove, list(list.files(preg_si_tmp, full.names = T)))
      do.call(file.remove, list(list.files(preg_tmp, full.names = T)))
      
    }
  } else {
    study_population_dir<-study_population_dir[grepl(study_population_dir, pattern="ALL_study_population", fixed=T)]
    study_population<-readRDS(paste0(g_intermediate, "populations/", study_population_dir))[,c("person_id","sex_at_instance_creation","birth_date","end_follow_up","start_follow_up","age_start_follow_up")]
    study_population<-study_population[sex_at_instance_creation=="F"]
    #create date when subjects arrive min age of pregnancy
    study_population[,start_follow_up_min:=birth_date + min_age_preg*365.25]
    study_population[,start_follow_up_min:=as.IDate(start_follow_up_min)]
    study_population[, start_follow_up_preg:=fifelse(start_follow_up_min>start_follow_up, start_follow_up_min, start_follow_up)]
    study_population[,start_follow_up_min:=NULL][,start_follow_up:=NULL]
    #create date when subjects arrive max age of pregnancy
    study_population[,end_follow_up_max:=birth_date + max_age_preg*365.25]
    study_population[,end_follow_up_max:=as.IDate(end_follow_up_max)]
    study_population[,end_follow_up_preg:=fifelse(end_follow_up_max>end_follow_up, end_follow_up, end_follow_up_max)]
    study_population[,end_follow_up_max:=NULL][,end_follow_up:=NULL]
    #if start_follow_up later than end_follow_up remove those records
    study_population[start_follow_up_preg>end_follow_up_preg, filter:=1]
    study_population<-study_population[is.na(filter)]
    study_population[,filter:=NULL]
    nr_std<-study_population[,.N]
    
    #MEANINGS TO BE EXCLUDED
    meanings_exclude_events<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="EVENTS",values], pattern = " "))
    meanings_exclude_mo<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="MEDICAL_OBSERVATIONS", values], pattern = " "))
    meanings_exclude_so<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="SURVEY_OBSERVATIONS", values], pattern = " "))
    meanings_exclude_si<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="SURVEY_ID", values], pattern = " "))
    
    meanings_birth_registry<-c(meanings_start_pregnancy,meanings_interruption_pregnancy,meanings_ongoing_pregnancy,meanings_end_pregnancy)
    meanings_birth_registry<-meanings_birth_registry[!is.na(meanings_birth_registry)]
    source(paste0(pre_dir,"Step_11_01_PREGNANCY_L3_pre_script_EVENTS.R"))
    source(paste0(pre_dir,"Step_11_02_PREGNANCY_L3_pre_script_MO.R"))
    source(paste0(pre_dir,"Step_11_03_PREGNANCY_L3_pre_script_SO.R"))
    source(paste0(pre_dir,"Step_11_04_PREGNANCY_L3_pre_script_SI.R"))
    source(paste0(pre_dir,"Step_11_05_PREGNANCY_L3_pre_script_Results.R"))
    
    do.call(file.remove, list(list.files(preg_ev_tmp, full.names = T)))
    do.call(file.remove, list(list.files(preg_m_tmp, full.names = T)))
    do.call(file.remove, list(list.files(preg_s_tmp, full.names = T)))
    do.call(file.remove, list(list.files(preg_si_tmp, full.names = T)))
    do.call(file.remove, list(list.files(preg_tmp, full.names = T)))
  }
}

date_running_end<-Sys.Date()
end_time<-Sys.time()

time_log<-data.table(DAP=data_access_provider_name,
                     Script="PREGNANCY", 
                     Start_date=date_running_start, 
                     End_date=date_running_end,
                     Time_elaspsed=format(end_time-initial_time, digits=2))
fwrite(time_log,paste0(output_dir,"/PREGNANCY/Time_log/time_pregnancy.csv"),row.names = F)

#Delete folders events, so, mo from tmp
unlink(paste0(tmp,"PREGNANCY_EV"), recursive = T)
unlink(paste0(tmp,"PREGNANCY_M"), recursive = T)
unlink(paste0(tmp,"PREGNANCY_S"), recursive = T)
unlink(paste0(tmp,"PREGNANCY_SI"), recursive = T)
unlink(paste0(tmp,"PREG"), recursive = T)

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


