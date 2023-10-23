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
source(paste0(pre_dir,"parameters/date_parameters.R"))
source(paste0(pre_dir,"/functions/create_age_band.R"))
source(paste0(pre_dir,"/functions/create_folder.R"))
source(paste0(pre_dir,"/functions/create_categories.R"))
source(paste0(pre_dir,"/parameters/create_categories_masking.R"))

####vaccine indicator####
####Read excel file####
parameter_file_fl<-list.files(paste0(projectFolder,"/p_parameters/"),"study_parameters")
parameter_file<-as.data.table(read_excel(paste0(projectFolder,"/p_parameters/",parameter_file_fl),col_types = "text", sheet = "study_parameters"))
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


#functions
#calculate the number of records for desired atc level by meaning and year
#counts are stratified by meaning,year and atc code
#total are stratified by meaning and year
m_year_atc<-function(dt, year_var, meaning_var, atc_var, level_num,indicator_to_run){
  dt[,meaning:=dt[[meaning_var]]]
  if(indicator_to_run=="vx_atc"){
  dt[,atc_sub:=substr(get(atc_var),1,level_num)]
  a.1<-dt[complete.cases(meaning) & complete.cases(year) & complete.cases(atc_sub), .N, by=.(year, meaning, atc_sub)]
  setnames(a.1, "N", "count")
  setnames(a.1, "atc_sub", "vaccine_indicator")
  a.2<-dt[complete.cases(meaning) & complete.cases(year) & complete.cases(atc_sub), .N, by=.(meaning, year)]
  setnames(a.2, "N", "total")
  }
  
  if(sum(indicator_to_run=="vx_type"|indicator_to_run=="both")>0){
    dt[,atc_var:=vaccine_indicator]
    a.1<-dt[complete.cases(meaning) & complete.cases(year) & complete.cases(atc_var), .N, by=.(year, meaning, atc_var)]
    setnames(a.1, "N", "count")
    setnames(a.1, "atc_var", "vaccine_indicator")
    a.2<-dt[complete.cases(meaning) & complete.cases(year) & complete.cases(atc_var), .N, by=.(meaning, year)]
    setnames(a.2, "N", "total")
  }
  results<-list("count"=a.1, "total"=a.2)
  return(results)
}
####Create output folders####
if(subpopulations_present=="No"){
  #Create output folders
  create_folder(projectFolder,"g_output/", "VACCINES")
  create_folder(projectFolder,"g_output/VACCINES/", "Time_log")
  create_folder(projectFolder,"g_output/VACCINES/", "Reports")
  create_folder(projectFolder,"g_output/VACCINES/", "GDPR")
  create_folder(projectFolder,"g_output/VACCINES/", "Masked")
  
  create_folder(projectFolder,"g_intermediate/tmp/", "VACCINES")
  create_folder(projectFolder,"g_intermediate/populations/", "VACCINES")
  
  #set up some paths
  populations_dir<-paste0(projectFolder,"/g_intermediate/populations/")
  tmp<-paste0(projectFolder,"/g_intermediate/tmp/")
  vaccines_tmp<-paste0(tmp,"VACCINES/")
  vaccines_pop<-paste0(populations_dir,"VACCINES/")
  vacc_dir<-paste0(projectFolder,"/g_output/", "VACCINES/")
  
}

####Main script####
if(length(actual_tables$VACCINES)>0){
if (subpopulations_present=="Yes"){
  for (s in 1: length(subpopulations_names)){
    study_sub_population<-study_population_dir[grepl(study_population_dir, pattern=paste0(subpopulations_names[s],"_study_population"), fixed=T)]
    study_sub_population<-study_sub_population[grepl(study_sub_population, pattern=paste0("^", subpopulations_names[s]))]
    study_population<-readRDS(paste0(g_intermediate, "populations/", study_sub_population))[,c("person_id","sex_at_instance_creation","birth_date","end_follow_up","start_follow_up","age_start_follow_up")]
    
    study_population<-study_population[,person_id:=as.character(person_id)]
    study_population<-study_population[sex_at_instance_creation %in% c("F","M")]
    nr_std<-study_population[,.N]
    study_population[,vaccines_rec:=0]
    #MEANINGS TO BE EXCLUDED
    meanings_exclude_vx<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="VACCINES" & other==subpopulations_names[s],values], pattern = " "))
    
    source(paste0(pre_dir, "Step_09_01_VACCINES_L3_pre_script_2.R"))
    source(paste0(pre_dir, "Step_09_02_VACCINES_L3_counts_new_type.R"))
    source(paste0(pre_dir, "Step_09_03_VACCINES_L3_rates_new.R"))
    
    #clean vaccines_tmp
    for(i in 1:length(list.files(vaccines_tmp))){
      unlink(paste0(vaccines_tmp,list.files(vaccines_tmp)[i]))
    }
    
    
    rm(nr_std)
  }
} else {
  study_population_dir<-study_population_dir[grepl(study_population_dir, pattern="ALL_study_population", fixed=T)]
  study_population<-readRDS(paste0(g_intermediate, "populations/", study_population_dir))[,c("person_id","sex_at_instance_creation","birth_date","end_follow_up","start_follow_up","age_start_follow_up")]
  study_population<-study_population[,person_id:=as.character(person_id)]
  #select only females and males
  study_population<-study_population[sex_at_instance_creation %in% c("F","M")]
  nr_std<-study_population[,.N]
  study_population[,vaccines_rec:=0]
  #MEANINGS TO BE EXCLUDED
  meanings_exclude_vx<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="VACCINES",values], pattern = " "))
  
  source(paste0(pre_dir, "Step_09_01_VACCINES_L3_pre_script_2.R"))
  source(paste0(pre_dir, "Step_09_02_VACCINES_L3_counts_new_type.R"))
  source(paste0(pre_dir, "Step_09_03_VACCINES_L3_rates_new.R"))
  
  do.call(file.remove, list(list.files(vaccines_tmp, full.names = T)))
  
  rm(nr_std)
  
  
}
}

date_running_end<-Sys.Date()
end_time<-Sys.time()

time_log<-data.table(DAP=data_access_provider_name,
                     Script="VACCINES", 
                     Start_date=date_running_start, 
                     End_date=date_running_end,
                     Time_elaspsed=format(end_time-initial_time, digits=2))
fwrite(time_log,paste0(output_dir,"/VACCINES/Time_log/time_vaccines.csv"),row.names = F)

####Clean environment####
#Delete folders vaccines from tmp
unlink(paste0(tmp,"VACCINES"), recursive = T)


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







