
#Author: Vjola Hoxhaj Drs./Roel Elbers MSc.
#email: v.hoxhaj@umcutrecht.nl/r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021

initial_time<-Sys.time()
date_running_start<-Sys.Date()
#####Study_population and Source_population#####
#####Create output folders####
source(paste0(pre_dir,"/functions/create_folder.R"))

create_folder(projectFolder,"/", "g_output")
create_folder(projectFolder,"/", "g_intermediate")
create_folder(projectFolder,"g_intermediate/", "populations")
create_folder(projectFolder,"g_intermediate/", "tmp")

if(subpopulations_present=="No"){
  #Create output folders
  create_folder(projectFolder,"g_output/", "STUDY_SOURCE_POPULATION")
  create_folder(projectFolder,"g_output/STUDY_SOURCE_POPULATION/", "Time_log")
  create_folder(projectFolder,"g_output/STUDY_SOURCE_POPULATION/", "Reports")
  create_folder(projectFolder,"g_output/STUDY_SOURCE_POPULATION/", "GDPR")
  create_folder(projectFolder,"g_output/STUDY_SOURCE_POPULATION/", "Masked")
  create_folder(projectFolder,"g_output/STUDY_SOURCE_POPULATION/", "RDS")
  
  create_folder(projectFolder,"g_intermediate/tmp/", "STUDY_SOURCE_POPULATION")
  
  #set up some paths
  populations_dir<-paste0(projectFolder,"/g_intermediate/populations/")
  tmp<-paste0(projectFolder,"/g_intermediate/tmp/")
  
  #remove environment file
  if(length(list.files(paste0(projectFolder,"/g_intermediate/"),"environment"))>0){
    unlink(paste0(g_intermediate,list.files(paste0(projectFolder,"/g_intermediate/"),"environment")))
  }
}
if("available_diagnoses_list.csv" %in% list.files(paste0(projectFolder,"/p_parameters/"))){
  file.remove(paste0(projectFolder,"/p_parameters/available_diagnoses_list.csv"))
}

#######################################################
#std_source_pop_dir output folder for study_source population
#std_pop_tmp output folder for temporary files
#############################################################

#Load functions
source(paste0(pre_dir,"functions/", "CreateSpells_v15.R"))
source(paste0(pre_dir,"functions/", "CountPersonTimeV12.5.R"))
source(paste0(pre_dir,"functions/", "CountPersonTimeV13.6.R"))
source(paste0(pre_dir,"functions/", "DRECountThresholdV3.R"))
source(paste0(pre_dir,"functions/", "FUNCTIONS.R"))
source(paste0(pre_dir,"functions/", "IMPORT_PATTERN.R"))

#Set parameters
source(paste0(pre_dir,"Step_00_SetParameters.R"))

#Preparation of analyses input tables
source(paste0(pre_dir,"Step_01_CreateSpells.R"))
source(paste0(pre_dir,"Step_02_PreparePersonsTable.R"))
source(paste0(pre_dir, "Step_03_CreateSourceTable.R"))
source(paste0(pre_dir,"Step_04_CreateStudyPopulation.R"))

#
source(paste0(pre_dir,"Step_05_AddVariablesSourcePopulation.R"))
source(paste0(pre_dir,"Step_06_AddVariablesStudyPopulation.R"))
source(paste0(pre_dir,"Step_07_RunCountPersonTimeByN.R"))

#Save analyses output tables in output
source(paste0(pre_dir,"Step_Report_01_01_PopulationTree.R"))
source(paste0(pre_dir,"Step_Report_01_02_CompareStudySource.R"))
source(paste0(pre_dir,"Step_Report_01_03_Persontime.R"))
source(paste0(pre_dir,"Step_Report_01_0405_CountPersontime.R"))
source(paste0(pre_dir,"Step_Report_01_0609_AnalyseDates.R"))
source(paste0(pre_dir,"Step_Report_01_1011_FollowUp.R"))
source(paste0(pre_dir,"Step_Report_01_12_Visits.R"))
source(paste0(pre_dir,"Step_Report_01_13_LifeStyle_2.R"))
source(paste0(pre_dir,"Step_Report_01_Mask.R"))

date_running_end<-Sys.Date()
end_time<-Sys.time()

time_log<-data.table(DAP=data_access_provider_name,
                     Script="Study_source_population and Visit_and_lifestyle_factors", 
                     Start_date=date_running_start, 
                     End_date=date_running_end,
                     Time_elaspsed=format(end_time-initial_time, digits=2))
fwrite(time_log,paste0(output_dir,"STUDY_SOURCE_POPULATION/Time_log/time_study_source_population.csv"),row.names = F)