
#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021

if(SUBP) {
  SCHEME_03 <- copy(subpopulation_meanings)
  SCHEME_03 <- SCHEME_03[, ':=' (file_in = paste0(subpopulations,"_OBS_SPELLS.rds"), file_out = paste0(subpopulations,"_source_population.rds"), folder_out = "tmp2") ]
  SCHEME_03 <- rbind(data.frame(subpopulations = c("ALL"),meaning_sets = "ALL",file_in = "ALL_OBS_SPELLS.rds", file_out = "ALL_source_population.rds", folder_out = "tmp2"),SCHEME_03)
  }
  
if(!SUBP) SCHEME_03 <- data.frame(subpopulations = c("ALL"),file_in = "ALL_OBS_SPELLS.rds", file_out = "ALL_source_population.rds", folder_out = "tmp2")

# SCHEME_03$nrows <- as.integer(NA)
# SCHEME_03$ncols <- as.integer(NA)
# SCHEME_03$ncolsneeded <- 19

PERSONS <- readRDS(paste0(std_pop_tmp,"PERSONS.rds"))

for(i in 1:nrow(SCHEME_03)){
  
  SPELLS <- readRDS(paste0(std_pop_tmp,SCHEME_03[["file_in"]][i])) 
  if(!SUBP) if(any(duplicated(SPELLS[["person_id"]]))) stop("Duplicates in person or observation_period table") 
  if(SUBP) if(any(duplicated(SPELLS[,.(person_id)]))) stop("Duplicates in person or observation_period table") 
  
  #Overwrite num spell Request for  Vjola as extra check
  setorder(SPELLS, person_id, op_start_date)
  SPELLS <- SPELLS[, num_spell := seq_len(.N) , by = person_id]
  #SPELLS <- SPELLS[, num_spell3 := cumsum(!is.na(op_start_date)) , by = person_id]
  
  print(paste0("Merge person table with observation_periods table ",SCHEME_03[["subpopulations"]][i]))
  setkey(PERSONS,"person_id")
  setkey(SPELLS,"person_id")
  
  SOURCE_POPULATION <- merge(PERSONS,SPELLS,all.x = T)
  
  ##Would did be neccesarry?? Maybe some diagnoses are before birth but observation age starts at o year 
  print(paste0("If op_start_date is before birth_date replace op_start_date with birth_date ",SCHEME_03[["subpopulations"]][i]))
  SOURCE_POPULATION <- SOURCE_POPULATION[op_start_date < birth_date, op_start_date := birth_date]
  SOURCE_POPULATION[,start_study_date:=as.character(start_study_date2)]
  SOURCE_POPULATION[,start_study_date:=as.IDate(start_study_date,"%Y%m%d")]
  #create the population_distribution_date
  # SOURCE_POPULATION[start_study_date>=op_start_date,use_start:=1]
  # SOURCE_POPULATION[use_start==1,age_start_study := floor(time_length(interval(birth_date, start_study_date),"year"))]
  # SOURCE_POPULATION[is.na(use_start),age_start_study := floor(time_length(interval(birth_date, op_start_date),"year"))]
  # #export number of subjects where op_start_date was used and number of subjects where start_study_date_was used
  # start_use<-SOURCE_POPULATION[use_start==1,.N, by=sex_at_instance_creation]
  # start_use[,Date_variable_used:="start_study_date"]
  # op_use<-SOURCE_POPULATION[is.na(use_start),.N, by=sex_at_instance_creation] 
  # op_use[,Date_variable_used:="op_start_date"]
  # info_pop<-data.table(rbind(start_use,op_use))
  # setnames(info_pop,"N","no_subjects")
  # info_pop<-info_pop[!is.na(no_subjects)]
  # rm(start_use,op_use)
  # SOURCE_POPULATION[,use_start:=NULL][,start_study_date:=NULL]
  # fwrite(info_pop,paste0(projectFolder,"/g_output/STUDY_SOURCE_POPULATION/source_population_info.csv"))
  SOURCE_POPULATION[,population_distribution_date:=as.IDate(population_distribution_date,"%Y%m%d")]
  SOURCE_POPULATION[,age_start_study := floor(time_length(interval(birth_date, population_distribution_date),"year"))]
  SOURCE_POPULATION[,population_distribution_date:=NULL]
  
  print(paste0("Calculate age at op_start_date and op_end_date and dates of which Age_min and Age_max are reached  ",SCHEME_03[["subpopulations"]][i]))
  SOURCE_POPULATION <- SOURCE_POPULATION[, ':=' 
                   (age_op_start_date = floor(time_length(interval(birth_date, op_start_date),"year")),
                     age_op_end_date = floor(time_length(interval(birth_date, op_end_date),"year")),
                     date_min = as.IDate(add_with_rollback(birth_date, period(Age_min,units = "year"), roll_to_first = T, preserve_hms = T)),
                     date_max = as.IDate(add_with_rollback(birth_date, period(Age_max + 1,units = "year"), roll_to_first = T, preserve_hms = T))-1
                   )
  ]  

  
  SOURCE_POPULATION <- SOURCE_POPULATION[!is.na(death_date), date_max := ifelse(date_max > death_date, death_date, date_max)]
  
  
  SOURCE_POPULATION <- SOURCE_POPULATION[, Population := SCHEME_03[["subpopulations"]][i]]
  #SCHEME_03[i,"nrows"] <- nrow(SOURCE_POPULATION)
  #SCHEME_03[i,"ncols"] <- ncol(SOURCE_POPULATION)
  
  saveRDS(SOURCE_POPULATION,file = paste0(std_pop_tmp,SCHEME_03[["file_out"]][i]))
  
  rm(SPELLS,SOURCE_POPULATION)
  gc()
}

saveRDS(SCHEME_03,file = paste0(std_pop_tmp,"SCHEME_03.rds"))

rm(SCHEME_03,PERSONS)
gc()
