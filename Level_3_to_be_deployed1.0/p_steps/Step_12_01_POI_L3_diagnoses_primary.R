#Author: Vjola Hoxhaj Drs.
#email: v.hoxhaj@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 05/12/2021

#Diagnoses, medicines use/vaccine exposure and pregnancy
cat(green(print("The script diagnoses primary has started.")))
####Diagnoses folders#####
if("DIAGNOSES" %in% list.files(populations_dir)){
  if(length(list.files(paste0(populations_dir, "DIAGNOSES/")))>0){
diagnoses_files<-list.files(paste0(populations_dir, "DIAGNOSES/"))
if(diagnoses_of_interest[,.N]>0){
#Select diagnoses of interest
diag_matrix<-data.table(file_name=diagnoses_files)
condition_list<-list()
for(i in 1:diag_matrix[,.N]){
  condition_list[[i]]<-unlist(str_split(diag_matrix[i,file_name],"_"))[2]
}
condition_list<-as.data.table(do.call(rbind,condition_list))
diag_matrix[,condition:=condition_list[,1]]
rm(condition_list)
diag_matrix<-diag_matrix[condition %in% diagnoses_of_interest[,condition]]
diagnoses_files<-diag_matrix[,file_name]
rm(diag_matrix)
}

files<-list()
for (i in 1: length(diagnoses_files)){
  files<-append(files,unique(list(unlist(str_split(diagnoses_files[i],"_"))[2])))
}
files<-do.call(c,files)
#remove duplicates 
files<-files[!duplicated(files)]
#create list with names year_condition
diagnoses_list<-vector(mode="list", length=length(files))
names(diagnoses_list)<-files
rm(files)
#separate all files into the right category
for (i in 1:length(diagnoses_list)){
  diagnoses_list[[i]]<-diagnoses_files[str_detect(diagnoses_files, paste0("_", names(diagnoses_list)[i], "_"))]
}
rm(diagnoses_files)
diagnoses_files<-diagnoses_list
rm(diagnoses_list)
}else{diagnoses_files<-NULL}
}else{diagnoses_files<-NULL}
####Medicines folders####
#load medicines files by year
if("MEDICINES" %in% list.files(populations_dir)){
  if(length(list.files(paste0(populations_dir, "MEDICINES/")))>0){
medicines_files<-list.files(paste0(populations_dir, "MEDICINES/"))
#create data lists for each of the files by combining in year and first letter
files<-list()
for (i in 1: length(medicines_files)){
  files<-append(files,unique(list(substr(medicines_files[i],1,6))))
}

files<-do.call(c,files)
#remove duplicates 
files<-files[!duplicated(files)]
#create list with names letter_year_f
med_list<-vector(mode="list", length=length(files))
names(med_list)<-files
rm(files)
#separate all files into the right category
for (i in 1:length(med_list)){
  med_list[[i]]<-medicines_files[startsWith(medicines_files, names(med_list)[i])]
}
rm(medicines_files)
medicines_files<-med_list
rm(med_list)
  }else{medicines_files<-NULL}
}else{medicines_files<-NULL}
####Vaccines folders####
#load vaccines files by year
if("VACCINES" %in% list.files(populations_dir)){
  if(length(list.files(paste0(populations_dir, "VACCINES/")))>0){
vaccines_files<-list.files(paste0(populations_dir, "VACCINES/"))
#create data lists for each of the files by combining in year and first letter
files<-list()
for (i in 1: length(vaccines_files)){
  files<-append(files,unique(list(substr(vaccines_files[i],1,6))))
}

files<-do.call(c,files)
#remove duplicates 
files<-files[!duplicated(files)]
#create list with names letter_year_f
vacc_list<-vector(mode="list", length=length(files))
names(vacc_list)<-files
rm(files)
#separate all files into the right category
for (i in 1:length(vacc_list)){
  vacc_list[[i]]<-vaccines_files[startsWith(vaccines_files, names(vacc_list)[i])]
}
rm(vaccines_files)
vaccines_files<-vacc_list
rm(vacc_list)
  }else{vaccines_files<-NULL}
}else{vaccines_files<-NULL}
####Pregnancy folders####
if("PREGNANCY" %in% list.files(populations_dir)){
  if(length(list.files(paste0(populations_dir, "PREGNANCY/")))>0){
#load pregnancy files by year
pregnancy_files<-list.files(paste0(populations_dir, "PREGNANCY/"))
#create data lists for each of the files by combining in year and first letter
files<-list()
for (i in 1: length(pregnancy_files)){
  files<-append(files,unique(list(unlist(str_split(pregnancy_files[i],"\\."))[1])))
}

files<-do.call(c,files)
#remove duplicates 
files<-files[!duplicated(files)]
#create list with names letter_year_f
preg_list<-vector(mode="list", length=length(files))
names(preg_list)<-files
rm(files)
#separate all files into the right category
for (i in 1:length(preg_list)){
  preg_list[[i]]<-pregnancy_files[startsWith(pregnancy_files, names(preg_list)[i])]
}
rm(pregnancy_files)
pregnancy_files<-preg_list
rm(preg_list)
  }else{pregnancy_files<-NULL}
}else{pregnancy_files<-NULL}


#####Diagnoses primary analysis####
if(!is.null(diagnoses_files)){
####Start diagnosis analysis####
remove_subjects<-data.table(event_definition=names(diagnoses_files),original_rows=0,excluded_records=0)

for (condition_ind in 1:length(diagnoses_files)){
  #####Load diagnoses file####
  diag_file<-lapply(paste0(populations_dir,"DIAGNOSES/", diagnoses_files[[condition_ind]]), readRDS)
  diag_file<-as.data.table(rbindlist(diag_file, fill = T))
  #create year end variable from year of follow up, will be used to check when a person can be counted in the numerator/denominator of prevalence use
  diag_file[,end_year:=year(end_follow_up)]
  #remove unnecessary columns
  diag_file[,event_code:=NULL][,event_vocabulary:=NULL][,end_follow_up:=NULL][,start_follow_up:=NULL][,age_start_follow_up:=NULL][,code_nodot:=NULL][,truncated_code:=NULL]
  #rename some columns before merging
  setnames(diag_file, "meaning","meaning_diagnosis")
  setnames(diag_file, "year","year_diagnosis")
  setnames(diag_file, "sex_at_instance_creation","sex")
  #sort by person id and event date
  diag_file<-diag_file[order(person_id, event_date)]
  #add index to check number of records per person
  diag_file<-diag_file[,rowid:=rowid(person_id)]
  #Count number of duplicated rec
  dup_rec<-diag_file[rowid!=1,.N]
  #save number of excluded records
  remove_subjects[event_definition==names(diagnoses_files)[condition_ind],original_rows:=diag_file[,.N]]
  remove_subjects[event_definition==names(diagnoses_files)[condition_ind],excluded_records:=dup_rec]
  rm(dup_rec)
  #keep one record per person(the earliest date)
  diag_file<-diag_file[rowid==1]
  diag_file[,rowid:=NULL]
  
  #years present
  years<-sort(diag_file[!duplicated(year_diagnosis), year_diagnosis])
  #create year vector
  #prevalence will be start from the first diagnoses date as we are interested only in persons with a diagnoses(denominator)
  year_diag<-seq(min(years), max(diag_file[!duplicated(end_year),end_year]),1)

  #number of people with diagnoses by sex and year
  counts_diag_sex_y<-vector(mode="list", length(year_diag))
  for(counts_ind in 1: length(counts_diag_sex_y)){
    diag_file[,filter:=0]
    diag_file[year_diag[counts_ind]<=end_year & year_diagnosis<=year_diag[counts_ind],filter:=1]
    counts_diag_sex_y[[counts_ind]]<-data.table(event_definition=diag_file[!duplicated(condition),condition], year=year_diag[counts_ind], diag_file[filter==1][!duplicated(person_id)][year_diagnosis<=year_diag[counts_ind], .N, by="sex"])
    diag_file[,filter:=NULL]
  }
  counts_diag_sex_y<-as.data.table(do.call(rbind,counts_diag_sex_y))
  setnames(counts_diag_sex_y, "N", "no_subjects")
  #export to total counts by sex
  
  #number of people with diagnoses by year
  counts_diag_y<-vector(mode="list", length(year_diag))
  for(counts_ind in 1: length(counts_diag_y)){
    diag_file[year_diag[counts_ind]<=end_year & year_diagnosis<=year_diag[counts_ind],filter:=1]
    counts_diag_y[[counts_ind]]<-data.table(event_definition=diag_file[!duplicated(condition),condition], year=year_diag[counts_ind], no_subjects=diag_file[filter==1][!duplicated(person_id)][year_diagnosis<=year_diag[counts_ind], .N])
    diag_file[,filter:=NULL]
     }
  counts_diag_y<-as.data.table(do.call(rbind,counts_diag_y))
  
  #summary of included subjects
  diag_sum_sex_y<-vector(mode="list", length(year_diag))
  for(counts_ind in 1: length(diag_sum_sex_y)){
    diag_file[,filter:=0]
    diag_file[year_diag[counts_ind]<=end_year & year_diagnosis<=year_diag[counts_ind],filter:=1]
    diag_sum_sex_y[[counts_ind]]<-data.table(event_definition=diag_file[!duplicated(condition),condition], year=year_diag[counts_ind], diag_file[filter==1][!duplicated(person_id)][, .N, by=c("sex", "year_diagnosis","meaning_diagnosis")])
    diag_file[,filter:=NULL]
  }
  diag_sum_sex_y<-as.data.table(do.call(rbind,diag_sum_sex_y))
  setnames(diag_sum_sex_y, "N", "no_subjects")
  #export to total counts by sex
  saveRDS(diag_sum_sex_y,paste0(poi_tmp,names(diagnoses_files)[condition_ind],"_summary_diag_smy.rds"))
  rm(diag_sum_sex_y)
  
  diag_sum_sex<-vector(mode="list", length(year_diag))
  for(counts_ind in 1: length(diag_sum_sex)){
    diag_file[,filter:=0]
    diag_file[year_diag[counts_ind]<=end_year & year_diagnosis<=year_diag[counts_ind],filter:=1]
    diag_sum_sex[[counts_ind]]<-data.table(event_definition=diag_file[!duplicated(condition),condition], year=year_diag[counts_ind], diag_file[filter==1][!duplicated(person_id)][, .N, by=c("sex","meaning_diagnosis")])
    diag_file[,filter:=NULL]
  }
  diag_sum_sex<-as.data.table(do.call(rbind,diag_sum_sex))
  setnames(diag_sum_sex, "N", "no_subjects")
  diag_sum_sex[,year_diagnosis:="N/A"]
  #export to total counts by sex
  saveRDS(diag_sum_sex,paste0(poi_tmp,names(diagnoses_files)[condition_ind],"_summary_diag_sm.rds"))
  rm(diag_sum_sex)
  
  ####medicines analysis####
  if(!is.null(medicines_files)){
  #create list to save files
  diag_med_prior_dates<-list()
  diag_med_prevalence_total<-list()
  diag_med_prevalence_sex<-list()
  diag_medicines_total<-list()
  diag_medicines_sex<-list()
  diag_medicines_meaning<-list()
  
  index<-1
  print("Calculate counts of medicines use after a diagnoses.")
  for(medicine_ind in 1:length(medicines_files)){
    print(paste0("Calculating rates for diagnoses and medicine use:", names(diagnoses_files)[condition_ind], "_", names(medicines_files)[medicine_ind]))
    med_file<-lapply(paste0(populations_dir,"MEDICINES/", medicines_files[[medicine_ind]]), readRDS)
    med_file<-as.data.table(rbindlist(med_file, fill = T))
    #remove unnecessary columns
    if("end_follow_up" %in% names(med_file)){med_file[,end_follow_up:=NULL]}
    if("start_follow_up" %in% names(med_file)){med_file[,start_follow_up:=NULL]}
    if("age_start_follow_up" %in% names(med_file)){med_file [,age_start_follow_up:=NULL]}
    #rename columns
    setnames(med_file, "meaning","meaning_medicines")
    setnames(med_file, "year","year_medicines")
    #truncate the atc code to 4 digits
    med_file<-med_file[,medicinal_product_atc_code:=substr(medicinal_product_atc_code,1,4)]
    #merge with the diagosis file by keeping all records
    med_file<-merge.data.table(diag_file,med_file, by=c("person_id", "birth_date"), all.x=T, allow.cartesian = T)
    #remove all records with no diagnosis
    med_file<-med_file[!is.na(event_date)]
    #remove all records with no medicines
    med_file<-med_file[!is.na(medicines_date)]
    if(med_file[,.N]>0){
    #difference between event date and medicine date
    med_file[,medicines_date:=as.IDate(medicines_date)][,event_date:=as.IDate(event_date)]
    med_file[,diff:=medicines_date-event_date]
    med_file[diff<=0, remove:=1]
    #save number of excluded records
    removed_rec<-med_file[remove==1,.N]
    names(removed_rec)<-c("excluded_records")
    diag_med_prior_dates[[index]]<-data.table(event_definition=diag_file[!duplicated(condition), condition], medicine_group=names(medicines_files)[medicine_ind], removed_rec)
    rm(removed_rec)
    #remove records
    med_file<-med_file[is.na(remove)]
    med_file[,remove:=NULL][,diff:=NULL]
    
    if(med_file[,.N]>0){
    #calculate prevalence of medicine use:number of people having a prescription/dispensing after a diagnosis/total number of people having a diagnosis by year
    diag_med_prev_y<-med_file[!duplicated(person_id), .N, by=c("year_medicines","medicinal_product_atc_code","condition")]
    setnames(diag_med_prev_y,"year_medicines","year")
    setnames(diag_med_prev_y,"medicinal_product_atc_code","atc_code_4")
    setnames(diag_med_prev_y,"N","no_users")
    setnames(diag_med_prev_y,"condition","event_definition")
    diag_med_prevalence_total[[index]]<-diag_med_prev_y
    rm(diag_med_prev_y)
    
    #calculate prevalence of medicine use:number of people having a prescription/dispensing after a diagnosis/total number of people having a diagnosis by year and sex
    diag_med_prev_y_sex<-med_file[!duplicated(person_id), .N, by=c("year_medicines","medicinal_product_atc_code","condition","sex")]
    setnames(diag_med_prev_y_sex,"year_medicines","year")
    setnames(diag_med_prev_y_sex,"medicinal_product_atc_code","atc_code_4")
    setnames(diag_med_prev_y_sex,"N","no_users")
    setnames(diag_med_prev_y_sex,"condition","event_definition")
    diag_med_prevalence_sex[[index]]<-diag_med_prev_y_sex
    rm(diag_med_prev_y_sex)
    
    #calculate counts of medicine use:number of prescriptions/dispensing after a diagnosis/total number of records by year
    diag_med_y<-med_file[, .N, by=c("year_medicines","medicinal_product_atc_code","condition")]
    setnames(diag_med_y,"year_medicines","year")
    setnames(diag_med_y,"medicinal_product_atc_code","atc_code_4")
    setnames(diag_med_y,"N","no_records")
    setnames(diag_med_y,"condition","event_definition")
    
    diag_med_y_t<-med_file[, .N, by=c("year_medicines","condition")]
    setnames(diag_med_y_t,"year_medicines","year")
    setnames(diag_med_y_t,"N","total_records")
    setnames(diag_med_y_t,"condition","event_definition")
    diag_med_y<-merge.data.table(diag_med_y,diag_med_y_t,all=T, by=c("event_definition","year"))
    
    diag_medicines_total[[index]]<-diag_med_y
    rm(diag_med_y,diag_med_y_t)
    
    #calculate counts of medicine use:number of prescriptions/dispensing after a diagnosis/total number of records by year and sex
    diag_med_y_sex<-med_file[, .N, by=c("year_medicines","medicinal_product_atc_code","condition","sex")]
    setnames(diag_med_y_sex,"year_medicines","year")
    setnames(diag_med_y_sex,"medicinal_product_atc_code","atc_code_4")
    setnames(diag_med_y_sex,"N","no_records")
    setnames(diag_med_y_sex,"condition","event_definition")
    
    diag_med_y_sex_t<-med_file[, .N, by=c("year_medicines","condition","sex")]
    setnames(diag_med_y_sex_t,"year_medicines","year")
    setnames(diag_med_y_sex_t,"N","total_records")
    setnames(diag_med_y_sex_t,"condition","event_definition")
    diag_med_y_sex<-merge.data.table(diag_med_y_sex,diag_med_y_sex_t,all=T, by=c("event_definition","sex", "year"))
    
    diag_medicines_sex[[index]]<-diag_med_y_sex
    rm(diag_med_y_sex,diag_med_y_sex_t)
    
    #calculate counts of medicine use:number of prescriptions/dispensing after a diagnosis/total number of records by year and meaning
    med_file[,meaning:=paste(meaning_diagnosis, meaning_medicines, sep=":")]
    diag_med_y_meaning<-med_file[!is.na(year_medicines)][, .N, by=c("condition","meaning","year_medicines","medicinal_product_atc_code")]
    setnames(diag_med_y_meaning,"year_medicines","year")
    setnames(diag_med_y_meaning,"medicinal_product_atc_code","atc_code_4")
    setnames(diag_med_y_meaning,"N","no_records")
    setnames(diag_med_y_meaning,"condition","event_definition")
    
    diag_med_y_meaning_t<-med_file[!is.na(year_medicines)][, .N, by=c("condition","meaning","year_medicines")]
    setnames(diag_med_y_meaning_t,"year_medicines","year")
    setnames(diag_med_y_meaning_t,"N","total_records")
    setnames(diag_med_y_meaning_t,"condition","event_definition")
    diag_med_y_meaning<-merge.data.table(diag_med_y_meaning,diag_med_y_meaning_t,all=T, by=c("event_definition","meaning", "year"))
    
    diag_medicines_meaning[[index]]<-diag_med_y_meaning
    rm(diag_med_y_meaning,diag_med_y_meaning_t)
    }
    }
    index<-index+1
    rm(med_file)
    gc()
  }
  
  ####combine results medicines####
  diag_med_prevalence_total<-as.data.table(rbindlist(diag_med_prevalence_total))
  if(diag_med_prevalence_total[,.N]>0){
    diag_med_prevalence_total<-diag_med_prevalence_total[,lapply(.SD, sum), .SDcols=c("no_users"), by=c("event_definition","year","atc_code_4")]
  }else{diag_med_prevalence_total<-NULL}
  
  diag_med_prevalence_sex<-as.data.table(rbindlist(diag_med_prevalence_sex))
  if(diag_med_prevalence_sex[,.N]>0){
    diag_med_prevalence_sex<-diag_med_prevalence_sex[,lapply(.SD, sum), .SDcols=c("no_users"), by=c("event_definition","sex", "year","atc_code_4")]
  }else{diag_med_prevalence_sex<-NULL}
  
  diag_medicines_total<-as.data.table(rbindlist(diag_medicines_total))
  if(diag_medicines_total[,.N]>0){
    diag_medicines_total<-diag_medicines_total[,lapply(.SD, sum), .SDcols=c("no_records","total_records"), by=c("event_definition","year","atc_code_4")]
  }else{diag_medicines_total<-NULL}
  
  diag_medicines_sex<-as.data.table(rbindlist(diag_medicines_sex))
  if(diag_medicines_sex[,.N]>0){
    diag_medicines_sex<-diag_medicines_sex[,lapply(.SD, sum), .SDcols=c("no_records","total_records"), by=c("event_definition","sex", "year","atc_code_4")]
  }else{diag_medicines_sex<-NULL}
  
  diag_medicines_meaning<-as.data.table(rbindlist(diag_medicines_meaning))
  if(diag_medicines_meaning[,.N]>0){
    diag_medicines_meaning<-diag_medicines_meaning[,lapply(.SD, sum), .SDcols=c("no_records","total_records"), by=c("event_definition","meaning", "year","atc_code_4")]
  }else{diag_medicines_meaning<-NULL}
  
  diag_med_prior_dates<-as.data.table(rbindlist(diag_med_prior_dates))
  if(diag_med_prior_dates[,.N]>0){
    diag_med_prior_dates<-diag_med_prior_dates[,lapply(.SD, sum), .SDcols=c("removed_rec"), by=c("event_definition","medicine_group")]
  }else{diag_med_prior_dates<-NULL}
  
  #export medicines counts
  if(!is.null(diag_medicines_total)){saveRDS(diag_medicines_total,paste0(poi_tmp,names(diagnoses_files)[condition_ind], "_diag_medicines_total.rds"))}
  rm(diag_medicines_total)
  if(!is.null(diag_medicines_sex)){saveRDS(diag_medicines_sex,paste0(poi_tmp,names(diagnoses_files)[condition_ind], "_diag_medicines_sex.rds"))}
  rm(diag_medicines_sex)
  if(!is.null(diag_medicines_meaning)){saveRDS(diag_medicines_meaning,paste0(poi_tmp,names(diagnoses_files)[condition_ind], "_diag_medicines_meaning.rds"))}
  rm(diag_medicines_meaning)
  
  #combine the prevalence data
  #total
  if(!is.null(diag_med_prevalence_total)){
    diag_med_prevalence_total<-merge.data.table(diag_med_prevalence_total,counts_diag_y, by=c("event_definition","year"), all=T)
  }else{
    diag_med_prevalence_total<-data.table(event_definition=names(diagnoses_files)[condition_ind], atc_code_4="N/A",no_users=0)
    diag_med_prevalence_total<-merge.data.table(diag_med_prevalence_total,counts_diag_y, by=c("event_definition"), all=T)
  }
  saveRDS(diag_med_prevalence_total,paste0(poi_tmp,names(diagnoses_files)[condition_ind], "_prevalence_diag_medicines_total.rds"))
  rm(diag_med_prevalence_total)
  #sex
  if(!is.null(diag_med_prevalence_sex)){
    diag_med_prevalence_sex<-merge.data.table(diag_med_prevalence_sex,counts_diag_sex_y, by=c("event_definition","sex", "year"), all=T)
  } else {
    diag_med_prevalence_sex<-data.table(event_definition=names(diagnoses_files)[condition_ind], atc_code_4="N/A",no_users=0)
    diag_med_prevalence_sex<-merge.data.table(diag_med_prevalence_sex,counts_diag_sex_y, by=c("event_definition"), all=T)
  }
  saveRDS(diag_med_prevalence_sex,paste0(poi_tmp,names(diagnoses_files)[condition_ind], "_prevalence_diag_medicines_sex.rds"))
  rm(diag_med_prevalence_sex)
  
  #combine and export the excluded records
  if(!is.null(diag_med_prior_dates[,.N])){
    #remove empty cells
    diag_med_prior_dates<-diag_med_prior_dates[removed_rec!=0]
    saveRDS(diag_med_prior_dates,paste0(poi_tmp,names(diagnoses_files)[condition_ind], "_medicines_prior_date_excluded.rds"))
  }
  rm(diag_med_prior_dates)
  
  }
  ####vaccines analysis####
  if(!is.null(vaccines_files)){
  #create list to save files
  diag_vacc_prior_dates<-list()
  diag_vacc_prevalence_total<-list()
  diag_vacc_prevalence_sex<-list()
  diag_vaccines_total<-list()
  diag_vaccines_sex<-list()
  diag_vaccines_meaning<-list()
  duplicated_vacc_rec<-list()
  
  index<-1
  print("Calculate counts of vaccines administration after a diagnoses.")
  for(vacc_ind in 1:length(vaccines_files)){
    print(paste0("Calculating rates for diagnoses and vaccine administration:", names(diagnoses_files)[condition_ind], "_", names(vaccines_files)[vacc_ind]))
    vacc_file<-lapply(paste0(populations_dir,"VACCINES/", vaccines_files[[vacc_ind]]), readRDS)
    vacc_file<-as.data.table(rbindlist(vacc_file, fill = T))
    #remove duplicated vaccine records
    vacc_file[,dup_ind:=rowid(person_id,vaccines_date)]
    duplicated_vacc_rec[[vacc_ind]]<-data.table(event_definition=names(diagnoses_files)[condition_ind],vaccine_group=names(vaccines_files)[vacc_ind], duplicated_rec=vacc_file[dup_ind>1,.N])
    vacc_file<-vacc_file[dup_ind==1]
    vacc_file[,dup_ind:=NULL]
    #remove unnecessary columns
    if("end_follow_up" %in% names(vacc_file)){vacc_file[,end_follow_up:=NULL]}
    if("start_follow_up" %in% names(vacc_file)){vacc_file[,start_follow_up:=NULL]}
    if("age_start_follow_up" %in% names(vacc_file)){vacc_file [,age_start_follow_up:=NULL]}
    #rename columns
    setnames(vacc_file, "meaning","meaning_vaccines")
    setnames(vacc_file, "year","year_vaccines")
    #truncate the atc code to 4 digits
    if(indicator_to_run=="vx_atc"){
    vacc_file<-vacc_file[,vaccine_indicator:=substr(vaccine_indicator,1,4)]
    }
    #merge with the diagosis file by keeping all records
    vacc_file<-merge.data.table(diag_file,vacc_file, by=c("person_id", "birth_date"), all.x=T, allow.cartesian = T)
    #remove all records with no diagnosis
    vacc_file<-vacc_file[!is.na(event_date)]
    #remove all records with no vacc date
    vacc_file<-vacc_file[!is.na(vaccines_date)]
    if(vacc_file[,.N]>0){
    #difference between event date and vaccine date
    vacc_file[,vaccines_date:=as.IDate(vaccines_date)][,event_date:=as.IDate(event_date)]
    vacc_file[,diff:=vaccines_date-event_date]
    vacc_file[diff<=0, remove:=1]
    #save number of excluded records
    removed_rec<-vacc_file[remove==1,.N]
    names(removed_rec)<-c("excluded_records")
    diag_vacc_prior_dates[[index]]<-data.table(event_definition=diag_file[!duplicated(condition), condition], vaccine_group=names(vaccines_files)[vacc_ind], removed_rec)
    rm(removed_rec)
    #remove records
    vacc_file<-vacc_file[is.na(remove)]
    vacc_file[,remove:=NULL][,diff:=NULL]
     
    if(vacc_file[,.N]>0){
    #calculate prevalence of vaccine administration:number of people having a prescription/dispensing after a diagnosis/total number of people having a diagnosis by year
    diag_vacc_prev_y<-vacc_file[!duplicated(person_id), .N, by=c("year_vaccines","vaccine_indicator","condition")]
    setnames(diag_vacc_prev_y,"year_vaccines","year")
    setnames(diag_vacc_prev_y,"vaccine_indicator","atc_code_4")
    setnames(diag_vacc_prev_y,"N","no_users")
    setnames(diag_vacc_prev_y,"condition","event_definition")
    diag_vacc_prevalence_total[[index]]<-diag_vacc_prev_y
    rm(diag_vacc_prev_y)
    
    #calculate prevalence of vaccine administration:number of people having a prescription/dispensing after a diagnosis/total number of people having a diagnosis by year and sex
    diag_vacc_prev_y_sex<-vacc_file[!duplicated(person_id), .N, by=c("year_vaccines","vaccine_indicator","condition","sex")]
    setnames(diag_vacc_prev_y_sex,"year_vaccines","year")
    setnames(diag_vacc_prev_y_sex,"vaccine_indicator","atc_code_4")
    setnames(diag_vacc_prev_y_sex,"N","no_users")
    setnames(diag_vacc_prev_y_sex,"condition","event_definition")
    diag_vacc_prevalence_sex[[index]]<-diag_vacc_prev_y_sex
    rm(diag_vacc_prev_y_sex)
    
    #calculate counts of vaccine administration:number of prescriptions/dispensing after a diagnosis/total number of records by year
    diag_vacc_y<-vacc_file[, .N, by=c("year_vaccines","vaccine_indicator","condition")]
    setnames(diag_vacc_y,"year_vaccines","year")
    setnames(diag_vacc_y,"vaccine_indicator","atc_code_4")
    setnames(diag_vacc_y,"N","no_records")
    setnames(diag_vacc_y,"condition","event_definition")
    
    diag_vacc_y_t<-vacc_file[, .N, by=c("year_vaccines","condition")]
    setnames(diag_vacc_y_t,"year_vaccines","year")
    setnames(diag_vacc_y_t,"N","total_records")
    setnames(diag_vacc_y_t,"condition","event_definition")
    diag_vacc_y<-merge.data.table(diag_vacc_y,diag_vacc_y_t,all=T, by=c("event_definition","year"))
    
    diag_vaccines_total[[index]]<-diag_vacc_y
    rm(diag_vacc_y,diag_vacc_y_t)
    
    #calculate counts of vaccine administration:number of prescriptions/dispensing after a diagnosis/total number of records by year and sex
    diag_vacc_y_sex<-vacc_file[, .N, by=c("year_vaccines","vaccine_indicator","condition","sex")]
    setnames(diag_vacc_y_sex,"year_vaccines","year")
    setnames(diag_vacc_y_sex,"vaccine_indicator","atc_code_4")
    setnames(diag_vacc_y_sex,"N","no_records")
    setnames(diag_vacc_y_sex,"condition","event_definition")
    
    diag_vacc_y_sex_t<-vacc_file[, .N, by=c("year_vaccines","condition","sex")]
    setnames(diag_vacc_y_sex_t,"year_vaccines","year")
    setnames(diag_vacc_y_sex_t,"N","total_records")
    setnames(diag_vacc_y_sex_t,"condition","event_definition")
    diag_vacc_y_sex<-merge.data.table(diag_vacc_y_sex,diag_vacc_y_sex_t,all=T, by=c("event_definition","sex", "year"))
    
    diag_vaccines_sex[[index]]<-diag_vacc_y_sex
    rm(diag_vacc_y_sex,diag_vacc_y_sex_t)
    
    #calculate counts of vaccine administration:number of prescriptions/dispensing after a diagnosis/total number of records by year and meaning
    vacc_file[,meaning:=paste(meaning_diagnosis, meaning_vaccines, sep=":")]
    diag_vacc_y_meaning<-vacc_file[, .N, by=c("condition","meaning","year_vaccines","vaccine_indicator")]
    setnames(diag_vacc_y_meaning,"year_vaccines","year")
    setnames(diag_vacc_y_meaning,"vaccine_indicator","atc_code_4")
    setnames(diag_vacc_y_meaning,"N","no_records")
    setnames(diag_vacc_y_meaning,"condition","event_definition")
    
    diag_vacc_y_meaning_t<-vacc_file[, .N, by=c("condition","meaning","year_vaccines")]
    setnames(diag_vacc_y_meaning_t,"year_vaccines","year")
    setnames(diag_vacc_y_meaning_t,"N","total_records")
    setnames(diag_vacc_y_meaning_t,"condition","event_definition")
    diag_vacc_y_meaning<-merge.data.table(diag_vacc_y_meaning,diag_vacc_y_meaning_t,all=T, by=c("event_definition","meaning", "year"))
    
    diag_vaccines_meaning[[index]]<-diag_vacc_y_meaning
    rm(diag_vacc_y_meaning,diag_vacc_y_meaning_t)
    }
    }
    index<-index+1
    rm(vacc_file)
    gc()
  }
  
  ####combine results vaccines####
  diag_vacc_prevalence_total<-as.data.table(rbindlist(diag_vacc_prevalence_total))
  if(diag_vacc_prevalence_total[,.N]>0){
    diag_vacc_prevalence_total<-diag_vacc_prevalence_total[,lapply(.SD, sum), .SDcols=c("no_users"), by=c("event_definition","year","atc_code_4")]
  }else{diag_vacc_prevalence_total<-NULL}
  
  diag_vacc_prevalence_sex<-as.data.table(rbindlist(diag_vacc_prevalence_sex))
  if(diag_vacc_prevalence_sex[,.N]>0){
    diag_vacc_prevalence_sex<-diag_vacc_prevalence_sex[,lapply(.SD, sum), .SDcols=c("no_users"), by=c("event_definition","sex", "year","atc_code_4")]
  }else{diag_vacc_prevalence_sex<-NULL}
  
  diag_vaccines_total<-as.data.table(rbindlist(diag_vaccines_total))
  if(diag_vaccines_total[,.N]>0){
    diag_vaccines_total<-diag_vaccines_total[,lapply(.SD, sum), .SDcols=c("no_records","total_records"), by=c("event_definition","year","atc_code_4")]
  }else{diag_vaccines_total<-NULL}
  
  diag_vaccines_sex<-as.data.table(rbindlist(diag_vaccines_sex))
  if(diag_vaccines_sex[,.N]>0){
    diag_vaccines_sex<-diag_vaccines_sex[,lapply(.SD, sum), .SDcols=c("no_records","total_records"), by=c("event_definition","sex", "year","atc_code_4")]
  }else{diag_vaccines_sex<-NULL}
  
  diag_vaccines_meaning<-as.data.table(rbindlist(diag_vaccines_meaning))
  if(diag_vaccines_meaning[,.N]>0){
    diag_vaccines_meaning<-diag_vaccines_meaning[,lapply(.SD, sum), .SDcols=c("no_records","total_records"), by=c("event_definition","meaning", "year","atc_code_4")]
  }else{diag_vaccines_meaning<-NULL}
  
  diag_vacc_prior_dates<-as.data.table(rbindlist(diag_vacc_prior_dates))
  if(diag_vacc_prior_dates[,.N]>0){
    diag_vacc_prior_dates<-diag_vacc_prior_dates[,lapply(.SD, sum), .SDcols=c("removed_rec"), by=c("event_definition","vaccine_group")]
  }else{diag_vacc_prior_dates<-NULL}
  
  duplicated_vacc_rec<-as.data.table(rbindlist(duplicated_vacc_rec))
  if(duplicated_vacc_rec[,.N]>0){
    duplicated_vacc_rec<-duplicated_vacc_rec[,lapply(.SD, sum), .SDcols=c("duplicated_rec"), by=c("event_definition", "vaccine_group")]
  }else{duplicated_vacc_rec<-NULL}
  
  #export vaccines counts
  if(!is.null(diag_vaccines_total)){saveRDS(diag_vaccines_total,paste0(poi_tmp,names(diagnoses_files)[condition_ind], "_diag_vaccines_total.rds"))}
  rm(diag_vaccines_total)
  if(!is.null(diag_vaccines_sex)){saveRDS(diag_vaccines_sex,paste0(poi_tmp,names(diagnoses_files)[condition_ind], "_diag_vaccines_sex.rds"))}
  rm(diag_vaccines_sex)
  if(!is.null(diag_vaccines_meaning)){saveRDS(diag_vaccines_meaning,paste0(poi_tmp,names(diagnoses_files)[condition_ind], "_diag_vaccines_meaning.rds"))}
  rm(diag_vaccines_meaning)
  
  #combine the prevalence data
  #total
  if(!is.null(diag_vacc_prevalence_total)){
    diag_vacc_prevalence_total<-merge.data.table(diag_vacc_prevalence_total,counts_diag_y, by=c("event_definition","year"), all=T)
  }else{
    diag_vacc_prevalence_total<-data.table(event_definition=names(diagnoses_files)[condition_ind], atc_code_4="N/A",no_users=0)
    diag_vacc_prevalence_total<-merge.data.table(diag_vacc_prevalence_total,counts_diag_y, by=c("event_definition"), all=T)
  }
  saveRDS(diag_vacc_prevalence_total,paste0(poi_tmp,names(diagnoses_files)[condition_ind], "_prevalence_diag_vaccines_total.rds"))
  rm(diag_vacc_prevalence_total)
  #sex
  if(!is.null(diag_vacc_prevalence_sex)){
    diag_vacc_prevalence_sex<-merge.data.table(diag_vacc_prevalence_sex,counts_diag_sex_y, by=c("event_definition","sex", "year"), all=T)
  } else {
    diag_vacc_prevalence_sex<-data.table(event_definition=names(diagnoses_files)[condition_ind], atc_code_4="N/A",no_users=0)
    diag_vacc_prevalence_sex<-merge.data.table(diag_vacc_prevalence_sex,counts_diag_sex_y, by=c("event_definition"), all=T)
  }
  saveRDS(diag_vacc_prevalence_sex,paste0(poi_tmp,names(diagnoses_files)[condition_ind], "_prevalence_diag_vaccines_sex.rds"))
  rm(diag_vacc_prevalence_sex)
  
  #combine and export the excluded records
  if(!is.null(diag_vacc_prior_dates)){
    #remove empty cells
    diag_vacc_prior_dates<-diag_vacc_prior_dates[removed_rec!=0]
    if(diag_vacc_prior_dates[,.N]>0){
    saveRDS(diag_vacc_prior_dates,paste0(poi_tmp, names(diagnoses_files)[condition_ind], "_vaccines_prior_date_excluded.rds"))
    }
  }
  rm(diag_vacc_prior_dates)

  
  if(!is.null(duplicated_vacc_rec)){
    #remove empty cells
    duplicated_vacc_rec<-duplicated_vacc_rec[duplicated_rec!=0]
    if(duplicated_vacc_rec[,.N]>0){
    saveRDS(duplicated_vacc_rec,paste0(poi_tmp,names(diagnoses_files)[condition_ind], "_vaccines_duplicated_rec_excluded.rds"))
    }
  }
  rm(duplicated_vacc_rec)
  }
  ####pregnancy analysis####
  if(!is.null(pregnancy_files)){
  #create list to save files
  diag_preg_prior_dates<-list()
  diag_preg_prevalence_total<-list()
  diag_pregnancy_total<-list()
  diag_pregnancy_sex<-list()
  diag_pregnancy_meaning<-list()
  
  index<-1
  print("Calculate counts of pregnancy records after a diagnoses.")
  for(pregnancy_ind in 1:length(pregnancy_files)){
    print(paste0("Calculating rates for diagnoses and pregnancy records:", names(diagnoses_files)[condition_ind], "_", names(pregnancy_files)[pregnancy_ind]))
    preg_file<-lapply(paste0(populations_dir,"PREGNANCY/", pregnancy_files[[pregnancy_ind]]), readRDS)
    preg_file<-as.data.table(rbindlist(preg_file, fill = T))
    #check if there are two condition columns, remove one
    if(sum(names(preg_file)=="condition")==2){preg_file<-preg_file[,condition:=NULL]}
    #remove unnecessary columns
    if("end_follow_up_preg" %in% names(preg_file)){preg_file[,end_follow_up_preg:=NULL]}
    if("start_follow_up_preg" %in% names(preg_file)){preg_file[,start_follow_up_preg:=NULL]}
    if("age_start_follow_up" %in% names(preg_file)){preg_file [,age_start_follow_up:=NULL]}
    if("filter" %in% names(preg_file)){preg_file [,filter:=NULL]}
    if("lag" %in% names(preg_file)){preg_file [,lag:=NULL]}
    #rename columns
    setnames(preg_file, "meaning","meaning_pregnancy")
    setnames(preg_file, "year","year_pregnancy")
    setnames(preg_file, "sex_at_instance_creation","sex")
    setnames(preg_file, "pregnancy_code_date","pregnancy_date")
    setnames(preg_file, "condition","stage_of_pregnancy")
    #merge with the diagosis file by keeping all records
    preg_file<-merge.data.table(diag_file,preg_file, by=c("person_id", "birth_date","sex"), all.x=T, allow.cartesian = T)
    preg_file<-preg_file[sex=="F"]
    #remove all records with no diagnosis
    preg_file<-preg_file[!is.na(event_date)]
    #Remove all records with no pregnancy date
    preg_file<-preg_file[!is.na(pregnancy_date)]
    if(preg_file[,.N]>0){
    #difference between event date and pregnancy date
    preg_file[,pregnancy_date:=as.IDate(pregnancy_date)][,event_date:=as.IDate(event_date)]
    preg_file[,diff:=pregnancy_date-event_date]
    preg_file[diff<=0, remove:=1]
    #save number of excluded records
    removed_rec<-preg_file[remove==1,.N]
    names(removed_rec)<-c("excluded_records")
    diag_preg_prior_dates[[index]]<-data.table(event_definition=diag_file[!duplicated(condition), condition], pregnancy_group=names(pregnancy_files)[pregnancy_ind], removed_rec)
    rm(removed_rec)
    #remove records
    preg_file<-preg_file[is.na(remove)]
    preg_file[,remove:=NULL][,diff:=NULL]
    if(preg_file[,.N]>0){
    #save the diag_preg file to diag_preg populations folder to be used in events_pregnancy_medicines and events_pregnancy_vaccines
    saveRDS(preg_file, paste0(populations_dir,"EVENTS_PREGNANCY/", names(diagnoses_files)[condition_ind], "_", names(pregnancy_files)[pregnancy_ind], ".rds"))
    
    #calculate prevalence of pregnancy event:number of women having a pregnancy record after a diagnosis/total number of women having a diagnosis by year
    diag_preg_y<-preg_file[!duplicated(person_id), .N, by=c("year_pregnancy","stage_of_pregnancy","condition")]
    setnames(diag_preg_y,"year_pregnancy","year")
    setnames(diag_preg_y,"N","no_pregnant_women")
    setnames(diag_preg_y,"condition","event_definition")
    diag_preg_prevalence_total[[index]]<-diag_preg_y
    rm(diag_preg_y)
    
    #calculate counts of pregnancy records:number of pregnancy record after a diagnosis/total number of records by year
    diag_preg_y<-preg_file[, .N, by=c("year_pregnancy","stage_of_pregnancy","condition")]
    setnames(diag_preg_y,"year_pregnancy","year")
    setnames(diag_preg_y,"N","no_pregnancy_records")
    setnames(diag_preg_y,"condition","event_definition")
    
    diag_preg_y_t<-preg_file[, .N, by=c("year_pregnancy","condition")]
    setnames(diag_preg_y_t,"year_pregnancy","year")
    setnames(diag_preg_y_t,"N","total_records")
    setnames(diag_preg_y_t,"condition","event_definition")
    diag_preg_y<-merge.data.table(diag_preg_y,diag_preg_y_t,all=T, by=c("event_definition","year"))
    
    diag_pregnancy_total[[index]]<-diag_preg_y
    rm(diag_preg_y,diag_preg_y_t)
    
    #calculate counts of pregnancy records:number of pregnancy record after a diagnosis by meaning/total number of records by year and meaning
    preg_file[,meaning:=paste(meaning_diagnosis,meaning_pregnancy,sep=":")]
    diag_preg_y_m<-preg_file[, .N, by=c("year_pregnancy","stage_of_pregnancy","condition","meaning")]
    setnames(diag_preg_y_m,"year_pregnancy","year")
    setnames(diag_preg_y_m,"N","no_records")
    setnames(diag_preg_y_m,"condition","event_definition")
    
    diag_preg_y_m_t<-preg_file[, .N, by=c("year_pregnancy","condition","meaning")]
    setnames(diag_preg_y_m_t,"year_pregnancy","year")
    setnames(diag_preg_y_m_t,"N","total_records")
    setnames(diag_preg_y_m_t,"condition","event_definition")
    diag_preg_y_m<-merge.data.table(diag_preg_y_m,diag_preg_y_m_t,all=T, by=c("event_definition","meaning", "year"))
    
    diag_pregnancy_meaning[[index]]<-diag_preg_y_m
    rm(diag_preg_y_m,diag_preg_y_m_t)
    }
    }
    index<-index+1
    rm(preg_file)
    gc()
  }
  
  ####combine results pregnancy####
  diag_preg_prevalence_total<-as.data.table(rbindlist(diag_preg_prevalence_total))
  if(diag_preg_prevalence_total[,.N]>0){
    diag_preg_prevalence_total<-diag_preg_prevalence_total[,lapply(.SD, sum), .SDcols=c("no_pregnant_women"), by=c("event_definition","stage_of_pregnancy", "year")]
  }else{diag_preg_prevalence_total<-NULL}
  
  diag_pregnancy_total<-as.data.table(rbindlist(diag_pregnancy_total))
  if(diag_pregnancy_total[,.N]>0){
  diag_pregnancy_total<-diag_pregnancy_total[!is.na(no_pregnancy_records) & !is.na(total_records)]
  if(diag_pregnancy_total[,.N]>0){
    diag_pregnancy_total<-diag_pregnancy_total[,lapply(.SD, sum), .SDcols=c("no_pregnancy_records","total_records"), by=c("event_definition","stage_of_pregnancy","year")]
  } else {diag_pregnancy_total<-NULL}
  }
  
  diag_pregnancy_meaning<-as.data.table(rbindlist(diag_pregnancy_meaning))
  if(diag_pregnancy_meaning[,.N]>0){
    diag_pregnancy_meaning<-diag_pregnancy_meaning[,lapply(.SD, sum), .SDcols=c("no_records","total_records"), by=c("event_definition","stage_of_pregnancy", "meaning", "year")]
  }else{diag_pregnancy_meaning<-NULL}
  
  diag_preg_prior_dates<-as.data.table(rbindlist(diag_preg_prior_dates))
  if(diag_preg_prior_dates[,.N]>0){
    diag_preg_prior_dates<-diag_preg_prior_dates[,lapply(.SD, sum), .SDcols=c("removed_rec"), by=c("event_definition","pregnancy_group")]
  }else{diag_preg_prior_dates<-NULL}
  
  #export medicines counts
  if(!is.null(diag_pregnancy_total)){saveRDS(diag_pregnancy_total,paste0(poi_tmp,names(diagnoses_files)[condition_ind], "_diag_pregnancy_total.rds"))}
  rm(diag_pregnancy_total)
  if(!is.null(diag_pregnancy_meaning)){saveRDS(diag_pregnancy_meaning,paste0(poi_tmp,names(diagnoses_files)[condition_ind], "_diag_pregnancy_meaning.rds"))}
  rm(diag_pregnancy_meaning)
  
  #combine the prevalence data
  #total
  if(!is.null(diag_preg_prevalence_total)){
    diag_preg_prevalence_total<-merge.data.table(diag_preg_prevalence_total,counts_diag_y, by=c("event_definition","year"), all=T)
  }else{
    diag_preg_prevalence_total<-data.table(event_definition=names(diagnoses_files)[condition_ind], stage_of_pregnancy="N/A",no_pregnant_women=0)
    diag_preg_prevalence_total<-merge.data.table(diag_preg_prevalence_total,counts_diag_y, by=c("event_definition"), all=T)
  }
  saveRDS(diag_preg_prevalence_total,paste0(poi_tmp,names(diagnoses_files)[condition_ind], "_prevalence_diag_pregnancy_total.rds"))
  rm(diag_preg_prevalence_total)
  
  
  #combine and export the excluded records
  if(!is.null(diag_preg_prior_dates[,.N])){
    diag_preg_prior_dates<-diag_preg_prior_dates[removed_rec!=0]
    saveRDS(diag_preg_prior_dates,paste0(poi_tmp,names(diagnoses_files)[condition_ind], "_pregnancy_prior_date_excluded.rds"))
  }
  rm(diag_preg_prior_dates)
  

  }
  rm(diag_file)
}
gc()

#####Combine results for summary diagnoses#####
summary_diag_smy_files<-list.files(poi_tmp,"summary_diag_smy.rds")
if(length(summary_diag_smy_files>0)){
  summary_diag_smy<-lapply(paste0(poi_tmp,summary_diag_smy_files), readRDS)
  summary_diag_smy<-as.data.table(rbindlist(summary_diag_smy,fill = T))
  #apply sum to make sure all categories are combined
  summary_diag_smy<-summary_diag_smy[,lapply(.SD, sum), by=c("event_definition","meaning_diagnosis","sex", "year_diagnosis", "year"), .SDcols=c("no_subjects")]
  
  #remove all files in poi tmp
  for(i in 1:length(summary_diag_smy_files)){
    file.remove(paste0(poi_tmp, summary_diag_smy_files[i]))
  }
  rm(summary_diag_smy_files)
  
  summary_diag_smy[, data_access_provider:=data_access_provider_name]
  summary_diag_smy[, data_source:=data_source_name]
  
  #save results to g_output/POI
  if(subpopulations_present=="Yes"){
    fwrite(summary_diag_smy, paste0(poi_dir, subpopulations_names[s],format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_summary_smy.csv"), row.names = F)

    } else {
    fwrite(summary_diag_smy, paste0(poi_dir, format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_summary_smy.csv"), row.names = F)

    }
  
  #Masked version
  summary_diag_smy[, no_subjects:= as.character(no_subjects)][as.numeric(no_subjects) > 0 & as.numeric(no_subjects) < 5, no_subjects := "<5"]
  if(subpopulations_present=="Yes"){
    fwrite(summary_diag_smy, paste0(poi_dir, subpopulations_names[s],"/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_summary_smy_masked.csv"), row.names = F)
    fwrite(summary_diag_smy, paste0(poi_dir, subpopulations_names[s],"/GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_summary_smy_masked.csv"), row.names = F)
    
  } else {
    fwrite(summary_diag_smy, paste0(poi_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_summary_smy_masked.csv"), row.names = F)
    fwrite(summary_diag_smy, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_summary_smy_masked.csv"), row.names = F)
  }
  
  
  rm(summary_diag_smy)
}

summary_diag_sm_files<-list.files(poi_tmp,"summary_diag_sm.rds")
if(length(summary_diag_sm_files>0)){
  summary_diag_sm<-lapply(paste0(poi_tmp,summary_diag_sm_files), readRDS)
  summary_diag_sm<-as.data.table(rbindlist(summary_diag_sm,fill = T))
  #apply sum to make sure all categories are combined
  summary_diag_sm<-summary_diag_sm[,lapply(.SD, sum), by=c("event_definition","meaning_diagnosis","sex", "year"), .SDcols=c("no_subjects")]
  
  #remove all files in poi tmp
  for(i in 1:length(summary_diag_sm_files)){
    file.remove(paste0(poi_tmp, summary_diag_sm_files[i]))
  }
  rm(summary_diag_sm_files)
  
  summary_diag_sm[, data_access_provider:=data_access_provider_name]
  summary_diag_sm[, data_source:=data_source_name]
  
  #save results to g_output/EVENTS_MEDICINES
  if(subpopulations_present=="Yes"){
    fwrite(summary_diag_sm, paste0(poi_dir, subpopulations_names[s],format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_summary_sm.csv"), row.names = F)
      } else {
    fwrite(summary_diag_sm, paste0(poi_dir, format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_summary_sm.csv"), row.names = F)
      }
  
  summary_diag_sm[, no_subjects:= as.character(no_subjects)][as.numeric(no_subjects) > 0 & as.numeric(no_subjects) < 5, no_subjects := "<5"]
  if(subpopulations_present=="Yes"){
    fwrite(summary_diag_sm, paste0(poi_dir, subpopulations_names[s],"Masked/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_summary_sm_masked.csv"), row.names = F)
    fwrite(summary_diag_sm, paste0(poi_dir, subpopulations_names[s],"/GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_summary_sm_masked.csv"), row.names = F)
    
  } else {
    fwrite(summary_diag_sm, paste0(poi_dir,"Masked/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_summary_sm_masked.csv"), row.names = F)
    fwrite(summary_diag_sm, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_summary_sm_masked.csv"), row.names = F)
  }
  
  rm(summary_diag_sm)
}

#####Combine results for medicines use after a diagnoses#####
medicines_files<-length(list.files(poi_tmp,"prevalence_diag_medicines_total|prevalence_diag_medicines_sex|diag_medicines_sex|diag_medicines_meaning|diag_medicines_total|medicines_prior_date_excluded"))

if(medicines_files>0){
####Prevalence of medicine use after a diagnosis by year, atc and event definition####
if(length(list.files(poi_tmp,"prevalence_diag_medicines_total.rds"))>0){ 
prevalence_diag_medicines_total_fl<-list.files(poi_tmp, "prevalence_diag_medicines_total.rds")#prevalence of medicines records by year
prevalence_diag_medicines_total<-lapply(paste0(poi_tmp,prevalence_diag_medicines_total_fl), readRDS)
prevalence_diag_medicines_total<-as.data.table(rbindlist(prevalence_diag_medicines_total,fill = T))
#make sure there are no zeros in no_subjects
prevalence_diag_medicines_total<-prevalence_diag_medicines_total[!is.na(no_subjects)]
#replace NA in atc_code_4 with N/A
prevalence_diag_medicines_total[is.na(atc_code_4), atc_code_4:="N/A"]
#replace NA in no_users with 0
prevalence_diag_medicines_total[is.na(no_users), no_users:=0]
#apply sum to make sure all categories are combined
prevalence_diag_medicines_total<-prevalence_diag_medicines_total[,lapply(.SD, sum), by=c("event_definition","year","atc_code_4"), .SDcols=c("no_users","no_subjects")]
#separate atc_code=="N/A"
no_users_present<-prevalence_diag_medicines_total[atc_code_4=="N/A"]
if(no_users_present[,.N]>0){
no_users_present[,atc_code_4:=NULL][,no_users:=NULL]
no_users_present[ , new := do.call(paste, c(.SD, sep = "_"))]
events<-no_users_present[!duplicated(event_definition),event_definition]
list<-vector(mode="list", length=length(events))
for(ls in 1:length(list)){
  atc_codes_to_add<-prevalence_diag_medicines_total[event_definition==events[ls]][!duplicated(atc_code_4),atc_code_4]
  atc_codes_to_add<-atc_codes_to_add[!atc_codes_to_add %in% "N/A"]
  if(length(atc_codes_to_add)==0){
    a<-NULL
  }else{
  a<-as.data.table(expand.grid(no_users_present[event_definition==events[ls],new],atc_codes_to_add))
  names(a)<-c("combine", "atc_code_4")
  a[,c("event_definition","year","no_subjects"):=tstrsplit(combine,"_", fixed=T)]
  a[,combine:=NULL]
  }
  list[[ls]]<-a
  rm(a)
}
list<-as.data.table(rbindlist(list))
list[,no_users:=0]
prevalence_diag_medicines_total<-prevalence_diag_medicines_total[!atc_code_4 %in% "N/A"]
prevalence_diag_medicines_total<-as.data.table(rbind(prevalence_diag_medicines_total,list,fill=T))
rm(list,no_users_present)
}else{rm(no_users_present)}
prevalence_diag_medicines_total[,no_users:=as.numeric(no_users)][,no_subjects:=as.numeric(no_subjects)]
#calculate prevalence
prevalence_diag_medicines_total[,prevalence_medicine_use:=round((no_users/no_subjects)*100,1)]
prevalence_diag_medicines_total[, data_access_provider:=data_access_provider_name]
prevalence_diag_medicines_total[, data_source:=data_source_name]

#remove all files in poi tmp
for(i in 1:length(prevalence_diag_medicines_total_fl)){
  file.remove(paste0(poi_tmp, prevalence_diag_medicines_total_fl[i]))
}
rm(prevalence_diag_medicines_total_fl)
prevalence_diag_medicines_total<-prevalence_diag_medicines_total[!is.na(event_definition)]
#save results to g_output/EVENTS_MEDICINES
if(subpopulations_present=="Yes"){
  fwrite(prevalence_diag_medicines_total, paste0(poi_dir, subpopulations_names[s], "/EVENTS_MEDICINES/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_med_prevalence_y.csv"), row.names = F)
  } else {
  fwrite(prevalence_diag_medicines_total, paste0(poi_dir,"EVENTS_MEDICINES/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_med_prevalence_y.csv"), row.names = F)
  }

prevalence_diag_medicines_total[,no_users:=as.numeric(no_users)][,no_subjects:=as.numeric(no_subjects)]
prevalence_diag_medicines_total[users_categories, u_range := i.range[no_users >= min & no_users <= max], on = .(no_users >= min, no_users <= max)]
prevalence_diag_medicines_total[users_categories, s_range := i.range[no_subjects >= min & no_subjects <= max], on = .(no_subjects >= min, no_subjects <= max)]

gdpr_file<-prevalence_diag_medicines_total[,-c("no_users","no_subjects"),with=F]
setnames(gdpr_file,"u_range","no_users")
setnames(gdpr_file,"s_range","no_subjects")
setcolorder(gdpr_file,c("event_definition", "year","atc_code_4","no_users", "no_subjects","prevalence_medicine_use"))

if(subpopulations_present=="Yes"){
  fwrite(gdpr_file, paste0(poi_dir,subpopulations_names[s], "/GDPR/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_med_prevalence_y_masked.csv"), row.names = F)
} else {
  fwrite(gdpr_file, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_med_prevalence_y_masked.csv"), row.names = F)
}
rm(gdpr_file)

prevalence_diag_medicines_total[,u_range:=NULL][,s_range:=NULL]
#apply masking
prevalence_diag_medicines_total[, no_users:= as.character(no_users)][as.numeric(no_users) > 0 & as.numeric(no_users) < 5, no_users := "<5"]
prevalence_diag_medicines_total[, no_subjects:= as.character(no_subjects)][as.numeric(no_subjects) > 0 & as.numeric(no_subjects) < 5, no_subjects := "<5"]
prevalence_diag_medicines_total[, prevalence_medicine_use:= as.character(prevalence_medicine_use)][no_users=="<5" | no_subjects=="<5", prevalence_medicine_use := "N/A"]

if(subpopulations_present=="Yes"){
  fwrite(prevalence_diag_medicines_total, paste0(poi_dir,subpopulations_names[s], "/EVENTS_MEDICINES/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_diag_med_prevalence_y_masked.csv"), row.names = F)
  fwrite(prevalence_diag_medicines_total, paste0(poi_dir,subpopulations_names[s], "/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_diag_med_prevalence_y_masked.csv"), row.names = F)
  
  } else {
  fwrite(prevalence_diag_medicines_total, paste0(poi_dir, "EVENTS_MEDICINES/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_med_prevalence_y_masked.csv"), row.names = F)
    fwrite(prevalence_diag_medicines_total, paste0(poi_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_med_prevalence_y_masked.csv"), row.names = F)
    
    }

rm(prevalence_diag_medicines_total)
}
####Prevalence of medicine use after a diagnosis by year, atc, sex and event definition####
  if(length(list.files(poi_tmp,"prevalence_diag_medicines_sex.rds"))>0){ 
prevalence_diag_medicines_sex_fl<-list.files(poi_tmp, "prevalence_diag_medicines_sex.rds")#prevalence of medicines records by year and sex
prevalence_diag_medicines_sex<-lapply(paste0(poi_tmp,prevalence_diag_medicines_sex_fl), readRDS)
prevalence_diag_medicines_sex<-as.data.table(rbindlist(prevalence_diag_medicines_sex,fill = T))
#make sure there are no zeros in no_subjects
prevalence_diag_medicines_sex<-prevalence_diag_medicines_sex[!is.na(no_subjects)]
#replace NA in atc_code_4 with N/A
prevalence_diag_medicines_sex[is.na(atc_code_4), atc_code_4:="N/A"]
#replace NA in no_users with 0
prevalence_diag_medicines_sex[is.na(no_users), no_users:=0]
#apply sum to make sure all categories are combined
prevalence_diag_medicines_sex<-prevalence_diag_medicines_sex[,lapply(.SD, sum), by=c("event_definition","sex", "year","atc_code_4"), .SDcols=c("no_users","no_subjects")]

#start with females
#separate atc_code=="N/A"
no_users_present<-prevalence_diag_medicines_sex[atc_code_4=="N/A" & sex=="F"]
if(no_users_present[,.N]>0){
no_users_present[,atc_code_4:=NULL][,no_users:=NULL]
no_users_present[ , new := do.call(paste, c(.SD, sep = "_"))]
events<-no_users_present[!duplicated(event_definition),event_definition]
list<-vector(mode="list", length=length(events))
for(ls in 1:length(list)){
  atc_codes_to_add<-prevalence_diag_medicines_sex[sex=="F"][event_definition==events[ls]][!duplicated(atc_code_4),atc_code_4]
  atc_codes_to_add<-atc_codes_to_add[!atc_codes_to_add %in% "N/A"]
  if(length(atc_codes_to_add)==0){
  a<-NULL
  }else{
  a<-as.data.table(expand.grid(no_users_present[event_definition==events[ls],new],atc_codes_to_add))
  names(a)<-c("combine", "atc_code_4")
  a[,c("event_definition","sex", "year","no_subjects"):=tstrsplit(combine,"_", fixed=T)]
  a[,combine:=NULL]
  }
  list[[ls]]<-a
  rm(a)
}
list<-as.data.table(rbindlist(list))
list[,no_users:=0]
prevalence_diag_medicines_sex_f<-prevalence_diag_medicines_sex[sex=="F"][!atc_code_4 %in% "N/A"]
prevalence_diag_medicines_sex_f<-as.data.table(rbind(prevalence_diag_medicines_sex_f,list,fill=T))
rm(list,no_users_present)
}else{
  rm(no_users_present)
  prevalence_diag_medicines_sex_f<-prevalence_diag_medicines_sex[sex=="F"][!atc_code_4 %in% "N/A"]
  }

#start with males
#separate atc_code=="N/A"
no_users_present<-prevalence_diag_medicines_sex[atc_code_4=="N/A" & sex=="M"]
if(no_users_present[,.N]>0){
  no_users_present[,atc_code_4:=NULL][,no_users:=NULL]
  no_users_present[ , new := do.call(paste, c(.SD, sep = "_"))]
  events<-no_users_present[!duplicated(event_definition),event_definition]
  list<-vector(mode="list", length=length(events))
  for(ls in 1:length(list)){
    atc_codes_to_add<-prevalence_diag_medicines_sex[sex=="M"][event_definition==events[ls]][!duplicated(atc_code_4),atc_code_4]
    atc_codes_to_add<-atc_codes_to_add[!atc_codes_to_add %in% "N/A"]
    if(length(atc_codes_to_add)==0){
      a<-NULL
    }else{
    a<-as.data.table(expand.grid(no_users_present[event_definition==events[ls],new],atc_codes_to_add))
    names(a)<-c("combine", "atc_code_4")
    a[,c("event_definition","sex", "year","no_subjects"):=tstrsplit(combine,"_", fixed=T)]
    a[,combine:=NULL]
    }
    list[[ls]]<-a
    rm(a)
  }
  list<-as.data.table(rbindlist(list))
  list[,no_users:=0]
  prevalence_diag_medicines_sex_m<-prevalence_diag_medicines_sex[sex=="M"][!atc_code_4 %in% "N/A"]
  prevalence_diag_medicines_sex_m<-as.data.table(rbind(prevalence_diag_medicines_sex_m,list,fill=T))
  rm(list,no_users_present)
}else{
  rm(no_users_present)
  prevalence_diag_medicines_sex_m<-prevalence_diag_medicines_sex[sex=="M"][!atc_code_4 %in% "N/A"]}
rm(prevalence_diag_medicines_sex)
prevalence_diag_medicines_sex<-as.data.table(rbind(prevalence_diag_medicines_sex_m,prevalence_diag_medicines_sex_f))
prevalence_diag_medicines_sex[,no_users:=as.numeric(no_users)][,no_subjects:=as.numeric(no_subjects)]

#calculate prevalence
prevalence_diag_medicines_sex[,prevalence_medicine_use:=round((no_users/no_subjects)*100,1)]
prevalence_diag_medicines_sex[, data_access_provider:=data_access_provider_name]
prevalence_diag_medicines_sex[, data_source:=data_source_name]

#remove all files in poi tmp
for(i in 1:length(prevalence_diag_medicines_sex_fl)){
  file.remove(paste0(poi_tmp, prevalence_diag_medicines_sex_fl[i]))
}
rm(prevalence_diag_medicines_sex_fl)
prevalence_diag_medicines_sex<-prevalence_diag_medicines_sex[!is.na(event_definition)]

#save results to g_output/EVENTS_MEDICINES
if(subpopulations_present=="Yes"){
  fwrite(prevalence_diag_medicines_sex, paste0(poi_dir, subpopulations_names[s], "/EVENTS_MEDICINES/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_med_prevalence_ys.csv"), row.names = F)
} else {
  fwrite(prevalence_diag_medicines_sex, paste0(poi_dir,"EVENTS_MEDICINES/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_med_prevalence_ys.csv"), row.names = F)
}

prevalence_diag_medicines_sex[,no_users:=as.numeric(no_users)][,no_subjects:=as.numeric(no_subjects)]
prevalence_diag_medicines_sex[users_categories, u_range := i.range[no_users >= min & no_users <= max], on = .(no_users >= min, no_users <= max)]
prevalence_diag_medicines_sex[users_categories, s_range := i.range[no_subjects >= min & no_subjects <= max], on = .(no_subjects >= min, no_subjects <= max)]

gdpr_file<-prevalence_diag_medicines_sex[,-c("no_users","no_subjects"),with=F]
setnames(gdpr_file,"u_range","no_users")
setnames(gdpr_file,"s_range","no_subjects")
setcolorder(gdpr_file,c("event_definition","sex", "year","atc_code_4","no_users", "no_subjects","prevalence_medicine_use"))

if(subpopulations_present=="Yes"){
  fwrite(gdpr_file, paste0(poi_dir,subpopulations_names[s], "/GDPR/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_med_prevalence_ys_masked.csv"), row.names = F)
} else {
  fwrite(gdpr_file, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_med_prevalence_ys_masked.csv"), row.names = F)
}
rm(gdpr_file)

prevalence_diag_medicines_sex[,u_range:=NULL][,s_range:=NULL]
#apply masking
prevalence_diag_medicines_sex[, no_users:= as.character(no_users)][as.numeric(no_users) > 0 & as.numeric(no_users) < 5, no_users := "<5"]
prevalence_diag_medicines_sex[, no_subjects:= as.character(no_subjects)][as.numeric(no_subjects) > 0 & as.numeric(no_subjects) < 5, no_subjects := "<5"]
prevalence_diag_medicines_sex[, prevalence_medicine_use:= as.character(prevalence_medicine_use)][no_users=="<5" | no_subjects=="<5", prevalence_medicine_use := "N/A"]

if(subpopulations_present=="Yes"){
  fwrite(prevalence_diag_medicines_sex, paste0(poi_dir,subpopulations_names[s], "/EVENTS_MEDICINES/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_diag_med_prevalence_ys_masked.csv"), row.names = F)
  fwrite(prevalence_diag_medicines_sex, paste0(poi_dir,subpopulations_names[s], "/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_diag_med_prevalence_ys_masked.csv"), row.names = F)
  
  } else {
  fwrite(prevalence_diag_medicines_sex, paste0(poi_dir, "EVENTS_MEDICINES/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_med_prevalence_ys_masked.csv"), row.names = F)
    fwrite(prevalence_diag_medicines_sex, paste0(poi_dir, "/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_med_prevalence_ys_masked.csv"), row.names = F)
    }

rm(prevalence_diag_medicines_sex)
}
####Counts of medicine records by sex####
  if(length(list.files(poi_tmp,"diag_medicines_sex.rds"))>0){ 
diag_medicines_sex_fl<-list.files(poi_tmp, "diag_medicines_sex.rds")#counts of medicines records by year and sex
diag_medicines_sex<-lapply(paste0(poi_tmp,diag_medicines_sex_fl), readRDS)
diag_medicines_sex<-as.data.table(rbindlist(diag_medicines_sex,fill = T))
#make sure there are no zeros in no_subjects
diag_medicines_sex<-diag_medicines_sex[!is.na(total_records)]
#apply sum to make sure all categories are combined
diag_medicines_sex<-diag_medicines_sex[,lapply(.SD, sum), by=c("event_definition","sex", "year","atc_code_4"), .SDcols=c("no_records","total_records")]
#calculate percentage
diag_medicines_sex[,percentage:=round((no_records/total_records)*100,1)]
diag_medicines_sex[, data_access_provider:=data_access_provider_name]
diag_medicines_sex[, data_source:=data_source_name]

#remove all files in poi tmp
for(i in 1:length(diag_medicines_sex_fl)){
  file.remove(paste0(poi_tmp, diag_medicines_sex_fl[i]))
}
rm(diag_medicines_sex_fl)

diag_medicines_sex<-diag_medicines_sex[!is.na(event_definition)]

#save results to g_output/EVENTS_MEDICINES
if(subpopulations_present=="Yes"){
  fwrite(diag_medicines_sex, paste0(poi_dir, subpopulations_names[s], "/EVENTS_MEDICINES/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_med_counts_ys.csv"), row.names = F)
} else {
  fwrite(diag_medicines_sex, paste0(poi_dir,"EVENTS_MEDICINES/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_med_counts_ys.csv"), row.names = F)
}

diag_medicines_sex[,no_records:=as.numeric(no_records)][,total_records:=as.numeric(total_records)]
diag_medicines_sex[records_categories, u_range := i.range[no_records >= min & no_records <= max], on = .(no_records >= min, no_records <= max)]
diag_medicines_sex[records_categories, s_range := i.range[total_records >= min & total_records <= max], on = .(total_records >= min, total_records <= max)]

gdpr_file<-diag_medicines_sex[,-c("no_records","total_records"),with=F]
setnames(gdpr_file,"u_range","no_records")
setnames(gdpr_file,"s_range","total_records")
setcolorder(gdpr_file,c("event_definition","sex", "year","atc_code_4","no_records", "total_records","percentage"))

if(subpopulations_present=="Yes"){
  fwrite(gdpr_file, paste0(poi_dir,subpopulations_names[s], "/GDPR/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_med_counts_ys_masked.csv"), row.names = F)
} else {
  fwrite(gdpr_file, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_med_counts_ys_masked.csv"), row.names = F)
}
rm(gdpr_file)

diag_medicines_sex[,u_range:=NULL][,s_range:=NULL]
#apply masking
diag_medicines_sex[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
diag_medicines_sex[, total_records:= as.character(total_records)][as.numeric(total_records) > 0 & as.numeric(total_records) < 5, total_records := "<5"]
diag_medicines_sex[, percentage:= as.character(percentage)][no_records=="<5" | total_records=="<5", percentage := "N/A"]

if(subpopulations_present=="Yes"){
  fwrite(diag_medicines_sex, paste0(poi_dir,subpopulations_names[s], "/EVENTS_MEDICINES/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_diag_med_counts_ys_masked.csv"), row.names = F)
  fwrite(diag_medicines_sex, paste0(poi_dir,subpopulations_names[s],"/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_diag_med_counts_ys_masked.csv"), row.names = F)
  
  } else {
  fwrite(diag_medicines_sex, paste0(poi_dir, "EVENTS_MEDICINES/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_med_counts_ys_masked.csv"), row.names = F)
    fwrite(diag_medicines_sex, paste0(poi_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_med_counts_ys_masked.csv"), row.names = F)
    
    }

rm(diag_medicines_sex)
}
####Counts of medicine records by meaning####
  if(length(list.files(poi_tmp,"diag_medicines_meaning.rds"))>0){ 
diag_medicines_meaning_fl<-list.files(poi_tmp, "diag_medicines_meaning.rds")#counts of medicines records by year and sex
diag_medicines_meaning<-lapply(paste0(poi_tmp,diag_medicines_meaning_fl), readRDS)
diag_medicines_meaning<-as.data.table(rbindlist(diag_medicines_meaning,fill = T))
#make sure there are no zeros in no_subjects
diag_medicines_meaning<-diag_medicines_meaning[!is.na(total_records)]
#apply sum to make sure all categories are combined
diag_medicines_meaning<-diag_medicines_meaning[,lapply(.SD, sum), by=c("event_definition","meaning", "year","atc_code_4"), .SDcols=c("no_records","total_records")]
#calculate percentage
diag_medicines_meaning[,percentage:=round((no_records/total_records)*100,1)]
diag_medicines_meaning[, data_access_provider:=data_access_provider_name]
diag_medicines_meaning[, data_source:=data_source_name]

#remove all files in poi tmp
for(i in 1:length(diag_medicines_meaning_fl)){
  file.remove(paste0(poi_tmp, diag_medicines_meaning_fl[i]))
}
rm(diag_medicines_meaning_fl)
diag_medicines_meaning<-diag_medicines_meaning[!is.na(event_definition)]

#save results to g_output/EVENTS_MEDICINES
if(subpopulations_present=="Yes"){
  fwrite(diag_medicines_meaning, paste0(poi_dir, subpopulations_names[s], "/EVENTS_MEDICINES/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_med_counts_ym.csv"), row.names = F)
} else {
  fwrite(diag_medicines_meaning, paste0(poi_dir,"EVENTS_MEDICINES/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_med_counts_ym.csv"), row.names = F)
}

diag_medicines_meaning[,no_records:=as.numeric(no_records)][,total_records:=as.numeric(total_records)]
diag_medicines_meaning[records_categories, u_range := i.range[no_records >= min & no_records <= max], on = .(no_records >= min, no_records <= max)]
diag_medicines_meaning[records_categories, s_range := i.range[total_records >= min & total_records <= max], on = .(total_records >= min, total_records <= max)]

gdpr_file<-diag_medicines_meaning[,-c("no_records","total_records"),with=F]
setnames(gdpr_file,"u_range","no_records")
setnames(gdpr_file,"s_range","total_records")
setcolorder(gdpr_file,c("event_definition","meaning", "year","atc_code_4","no_records", "total_records","percentage"))

if(subpopulations_present=="Yes"){
  fwrite(gdpr_file, paste0(poi_dir,subpopulations_names[s], "/GDPR/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_med_counts_ym_masked.csv"), row.names = F)
  } else {
  fwrite(gdpr_file, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_med_counts_ym_masked.csv"), row.names = F)
}
rm(gdpr_file)

diag_medicines_meaning[,u_range:=NULL][,s_range:=NULL]
#apply masking
diag_medicines_meaning[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
diag_medicines_meaning[, total_records:= as.character(total_records)][as.numeric(total_records) > 0 & as.numeric(total_records) < 5, total_records := "<5"]
diag_medicines_meaning[, percentage:= as.character(percentage)][no_records=="<5" | total_records=="<5", percentage := "N/A"]

if(subpopulations_present=="Yes"){
  fwrite(diag_medicines_meaning, paste0(poi_dir,subpopulations_names[s], "/EVENTS_MEDICINES/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_diag_med_counts_ym_masked.csv"), row.names = F)
  fwrite(diag_medicines_meaning, paste0(poi_dir,subpopulations_names[s],"/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_diag_med_counts_ym_masked.csv"), row.names = F)
  
  } else {
  fwrite(diag_medicines_meaning, paste0(poi_dir, "EVENTS_MEDICINES/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_med_counts_ym_masked.csv"), row.names = F)
    fwrite(diag_medicines_meaning, paste0(poi_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_med_counts_ym_masked.csv"), row.names = F)
    
    }

rm(diag_medicines_meaning)
}
####Counts of medicine records by year####
  if(length(list.files(poi_tmp,"diag_medicines_total.rds"))>0){ 
diag_medicines_fl<-list.files(poi_tmp, "diag_medicines_total.rds")#counts of medicines records by year and sex
diag_medicines<-lapply(paste0(poi_tmp,diag_medicines_fl), readRDS)
diag_medicines<-as.data.table(rbindlist(diag_medicines,fill = T))
#make sure there are no zeros in no_subjects
diag_medicines<-diag_medicines[!is.na(total_records)]
#apply sum to make sure all categories are combined
diag_medicines<-diag_medicines[,lapply(.SD, sum), by=c("event_definition", "year","atc_code_4"), .SDcols=c("no_records","total_records")]
#calculate percentage
diag_medicines[,percentage:=round((no_records/total_records)*100,1)]
diag_medicines[, data_access_provider:=data_access_provider_name]
diag_medicines[, data_source:=data_source_name]

#remove all files in poi tmp
for(i in 1:length(diag_medicines_fl)){
  file.remove(paste0(poi_tmp, diag_medicines_fl[i]))
}
rm(diag_medicines_fl)
diag_medicines<-diag_medicines[!is.na(event_definition)]

#save results to g_output/EVENTS_MEDICINES
if(subpopulations_present=="Yes"){
  fwrite(diag_medicines, paste0(poi_dir, subpopulations_names[s], "/EVENTS_MEDICINES/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_med_counts_y.csv"), row.names = F)
} else {
  fwrite(diag_medicines, paste0(poi_dir,"EVENTS_MEDICINES/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_med_counts_y.csv"), row.names = F)
}

diag_medicines[,no_records:=as.numeric(no_records)][,total_records:=as.numeric(total_records)]
diag_medicines[records_categories, u_range := i.range[no_records >= min & no_records <= max], on = .(no_records >= min, no_records <= max)]
diag_medicines[records_categories, s_range := i.range[total_records >= min & total_records <= max], on = .(total_records >= min, total_records <= max)]

gdpr_file<-diag_medicines[,-c("no_records","total_records"),with=F]
setnames(gdpr_file,"u_range","no_records")
setnames(gdpr_file,"s_range","total_records")
setcolorder(gdpr_file,c("event_definition", "year","atc_code_4","no_records", "total_records","percentage"))

if(subpopulations_present=="Yes"){
  fwrite(gdpr_file, paste0(poi_dir,subpopulations_names[s], "/GDPR/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_med_counts_y_masked.csv"), row.names = F)
} else {
  fwrite(gdpr_file, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_med_counts_y_masked.csv"), row.names = F)
}
rm(gdpr_file)

diag_medicines[,u_range:=NULL][,s_range:=NULL]
#apply masking
diag_medicines[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
diag_medicines[, total_records:= as.character(total_records)][as.numeric(total_records) > 0 & as.numeric(total_records) < 5, total_records := "<5"]
diag_medicines[, percentage:= as.character(percentage)][no_records=="<5" | total_records=="<5", percentage := "N/A"]

if(subpopulations_present=="Yes"){
  fwrite(diag_medicines, paste0(poi_dir,subpopulations_names[s], "/EVENTS_MEDICINES/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_diag_med_counts_y_masked.csv"), row.names = F)
  fwrite(diag_medicines, paste0(poi_dir,subpopulations_names[s], "/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_diag_med_counts_y_masked.csv"), row.names = F)
  
  } else {
  fwrite(diag_medicines, paste0(poi_dir, "EVENTS_MEDICINES/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_med_counts_y_masked.csv"), row.names = F)
    fwrite(diag_medicines, paste0(poi_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_med_counts_y_masked.csv"), row.names = F)
    }

rm(diag_medicines)
}
####Combine results for excluded records dating prior to diagnosis####
  if(length(list.files(poi_tmp,"medicines_prior_date_excluded.rds"))>0){ 
med_excluded_prior_fl<-list.files(poi_tmp, "medicines_prior_date_excluded.rds")
med_excluded_prior<-lapply(paste0(poi_tmp,med_excluded_prior_fl), readRDS)
med_excluded_prior<-as.data.table(rbindlist(med_excluded_prior,fill = T))
med_excluded_prior[,year:=substr(medicine_group,3,6)]
med_excluded_prior[,medicine_group:=NULL]
med_excluded_prior<-med_excluded_prior[,lapply(.SD, sum), .SDcols="removed_rec", by=c("event_definition","year")]

#remove all files in poi tmp
for(i in 1:length(med_excluded_prior_fl)){
  file.remove(paste0(poi_tmp, med_excluded_prior_fl[i]))
}
rm(med_excluded_prior_fl)

#save results to g_output/EVENTS_VACCINES
if(subpopulations_present=="Yes"){
  fwrite(med_excluded_prior, paste0(poi_dir, subpopulations_names[s], "/EVENTS_MEDICINES/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_med_prior_removed.csv"), row.names = F)
  fwrite(med_excluded_prior, paste0(poi_dir, subpopulations_names[s], "/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_med_prior_removed.csv"), row.names = F)
  fwrite(med_excluded_prior, paste0(poi_dir, subpopulations_names[s], "/GDPR/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_med_prior_removed.csv"), row.names = F)
  
  } else {
  fwrite(med_excluded_prior, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_med_prior_removed.csv"), row.names = F)
    fwrite(med_excluded_prior, paste0(poi_dir,"Masked/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_med_prior_removed.csv"), row.names = F)
      fwrite(med_excluded_prior, paste0(poi_dir,"EVENTS_MEDICINES/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_med_prior_removed.csv"), row.names = F)
    
    }

rm(med_excluded_prior)
  }

}

#####Combine results for vaccines exposure after a diagnoses#####
vaccines_files<-length(list.files(poi_tmp,"prevalence_diag_vaccines_total|prevalence_diag_vaccines_sex|diag_vaccines_sex|diag_vaccines_meaning|diag_vaccines_total|vaccines_prior_date_excluded|vaccines_duplicated_rec_excluded"))

if(vaccines_files>0){
  ####Prevalence of vaccine exposure after a diagnosis by year, atc and event definition####
  if(length(list.files(poi_tmp,"prevalence_diag_vaccines_total.rds"))>0){ 
  prevalence_diag_vaccines_total_fl<-list.files(poi_tmp, "prevalence_diag_vaccines_total.rds")#prevalence of vaccines records by year
  prevalence_diag_vaccines_total<-lapply(paste0(poi_tmp,prevalence_diag_vaccines_total_fl), readRDS)
  prevalence_diag_vaccines_total<-as.data.table(rbindlist(prevalence_diag_vaccines_total,fill = T))
  #make sure there are no zeros in no_subjects
  prevalence_diag_vaccines_total<-prevalence_diag_vaccines_total[!is.na(no_subjects)]
  #replace NA in atc_code_4 with N/A
  prevalence_diag_vaccines_total[is.na(atc_code_4), atc_code_4:="N/A"]
  #replace NA in no_users with 0
  prevalence_diag_vaccines_total[is.na(no_users), no_users:=0]
  #apply sum to make sure all categories are combined
  prevalence_diag_vaccines_total<-prevalence_diag_vaccines_total[,lapply(.SD, sum), by=c("event_definition","year","atc_code_4"), .SDcols=c("no_users","no_subjects")]
  #separate atc_code=="N/A"
  no_users_present<-prevalence_diag_vaccines_total[atc_code_4=="N/A"]
  if(no_users_present[,.N]>0){
  no_users_present[,atc_code_4:=NULL][,no_users:=NULL]
  no_users_present[ , new := do.call(paste, c(.SD, sep = "_"))]
  events<-no_users_present[!duplicated(event_definition),event_definition]
  list<-vector(mode="list", length=length(events))
  for(ls in 1:length(list)){
    atc_codes_to_add<-prevalence_diag_vaccines_total[event_definition==events[ls]][!duplicated(atc_code_4),atc_code_4]
    atc_codes_to_add<-atc_codes_to_add[!atc_codes_to_add %in% "N/A"]
    if(length(atc_codes_to_add)==0){
      a<-NULL
    }else{
    a<-as.data.table(expand.grid(no_users_present[event_definition==events[ls],new],atc_codes_to_add))
    names(a)<-c("combine", "atc_code_4")
    a[,c("event_definition","year","no_subjects"):=tstrsplit(combine,"_", fixed=T)]
    a[,combine:=NULL]
    }
    list[[ls]]<-a
    rm(a)
  }
  list<-as.data.table(rbindlist(list))
  list[,no_users:=0]
  prevalence_diag_vaccines_total<-prevalence_diag_vaccines_total[!atc_code_4 %in% "N/A"]
  prevalence_diag_vaccines_total<-as.data.table(rbind(prevalence_diag_vaccines_total,list,fill=T))
  rm(list,no_users_present)
  }else{rm(no_users_present)}
  prevalence_diag_vaccines_total[,no_users:=as.numeric(no_users)][,no_subjects:=as.numeric(no_subjects)]
  
  #calculate prevalence
  prevalence_diag_vaccines_total[,prevalence_vaccine_exposure:=round((no_users/no_subjects)*100,1)]
  
  setnames(prevalence_diag_vaccines_total,"atc_code_4", "vaccine_indicator")
  #remove all files in poi tmp
  for(i in 1:length(prevalence_diag_vaccines_total_fl)){
    file.remove(paste0(poi_tmp, prevalence_diag_vaccines_total_fl[i]))
  }
  rm(prevalence_diag_vaccines_total_fl)
  prevalence_diag_vaccines_total<-prevalence_diag_vaccines_total[!is.na(event_definition)]
  prevalence_diag_vaccines_total[, data_access_provider:=data_access_provider_name]
  prevalence_diag_vaccines_total[, data_source:=data_source_name]
  
  #save results to g_output/EVENTS_VACCINES
  if(subpopulations_present=="Yes"){
    fwrite(prevalence_diag_vaccines_total, paste0(poi_dir, subpopulations_names[s], "/EVENTS_VACCINES/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_vacc_prevalence_y.csv"), row.names = F)
  } else {
    fwrite(prevalence_diag_vaccines_total, paste0(poi_dir,"EVENTS_VACCINES/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_vacc_prevalence_y.csv"), row.names = F)
  }
  
  prevalence_diag_vaccines_total[,no_users:=as.numeric(no_users)][,no_subjects:=as.numeric(no_subjects)]
  prevalence_diag_vaccines_total[users_categories, u_range := i.range[no_users >= min & no_users <= max], on = .(no_users >= min, no_users <= max)]
  prevalence_diag_vaccines_total[users_categories, s_range := i.range[no_subjects >= min & no_subjects <= max], on = .(no_subjects >= min, no_subjects <= max)]
  
  gdpr_file<-prevalence_diag_vaccines_total[,-c("no_users","no_subjects"),with=F]
  setnames(gdpr_file,"u_range","no_users")
  setnames(gdpr_file,"s_range","no_subjects")
  setcolorder(gdpr_file,c("event_definition", "year","vaccine_indicator","no_users", "no_subjects","prevalence_vaccine_exposure"))
  
  if(subpopulations_present=="Yes"){
    fwrite(gdpr_file, paste0(poi_dir,subpopulations_names[s], "/GDPR/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_vacc_prevalence_y_masked.csv"), row.names = F)
  } else {
    fwrite(gdpr_file, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_vacc_prevalence_y_masked.csv"), row.names = F)
  }
  rm(gdpr_file)
  
  prevalence_diag_vaccines_total[,u_range:=NULL][,s_range:=NULL]
  #apply masking
  prevalence_diag_vaccines_total[, no_users:= as.character(no_users)][as.numeric(no_users) > 0 & as.numeric(no_users) < 5, no_users := "<5"]
  prevalence_diag_vaccines_total[, no_subjects:= as.character(no_subjects)][as.numeric(no_subjects) > 0 & as.numeric(no_subjects) < 5, no_subjects := "<5"]
  prevalence_diag_vaccines_total[, prevalence_vaccine_exposure:= as.character(prevalence_vaccine_exposure)][no_users=="<5" | no_subjects=="<5", prevalence_vaccine_exposure := "N/A"]
  
  if(subpopulations_present=="Yes"){
    fwrite(prevalence_diag_vaccines_total, paste0(poi_dir,subpopulations_names[s], "/EVENTS_VACCINES/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_diag_vacc_prevalence_y_masked.csv"), row.names = F)
    fwrite(prevalence_diag_vaccines_total, paste0(poi_dir,subpopulations_names[s], "/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_diag_vacc_prevalence_y_masked.csv"), row.names = F)
    
      } else {
    fwrite(prevalence_diag_vaccines_total, paste0(poi_dir, "EVENTS_VACCINES/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_vacc_prevalence_y_masked.csv"), row.names = F)
        fwrite(prevalence_diag_vaccines_total, paste0(poi_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_vacc_prevalence_y_masked.csv"), row.names = F)
        
          }
  
  rm(prevalence_diag_vaccines_total)
  }
  ####Prevalence of vaccine exposure after a diagnosis by year, atc, sex and event definition####
  if(length(list.files(poi_tmp,"prevalence_diag_vaccines_sex.rds"))>0){ 
  prevalence_diag_vaccines_sex_fl<-list.files(poi_tmp, "prevalence_diag_vaccines_sex.rds")#prevalence of vaccines records by year and sex
  prevalence_diag_vaccines_sex<-lapply(paste0(poi_tmp,prevalence_diag_vaccines_sex_fl), readRDS)
  prevalence_diag_vaccines_sex<-as.data.table(rbindlist(prevalence_diag_vaccines_sex,fill = T))
  #make sure there are no zeros in no_subjects
  prevalence_diag_vaccines_sex<-prevalence_diag_vaccines_sex[!is.na(no_subjects)]
  #replace NA in atc_code_4 with N/A
  prevalence_diag_vaccines_sex[is.na(atc_code_4), atc_code_4:="N/A"]
  #replace NA in no_users with 0
  prevalence_diag_vaccines_sex[is.na(no_users), no_users:=0]
  #apply sum to make sure all categories are combined
  prevalence_diag_vaccines_sex<-prevalence_diag_vaccines_sex[,lapply(.SD, sum), by=c("event_definition","sex", "year","atc_code_4"), .SDcols=c("no_users","no_subjects")]
  
  #start with females
  #separate atc_code=="N/A"
  no_users_present<-prevalence_diag_vaccines_sex[atc_code_4=="N/A" & sex=="F"]
  if(no_users_present[,.N]>0){
    no_users_present[,atc_code_4:=NULL][,no_users:=NULL]
    no_users_present[ , new := do.call(paste, c(.SD, sep = "_"))]
    events<-no_users_present[!duplicated(event_definition),event_definition]
    list<-vector(mode="list", length=length(events))
    for(ls in 1:length(list)){
      atc_codes_to_add<-prevalence_diag_vaccines_sex[sex=="F"][event_definition==events[ls]][!duplicated(atc_code_4),atc_code_4]
      atc_codes_to_add<-atc_codes_to_add[!atc_codes_to_add %in% "N/A"]
      if(length(atc_codes_to_add)==0){
        a<-NULL
        }else{
      a<-as.data.table(expand.grid(no_users_present[event_definition==events[ls],new],atc_codes_to_add))
      names(a)<-c("combine", "atc_code_4")
      a[,c("event_definition","sex", "year","no_subjects"):=tstrsplit(combine,"_", fixed=T)]
      a[,combine:=NULL]
      }
      list[[ls]]<-a
      rm(a)
    }
    list<-as.data.table(rbindlist(list))
    list[,no_users:=0]
    prevalence_diag_vaccines_sex_f<-prevalence_diag_vaccines_sex[sex=="F"][!atc_code_4 %in% "N/A"]
    prevalence_diag_vaccines_sex_f<-as.data.table(rbind(prevalence_diag_vaccines_sex_f,list,fill=T))
    rm(list,no_users_present)
  }else{
    rm(no_users_present)
    prevalence_diag_vaccines_sex_f<-prevalence_diag_vaccines_sex[sex=="F"][!atc_code_4 %in% "N/A"]
  }
  
  #start with males
  #separate atc_code=="N/A"
  no_users_present<-prevalence_diag_vaccines_sex[atc_code_4=="N/A" & sex=="M"]
  if(no_users_present[,.N]>0){
    no_users_present[,atc_code_4:=NULL][,no_users:=NULL]
    no_users_present[ , new := do.call(paste, c(.SD, sep = "_"))]
    events<-no_users_present[!duplicated(event_definition),event_definition]
    list<-vector(mode="list", length=length(events))
    for(ls in 1:length(list)){
      atc_codes_to_add<-prevalence_diag_vaccines_sex[sex=="M"][event_definition==events[ls]][!duplicated(atc_code_4),atc_code_4]
      atc_codes_to_add<-atc_codes_to_add[!atc_codes_to_add %in% "N/A"]
      if(length(atc_codes_to_add)==0){
        a<-NULL
      }else{
      a<-as.data.table(expand.grid(no_users_present[event_definition==events[ls],new],atc_codes_to_add))
      names(a)<-c("combine", "atc_code_4")
      a[,c("event_definition","sex", "year","no_subjects"):=tstrsplit(combine,"_", fixed=T)]
      a[,combine:=NULL]
      }
      list[[ls]]<-a
      rm(a)
    }
    list<-as.data.table(rbindlist(list))
    list[,no_users:=0]
    prevalence_diag_vaccines_sex_m<-prevalence_diag_vaccines_sex[sex=="M"][!atc_code_4 %in% "N/A"]
    prevalence_diag_vaccines_sex_m<-as.data.table(rbind(prevalence_diag_vaccines_sex_m,list,fill=T))
    rm(list,no_users_present)
  }else{
    rm(no_users_present)
  prevalence_diag_vaccines_sex_m<-prevalence_diag_vaccines_sex[sex=="M"][!atc_code_4 %in% "N/A"]}
  rm(prevalence_diag_vaccines_sex)
  prevalence_diag_vaccines_sex<-as.data.table(rbind(prevalence_diag_vaccines_sex_m,prevalence_diag_vaccines_sex_f))
  prevalence_diag_vaccines_sex[,no_users:=as.numeric(no_users)][,no_subjects:=as.numeric(no_subjects)]
  
  
  #calculate prevalence
  prevalence_diag_vaccines_sex[,prevalence_vaccine_exposure:=round((no_users/no_subjects)*100,1)]
  
  #remove all files in poi tmp
  for(i in 1:length(prevalence_diag_vaccines_sex_fl)){
    file.remove(paste0(poi_tmp, prevalence_diag_vaccines_sex_fl[i]))
  }
  rm(prevalence_diag_vaccines_sex_fl)
  
  setnames(prevalence_diag_vaccines_sex,"atc_code_4", "vaccine_indicator")
  prevalence_diag_vaccines_sex<-prevalence_diag_vaccines_sex[!is.na(event_definition)]
  prevalence_diag_vaccines_sex[, data_access_provider:=data_access_provider_name]
  prevalence_diag_vaccines_sex[, data_source:=data_source_name]
  
  #save results to g_output/EVENTS_VACCINES
  if(subpopulations_present=="Yes"){
    fwrite(prevalence_diag_vaccines_sex, paste0(poi_dir, subpopulations_names[s], "/EVENTS_VACCINES/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_vacc_prevalence_ys.csv"), row.names = F)
  } else {
    fwrite(prevalence_diag_vaccines_sex, paste0(poi_dir,"EVENTS_VACCINES/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_vacc_prevalence_ys.csv"), row.names = F)
  }
  
  prevalence_diag_vaccines_sex[,no_users:=as.numeric(no_users)][,no_subjects:=as.numeric(no_subjects)]
  prevalence_diag_vaccines_sex[users_categories, u_range := i.range[no_users >= min & no_users <= max], on = .(no_users >= min, no_users <= max)]
  prevalence_diag_vaccines_sex[users_categories, s_range := i.range[no_subjects >= min & no_subjects <= max], on = .(no_subjects >= min, no_subjects <= max)]
  
  gdpr_file<-prevalence_diag_vaccines_sex[,-c("no_users","no_subjects"),with=F]
  setnames(gdpr_file,"u_range","no_users")
  setnames(gdpr_file,"s_range","no_subjects")
  setcolorder(gdpr_file,c("event_definition", "year","vaccine_indicator","no_users", "no_subjects","prevalence_vaccine_exposure"))
  
  if(subpopulations_present=="Yes"){
    fwrite(gdpr_file, paste0(poi_dir,subpopulations_names[s], "/GDPR/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_vacc_prevalence_ys_masked.csv"), row.names = F)
  } else {
    fwrite(gdpr_file, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_vacc_prevalence_ys_masked.csv"), row.names = F)
  }
  rm(gdpr_file)
  
  prevalence_diag_vaccines_sex[,u_range:=NULL][,s_range:=NULL]
  #apply masking
  prevalence_diag_vaccines_sex[, no_users:= as.character(no_users)][as.numeric(no_users) > 0 & as.numeric(no_users) < 5, no_users := "<5"]
  prevalence_diag_vaccines_sex[, no_subjects:= as.character(no_subjects)][as.numeric(no_subjects) > 0 & as.numeric(no_subjects) < 5, no_subjects := "<5"]
  prevalence_diag_vaccines_sex[, prevalence_vaccine_exposure:= as.character(prevalence_vaccine_exposure)][no_users=="<5" | no_subjects=="<5", prevalence_vaccine_exposure := "N/A"]
  
  if(subpopulations_present=="Yes"){
    fwrite(prevalence_diag_vaccines_sex, paste0(poi_dir,subpopulations_names[s], "/EVENTS_VACCINES/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_diag_vacc_prevalence_ys_masked.csv"), row.names = F)
    fwrite(prevalence_diag_vaccines_sex, paste0(poi_dir,subpopulations_names[s], "/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_diag_vacc_prevalence_ys_masked.csv"), row.names = F)
    
      } else {
    fwrite(prevalence_diag_vaccines_sex, paste0(poi_dir, "EVENTS_VACCINES/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_vacc_prevalence_ys_masked.csv"), row.names = F)
        fwrite(prevalence_diag_vaccines_sex, paste0(poi_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_vacc_prevalence_ys_masked.csv"), row.names = F)
        
          }
  
  rm(prevalence_diag_vaccines_sex)
  }
  ####Counts of vaccine records by sex####
  if(length(list.files(poi_tmp,"diag_vaccines_sex.rds"))>0){ 
  diag_vaccines_sex_fl<-list.files(poi_tmp, "diag_vaccines_sex.rds")#counts of vaccines records by year and sex
  diag_vaccines_sex<-lapply(paste0(poi_tmp,diag_vaccines_sex_fl), readRDS)
  diag_vaccines_sex<-as.data.table(rbindlist(diag_vaccines_sex,fill = T))
  #make sure there are no zeros in no_subjects
  diag_vaccines_sex<-diag_vaccines_sex[!is.na(total_records)]
  #apply sum to make sure all categories are combined
  diag_vaccines_sex<-diag_vaccines_sex[,lapply(.SD, sum), by=c("event_definition","sex", "year","atc_code_4"), .SDcols=c("no_records","total_records")]
  #calculate percentage
  diag_vaccines_sex[,percentage:=round((no_records/total_records)*100,1)]
  
  #remove all files in poi tmp
  for(i in 1:length(diag_vaccines_sex_fl)){
    file.remove(paste0(poi_tmp, diag_vaccines_sex_fl[i]))
  }
  rm(diag_vaccines_sex_fl)
  
  setnames(diag_vaccines_sex,"atc_code_4", "vaccine_indicator")
  
  diag_vaccines_sex<-diag_vaccines_sex[!is.na(event_definition)]
  diag_vaccines_sex[, data_access_provider:=data_access_provider_name]
  diag_vaccines_sex[, data_source:=data_source_name]
  
  #save results to g_output/EVENTS_VACCINES
  if(subpopulations_present=="Yes"){
    fwrite(diag_vaccines_sex, paste0(poi_dir, subpopulations_names[s], "/EVENTS_VACCINES/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_vacc_counts_ys.csv"), row.names = F)
  } else {
    fwrite(diag_vaccines_sex, paste0(poi_dir,"EVENTS_VACCINES/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_vacc_counts_ys.csv"), row.names = F)
  }
  
  diag_vaccines_sex[,no_records:=as.numeric(no_records)][,total_records:=as.numeric(total_records)]
  diag_vaccines_sex[records_categories, u_range := i.range[no_records >= min & no_records <= max], on = .(no_records >= min, no_records <= max)]
  diag_vaccines_sex[records_categories, s_range := i.range[total_records >= min & total_records <= max], on = .(total_records >= min, total_records <= max)]
  
  gdpr_file<-diag_vaccines_sex[,-c("no_records","total_records"),with=F]
  setnames(gdpr_file,"u_range","no_records")
  setnames(gdpr_file,"s_range","total_records")
  setcolorder(gdpr_file,c("event_definition","sex", "year","vaccine_indicator","no_records", "total_records","percentage"))
  
  if(subpopulations_present=="Yes"){
    fwrite(gdpr_file, paste0(poi_dir,subpopulations_names[s], "/GDPR/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_vacc_counts_ys_masked.csv"), row.names = F)
  } else {
    fwrite(gdpr_file, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_vacc_counts_ys_masked.csv"), row.names = F)
  }
  rm(gdpr_file)
  
  diag_vaccines_sex[,u_range:=NULL][,s_range:=NULL]
  #apply masking
  diag_vaccines_sex[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
  diag_vaccines_sex[, total_records:= as.character(total_records)][as.numeric(total_records) > 0 & as.numeric(total_records) < 5, total_records := "<5"]
  diag_vaccines_sex[, percentage:= as.character(percentage)][no_records=="<5" | total_records=="<5", percentage := "N/A"]
  
  if(subpopulations_present=="Yes"){
    fwrite(diag_vaccines_sex, paste0(poi_dir,subpopulations_names[s], "/EVENTS_VACCINES/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_diag_vacc_counts_ys_masked.csv"), row.names = F)
    fwrite(diag_vaccines_sex, paste0(poi_dir,subpopulations_names[s], "/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_diag_vacc_counts_ys_masked.csv"), row.names = F)
    
      } else {
    fwrite(diag_vaccines_sex, paste0(poi_dir, "EVENTS_VACCINES/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_vacc_counts_ys_masked.csv"), row.names = F)
        fwrite(diag_vaccines_sex, paste0(poi_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_vacc_counts_ys_masked.csv"), row.names = F)
        
          }
  
  rm(diag_vaccines_sex)
  }
  ####Counts of vaccine records by meaning####
  if(length(list.files(poi_tmp,"diag_vaccines_meaning.rds"))>0){ 
  diag_vaccines_meaning_fl<-list.files(poi_tmp, "diag_vaccines_meaning.rds")#counts of vaccines records by year and sex
  diag_vaccines_meaning<-lapply(paste0(poi_tmp,diag_vaccines_meaning_fl), readRDS)
  diag_vaccines_meaning<-as.data.table(rbindlist(diag_vaccines_meaning,fill = T))
  #make sure there are no zeros in no_subjects
  diag_vaccines_meaning<-diag_vaccines_meaning[!is.na(total_records)]
  #apply sum to make sure all categories are combined
  diag_vaccines_meaning<-diag_vaccines_meaning[,lapply(.SD, sum), by=c("event_definition","meaning", "year","atc_code_4"), .SDcols=c("no_records","total_records")]
  #calculate percentage
  diag_vaccines_meaning[,percentage:=round((no_records/total_records)*100,1)]
  
  #remove all files in poi tmp
  for(i in 1:length(diag_vaccines_meaning_fl)){
    file.remove(paste0(poi_tmp, diag_vaccines_meaning_fl[i]))
  }
  rm(diag_vaccines_meaning_fl)
  
  setnames(diag_vaccines_meaning,"atc_code_4", "vaccine_indicator")
  diag_vaccines_meaning<-diag_vaccines_meaning[!is.na(event_definition)]
  diag_vaccines_meaning[, data_access_provider:=data_access_provider_name]
  diag_vaccines_meaning[, data_source:=data_source_name]
  
  #save results to g_output/EVENTS_VACCINES
  if(subpopulations_present=="Yes"){
    fwrite(diag_vaccines_meaning, paste0(poi_dir, subpopulations_names[s], "/EVENTS_VACCINES/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_vacc_counts_ym.csv"), row.names = F)
  } else {
    fwrite(diag_vaccines_meaning, paste0(poi_dir,"EVENTS_VACCINES/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_vacc_counts_ym.csv"), row.names = F)
  }
  
  diag_vaccines_meaning[,no_records:=as.numeric(no_records)][,total_records:=as.numeric(total_records)]
  diag_vaccines_meaning[records_categories, u_range := i.range[no_records >= min & no_records <= max], on = .(no_records >= min, no_records <= max)]
  diag_vaccines_meaning[records_categories, s_range := i.range[total_records >= min & total_records <= max], on = .(total_records >= min, total_records <= max)]
  
  gdpr_file<-diag_vaccines_meaning[,-c("no_records","total_records"),with=F]
  setnames(gdpr_file,"u_range","no_records")
  setnames(gdpr_file,"s_range","total_records")
  setcolorder(gdpr_file,c("event_definition","meaning", "year","vaccine_indicator","no_records", "total_records","percentage"))
  
  if(subpopulations_present=="Yes"){
    fwrite(gdpr_file, paste0(poi_dir,subpopulations_names[s], "/GDPR/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_vacc_counts_ym_masked.csv"), row.names = F)
  } else {
    fwrite(gdpr_file, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_vacc_counts_ym_masked.csv"), row.names = F)
  }
  rm(gdpr_file)
  
  diag_vaccines_meaning[,u_range:=NULL][,s_range:=NULL]
  #apply masking
  diag_vaccines_meaning[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
  diag_vaccines_meaning[, total_records:= as.character(total_records)][as.numeric(total_records) > 0 & as.numeric(total_records) < 5, total_records := "<5"]
  diag_vaccines_meaning[, percentage:= as.character(percentage)][no_records=="<5" | total_records=="<5", percentage := "N/A"]
  
  if(subpopulations_present=="Yes"){
    fwrite(diag_vaccines_meaning, paste0(poi_dir,subpopulations_names[s], "/EVENTS_VACCINES/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_diag_vacc_counts_ym_masked.csv"), row.names = F)
    fwrite(diag_vaccines_meaning, paste0(poi_dir,subpopulations_names[s], "/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_diag_vacc_counts_ym_masked.csv"), row.names = F)
    
      } else {
    fwrite(diag_vaccines_meaning, paste0(poi_dir, "EVENTS_VACCINES/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_vacc_counts_ym_masked.csv"), row.names = F)
        fwrite(diag_vaccines_meaning, paste0(poi_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_vacc_counts_ym_masked.csv"), row.names = F)
        
          }
  
  rm(diag_vaccines_meaning)
  }
  ####Counts of vaccine records by year####
  if(length(list.files(poi_tmp,"diag_vaccines_total.rds"))>0){ 
  diag_vaccines_fl<-list.files(poi_tmp, "diag_vaccines_total.rds")#counts of vaccines records by year and sex
  diag_vaccines<-lapply(paste0(poi_tmp,diag_vaccines_fl), readRDS)
  diag_vaccines<-as.data.table(rbindlist(diag_vaccines,fill = T))
  #make sure there are no zeros in no_subjects
  diag_vaccines<-diag_vaccines[!is.na(total_records)]
  #apply sum to make sure all categories are combined
  diag_vaccines<-diag_vaccines[,lapply(.SD, sum), by=c("event_definition", "year","atc_code_4"), .SDcols=c("no_records","total_records")]
  #calculate percentage
  diag_vaccines[,percentage:=round((no_records/total_records)*100,1)]
  
  #remove all files in poi tmp
  for(i in 1:length(diag_vaccines_fl)){
    file.remove(paste0(poi_tmp, diag_vaccines_fl[i]))
  }
  rm(diag_vaccines_fl)
  
  setnames(diag_vaccines,"atc_code_4", "vaccine_indicator")
  diag_vaccines<-diag_vaccines[!is.na(event_definition)]
  diag_vaccines[, data_access_provider:=data_access_provider_name]
  diag_vaccines[, data_source:=data_source_name]
  
  #save results to g_output/EVENTS_VACCINES
  if(subpopulations_present=="Yes"){
    fwrite(diag_vaccines, paste0(poi_dir, subpopulations_names[s], "/EVENTS_VACCINES/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_vacc_counts_y.csv"), row.names = F)
  } else {
    fwrite(diag_vaccines, paste0(poi_dir,"EVENTS_VACCINES/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_vacc_counts_y.csv"), row.names = F)
  }
  
  diag_vaccines[,no_records:=as.numeric(no_records)][,total_records:=as.numeric(total_records)]
  diag_vaccines[records_categories, u_range := i.range[no_records >= min & no_records <= max], on = .(no_records >= min, no_records <= max)]
  diag_vaccines[records_categories, s_range := i.range[total_records >= min & total_records <= max], on = .(total_records >= min, total_records <= max)]
  
  gdpr_file<-diag_vaccines[,-c("no_records","total_records"),with=F]
  setnames(gdpr_file,"u_range","no_records")
  setnames(gdpr_file,"s_range","total_records")
  setcolorder(gdpr_file,c("event_definition", "year","vaccine_indicator","no_records", "total_records","percentage"))
  
  if(subpopulations_present=="Yes"){
    fwrite(gdpr_file, paste0(poi_dir,subpopulations_names[s], "/GDPR/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_vacc_counts_y_masked.csv"), row.names = F)
  } else {
    fwrite(gdpr_file, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_vacc_counts_y_masked.csv"), row.names = F)
  }
  rm(gdpr_file)
  
  diag_vaccines[,u_range:=NULL][,s_range:=NULL]
  #apply masking
  diag_vaccines[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
  diag_vaccines[, total_records:= as.character(total_records)][as.numeric(total_records) > 0 & as.numeric(total_records) < 5, total_records := "<5"]
  diag_vaccines[, percentage:= as.character(percentage)][no_records=="<5" | total_records=="<5", percentage := "N/A"]
  
  if(subpopulations_present=="Yes"){
    fwrite(diag_vaccines, paste0(poi_dir,subpopulations_names[s], "/EVENTS_VACCINES/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_diag_vacc_counts_y_masked.csv"), row.names = F)
    fwrite(diag_vaccines, paste0(poi_dir,subpopulations_names[s], "/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_diag_vacc_counts_y_masked.csv"), row.names = F)
    
      } else {
    fwrite(diag_vaccines, paste0(poi_dir, "EVENTS_VACCINES/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_vacc_counts_y_masked.csv"), row.names = F)
        fwrite(diag_vaccines, paste0(poi_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_vacc_counts_y_masked.csv"), row.names = F)
        
          }
  
  rm(diag_vaccines)
  }
  
  ####Combine results for excluded records dating prior to diagnosis####
  if(length(list.files(poi_tmp,"vaccines_prior_date_excluded.rds"))>0){ 
  vacc_excluded_prior_fl<-list.files(poi_tmp, "vaccines_prior_date_excluded.rds")
  vacc_excluded_prior<-lapply(paste0(poi_tmp,vacc_excluded_prior_fl), readRDS)
  vacc_excluded_prior<-as.data.table(rbindlist(vacc_excluded_prior,fill = T))
  vacc_excluded_prior[,year:=substr(vaccine_group,3,6)]
  vacc_excluded_prior[,vaccine_group:=NULL]
  vacc_excluded_prior<-vacc_excluded_prior[,lapply(.SD, sum), .SDcols="removed_rec", by=c("event_definition","year")]
  
  #remove all files in poi tmp
  for(i in 1:length(vacc_excluded_prior_fl)){
    file.remove(paste0(poi_tmp, vacc_excluded_prior_fl[i]))
  }
  rm(vacc_excluded_prior_fl)
  
  #save results to g_output/EVENTS_VACCINES
  if(subpopulations_present=="Yes"){
    fwrite(vacc_excluded_prior, paste0(poi_dir, subpopulations_names[s], "/EVENTS_VACCINES/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_vacc_prior_removed.csv"), row.names = F)
  } else {
    fwrite(vacc_excluded_prior, paste0(poi_dir,"EVENTS_VACCINES/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_vacc_prior_removed.csv"), row.names = F)
  }
  
  if(subpopulations_present=="Yes"){
    fwrite(vacc_excluded_prior, paste0(poi_dir, subpopulations_names[s], "/GDPR/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_vacc_prior_removed.csv"), row.names = F)
    fwrite(vacc_excluded_prior, paste0(poi_dir, subpopulations_names[s], "/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_vacc_prior_removed.csv"), row.names = F)
    
      } else {
    fwrite(vacc_excluded_prior, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_vacc_prior_removed.csv"), row.names = F)
        fwrite(vacc_excluded_prior, paste0(poi_dir,"Masked/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_vacc_prior_removed.csv"), row.names = F)
        
          }
  
  rm(vacc_excluded_prior)
  } 
  
  ####Combine results for duplicated vaccine records####
  if(length(list.files(poi_tmp,"vaccines_duplicated_rec_excluded.rds"))>0){ 
    vaccines_duplicated_rec_excluded_fl<-list.files(poi_tmp, "vaccines_duplicated_rec_excluded.rds")
    vaccines_duplicated_rec_excluded<-lapply(paste0(poi_tmp,vaccines_duplicated_rec_excluded_fl), readRDS)
    vaccines_duplicated_rec_excluded<-as.data.table(rbindlist(vaccines_duplicated_rec_excluded,fill = T))
    vaccines_duplicated_rec_excluded[,year:=substr(vaccine_group,3,6)]
    vaccines_duplicated_rec_excluded[,vaccine_group:=NULL]
    vaccines_duplicated_rec_excluded<-vaccines_duplicated_rec_excluded[,lapply(.SD, sum), .SDcols="duplicated_rec", by=c("event_definition", "year")]
    
    #remove all files in poi tmp
    for(i in 1:length(vaccines_duplicated_rec_excluded_fl)){
      file.remove(paste0(poi_tmp, vaccines_duplicated_rec_excluded_fl[i]))
    }
    rm(vaccines_duplicated_rec_excluded_fl)
    
    #save results to g_output/EVENTS_VACCINES
    if(subpopulations_present=="Yes"){
      fwrite(vaccines_duplicated_rec_excluded, paste0(poi_dir, subpopulations_names[s], "/EVENTS_VACCINES/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_vacc_dup_removed.csv"), row.names = F)
    } else {
      fwrite(vaccines_duplicated_rec_excluded, paste0(poi_dir,"EVENTS_VACCINES/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_vacc_dup_removed.csv"), row.names = F)
    }
    
    if(subpopulations_present=="Yes"){
      fwrite(vaccines_duplicated_rec_excluded, paste0(poi_dir, subpopulations_names[s], "/GDPR/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_vacc_dup_removed.csv"), row.names = F)
      fwrite(vaccines_duplicated_rec_excluded, paste0(poi_dir, subpopulations_names[s], "/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_vacc_dup_removed.csv"), row.names = F)
      
          } else {
      fwrite(vaccines_duplicated_rec_excluded, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_vacc_dup_removed.csv"), row.names = F)
            fwrite(vaccines_duplicated_rec_excluded, paste0(poi_dir,"Masked/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_vacc_dup_removed.csv"), row.names = F)
            
                }
    
    rm(vaccines_duplicated_rec_excluded)
  } 
  
}

#####Combine results for pregnancy records after a diagnoses#####
pregnancy_files<-length(list.files(poi_tmp,"prevalence_diag_pregnancy_total|diag_pregnancy_meaning|diag_pregnancy_total|pregnancy_prior_date_excluded"))

if(pregnancy_files>0){
  ####Prevalence of preganncy records after a diagnosis by year, stage of preg and event definition####
  if(length(list.files(poi_tmp,"prevalence_diag_pregnancy_total.rds"))>0){ 
  prevalence_diag_pregnancy_total_fl<-list.files(poi_tmp, "prevalence_diag_pregnancy_total.rds")#prevalence of pregnancy records by year
  prevalence_diag_pregnancy_total<-lapply(paste0(poi_tmp,prevalence_diag_pregnancy_total_fl), readRDS)
  prevalence_diag_pregnancy_total<-as.data.table(rbindlist(prevalence_diag_pregnancy_total,fill = T))
  #make sure there are no zeros in no_subjects
  prevalence_diag_pregnancy_total<-prevalence_diag_pregnancy_total[!is.na(no_subjects)]
  #replace NA in stage_of_pregnancy with N/A
  prevalence_diag_pregnancy_total[is.na(stage_of_pregnancy), stage_of_pregnancy:="N/A"]
  #replace NA in no_pregnant_women with 0
  prevalence_diag_pregnancy_total[is.na(no_pregnant_women), no_pregnant_women:=0]
  #apply sum to make sure all categories are combined
  prevalence_diag_pregnancy_total<-prevalence_diag_pregnancy_total[,lapply(.SD, sum), by=c("event_definition","year","stage_of_pregnancy"), .SDcols=c("no_pregnant_women","no_subjects")]
  
  #separate atc_code=="N/A"
  no_users_present<-as.data.table(prevalence_diag_pregnancy_total[stage_of_pregnancy=="N/A"])
  if(no_users_present[,.N]>0){
    no_users_present[,stage_of_pregnancy:=NULL][,no_pregnant_women:=NULL]
    no_users_present[ , new := do.call(paste, c(.SD, sep = "_"))]
    events<-no_users_present[!duplicated(event_definition),event_definition]
    list<-vector(mode="list", length=length(events))
    for(ls in 1:length(list)){
      stages_to_add<-prevalence_diag_pregnancy_total[event_definition==events[ls]][!duplicated(stage_of_pregnancy),stage_of_pregnancy]
      stages_to_add<-stages_to_add[!stages_to_add %in% "N/A"]
      if(length(stages_to_add)==0){
        a<-NULL
      }else{
      a<-as.data.table(expand.grid(no_users_present[event_definition==events[ls],new],stages_to_add))
      names(a)<-c("combine", "stage_of_pregnancy")
      a[,c("event_definition","year","no_subjects"):=tstrsplit(combine,"_", fixed=T)]
      a[,combine:=NULL]
      }
      list[[ls]]<-a
      rm(a)
    }
    list<-as.data.table(rbindlist(list))
    list[,no_pregnant_women:=0]
    prevalence_diag_pregnancy_total<-as.data.table(prevalence_diag_pregnancy_total[!stage_of_pregnancy %in% "N/A"])
    prevalence_diag_pregnancy_total<-as.data.table(rbind(prevalence_diag_pregnancy_total,list,fill=T))
    rm(list,no_users_present)
  }else{rm(no_users_present)}
  prevalence_diag_pregnancy_total[,no_pregnant_women:=as.numeric(no_pregnant_women)][,no_subjects:=as.numeric(no_subjects)]
  
  #calculate prevalence
  prevalence_diag_pregnancy_total[,prevalence_pregnancies:=round((no_pregnant_women/no_subjects)*100,1)]
  prevalence_diag_pregnancy_total[, data_access_provider:=data_access_provider_name]
  prevalence_diag_pregnancy_total[, data_source:=data_source_name]
  
  #remove all files in poi tmp
  for(i in 1:length(prevalence_diag_pregnancy_total_fl)){
    file.remove(paste0(poi_tmp, prevalence_diag_pregnancy_total_fl[i]))
  }
  rm(prevalence_diag_pregnancy_total_fl)
  #save results to g_output/EVENTS_PREGNANCY
  if(subpopulations_present=="Yes"){
    fwrite(prevalence_diag_pregnancy_total, paste0(poi_dir, subpopulations_names[s], "/EVENTS_PREGNANCY/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_preg_prevalence_y.csv"), row.names = F)
  } else {
    fwrite(prevalence_diag_pregnancy_total, paste0(poi_dir,"EVENTS_PREGNANCY/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_preg_prevalence_y.csv"), row.names = F)
  }
  
  prevalence_diag_pregnancy_total[,no_pregnant_women:=as.numeric(no_pregnant_women)][,no_subjects:=as.numeric(no_subjects)]
  prevalence_diag_pregnancy_total[users_categories, u_range := i.range[no_pregnant_women >= min & no_pregnant_women <= max], on = .(no_pregnant_women >= min, no_pregnant_women <= max)]
  prevalence_diag_pregnancy_total[users_categories, s_range := i.range[no_subjects >= min & no_subjects <= max], on = .(no_subjects >= min, no_subjects <= max)]
  
  gdpr_file<-prevalence_diag_pregnancy_total[,-c("no_pregnant_women","no_subjects"),with=F]
  setnames(gdpr_file,"u_range","no_pregnant_women")
  setnames(gdpr_file,"s_range","no_subjects")
  setcolorder(gdpr_file,c("event_definition", "year","stage_of_pregnancy","no_pregnant_women", "no_subjects","prevalence_pregnancies"))
  
  if(subpopulations_present=="Yes"){
    fwrite(gdpr_file, paste0(poi_dir,subpopulations_names[s], "/GDPR/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_preg_prevalence_y_masked.csv"), row.names = F)
  } else {
    fwrite(gdpr_file, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_preg_prevalence_y_masked.csv"), row.names = F)
  }
  rm(gdpr_file)
  
  prevalence_diag_pregnancy_total[,u_range:=NULL][,s_range:=NULL]
  #apply masking
  prevalence_diag_pregnancy_total[, no_pregnant_women:= as.character(no_pregnant_women)][as.numeric(no_pregnant_women) > 0 & as.numeric(no_pregnant_women) < 5, no_pregnant_women := "<5"]
  prevalence_diag_pregnancy_total[, no_subjects:= as.character(no_subjects)][as.numeric(no_subjects) > 0 & as.numeric(no_subjects) < 5, no_subjects := "<5"]
  prevalence_diag_pregnancy_total[, prevalence_pregnancies:= as.character(prevalence_pregnancies)][no_pregnant_women=="<5" | no_subjects=="<5", prevalence_pregnancies := "N/A"]
  
  if(subpopulations_present=="Yes"){
    fwrite(prevalence_diag_pregnancy_total, paste0(poi_dir,subpopulations_names[s], "/EVENTS_PREGNANCY/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_diag_preg_prevalence_y_masked.csv"), row.names = F)
    fwrite(prevalence_diag_pregnancy_total, paste0(poi_dir,subpopulations_names[s], "/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_diag_preg_prevalence_y_masked.csv"), row.names = F)
    
      } else {
    fwrite(prevalence_diag_pregnancy_total, paste0(poi_dir, "EVENTS_PREGNANCY/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_preg_prevalence_y_masked.csv"), row.names = F)
        fwrite(prevalence_diag_pregnancy_total, paste0(poi_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_preg_prevalence_y_masked.csv"), row.names = F)
        
          }
  
  rm(prevalence_diag_pregnancy_total)
  } 
  ####Counts of pregnancy records by meaning####
  if(length(list.files(poi_tmp,"diag_pregnancy_meaning.rds"))>0){ 
  diag_pregnancy_meaning_fl<-list.files(poi_tmp, "diag_pregnancy_meaning.rds")#counts of pregnancy records by year and sex
  diag_pregnancy_meaning<-lapply(paste0(poi_tmp,diag_pregnancy_meaning_fl), readRDS)
  diag_pregnancy_meaning<-as.data.table(rbindlist(diag_pregnancy_meaning,fill = T))
  #make sure there are no zeros in no_subjects
  diag_pregnancy_meaning<-diag_pregnancy_meaning[!is.na(total_records)]
  #apply sum to make sure all categories are combined
  diag_pregnancy_meaning<-diag_pregnancy_meaning[,lapply(.SD, sum), by=c("event_definition","stage_of_pregnancy", "meaning", "year"), .SDcols=c("no_records","total_records")]
  #calculate percentage
  diag_pregnancy_meaning[,percentage:=round((no_records/total_records)*100,1)]
  diag_pregnancy_meaning[, data_access_provider:=data_access_provider_name]
  diag_pregnancy_meaning[, data_source:=data_source_name]
  
  
  #remove all files in poi tmp
  for(i in 1:length(diag_pregnancy_meaning_fl)){
    file.remove(paste0(poi_tmp, diag_pregnancy_meaning_fl[i]))
  }
  rm(diag_pregnancy_meaning_fl)
  #save results to g_output/EVENTS_PREGNANCY
  if(subpopulations_present=="Yes"){
    fwrite(diag_pregnancy_meaning, paste0(poi_dir, subpopulations_names[s], "/EVENTS_PREGNANCY/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_preg_counts_ym.csv"), row.names = F)
  } else {
    fwrite(diag_pregnancy_meaning, paste0(poi_dir,"EVENTS_PREGNANCY/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_preg_counts_ym.csv"), row.names = F)
  }
  
  diag_pregnancy_meaning[,no_records:=as.numeric(no_records)][,total_records:=as.numeric(total_records)]
  diag_pregnancy_meaning[records_categories, u_range := i.range[no_records >= min & no_records <= max], on = .(no_records >= min, no_records <= max)]
  diag_pregnancy_meaning[records_categories, s_range := i.range[total_records >= min & total_records <= max], on = .(total_records >= min, total_records <= max)]
  
  gdpr_file<-diag_pregnancy_meaning[,-c("no_records","total_records"),with=F]
  setnames(gdpr_file,"u_range","no_records")
  setnames(gdpr_file,"s_range","total_records")
  setcolorder(gdpr_file,c("event_definition","stage_of_pregnancy","meaning","year", "no_records", "total_records","percentage"))
  
  if(subpopulations_present=="Yes"){
    fwrite(gdpr_file, paste0(poi_dir,subpopulations_names[s], "/GDPR/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_preg_counts_ym_masked.csv"), row.names = F)
  } else {
    fwrite(gdpr_file, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_preg_counts_ym_masked.csv"), row.names = F)
  }
  rm(gdpr_file)
  
  diag_pregnancy_meaning[,u_range:=NULL][,s_range:=NULL]
  #apply masking
  diag_pregnancy_meaning[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
  diag_pregnancy_meaning[, total_records:= as.character(total_records)][as.numeric(total_records) > 0 & as.numeric(total_records) < 5, total_records := "<5"]
  diag_pregnancy_meaning[, percentage:= as.character(percentage)][no_records=="<5" | total_records=="<5", percentage := "N/A"]
  
  if(subpopulations_present=="Yes"){
    fwrite(diag_pregnancy_meaning, paste0(poi_dir,subpopulations_names[s], "/EVENTS_PREGNANCY/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_diag_vacc_counts_ym_masked.csv"), row.names = F)
    fwrite(diag_pregnancy_meaning, paste0(poi_dir,subpopulations_names[s], "/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_diag_vacc_counts_ym_masked.csv"), row.names = F)
    
     } else {
    fwrite(diag_pregnancy_meaning, paste0(poi_dir, "EVENTS_PREGNANCY/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_preg_counts_ym_masked.csv"), row.names = F)
       fwrite(diag_pregnancy_meaning, paste0(poi_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_preg_counts_ym_masked.csv"), row.names = F)
       
         }
  
  rm(diag_pregnancy_meaning)
  }
  ####Counts of pregnancy records by year####
  if(length(list.files(poi_tmp,"diag_pregnancy_total.rds"))>0){ 
  diag_pregnancy_fl<-list.files(poi_tmp, "diag_pregnancy_total.rds")#counts of pregnancy records by year and sex
  diag_pregnancy<-lapply(paste0(poi_tmp,diag_pregnancy_fl), readRDS)
  diag_pregnancy<-as.data.table(rbindlist(diag_pregnancy,fill = T))
  #make sure there are no zeros in no_subjects
  diag_pregnancy<-diag_pregnancy[!is.na(total_records)]
  #apply sum to make sure all categories are combined
  diag_pregnancy<-diag_pregnancy[,lapply(.SD, sum), by=c("event_definition","stage_of_pregnancy", "year"), .SDcols=c("no_pregnancy_records","total_records")]
  #calculate percentage
  diag_pregnancy[,percentage:=round((no_pregnancy_records/total_records)*100,1)]
  setnames(diag_pregnancy,"no_pregnancy_records","no_records")
  diag_pregnancy[, data_access_provider:=data_access_provider_name]
  diag_pregnancy[, data_source:=data_source_name]
  
  #remove all files in poi tmp
  for(i in 1:length(diag_pregnancy_fl)){
    file.remove(paste0(poi_tmp, diag_pregnancy_fl[i]))
  }
  rm(diag_pregnancy_fl)
  #save results to g_output/EVENTS_PREGNANCY
  if(subpopulations_present=="Yes"){
    fwrite(diag_pregnancy, paste0(poi_dir, subpopulations_names[s], "/EVENTS_PREGNANCY/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_preg_counts_y.csv"), row.names = F)
  } else {
    fwrite(diag_pregnancy, paste0(poi_dir,"EVENTS_PREGNANCY/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_preg_counts_y.csv"), row.names = F)
  }
  
  diag_pregnancy[,no_records:=as.numeric(no_records)][,total_records:=as.numeric(total_records)]
  diag_pregnancy[records_categories, u_range := i.range[no_records >= min & no_records <= max], on = .(no_records >= min, no_records <= max)]
  diag_pregnancy[records_categories, s_range := i.range[total_records >= min & total_records <= max], on = .(total_records >= min, total_records <= max)]
  
  gdpr_file<-diag_pregnancy[,-c("no_records","total_records"),with=F]
  setnames(gdpr_file,"u_range","no_records")
  setnames(gdpr_file,"s_range","total_records")
  setcolorder(gdpr_file,c("event_definition","stage_of_pregnancy","year", "no_records", "total_records","percentage"))
  
  if(subpopulations_present=="Yes"){
    fwrite(gdpr_file, paste0(poi_dir,subpopulations_names[s], "/GDPR/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_preg_counts_y_masked.csv"), row.names = F)
  } else {
    fwrite(gdpr_file, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_preg_counts_y_masked.csv"), row.names = F)
  }
  rm(gdpr_file)
  
  diag_pregnancy[,u_range:=NULL][,s_range:=NULL]
  #apply masking
  diag_pregnancy[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
  diag_pregnancy[, total_records:= as.character(total_records)][as.numeric(total_records) > 0 & as.numeric(total_records) < 5, total_records := "<5"]
  diag_pregnancy[, percentage:= as.character(percentage)][no_records=="<5" | total_records=="<5", percentage := "N/A"]
  
  if(subpopulations_present=="Yes"){
    fwrite(diag_pregnancy, paste0(poi_dir,subpopulations_names[s], "/EVENTS_PREGNANCY/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_diag_preg_counts_y_masked.csv"), row.names = F)
    fwrite(diag_pregnancy, paste0(poi_dir,subpopulations_names[s], "/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_diag_preg_counts_y_masked.csv"), row.names = F)
    
      } else {
    fwrite(diag_pregnancy, paste0(poi_dir, "EVENTS_PREGNANCY/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_preg_counts_y_masked.csv"), row.names = F)
        fwrite(diag_pregnancy, paste0(poi_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_preg_counts_y_masked.csv"), row.names = F)
        
          }
  
  rm(diag_pregnancy)
  }
  ####Combine results for excluded records dating prior to diagnosis####
  if(length(list.files(poi_tmp,"pregnancy_prior_date_excluded.rds"))>0){ 
  preg_excluded_prior_fl<-list.files(poi_tmp, "pregnancy_prior_date_excluded.rds")
  preg_excluded_prior<-lapply(paste0(poi_tmp,preg_excluded_prior_fl), readRDS)
  preg_excluded_prior<-as.data.table(rbindlist(preg_excluded_prior,fill = T))
  preg_excluded_prior[,year:=substr(pregnancy_group,1,4)]
  preg_excluded_prior[,pregnancy_group:=NULL]
  preg_excluded_prior<-preg_excluded_prior[,lapply(.SD, sum), .SDcols="removed_rec", by=c("event_definition","year")]
  
  #remove all files in poi tmp
  for(i in 1:length(preg_excluded_prior_fl)){
    file.remove(paste0(poi_tmp, preg_excluded_prior_fl[i]))
  }
  rm(preg_excluded_prior_fl)
  
  #save results to g_output/EVENTS_VACCINES
  if(subpopulations_present=="Yes"){
    fwrite(preg_excluded_prior, paste0(poi_dir, subpopulations_names[s], "/EVENTS_PREGNANCY/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_preg_prior_removed.csv"), row.names = F)
  } else {
    fwrite(preg_excluded_prior, paste0(poi_dir,"EVENTS_PREGNANCY/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_preg_prior_removed.csv"), row.names = F)
  }
  
  if(subpopulations_present=="Yes"){
    fwrite(preg_excluded_prior, paste0(poi_dir, subpopulations_names[s], "/GDPR/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_preg_prior_removed.csv"), row.names = F)
    fwrite(preg_excluded_prior, paste0(poi_dir, subpopulations_names[s], "/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_preg_prior_removed.csv"), row.names = F)
    
    } else {
    fwrite(preg_excluded_prior, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_preg_prior_removed.csv"), row.names = F)
      fwrite(preg_excluded_prior, paste0(poi_dir,"Masked/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_preg_prior_removed.csv"), row.names = F)
      
        }
  
  rm(preg_excluded_prior)
  }
}

#####Export number of repeated diagnoses#####
if(subpopulations_present=="Yes"){
  fwrite(remove_subjects, paste0(poi_dir, subpopulations_names[s], "/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_secondary_rec_removed.csv"), row.names = F)
} else {
  fwrite(remove_subjects, paste0(poi_dir, format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_secondary_rec_removed.csv"), row.names = F)
}

if(subpopulations_present=="Yes"){
  fwrite(remove_subjects, paste0(poi_dir, subpopulations_names[s], "/GDPR/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_secondary_rec_removed.csv"), row.names = F)
  fwrite(remove_subjects, paste0(poi_dir, subpopulations_names[s], "/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_diag_secondary_rec_removed.csv"), row.names = F)
  
  } else {
  fwrite(remove_subjects, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_secondary_rec_removed.csv"), row.names = F)
    fwrite(remove_subjects, paste0(poi_dir,"Masked/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_diag_secondary_rec_removed.csv"), row.names = F)
    
    }

rm(remove_subjects)
}

cat(green(print("The script diagnoses primary is finished.")))
