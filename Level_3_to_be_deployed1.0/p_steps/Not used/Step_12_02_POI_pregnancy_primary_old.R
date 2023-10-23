#Author: Vjola Hoxhaj Drs.
#email: v.hoxhaj@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 05/12/2021

#Pregnancy and medicines use/vaccine exposure
#Unit of observation:Pregnancy record

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


####Time lags for pregnancy stages####
time_lags<-data.table(stage_of_pregnancy=c("start_of_pregnancy","ongoing_pregnancy","interruption_pregnancy","end_of_pregnancy"),
                      min_time_period=c(0,32*7,24*7,40*7), max_time_period=c(40*7, 32*7, 24*7,0))
#start of pregnancy: will keep only records up to 40 weeks after the pregnancy records date
#ongoing pregnancy: we will keep records 32 weeks after pregnancy records date, and records
#####Pregnancy primary analysis####
if(!is.null(pregnancy_files)){
  ####Start pregnancy analysis####
  for (preg_ind in 1:length(pregnancy_files)){
    #####Load pregnancy file####
    preg_file<-lapply(paste0(populations_dir,"PREGNANCY/", pregnancy_files[[preg_ind]]), readRDS)
    preg_file<-do.call(rbind,preg_file)
    preg_file<-as.data.table(preg_file)
    #check if there are two condition columns, remove one
    if(sum(names(preg_file)=="condition")==2){preg_file<-preg_file[,condition:=NULL]}
    #create time lags for different type of preganncy stages
    preg_file[,min_time_period:=time_lags[stage_of_pregnancy==preg_file[!duplicated(condition),condition],min_time_period]]
    preg_file[,max_time_period:=time_lags[stage_of_pregnancy==preg_file[!duplicated(condition),condition],max_time_period]]
    #create year end variable from year of follow up, will be used to check when a person can be counted in the numerator/denominator of prevalence use
    preg_file[,end_year:=year(end_follow_up_preg)]
    #remove unnecessary columns
    preg_file[,sex_at_instance_creation:=NULL][,filter:=NULL][,lag:=NULL]
    #rename some columns before merging
    setnames(preg_file, "meaning","meaning_pregnancy")
    setnames(preg_file, "year","year_pregnancy")
    
    #number of pregnancy rec by stage of pregnancy and year
    counts_preg_stage_y<-preg_file[,.N, by=c("condition", "year_pregnancy")]
    names(counts_preg_stage_y)<-c("stage_of_pregnancy", "year","no_records")
    
    #number of pregnancy rec by year
    counts_preg_y<-preg_file[,.N, by=c("year_pregnancy")]
    names(counts_preg_y)<-c("year","no_records")
    
    #create preg_id as a indicator
    preg_file[,preg_id:=seq(1,preg_file[,.N], by=1)]
    ####medicines analysis####
    if(!is.null(medicines_files)){
      #create list to save files
      preg_med_prior_dates<-list()
      preg_med_prevalence_total<-list()
      preg_medicines_total<-list()
      preg_medicines_sex<-list()
      preg_medicines_meaning<-list()
      
      index<-1
      print("Calculate counts of medicines use after a pregnancy record")
      for(medicine_ind in 1:length(medicines_files)){
        print(paste0("Calculating rates for preganncy and medicine use:", names(pregnancy_files)[preg_ind], "_", names(medicines_files)[medicine_ind]))
        med_file<-lapply(paste0(populations_dir,"MEDICINES/", medicines_files[[medicine_ind]]), readRDS)
        med_file<-rbindlist(med_file, fill = T)
        med_file<-as.data.table(med_file)
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
        med_file<-merge.data.table(preg_file,med_file, by=c("person_id", "birth_date"), allow.cartesian = T)
        #remove all records with no pregnancy
        med_file<-med_file[!is.na(pregnancy_code_date)]
        #difference between pregnancy date and medicine date
        med_file[,medicines_date:=as.IDate(medicines_date)][,pregnancy_code_date:=as.IDate(pregnancy_code_date)]
        med_file[,diff:=medicines_date-pregnancy_code_date]
        med_file[diff>max_time_period | diff<=as.numeric(-min_time_period), remove:=1]
        med_file[,diff:=NULL]
        #check if any records that are after the end of follow up
        med_file[medicines_date<start_follow_up_preg | medicines_date>end_follow_up_preg, remove:=2]
        
        #save number of excluded records
        removed_rec<-med_file[remove==1|remove==2,.N, by="year_medicines"]
        names(removed_rec)<-c("year", "excluded_records")
        if(removed_rec[,.N]>0){
        preg_med_prior_dates[[index]]<-data.table(stage_of_pregnancy=preg_file[!duplicated(condition), condition],  removed_rec)
        }
        rm(removed_rec)
        #remove records
        med_file<-med_file[is.na(remove)]
        med_file[,remove:=NULL]
        #create a pregnancy identifier by combining person id and preganncy code date
        med_file[,rowid:=rowid(person_id,pregnancy_code_date)]#only rowid==1 means its a new pregnancy record, other number means the same pregnancy was exposed to more than one prescription
        #remove unecessary columns
        med_file[,start_follow_up_preg:=NULL][,end_follow_up_preg:=NULL][,min_time_period:=NULL][,max_time_period:=NULL][,preg_id:=NULL][,end_year:=NULL]
        
        if(med_file[,.N]>0){
          #save the diag_preg file to diag_preg populations folder to be used in events_pregnancy_medicines and events_pregnancy_vaccines
          saveRDS(med_file, paste0(populations_dir,"PREGNANCY_MEDICINES/", names(pregnancy_files)[preg_ind], "_", names(medicines_files)[medicine_ind], ".rds"))
          
          #calculate prevalence of medicine use:number of people having a prescription/dispensing after a pregnancy record/total number of pregnancy records by stage of pregnancy and year
          preg_med_prev_y<-med_file[!is.na(year_medicines)][rowid==1, .N, by=c("condition", "year_medicines")]
          setnames(preg_med_prev_y,"year_medicines","year")
          setnames(preg_med_prev_y,"N","no_exposed_pregnancies")
          setnames(preg_med_prev_y,"condition","stage_of_pregnancy")
          if(preg_med_prev_y[,.N]>0){
          preg_med_prevalence_total[[index]]<-preg_med_prev_y
          }
          rm(preg_med_prev_y)
 
          #calculate counts of medicine use:number of prescriptions/dispensing after a pregnancy/total number of records by year
          preg_med_y<-med_file[!is.na(year_medicines)][, .N, by=c("year_medicines","medicinal_product_atc_code","condition")]
          setnames(preg_med_y,"year_medicines","year")
          setnames(preg_med_y,"medicinal_product_atc_code","atc_code_4")
          setnames(preg_med_y,"N","no_records")
          setnames(preg_med_y,"condition","stage_of_pregnancy")
          
          preg_med_y_t<-med_file[!is.na(year_medicines)][, .N, by=c("year_medicines","condition")]
          setnames(preg_med_y_t,"year_medicines","year")
          setnames(preg_med_y_t,"N","total_records")
          setnames(preg_med_y_t,"condition","stage_of_pregnancy")
          preg_med_y<-merge.data.table(preg_med_y,preg_med_y_t,all=T, by=c("stage_of_pregnancy","year"))
          if(preg_med_y[,.N]>0){
          preg_medicines_total[[index]]<-preg_med_y
          }
          rm(preg_med_y,preg_med_y_t)
          
          #calculate counts of medicine use:number of prescriptions/dispensing after a pregnancy/total number of records by year and meaning
          med_file[,meaning:=paste(meaning_pregnancy, meaning_medicines, sep=":")]
          preg_med_y_meaning<-med_file[!is.na(year_medicines)][, .N, by=c("condition","meaning","year_medicines","medicinal_product_atc_code")]
          setnames(preg_med_y_meaning,"year_medicines","year")
          setnames(preg_med_y_meaning,"medicinal_product_atc_code","atc_code_4")
          setnames(preg_med_y_meaning,"N","no_records")
          setnames(preg_med_y_meaning,"condition","stage_of_pregnancy")
          
          preg_med_y_meaning_t<-med_file[!is.na(year_medicines)][, .N, by=c("condition","meaning","year_medicines")]
          setnames(preg_med_y_meaning_t,"year_medicines","year")
          setnames(preg_med_y_meaning_t,"N","total_records")
          setnames(preg_med_y_meaning_t,"condition","stage_of_pregnancy")
          preg_med_y_meaning<-merge.data.table(preg_med_y_meaning,preg_med_y_meaning_t,all=T, by=c("stage_of_pregnancy","meaning", "year"))
          if(preg_medicines_meaning[,.N]>0){
          preg_medicines_meaning[[index]]<-preg_med_y_meaning
          }
          rm(preg_med_y_meaning,preg_med_y_meaning_t)
        }
        index<-index+1
        rm(med_file)
        gc()
      }
      
      ####combine results medicines####
      preg_med_prevalence_total<-rbindlist(preg_med_prevalence_total)
      if(preg_med_prevalence_total[,.N]>0){
        preg_med_prevalence_total<-preg_med_prevalence_total[,lapply(.SD, sum), .SDcols=c("no_exposed_pregnancies"), by=c("stage_of_pregnancy","year")]
      }else{preg_med_prevalence_total<-NULL}
      
      preg_medicines_total<-rbindlist(preg_medicines_total)
      if(preg_medicines_total[,.N]>0){
        preg_medicines_total<-preg_medicines_total[,lapply(.SD, sum), .SDcols=c("no_records","total_records"), by=c("stage_of_pregnancy","year","atc_code_4")]
      }else{preg_medicines_total<-NULL}
      
      preg_medicines_meaning<-rbindlist(preg_medicines_meaning)
      if(preg_medicines_meaning[,.N]>0){
        preg_medicines_meaning<-preg_medicines_meaning[,lapply(.SD, sum), .SDcols=c("no_records","total_records"), by=c("stage_of_pregnancy","meaning", "year","atc_code_4")]
      }else{preg_medicines_meaning<-NULL}
      
      preg_med_prior_dates<-rbindlist(preg_med_prior_dates)
      if(preg_med_prior_dates[,.N]>0){
        preg_med_prior_dates<-preg_med_prior_dates[,lapply(.SD, sum), .SDcols=c("excluded_records"), by=c("stage_of_pregnancy","year")]
      }else{preg_med_prior_dates<-NULL}
      
      #export medicines counts
      if(!is.null(preg_medicines_total)){saveRDS(preg_medicines_total,paste0(poi_tmp,names(diagnoses_files)[condition_ind], "_preg_med_y.rds"))}
      rm(preg_medicines_total)
      if(!is.null(preg_medicines_meaning)){saveRDS(preg_medicines_meaning,paste0(poi_tmp,names(diagnoses_files)[condition_ind], "_preg_med_ym.rds"))}
      rm(preg_medicines_meaning)
      
      #combine the prevalence data
      #total by stage of pregnancy prevalence
      if(!is.null(preg_med_prevalence_total)){
        preg_med_prevalence_total_s<-merge.data.table(preg_med_prevalence_total,counts_preg_stage_y, by=c("stage_of_pregnancy","year"), all=T)
        preg_med_prevalence_total<-merge.data.table(preg_med_prevalence_total,counts_preg_y, by=c("year"), all=T)
        saveRDS(diag_med_prevalence_total_s,paste0(poi_tmp,names(pregnancy_files)[preg_ind], "_prevalence_preg_med_s.rds"))
        saveRDS(diag_med_prevalence_total,paste0(poi_tmp,names(pregnancy_files)[preg_ind], "_prevalence_preg_med_y.rds"))
      }else{
        preg_med_prevalence_total_s<-data.table(stage_of_pregnancy=names(pregnancy_files)[preg_ind], year="N/A",no_exposed_pregnancies=0)
        preg_med_prevalence_total_s<-merge.data.table(diag_med_prevalence_total,counts_preg_stage_y, by=c("stage_of_pregnancy"), all=T)
        preg_med_prevalence_total<-data.table(counts_preg_y, no_exposed_pregnancies=0)
        saveRDS(diag_med_prevalence_total_s,paste0(poi_tmp,names(pregnancy_files)[preg_ind], "_prevalence_preg_med_s.rds"))
        saveRDS(diag_med_prevalence_total,paste0(poi_tmp,names(pregnancy_files)[preg_ind], "_prevalence_preg_med_y.rds"))
      }
      
      rm(preg_med_prevalence_total,preg_med_prevalence_total_s)

      #combine and export the excluded records
      if(!is.null(preg_med_prior_dates[,.N])){
        #remove empty cells
        preg_med_prior_dates<-preg_med_prior_dates[excluded_records!=0]
        saveRDS(preg_med_prior_dates,paste0(poi_tmp,names(pregnancy_files)[preg_ind], "_preg_med_prior_date_excluded.rds"))
      }
      rm(preg_med_prior_dates)
      
    }
  }
}