#Author: Vjola Hoxhaj Drs.
#email: v.hoxhaj@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 05/12/2021

#Pregnancy and medicines use/vaccine exposure
#Unit of observation:Pregnancy record
cat(green(print("The script pregnancy primary has started.")))
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

#Create filter to use for data filtering(both for medicines and vaccines)
filter_medicines<-data.table(stage_of_pregnancy=c("start_of_pregnancy","ongoing_pregnancy","interruption_pregnancy","end_of_pregnancy"),
                             year_included=c(1,2,2,3))
# 1 means the current year and the year after
# 2 means the prior year, current year and year after
# 3 means the year prior and the current year

#####Pregnancy primary analysis####
if(!is.null(pregnancy_files)){
  ####Start pregnancy analysis####
  for (preg_ind in 1:length(pregnancy_files)){
    #####Load pregnancy file####
    preg_file<-lapply(paste0(populations_dir,"PREGNANCY/", pregnancy_files[[preg_ind]]), readRDS)
    preg_file<-as.data.table(rbindlist(preg_file, fill=T))
    if(preg_file[,.N]>0){
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
      names(counts_preg_stage_y)<-c("stage_of_pregnancy", "year","no_pregnancies")
      
      #number of pregnancy rec by year
      counts_preg_y<-preg_file[,.N, by=c("year_pregnancy")]
      names(counts_preg_y)<-c("year","no_pregnancies")
      
      #summary of included subjects
      preg_sum_f<-as.data.table(preg_file[!duplicated(person_id),.N, by=c("condition","meaning_pregnancy", "year_pregnancy")])
      setnames(preg_sum_f,"N","no_women")
      preg_sum<-as.data.table(preg_file[,.N, by=c("condition","meaning_pregnancy", "year_pregnancy")])
      setnames(preg_sum,"N","no_pregnancies")
      preg_sum_f<-merge.data.table(preg_sum_f,preg_sum, by=c("condition","meaning_pregnancy", "year_pregnancy"))
      rm(preg_sum)
      #export to total counts 
      saveRDS(preg_sum_f,paste0(poi_tmp,preg_file[!duplicated(condition),condition],"_summary_preg_m.rds"))
      rm(preg_sum_f)
      
      #create preg_id as a indicator
      preg_file[,preg_id:=seq(1,preg_file[,.N], by=1)]
      
      #stage of pregnancy:
      type_of_preg<- filter_medicines[stage_of_pregnancy==preg_file[!duplicated(condition), condition],year_included]
      if(type_of_preg == 1){year_filter<-c(as.numeric(substr(names(pregnancy_files)[preg_ind],1,4)), as.numeric(substr(names(pregnancy_files)[preg_ind],1,4))+1)}
      if(type_of_preg == 2){year_filter<-c(as.numeric(substr(names(pregnancy_files)[preg_ind],1,4))-1, as.numeric(substr(names(pregnancy_files)[preg_ind],1,4)), as.numeric(substr(names(pregnancy_files)[preg_ind],1,4))+1)}
      if(type_of_preg == 3){year_filter<-c(as.numeric(substr(names(pregnancy_files)[preg_ind],1,4))-1, as.numeric(substr(names(pregnancy_files)[preg_ind],1,4)))}
      ####medicines analysis####
      if(!is.null(medicines_files)){
        #define medicines files to be searched based on the time lag
        med_files_to_search<-names(medicines_files)[substr(names(medicines_files),3,6) %in% year_filter]
        
        if(length(med_files_to_search)>0){
          list_med<-list()
          index<-1
          for(medicine_ind in 1:length(med_files_to_search)){
            print(paste0("Load files for:", names(pregnancy_files)[preg_ind], "_", med_files_to_search[medicine_ind]))
            med_file<-lapply(paste0(populations_dir,"MEDICINES/", medicines_files[names(medicines_files)==med_files_to_search[[medicine_ind]]][[1]]), readRDS)
            med_file<-as.data.table(rbindlist(med_file, fill = T))
            list_med[[index]]<-med_file
            rm(med_file)
            index<-index+1
          }
          med_file<-as.data.table(rbindlist(list_med,fill = T))
          rm(list_med)
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
          med_file<-merge.data.table(preg_file, med_file, by=c("person_id", "birth_date"), all =T, allow.cartesian = T)
          #remove all records with no pregnancy
          med_file<-med_file[!is.na(pregnancy_code_date)]
          #remove all records with no medications
          med_file<-med_file[!is.na(medicines_date)]
          if(med_file[,.N]>0){
            #difference between pregnancy date and medicine date
            med_file[,medicines_date:=as.IDate(medicines_date)][,pregnancy_code_date:=as.IDate(pregnancy_code_date)]
            med_file[,diff:=medicines_date-pregnancy_code_date]
            med_file[,remove:=0]
            med_file[diff>max_time_period | diff<as.numeric(-min_time_period), remove:=1]
            med_file[,diff:=NULL]
            #check if any records that are after the end of follow up
            med_file[medicines_date<start_follow_up_preg | medicines_date>end_follow_up_preg, remove:=2]
            
            #save number of excluded records
            removed_rec<-med_file[remove==1|remove==2,.N, by="year_medicines"]
            names(removed_rec)<-c("year", "excluded_records")
            #remove empty cells
            preg_med_prior_dates<-removed_rec[excluded_records!=0]
            
            if(preg_med_prior_dates[,.N]>0){
            preg_med_prior_dates<-data.table(stage_of_pregnancy=preg_file[!duplicated(condition), condition],  removed_rec)
            saveRDS(preg_med_prior_dates,paste0(poi_tmp,names(pregnancy_files)[preg_ind], "_preg_med_prior_date_excluded.rds"))
            }
            rm(removed_rec,preg_med_prior_dates)
            
            #remove records
            med_file<-med_file[remove==0]
            med_file[,remove:=NULL]
            if(med_file[,.N]>0){
              #create a pregnancy identifier by combining person id and preganncy code date
              med_file[,rowid:=rowid(person_id,pregnancy_code_date)]#only rowid==1 means its a new pregnancy record, other number means the same pregnancy was exposed to more than one prescription
              med_file[,rowid_med:=rowid(person_id,pregnancy_code_date, medicinal_product_atc_code)]#only rowid==1 means its a new pregnancy record, other number means the same pregnancy was exposed to more than one prescription
              
              #remove unecessary columns
              med_file[,start_follow_up_preg:=NULL][,end_follow_up_preg:=NULL][,min_time_period:=NULL][,max_time_period:=NULL][,preg_id:=NULL][,end_year:=NULL]
              
              
              #save the diag_preg file to diag_preg populations folder to be used in events_pregnancy_medicines and events_pregnancy_vaccines
              #saveRDS(med_file, paste0(populations_dir,"PREGNANCY_MEDICINES/", names(pregnancy_files)[preg_ind], "_", med_files_to_search[medicine_ind], ".rds"))
              
              #calculate prevalence of medicine use:number of preg rec having a prescription/dispensing by stage and year of pregnancy/total number of pregnancy records by stage of pregnancy and year
              preg_med_prev_stage_y<-med_file[rowid==1, .N, by=c("condition", "year_pregnancy")]
              setnames(preg_med_prev_stage_y,"year_pregnancy","year")
              setnames(preg_med_prev_stage_y,"N","no_exposed_pregnancies")
              setnames(preg_med_prev_stage_y,"condition","stage_of_pregnancy")
              preg_med_prevalence_total_s<-merge.data.table(preg_med_prev_stage_y,counts_preg_stage_y, by=c("stage_of_pregnancy","year"), all=T, allow.cartesian = T)
              saveRDS(preg_med_prevalence_total_s,paste0(poi_tmp,names(pregnancy_files)[preg_ind], "_prevalence_preg_med_s.rds"))
              rm(preg_med_prev_stage_y,preg_med_prevalence_total_s)
              
              #calculate prevalence of medicine use:number of preg rec having a prescription/dispensing by year of medication/total number of pregnancy records by year
              preg_med_prev_y<-med_file[rowid==1, .N, by=c("year_pregnancy")]
              setnames(preg_med_prev_y,"year_pregnancy","year")
              setnames(preg_med_prev_y,"N","no_exposed_pregnancies")
              preg_med_prevalence_total<-merge.data.table(preg_med_prev_y,counts_preg_y, by=c("year"), all=T, allow.cartesian = T)
              saveRDS(preg_med_prevalence_total,paste0(poi_tmp,names(pregnancy_files)[preg_ind], "_prevalence_preg_med_y.rds"))
              rm(preg_med_prev_y,preg_med_prevalence_total)
              
              #calculate prevalence of medicine use:number of preg rec having a prescription/dispensing by stage,atc and year of medication/total number of pregnancy records by stage of pregnancy and year
              preg_med_prev_stage_my<-med_file[rowid_med==1, .N, by=c("condition", "year_pregnancy", "medicinal_product_atc_code")]
              setnames(preg_med_prev_stage_my,"year_pregnancy","year")
              setnames(preg_med_prev_stage_my,"N","no_exposed_pregnancies")
              setnames(preg_med_prev_stage_my,"condition","stage_of_pregnancy")
              setnames(preg_med_prev_stage_my,"medicinal_product_atc_code","atc_code_4")
              preg_med_prevalence_total_atc<-merge.data.table(preg_med_prev_stage_my,counts_preg_stage_y, by=c("stage_of_pregnancy","year"), all=T, allow.cartesian = T)
              saveRDS(preg_med_prevalence_total_atc,paste0(poi_tmp,names(pregnancy_files)[preg_ind], "_prevalence_preg_med_atc.rds"))
              rm(preg_med_prev_stage_my,preg_med_prevalence_total_atc)
              
              #calculate counts of medicine use:number of prescriptions/dispensing after a pregnancy/total number of records by year
              preg_med_y<-med_file[, .N, by=c("year_medicines","medicinal_product_atc_code","condition")]
              setnames(preg_med_y,"year_medicines","year")
              setnames(preg_med_y,"medicinal_product_atc_code","atc_code_4")
              setnames(preg_med_y,"N","no_records")
              setnames(preg_med_y,"condition","stage_of_pregnancy")
              
              preg_med_y_t<-med_file[, .N, by=c("year_medicines","condition")]
              setnames(preg_med_y_t,"year_medicines","year")
              setnames(preg_med_y_t,"N","total_records")
              setnames(preg_med_y_t,"condition","stage_of_pregnancy")
              preg_medicines_total<-merge.data.table(preg_med_y,preg_med_y_t,all=T, by=c("stage_of_pregnancy","year"))
              
              saveRDS(preg_medicines_total,paste0(poi_tmp,names(pregnancy_files)[preg_ind], "_preg_med_y.rds"))
              rm(preg_medicines_total,preg_med_y,preg_med_y_t)

              
              #calculate counts of medicine use:number of prescriptions/dispensing after a pregnancy/total number of records by year and meaning
              med_file[,meaning:=paste(meaning_pregnancy, meaning_medicines, sep=":")]
              preg_med_y_meaning<-med_file[, .N, by=c("condition","meaning","year_medicines","medicinal_product_atc_code")]
              setnames(preg_med_y_meaning,"year_medicines","year")
              setnames(preg_med_y_meaning,"medicinal_product_atc_code","atc_code_4")
              setnames(preg_med_y_meaning,"N","no_records")
              setnames(preg_med_y_meaning,"condition","stage_of_pregnancy")
              
              preg_med_y_meaning_t<-med_file[, .N, by=c("condition","meaning","year_medicines")]
              setnames(preg_med_y_meaning_t,"year_medicines","year")
              setnames(preg_med_y_meaning_t,"N","total_records")
              setnames(preg_med_y_meaning_t,"condition","stage_of_pregnancy")
              preg_medicines_meaning<-merge.data.table(preg_med_y_meaning,preg_med_y_meaning_t,all=T, by=c("stage_of_pregnancy","meaning", "year"))
              saveRDS(preg_medicines_meaning,paste0(poi_tmp,names(pregnancy_files)[preg_ind], "_preg_med_ym.rds"))
              rm(preg_medicines_meaning,preg_med_y_meaning,preg_med_y_meaning_t)
              
            }
          }
        }
      }
      ####vaccines analysis####
      if(!is.null(vaccines_files)){
        #define vaccines files to be searched based on the time lag
        vacc_files_to_search<-names(vaccines_files)[substr(names(vaccines_files),3,6) %in% year_filter]
        
        if(length(vacc_files_to_search)>0){
          list_vacc<-list()
          index<-1
          for(vaccine_ind in 1:length(vacc_files_to_search)){
            print(paste0("Load files for:", names(pregnancy_files)[preg_ind], "_", vacc_files_to_search[vaccine_ind]))
            vacc_file<-lapply(paste0(populations_dir,"VACCINES/", vaccines_files[names(vaccines_files)==vacc_files_to_search[[vaccine_ind]]][[1]]), readRDS)
            vacc_file<-as.data.table(rbindlist(vacc_file, fill = T))
            list_vacc[[index]]<-vacc_file
            rm(vacc_file)
            index<-index+1
          }
          vacc_file<-as.data.table(rbindlist(list_vacc,fill = T))
          rm(list_vacc)
          #remove duplicated vaccine records
          vacc_file[,dup_ind:=rowid(person_id,vaccines_date)]
          duplicated_vacc_rec<-data.table(stage_of_pregnancy=preg_file[!duplicated(condition),condition],vaccine_group=names(vaccines_files)[vaccine_ind], duplicated_rec=vacc_file[dup_ind>1,.N])
          duplicated_vacc_rec<-duplicated_vacc_rec[duplicated_rec!=0]
          if(duplicated_vacc_rec[,.N]>0){
            saveRDS(duplicated_vacc_rec,paste0(poi_tmp,names(pregnancy_files)[preg_ind], "_preg_vacc_duplicated_rec_excluded.rds"))
          }
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
          if(var_to_keep=="vaccine_indicator"){
          vacc_file<-vacc_file[,vaccine_indicator:=substr(vaccine_indicator,1,4)]
          }
          #merge with the diagosis file by keeping all records
          vacc_file<-merge.data.table(preg_file, vacc_file, by=c("person_id", "birth_date"), all =T, allow.cartesian = T)
          #remove all records with no pregnancy
          vacc_file<-vacc_file[!is.na(pregnancy_code_date)]
          #remove all records with no medications
          vacc_file<-vacc_file[!is.na(vaccines_date)]
          if(vacc_file[,.N]>0){
            #difference between pregnancy date and vaccine date
            vacc_file[,vaccines_date:=as.IDate(vaccines_date)][,pregnancy_code_date:=as.IDate(pregnancy_code_date)]
            vacc_file[,diff:=vaccines_date-pregnancy_code_date]
            vacc_file[,remove:=0]
            vacc_file[diff>max_time_period | diff<as.numeric(-min_time_period), remove:=1]
            vacc_file[,diff:=NULL]
            #check if any records that are after the end of follow up
            vacc_file[vaccines_date<start_follow_up_preg | vaccines_date>end_follow_up_preg, remove:=2]
            
            #save number of excluded records
            removed_rec<-vacc_file[remove==1|remove==2,.N, by="year_vaccines"]
            names(removed_rec)<-c("year", "excluded_records")
            #remove empty cells
            preg_vacc_prior_dates<-removed_rec[excluded_records!=0]
            
            if(preg_vacc_prior_dates[,.N]>0){
              preg_vacc_prior_dates<-data.table(stage_of_pregnancy=preg_file[!duplicated(condition), condition],  removed_rec)
              saveRDS(preg_vacc_prior_dates,paste0(poi_tmp,names(pregnancy_files)[preg_ind], "_preg_vacc_prior_date_excluded.rds"))
            }
            rm(removed_rec,preg_vacc_prior_dates)
            
            #remove records
            vacc_file<-vacc_file[remove==0]
            vacc_file[,remove:=NULL]
            if(vacc_file[,.N]>0){
              #create a pregnancy identifier by combining person id and preganncy code date
              vacc_file[,rowid:=rowid(person_id,pregnancy_code_date)]#only rowid==1 means its a new pregnancy record, other number means the same pregnancy was exposed to more than one prescription
              vacc_file[,rowid_vacc:=rowid(person_id,pregnancy_code_date, vaccine_indicator)]#only rowid==1 means its a new pregnancy record, other number means the same pregnancy was exposed to more than one prescription
              
              #remove unecessary columns
              vacc_file[,start_follow_up_preg:=NULL][,end_follow_up_preg:=NULL][,min_time_period:=NULL][,max_time_period:=NULL][,preg_id:=NULL][,end_year:=NULL]
              
              
              #save the diag_preg file to diag_preg populations folder to be used in events_pregnancy_vaccines and events_pregnancy_vaccines
              #saveRDS(vacc_file, paste0(populations_dir,"PREGNANCY_vaccines/", names(pregnancy_files)[preg_ind], "_", vacc_files_to_search[vaccine_ind], ".rds"))
              
              #calculate prevalence of vaccine use:number of preg rec having a prescription/dispensing by stage and year of pregnancy/total number of pregnancy records by stage of pregnancy and year
              preg_vacc_prev_stage_y<-vacc_file[rowid==1, .N, by=c("condition", "year_pregnancy")]
              setnames(preg_vacc_prev_stage_y,"year_pregnancy","year")
              setnames(preg_vacc_prev_stage_y,"N","no_exposed_pregnancies")
              setnames(preg_vacc_prev_stage_y,"condition","stage_of_pregnancy")
              preg_vacc_prevalence_total_s<-merge.data.table(preg_vacc_prev_stage_y,counts_preg_stage_y, by=c("stage_of_pregnancy","year"), all=T, allow.cartesian = T)
              saveRDS(preg_vacc_prevalence_total_s,paste0(poi_tmp,names(pregnancy_files)[preg_ind], "_prevalence_preg_vacc_s.rds"))
              rm(preg_vacc_prev_stage_y,preg_vacc_prevalence_total_s)
              
              #calculate prevalence of vaccine use:number of preg rec having a prescription/dispensing by year of medication/total number of pregnancy records by year
              preg_vacc_prev_y<-vacc_file[rowid==1, .N, by=c("year_pregnancy")]
              setnames(preg_vacc_prev_y,"year_pregnancy","year")
              setnames(preg_vacc_prev_y,"N","no_exposed_pregnancies")
              preg_vacc_prevalence_total<-merge.data.table(preg_vacc_prev_y,counts_preg_y, by=c("year"), all=T, allow.cartesian = T)
              saveRDS(preg_vacc_prevalence_total,paste0(poi_tmp,names(pregnancy_files)[preg_ind], "_prevalence_preg_vacc_y.rds"))
              rm(preg_vacc_prev_y,preg_vacc_prevalence_total)
              
              #calculate prevalence of vaccine use:number of preg rec having a prescription/dispensing by stage,atc and year of medication/total number of pregnancy records by stage of pregnancy and year
              preg_vacc_prev_stage_my<-vacc_file[rowid_vacc==1, .N, by=c("condition", "year_pregnancy", "vaccine_indicator")]
              setnames(preg_vacc_prev_stage_my,"year_pregnancy","year")
              setnames(preg_vacc_prev_stage_my,"N","no_exposed_pregnancies")
              setnames(preg_vacc_prev_stage_my,"condition","stage_of_pregnancy")
              setnames(preg_vacc_prev_stage_my,"vaccine_indicator","atc_code_4")
              preg_vacc_prevalence_total_atc<-merge.data.table(preg_vacc_prev_stage_my,counts_preg_stage_y, by=c("stage_of_pregnancy","year"), all=T, allow.cartesian = T)
              saveRDS(preg_vacc_prevalence_total_atc,paste0(poi_tmp,names(pregnancy_files)[preg_ind], "_prevalence_preg_vacc_atc.rds"))
              rm(preg_vacc_prev_stage_my,preg_vacc_prevalence_total_atc)
              
              #calculate counts of vaccine use:number of prescriptions/dispensing after a pregnancy/total number of records by year
              preg_vacc_y<-vacc_file[, .N, by=c("year_vaccines","vaccine_indicator","condition")]
              setnames(preg_vacc_y,"year_vaccines","year")
              setnames(preg_vacc_y,"vaccine_indicator","atc_code_4")
              setnames(preg_vacc_y,"N","no_records")
              setnames(preg_vacc_y,"condition","stage_of_pregnancy")
              
              preg_vacc_y_t<-vacc_file[, .N, by=c("year_vaccines","condition")]
              setnames(preg_vacc_y_t,"year_vaccines","year")
              setnames(preg_vacc_y_t,"N","total_records")
              setnames(preg_vacc_y_t,"condition","stage_of_pregnancy")
              preg_vaccines_total<-merge.data.table(preg_vacc_y,preg_vacc_y_t,all=T, by=c("stage_of_pregnancy","year"))
              
              saveRDS(preg_vaccines_total,paste0(poi_tmp,names(pregnancy_files)[preg_ind], "_preg_vacc_y.rds"))
              rm(preg_vaccines_total,preg_vacc_y,preg_vacc_y_t)
              
              
              #calculate counts of vaccine use:number of prescriptions/dispensing after a pregnancy/total number of records by year and meaning
              vacc_file[,meaning:=paste(meaning_pregnancy, meaning_vaccines, sep=":")]
              preg_vacc_y_meaning<-vacc_file[, .N, by=c("condition","meaning","year_vaccines","vaccine_indicator")]
              setnames(preg_vacc_y_meaning,"year_vaccines","year")
              setnames(preg_vacc_y_meaning,"vaccine_indicator","atc_code_4")
              setnames(preg_vacc_y_meaning,"N","no_records")
              setnames(preg_vacc_y_meaning,"condition","stage_of_pregnancy")
              
              preg_vacc_y_meaning_t<-vacc_file[, .N, by=c("condition","meaning","year_vaccines")]
              setnames(preg_vacc_y_meaning_t,"year_vaccines","year")
              setnames(preg_vacc_y_meaning_t,"N","total_records")
              setnames(preg_vacc_y_meaning_t,"condition","stage_of_pregnancy")
              preg_vaccines_meaning<-merge.data.table(preg_vacc_y_meaning,preg_vacc_y_meaning_t,all=T, by=c("stage_of_pregnancy","meaning", "year"))
              saveRDS(preg_vaccines_meaning,paste0(poi_tmp,names(pregnancy_files)[preg_ind], "_preg_vacc_ym.rds"))
              rm(preg_vaccines_meaning,preg_vacc_y_meaning,preg_vacc_y_meaning_t)
              
            }
          }
        }
      }
    }
  }
  
  #####Combine results for summary PREGNANCY#####
  summary_preg_files<-list.files(poi_tmp,"summary_preg_m.rds")
  if(length(summary_preg_files>0)){
    summary_preg<-lapply(paste0(poi_tmp,summary_preg_files), readRDS)
    summary_preg<-as.data.table(rbindlist(summary_preg,fill = T))
    #apply sum to make sure all categories are combined
    summary_preg<-summary_preg[,lapply(.SD, sum), by=c("condition","meaning_pregnancy","year_pregnancy"), .SDcols=c("no_women","no_pregnancies")]
    
    #remove all files in poi tmp
    for(i in 1:length(summary_preg_files)){
      file.remove(paste0(poi_tmp, summary_preg_files[i]))
    }
    rm(summary_preg_files)
    
    setnames(summary_preg, "condition", "stage_of_pregnancy")
    
    #save results to g_output/POI
    if(subpopulations_present=="Yes"){
      fwrite(summary_preg, paste0(poi_dir, subpopulations_names[s],format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_summary_m.csv"), row.names = F)
    } else {
      fwrite(summary_preg, paste0(poi_dir, format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_summary_m.csv"), row.names = F)
    }
    
    summary_preg[, no_women:= as.character(no_women)][as.numeric(no_women) > 0 & as.numeric(no_women) < 5, no_women := "<5"]
    summary_preg[, no_pregnancies:= as.character(no_pregnancies)][as.numeric(no_pregnancies) > 0 & as.numeric(no_pregnancies) < 5, no_pregnancies := "<5"]
    
    if(subpopulations_present=="Yes"){
      fwrite(summary_preg, paste0(poi_dir, subpopulations_names[s],"Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_summary_m_masked.csv"), row.names = F)
      fwrite(summary_preg, paste0(poi_dir, subpopulations_names[s],"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_summary_m_masked.csv"), row.names = F)
    } else {
      fwrite(summary_preg, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_summary_m_masked.csv"), row.names = F)
      fwrite(summary_preg, paste0(poi_dir,"Masked/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_summary_m_masked.csv"), row.names = F)
      
      }
    
    rm(summary_preg)
  }
  
  #####Combine results for medicines use after a pregnancy record#####
  medicines_files<-length(list.files(poi_tmp,"preg_med_y|preg_med_ym|prevalence_preg_med_s|prevalence_preg_med_y|prevalence_preg_med_atc|preg_med_prior_date_excluded"))
  
  if(medicines_files>0){
    ####Prevalence of exposed pregnancies by year of pregnancy####
    if(length(list.files(poi_tmp,"prevalence_preg_med_y.rds"))>0){ 
      prevalence_preg_med_y_fl<-list.files(poi_tmp, "prevalence_preg_med_y.rds")#prevalence of medicines records by year
      prevalence_preg_med_y<-lapply(paste0(poi_tmp,prevalence_preg_med_y_fl), readRDS)
      prevalence_preg_med_y<-as.data.table(rbindlist(prevalence_preg_med_y,fill = T))
      #make sure there are no zeros in no_subjects
      prevalence_preg_med_y<-prevalence_preg_med_y[!is.na(no_pregnancies)]
      #replace NA in no_users with 0
      prevalence_preg_med_y[is.na(no_exposed_pregnancies), no_exposed_pregnancies:=0]
      #apply sum to make sure all categories are combined
      prevalence_preg_med_y<-prevalence_preg_med_y[,lapply(.SD, sum), by=c("year"), .SDcols=c("no_exposed_pregnancies","no_pregnancies")]
      #separate atc_code=="N/A"
      # no_users_present<-prevalence_preg_med_y[atc_code_4=="N/A"]
      # if(no_users_present[,.N]>0){
      #   no_users_present[,atc_code_4:=NULL][,no_exposed_pregnancies:=NULL]
      #   no_users_present[ , new := do.call(paste, c(.SD, sep = "_"))]
      #   atc_codes_to_add<-prevalence_preg_med_y[!duplicated(atc_code_4),atc_code_4]
      #   atc_codes_to_add<-atc_codes_to_add[!atc_codes_to_add %in% "N/A"]
      #   if(length(atc_codes_to_add)==0){
      #     a<-NULL
      #   }else{
      #     a<-as.data.table(expand.grid(no_users_present[,new],atc_codes_to_add))
      #     names(a)<-c("combine", "atc_code_4")
      #     a[,c("year","no_pregnancies"):=tstrsplit(combine,"_", fixed=T)]
      #     a[,combine:=NULL]
      #     a[,no_exposed_pregnancies:=0]
      #   }
      #   prevalence_preg_med_y<-prevalence_preg_med_y[!atc_code_4 %in% "N/A"]
      #   prevalence_preg_med_y<-as.data.table(rbind(prevalence_preg_med_y,a))
      #   rm(no_users_present,a)
      # }else{rm(no_users_present)}
      prevalence_preg_med_y[,no_exposed_pregnancies:=as.numeric(no_exposed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
      
      #calculate prevalence
      prevalence_preg_med_y[,prevalence_medicine_use:=round((no_exposed_pregnancies/no_pregnancies)*100,1)]
      prevalence_preg_med_y[, data_access_provider:=data_access_provider_name]
      prevalence_preg_med_y[, data_source:=data_source_name]
      
      #remove all files in poi tmp
      for(i in 1:length(prevalence_preg_med_y_fl)){
        file.remove(paste0(poi_tmp, prevalence_preg_med_y_fl[i]))
      }
      rm(prevalence_preg_med_y_fl)
      #save results to g_output/PREGNANCY_MEDICINES
      if(subpopulations_present=="Yes"){
        fwrite(prevalence_preg_med_y, paste0(poi_dir, subpopulations_names[s], "/PREGNANCY_MEDICINES/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_med_prevalence_y.csv"), row.names = F)
      } else {
        fwrite(prevalence_preg_med_y, paste0(poi_dir,"PREGNANCY_MEDICINES/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_med_prevalence_y.csv"), row.names = F)
      }
      
      prevalence_preg_med_y[,no_exposed_pregnancies:=as.numeric(no_exposed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
      prevalence_preg_med_y[records_categories, u_range := i.range[no_exposed_pregnancies >= min & no_exposed_pregnancies <= max], on = .(no_exposed_pregnancies >= min, no_exposed_pregnancies <= max)]
      prevalence_preg_med_y[records_categories, s_range := i.range[no_pregnancies >= min & no_pregnancies <= max], on = .(no_pregnancies >= min, no_pregnancies <= max)]
      
      gdpr_file<-prevalence_preg_med_y[,-c("no_exposed_pregnancies","no_pregnancies"),with=F]
      setnames(gdpr_file,"u_range","no_exposed_pregnancies")
      setnames(gdpr_file,"s_range","no_pregnancies")
      setcolorder(gdpr_file,c("year","no_exposed_pregnancies", "no_pregnancies","prevalence_medicine_use"))
      
      if(subpopulations_present=="Yes"){
        fwrite(gdpr_file, paste0(poi_dir,subpopulations_names[s], "/GDPR/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_med_prevalence_y_masked.csv"), row.names = F)
      } else {
        fwrite(gdpr_file, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_med_prevalence_y_masked.csv"), row.names = F)
      }
      rm(gdpr_file)
      
      prevalence_preg_med_y[,u_range:=NULL][,s_range:=NULL]
      #apply masking
      prevalence_preg_med_y[, no_exposed_pregnancies:= as.character(no_exposed_pregnancies)][as.numeric(no_exposed_pregnancies) > 0 & as.numeric(no_exposed_pregnancies) < 5, no_exposed_pregnancies := "<5"]
      prevalence_preg_med_y[, no_pregnancies:= as.character(no_pregnancies)][as.numeric(no_pregnancies) > 0 & as.numeric(no_pregnancies) < 5, no_pregnancies := "<5"]
      prevalence_preg_med_y[, prevalence_medicine_use:= as.character(prevalence_medicine_use)][no_exposed_pregnancies=="<5" | no_pregnancies=="<5", prevalence_medicine_use := "N/A"]
      
      if(subpopulations_present=="Yes"){
        fwrite(prevalence_preg_med_y, paste0(poi_dir,subpopulations_names[s], "/PREGNANCY_MEDICINES/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_preg_med_prevalence_y_masked.csv"), row.names = F)
        fwrite(prevalence_preg_med_y, paste0(poi_dir,subpopulations_names[s], "/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_preg_med_prevalence_y_masked.csv"), row.names = F)
        
              } else {
        fwrite(prevalence_preg_med_y, paste0(poi_dir, "PREGNANCY_MEDICINES/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_med_prevalence_y_masked.csv"), row.names = F)
                fwrite(prevalence_preg_med_y, paste0(poi_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_med_prevalence_y_masked.csv"), row.names = F)
                
                      }
      
      rm(prevalence_preg_med_y)
    }
    
    ####Prevalence of exposed pregnancies by atc code and year of pregnancy####
    if(length(list.files(poi_tmp,"prevalence_preg_med_atc.rds"))>0){ 
      prevalence_preg_med_atc_fl<-list.files(poi_tmp, "prevalence_preg_med_atc.rds")#prevalence of medicines records by year
      prevalence_preg_med_atc<-lapply(paste0(poi_tmp,prevalence_preg_med_atc_fl), readRDS)
      prevalence_preg_med_atc<-as.data.table(rbindlist(prevalence_preg_med_atc,fill = T))
      #make sure there are no zeros in no_subjects
      prevalence_preg_med_atc<-prevalence_preg_med_atc[!is.na(no_pregnancies)]
      #replace NA in atc_code_4 with N/A
      prevalence_preg_med_atc[is.na(atc_code_4), atc_code_4:="N/A"]
      #replace NA in no_users with 0
      prevalence_preg_med_atc[is.na(no_exposed_pregnancies), no_exposed_pregnancies:=0]
      #apply sum to make sure all categories are combined
      prevalence_preg_med_atc<-prevalence_preg_med_atc[,lapply(.SD, sum), by=c("atc_code_4","year"), .SDcols=c("no_exposed_pregnancies","no_pregnancies")]
      #separate atc_code=="N/A"
      # no_users_present<-prevalence_preg_med_atc[atc_code_4=="N/A"]
      # if(no_users_present[,.N]>0){
      #   no_users_present[,atc_code_4:=NULL][,no_exposed_pregnancies:=NULL]
      #   no_users_present[ , new := do.call(paste, c(.SD, sep = "_"))]
      #   atc_codes_to_add<-prevalence_preg_med_atc[!duplicated(atc_code_4),atc_code_4]
      #   atc_codes_to_add<-atc_codes_to_add[!atc_codes_to_add %in% "N/A"]
      #   if(length(atc_codes_to_add)==0){
      #     a<-NULL
      #   }else{
      #     a<-as.data.table(expand.grid(no_users_present[,new],atc_codes_to_add))
      #     names(a)<-c("combine", "atc_code_4")
      #     a[,c("year","no_pregnancies"):=tstrsplit(combine,"_", fixed=T)]
      #     a[,combine:=NULL]
      #     a[,no_exposed_pregnancies:=0]
      #   }
      #   prevalence_preg_med_atc<-prevalence_preg_med_atc[!atc_code_4 %in% "N/A"]
      #   prevalence_preg_med_atc<-as.data.table(rbind(prevalence_preg_med_atc,a))
      #   rm(no_users_present,a)
      # }else{rm(no_users_present)}
      prevalence_preg_med_atc[,no_exposed_pregnancies:=as.numeric(no_exposed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
      
      #calculate prevalence
      prevalence_preg_med_atc[,prevalence_medicine_use:=round((no_exposed_pregnancies/no_pregnancies)*100,1)]
      prevalence_preg_med_atc[, data_access_provider:=data_access_provider_name]
      prevalence_preg_med_atc[, data_source:=data_source_name]
      
      #remove all files in poi tmp
      for(i in 1:length(prevalence_preg_med_atc_fl)){
        file.remove(paste0(poi_tmp, prevalence_preg_med_atc_fl[i]))
      }
      rm(prevalence_preg_med_atc_fl)
      #save results to g_output/PREGNANCY_MEDICINES
      if(subpopulations_present=="Yes"){
        fwrite(prevalence_preg_med_atc, paste0(poi_dir, subpopulations_names[s], "/PREGNANCY_MEDICINES/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_med_prevalence_atc.csv"), row.names = F)
      } else {
        fwrite(prevalence_preg_med_atc, paste0(poi_dir,"PREGNANCY_MEDICINES/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_med_prevalence_atc.csv"), row.names = F)
      }
      
      prevalence_preg_med_atc[,no_exposed_pregnancies:=as.numeric(no_exposed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
      prevalence_preg_med_atc[records_categories, u_range := i.range[no_exposed_pregnancies >= min & no_exposed_pregnancies <= max], on = .(no_exposed_pregnancies >= min, no_exposed_pregnancies <= max)]
      prevalence_preg_med_atc[records_categories, s_range := i.range[no_pregnancies >= min & no_pregnancies <= max], on = .(no_pregnancies >= min, no_pregnancies <= max)]
      
      gdpr_file<-prevalence_preg_med_atc[,-c("no_exposed_pregnancies","no_pregnancies"),with=F]
      setnames(gdpr_file,"u_range","no_exposed_pregnancies")
      setnames(gdpr_file,"s_range","no_pregnancies")
      setcolorder(gdpr_file,c("atc_code_4", "year","no_exposed_pregnancies", "no_pregnancies","prevalence_medicine_use"))

          if(subpopulations_present=="Yes"){
        fwrite(gdpr_file, paste0(poi_dir,subpopulations_names[s], "/GDPR/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_med_prevalence_atc_masked.csv"), row.names = F)
      } else {
        fwrite(gdpr_file, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_med_prevalence_atc_masked.csv"), row.names = F)
      }
      rm(gdpr_file)
      
      prevalence_preg_med_atc[,u_range:=NULL][,s_range:=NULL]
      #apply masking
      prevalence_preg_med_atc[, no_exposed_pregnancies:= as.character(no_exposed_pregnancies)][as.numeric(no_exposed_pregnancies) > 0 & as.numeric(no_exposed_pregnancies) < 5, no_exposed_pregnancies := "<5"]
      prevalence_preg_med_atc[, no_pregnancies:= as.character(no_pregnancies)][as.numeric(no_pregnancies) > 0 & as.numeric(no_pregnancies) < 5, no_pregnancies := "<5"]
      prevalence_preg_med_atc[, prevalence_medicine_use:= as.character(prevalence_medicine_use)][no_exposed_pregnancies=="<5" | no_pregnancies=="<5", prevalence_medicine_use := "N/A"]
      
      if(subpopulations_present=="Yes"){
        fwrite(prevalence_preg_med_atc, paste0(poi_dir,subpopulations_names[s], "/PREGNANCY_MEDICINES/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_preg_med_prevalence_atc_masked.csv"), row.names = F)
        fwrite(prevalence_preg_med_atc, paste0(poi_dir,subpopulations_names[s], "/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_preg_med_prevalence_atc_masked.csv"), row.names = F)
        
              } else {
        fwrite(prevalence_preg_med_atc, paste0(poi_dir, "PREGNANCY_MEDICINES/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_med_prevalence_atc_masked.csv"), row.names = F)
                fwrite(prevalence_preg_med_atc, paste0(poi_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_med_prevalence_atc_masked.csv"), row.names = F)
                
                      }
      
      rm(prevalence_preg_med_atc)
    }
    
    ####Prevalence of exposed pregnancies by stage of pregnancy,atc code_4 and year of pregnancy####
    if(length(list.files(poi_tmp,"prevalence_preg_med_s.rds"))>0){ 
      prevalence_preg_med_s_fl<-list.files(poi_tmp, "prevalence_preg_med_s.rds")#prevalence of medicines records by year and sex
      prevalence_preg_med_s<-lapply(paste0(poi_tmp,prevalence_preg_med_s_fl), readRDS)
      prevalence_preg_med_s<-as.data.table(rbindlist(prevalence_preg_med_s,fill = T))
      #make sure there are no zeros in no_subjects
      prevalence_preg_med_s<-prevalence_preg_med_s[!is.na(no_pregnancies)]
      #replace NA in no_users with 0
      prevalence_preg_med_s[is.na(no_exposed_pregnancies), no_exposed_pregnancies:=0]
      #apply sum to make sure all categories are combined
      prevalence_preg_med_s<-prevalence_preg_med_s[,lapply(.SD, sum), by=c("stage_of_pregnancy", "year"), .SDcols=c("no_exposed_pregnancies","no_pregnancies")]
      
      # #separate atc_code=="N/A"
      # no_users_present<-prevalence_preg_med_s[atc_code_4=="N/A"]
      # if(no_users_present[,.N]>0){
      #   no_users_present[,atc_code_4:=NULL][,no_exposed_pregnancies:=NULL]
      #   no_users_present[ , new := do.call(paste, c(.SD, sep = "_"))]
      #   events<-no_users_present[!duplicated(stage_of_pregnancy),stage_of_pregnancy]
      #   list<-vector(mode="list", length=length(events))
      #   for(ls in 1:length(list)){
      #     atc_codes_to_add<-prevalence_preg_med_s[stage_of_pregnancy==events[ls]][!duplicated(atc_code_4),atc_code_4]
      #     atc_codes_to_add<-atc_codes_to_add[!atc_codes_to_add %in% "N/A"]
      #     if(length(atc_codes_to_add)==0){
      #       a<-NULL
      #     }else{
      #       a<-as.data.table(expand.grid(no_users_present[stage_of_pregnancy==events[ls],new],atc_codes_to_add))
      #       names(a)<-c("combine", "atc_code_4")
      #       a[,c("stage_of_pregnancy","year","no_pregnancies"):=tstrsplit(combine,"_", fixed=T)]
      #       a[,combine:=NULL]
      #     }
      #     list[[ls]]<-a
      #     rm(a)
      #   }
      #   list<-rbindlist(list)
      #   list[,no_exposed_pregnancies:=0]
      #   prevalence_preg_med_s<-prevalence_preg_med_s[!atc_code_4 %in% "N/A"]
      #   prevalence_preg_med_s<-as.data.table(rbind(prevalence_preg_med_s,list))
      #   rm(list,no_users_present)
      # }else{rm(no_users_present)}
      prevalence_preg_med_s[,no_exposed_pregnancies:=as.numeric(no_exposed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
      
      #calculate prevalence
      prevalence_preg_med_s[,prevalence_medicine_use:=round((no_exposed_pregnancies/no_pregnancies)*100,1)]
      prevalence_preg_med_s[, data_access_provider:=data_access_provider_name]
      prevalence_preg_med_s[, data_source:=data_source_name]
      
    
      #remove all files in poi tmp
      for(i in 1:length(prevalence_preg_med_s_fl)){
        file.remove(paste0(poi_tmp, prevalence_preg_med_s_fl[i]))
      }
      rm(prevalence_preg_med_s_fl)
      #save results to g_output/PREGNANCY_MEDICINES
      if(subpopulations_present=="Yes"){
        fwrite(prevalence_preg_med_s, paste0(poi_dir, subpopulations_names[s], "/PREGNANCY_MEDICINES/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_med_prevalence_s.csv"), row.names = F)
      } else {
        fwrite(prevalence_preg_med_s, paste0(poi_dir,"PREGNANCY_MEDICINES/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_med_prevalence_s.csv"), row.names = F)
      }
      
      prevalence_preg_med_s[,no_exposed_pregnancies:=as.numeric(no_exposed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
      prevalence_preg_med_s[records_categories, u_range := i.range[no_exposed_pregnancies >= min & no_exposed_pregnancies <= max], on = .(no_exposed_pregnancies >= min, no_exposed_pregnancies <= max)]
      prevalence_preg_med_s[records_categories, s_range := i.range[no_pregnancies >= min & no_pregnancies <= max], on = .(no_pregnancies >= min, no_pregnancies <= max)]
      
      gdpr_file<-prevalence_preg_med_s[,-c("no_exposed_pregnancies","no_pregnancies"),with=F]
      setnames(gdpr_file,"u_range","no_exposed_pregnancies")
      setnames(gdpr_file,"s_range","no_pregnancies")
      setcolorder(gdpr_file,c("stage_of_pregnancy", "year","no_exposed_pregnancies", "no_pregnancies","prevalence_medicine_use"))
      
      if(subpopulations_present=="Yes"){
        fwrite(gdpr_file, paste0(poi_dir,subpopulations_names[s], "/GDPR/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_med_prevalence_s_masked.csv"), row.names = F)
      } else {
        fwrite(gdpr_file, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_med_prevalence_s_masked.csv"), row.names = F)
      }
      rm(gdpr_file)
      
      prevalence_preg_med_s[,u_range:=NULL][,s_range:=NULL]
      #apply masking
      prevalence_preg_med_s[, no_exposed_pregnancies:= as.character(no_exposed_pregnancies)][as.numeric(no_exposed_pregnancies) > 0 & as.numeric(no_exposed_pregnancies) < 5, no_exposed_pregnancies := "<5"]
      prevalence_preg_med_s[, no_pregnancies:= as.character(no_pregnancies)][as.numeric(no_pregnancies) > 0 & as.numeric(no_pregnancies) < 5, no_pregnancies := "<5"]
      prevalence_preg_med_s[, prevalence_medicine_use:= as.character(prevalence_medicine_use)][no_exposed_pregnancies=="<5" | no_pregnancies=="<5", prevalence_medicine_use := "N/A"]
      
      if(subpopulations_present=="Yes"){
        fwrite(prevalence_preg_med_s, paste0(poi_dir,subpopulations_names[s], "/PREGNANCY_MEDICINES/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_preg_med_prevalence_s_masked.csv"), row.names = F)
        fwrite(prevalence_preg_med_s, paste0(poi_dir,subpopulations_names[s], "/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_preg_med_prevalence_s_masked.csv"), row.names = F)
        
             } else {
        fwrite(prevalence_preg_med_s, paste0(poi_dir, "PREGNANCY_MEDICINES/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_med_prevalence_s_masked.csv"), row.names = F)
               fwrite(prevalence_preg_med_s, paste0(poi_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_med_prevalence_s_masked.csv"), row.names = F)
               
                     }
      
      rm(prevalence_preg_med_s)
    }
    
    ####Counts of medicine records by year####
    if(length(list.files(poi_tmp,"preg_med_y.rds"))>0){ 
      preg_med_y_fl<-list.files(poi_tmp, "preg_med_y.rds")#counts of medicines records by year and sex
      preg_med_y<-lapply(paste0(poi_tmp,preg_med_y_fl), readRDS)
      preg_med_y<-as.data.table(rbindlist(preg_med_y,fill = T))
      #make sure there are no zeros in no_subjects
      preg_med_y<-preg_med_y[!is.na(total_records)]
      #apply sum to make sure all categories are combined
      preg_med_y<-preg_med_y[,lapply(.SD, sum), by=c("stage_of_pregnancy","year","atc_code_4"), .SDcols=c("no_records","total_records")]
      #calculate percentage
      preg_med_y[,percentage:=round((no_records/total_records)*100,1)]
      preg_med_y[, data_access_provider:=data_access_provider_name]
      preg_med_y[, data_source:=data_source_name]
      
      #remove all files in poi tmp
      for(i in 1:length(preg_med_y_fl)){
        file.remove(paste0(poi_tmp, preg_med_y_fl[i]))
      }
      rm(preg_med_y_fl)
      #save results to g_output/PREGNANCY_MEDICINES
      if(subpopulations_present=="Yes"){
        fwrite(preg_med_y, paste0(poi_dir, subpopulations_names[s], "/PREGNANCY_MEDICINES/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_med_counts_ys.csv"), row.names = F)
      } else {
        fwrite(preg_med_y, paste0(poi_dir,"PREGNANCY_MEDICINES/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_med_counts_ys.csv"), row.names = F)
      }
      
      preg_med_y[,no_records:=as.numeric(no_records)][,total_records:=as.numeric(total_records)]
      preg_med_y[records_categories, u_range := i.range[no_records >= min & no_records <= max], on = .(no_records >= min, no_records <= max)]
      preg_med_y[records_categories, s_range := i.range[total_records >= min & total_records <= max], on = .(total_records >= min, total_records <= max)]
      
      gdpr_file<-preg_med_y[,-c("no_records","total_records"),with=F]
      setnames(gdpr_file,"u_range","no_records")
      setnames(gdpr_file,"s_range","total_records")
      setcolorder(gdpr_file,c("stage_of_pregnancy", "atc_code_4","no_records", "total_records","percentage"))
      

      if(subpopulations_present=="Yes"){
        fwrite(gdpr_file, paste0(poi_dir,subpopulations_names[s], "/GDPR/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_med_counts_ys_masked.csv"), row.names = F)
      } else {
        fwrite(gdpr_file, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_med_counts_ys_masked.csv"), row.names = F)
      }
      rm(gdpr_file)
      
      preg_med_y[,u_range:=NULL][,s_range:=NULL]
      #apply masking
      preg_med_y[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
      preg_med_y[, total_records:= as.character(total_records)][as.numeric(total_records) > 0 & as.numeric(total_records) < 5, total_records := "<5"]
      preg_med_y[, percentage:= as.character(percentage)][no_records=="<5" | total_records=="<5", percentage := "N/A"]
      
      if(subpopulations_present=="Yes"){
        fwrite(preg_med_y, paste0(poi_dir,subpopulations_names[s], "/PREGNANCY_MEDICINES/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_preg_med_counts_ys_masked.csv"), row.names = F)
        fwrite(preg_med_y, paste0(poi_dir,subpopulations_names[s], "/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_preg_med_counts_ys_masked.csv"), row.names = F)
        
             } else {
        fwrite(preg_med_y, paste0(poi_dir, "PREGNANCY_MEDICINES/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_med_counts_ys_masked.csv"), row.names = F)
               fwrite(preg_med_y, paste0(poi_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_med_counts_ys_masked.csv"), row.names = F)
               
                     }
      
      rm(preg_med_y)
    }
    ####Counts of medicine records by year and meaning####
    if(length(list.files(poi_tmp,"preg_med_ym.rds"))>0){ 
      preg_med_ym_fl<-list.files(poi_tmp, "preg_med_ym.rds")#counts of medicines records by year and sex
      preg_med_ym<-lapply(paste0(poi_tmp,preg_med_ym_fl), readRDS)
      preg_med_ym<-as.data.table(rbindlist(preg_med_ym,fill = T))
      #make sure there are no zeros in no_subjects
      preg_med_ym<-preg_med_ym[!is.na(total_records)]
      #apply sum to make sure all categories are combined
      preg_med_ym<-preg_med_ym[,lapply(.SD, sum), by=c("stage_of_pregnancy","meaning", "year","atc_code_4"), .SDcols=c("no_records","total_records")]
      #calculate percentage
      preg_med_ym[,percentage:=round((no_records/total_records)*100,1)]
      preg_med_ym[, data_access_provider:=data_access_provider_name]
      preg_med_ym[, data_source:=data_source_name]
      
      #remove all files in poi tmp
      for(i in 1:length(preg_med_ym_fl)){
        file.remove(paste0(poi_tmp, preg_med_ym_fl[i]))
      }
      rm(preg_med_ym_fl)
      #save results to g_output/PREGNANCY_MEDICINES
      if(subpopulations_present=="Yes"){
        fwrite(preg_med_ym, paste0(poi_dir, subpopulations_names[s], "/PREGNANCY_MEDICINES/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_med_counts_ym.csv"), row.names = F)
      } else {
        fwrite(preg_med_ym, paste0(poi_dir,"PREGNANCY_MEDICINES/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_med_counts_ym.csv"), row.names = F)
      }
      
      preg_med_ym[,no_records:=as.numeric(no_records)][,total_records:=as.numeric(total_records)]
      preg_med_ym[records_categories, u_range := i.range[no_records >= min & no_records <= max], on = .(no_records >= min, no_records <= max)]
      preg_med_ym[records_categories, s_range := i.range[total_records >= min & total_records <= max], on = .(total_records >= min, total_records <= max)]
      
      gdpr_file<-preg_med_ym[,-c("no_records","total_records"),with=F]
      setnames(gdpr_file,"u_range","no_records")
      setnames(gdpr_file,"s_range","total_records")
      setcolorder(gdpr_file,c("stage_of_pregnancy","meaning","year", "atc_code_4","no_records", "total_records","percentage"))
      
      if(subpopulations_present=="Yes"){
        fwrite(gdpr_file, paste0(poi_dir,subpopulations_names[s], "/GDPR/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_med_counts_ym_masked.csv"), row.names = F)
      } else {
        fwrite(gdpr_file, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_med_counts_ym_masked.csv"), row.names = F)
      }
      rm(gdpr_file)
      
      preg_med_ym[,u_range:=NULL][,s_range:=NULL]
      #apply masking
      preg_med_ym[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
      preg_med_ym[, total_records:= as.character(total_records)][as.numeric(total_records) > 0 & as.numeric(total_records) < 5, total_records := "<5"]
      preg_med_ym[, percentage:= as.character(percentage)][no_records=="<5" | total_records=="<5", percentage := "N/A"]
      
      if(subpopulations_present=="Yes"){
        fwrite(preg_med_ym, paste0(poi_dir,subpopulations_names[s], "/PREGNANCY_MEDICINES/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_preg_med_counts_ym_masked.csv"), row.names = F)
        fwrite(preg_med_ym, paste0(poi_dir,subpopulations_names[s], "/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_preg_med_counts_ym_masked.csv"), row.names = F)
        
             } else {
        fwrite(preg_med_ym, paste0(poi_dir, "PREGNANCY_MEDICINES/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_med_counts_ym_masked.csv"), row.names = F)
               fwrite(preg_med_ym, paste0(poi_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_med_counts_ym_masked.csv"), row.names = F)
               
                     }
      
      rm(preg_med_ym)
    }
    ####Combine results for excluded records dating prior to preg####
    if(length(list.files(poi_tmp,"preg_med_prior_date_excluded.rds"))>0){ 
      preg_med_prior_date_excluded_fl<-list.files(poi_tmp, "preg_med_prior_date_excluded.rds")
      preg_med_prior_date_excluded<-lapply(paste0(poi_tmp,preg_med_prior_date_excluded_fl), readRDS)
      preg_med_prior_date_excluded<-as.data.table(rbindlist(preg_med_prior_date_excluded,fill = T))
      preg_med_prior_date_excluded<-preg_med_prior_date_excluded[,lapply(.SD, sum), .SDcols="excluded_records", by=c("stage_of_pregnancy","year")]
      
      #remove all files in poi tmp
      for(i in 1:length(preg_med_prior_date_excluded_fl)){
        file.remove(paste0(poi_tmp, preg_med_prior_date_excluded_fl[i]))
      }
      rm(preg_med_prior_date_excluded_fl)
      
      #save results to g_output/PREGNANCY_MEDICINES
      if(subpopulations_present=="Yes"){
        fwrite(preg_med_prior_date_excluded, paste0(poi_dir, subpopulations_names[s], "/PREGNANCY_MEDICINES/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_med_prior_removed.csv"), row.names = F)
      } else {
        fwrite(preg_med_prior_date_excluded, paste0(poi_dir,"PREGNANCY_MEDICINES/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_med_prior_removed.csv"), row.names = F)
      }
      
      if(subpopulations_present=="Yes"){
        fwrite(preg_med_prior_date_excluded, paste0(poi_dir, subpopulations_names[s], "/GDPR/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_med_prior_removed.csv"), row.names = F)
        fwrite(preg_med_prior_date_excluded, paste0(poi_dir, subpopulations_names[s], "/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_med_prior_removed.csv"), row.names = F)
        
              } else {
        fwrite(preg_med_prior_date_excluded, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_med_prior_removed.csv"), row.names = F)
                fwrite(preg_med_prior_date_excluded, paste0(poi_dir,"Masked/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_med_prior_removed.csv"), row.names = F)
                
                      }
      
      
      rm(preg_med_prior_date_excluded)
    }
  } 
  
  #####Combine results for vaccine use after a pregnancy record#####
  vaccines_files<-length(list.files(poi_tmp,"preg_vacc_y|preg_vacc_ym|prevalence_preg_vacc_s|prevalence_preg_vacc_y|preg_vacc_prior_date_excluded|vacc_duplicated_rec_excluded"))
  
  if(vaccines_files>0){
    ####Prevalence of exposed pregnancies by year of pregnancy####
    if(length(list.files(poi_tmp,"prevalence_preg_vacc_y.rds"))>0){ 
      prevalence_preg_vacc_y_fl<-list.files(poi_tmp, "prevalence_preg_vacc_y.rds")#prevalence of medicines records by year
      prevalence_preg_vacc_y<-lapply(paste0(poi_tmp,prevalence_preg_vacc_y_fl), readRDS)
      prevalence_preg_vacc_y<-as.data.table(rbindlist(prevalence_preg_vacc_y,fill = T))
      #make sure there are no zeros in no_subjects
      prevalence_preg_vacc_y<-prevalence_preg_vacc_y[!is.na(no_pregnancies)]
      #replace NA in no_users with 0
      prevalence_preg_vacc_y[is.na(no_exposed_pregnancies), no_exposed_pregnancies:=0]
      #apply sum to make sure all categories are combined
      prevalence_preg_vacc_y<-prevalence_preg_vacc_y[,lapply(.SD, sum), by=c("year"), .SDcols=c("no_exposed_pregnancies","no_pregnancies")]
      #separate atc_code=="N/A"
      # no_users_present<-prevalence_preg_vacc_y[atc_code_4=="N/A"]
      # if(no_users_present[,.N]>0){
      #   no_users_present[,atc_code_4:=NULL][,no_exposed_pregnancies:=NULL]
      #   no_users_present[ , new := do.call(paste, c(.SD, sep = "_"))]
      #   atc_codes_to_add<-prevalence_preg_vacc_y[!duplicated(atc_code_4),atc_code_4]
      #   atc_codes_to_add<-atc_codes_to_add[!atc_codes_to_add %in% "N/A"]
      #   if(length(atc_codes_to_add)==0){
      #     a<-NULL
      #   }else{
      #     a<-as.data.table(expand.grid(no_users_present[,new],atc_codes_to_add))
      #     names(a)<-c("combine", "atc_code_4")
      #     a[,c("year","no_pregnancies"):=tstrsplit(combine,"_", fixed=T)]
      #     a[,combine:=NULL]
      #     a[,no_exposed_pregnancies:=0]
      #   }
      #   prevalence_preg_vacc_y<-prevalence_preg_vacc_y[!atc_code_4 %in% "N/A"]
      #   prevalence_preg_vacc_y<-as.data.table(rbind(prevalence_preg_vacc_y,a))
      #   rm(no_users_present,a)
      # }else{rm(no_users_present)}
      prevalence_preg_vacc_y[,no_exposed_pregnancies:=as.numeric(no_exposed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
      
      #calculate prevalence
      prevalence_preg_vacc_y[,prevalence_vaccine_exposure:=round((no_exposed_pregnancies/no_pregnancies)*100,1)]
      prevalence_preg_vacc_y[, data_access_provider:=data_access_provider_name]
      prevalence_preg_vacc_y[, data_source:=data_source_name]
      
      #remove all files in poi tmp
      for(i in 1:length(prevalence_preg_vacc_y_fl)){
        file.remove(paste0(poi_tmp, prevalence_preg_vacc_y_fl[i]))
      }
      rm(prevalence_preg_vacc_y_fl)
      
      #setnames(prevalence_preg_vacc_y, "atc_code_4", "vaccine_indicator")
      #save results to g_output/PREGNANCY_VACCINES
      if(subpopulations_present=="Yes"){
        fwrite(prevalence_preg_vacc_y, paste0(poi_dir, subpopulations_names[s], "/PREGNANCY_VACCINES/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_vacc_prevalence_y.csv"), row.names = F)
      } else {
        fwrite(prevalence_preg_vacc_y, paste0(poi_dir,"PREGNANCY_VACCINES/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_prevalence_y.csv"), row.names = F)
      }
      
      prevalence_preg_vacc_y[,no_exposed_pregnancies:=as.numeric(no_exposed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
      prevalence_preg_vacc_y[records_categories, u_range := i.range[no_exposed_pregnancies >= min & no_exposed_pregnancies <= max], on = .(no_exposed_pregnancies >= min, no_exposed_pregnancies <= max)]
      prevalence_preg_vacc_y[records_categories, s_range := i.range[no_pregnancies >= min & no_pregnancies <= max], on = .(no_pregnancies >= min, no_pregnancies <= max)]
      
      gdpr_file<-prevalence_preg_vacc_y[,-c("no_exposed_pregnancies","no_pregnancies"),with=F]
      setnames(gdpr_file,"u_range","no_exposed_pregnancies")
      setnames(gdpr_file,"s_range","no_pregnancies")
      setcolorder(gdpr_file,c("year","no_exposed_pregnancies", "no_pregnancies","prevalence_vaccine_exposure"))

      if(subpopulations_present=="Yes"){
        fwrite(gdpr_file, paste0(poi_dir,subpopulations_names[s], "/GDPR/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_vacc_prevalence_y_masked.csv"), row.names = F)
      } else {
        fwrite(gdpr_file, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_prevalence_y_masked.csv"), row.names = F)
      }
      rm(gdpr_file)
      
      prevalence_preg_vacc_y[,u_range:=NULL][,s_range:=NULL]
      #apply masking
      prevalence_preg_vacc_y[, no_exposed_pregnancies:= as.character(no_exposed_pregnancies)][as.numeric(no_exposed_pregnancies) > 0 & as.numeric(no_exposed_pregnancies) < 5, no_exposed_pregnancies := "<5"]
      prevalence_preg_vacc_y[, no_pregnancies:= as.character(no_pregnancies)][as.numeric(no_pregnancies) > 0 & as.numeric(no_pregnancies) < 5, no_pregnancies := "<5"]
      prevalence_preg_vacc_y[, prevalence_vaccine_exposure:= as.character(prevalence_vaccine_exposure)][no_exposed_pregnancies=="<5" | no_pregnancies=="<5", prevalence_vaccine_exposure := "N/A"]
      
      if(subpopulations_present=="Yes"){
        fwrite(prevalence_preg_vacc_y, paste0(poi_dir,subpopulations_names[s], "/PREGNANCY_VACCINES/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_preg_vacc_prevalence_y_masked.csv"), row.names = F)
        fwrite(prevalence_preg_vacc_y, paste0(poi_dir,subpopulations_names[s], "/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_preg_vacc_prevalence_y_masked.csv"), row.names = F)
        
              } else {
        fwrite(prevalence_preg_vacc_y, paste0(poi_dir, "PREGNANCY_VACCINES/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_prevalence_y_masked.csv"), row.names = F)
                fwrite(prevalence_preg_vacc_y, paste0(poi_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_prevalence_y_masked.csv"), row.names = F)
                
                      }
      
      rm(prevalence_preg_vacc_y)
    }
    ####Prevalence of exposed pregnancies by atc code and year of pregnancy####
    if(length(list.files(poi_tmp,"prevalence_preg_vacc_atc.rds"))>0){ 
      prevalence_preg_vacc_atc_fl<-list.files(poi_tmp, "prevalence_preg_vacc_atc.rds")#prevalence of medicines records by year
      prevalence_preg_vacc_atc<-lapply(paste0(poi_tmp,prevalence_preg_vacc_atc_fl), readRDS)
      prevalence_preg_vacc_atc<-as.data.table(rbindlist(prevalence_preg_vacc_atc,fill = T))
      #make sure there are no zeros in no_subjects
      prevalence_preg_vacc_atc<-prevalence_preg_vacc_atc[!is.na(no_pregnancies)]
      #replace NA in atc_code_4 with N/A
      prevalence_preg_vacc_atc[is.na(atc_code_4), atc_code_4:="N/A"]
      #replace NA in no_users with 0
      prevalence_preg_vacc_atc[is.na(no_exposed_pregnancies), no_exposed_pregnancies:=0]
      #apply sum to make sure all categories are combined
      prevalence_preg_vacc_atc<-prevalence_preg_vacc_atc[,lapply(.SD, sum), by=c("atc_code_4","year"), .SDcols=c("no_exposed_pregnancies","no_pregnancies")]
      #separate atc_code=="N/A"
      # no_users_present<-prevalence_preg_vacc_atc[atc_code_4=="N/A"]
      # if(no_users_present[,.N]>0){
      #   no_users_present[,atc_code_4:=NULL][,no_exposed_pregnancies:=NULL]
      #   no_users_present[ , new := do.call(paste, c(.SD, sep = "_"))]
      #   atc_codes_to_add<-prevalence_preg_vacc_atc[!duplicated(atc_code_4),atc_code_4]
      #   atc_codes_to_add<-atc_codes_to_add[!atc_codes_to_add %in% "N/A"]
      #   if(length(atc_codes_to_add)==0){
      #     a<-NULL
      #   }else{
      #     a<-as.data.table(expand.grid(no_users_present[,new],atc_codes_to_add))
      #     names(a)<-c("combine", "atc_code_4")
      #     a[,c("year","no_pregnancies"):=tstrsplit(combine,"_", fixed=T)]
      #     a[,combine:=NULL]
      #     a[,no_exposed_pregnancies:=0]
      #   }
      #   prevalence_preg_vacc_atc<-prevalence_preg_vacc_atc[!atc_code_4 %in% "N/A"]
      #   prevalence_preg_vacc_atc<-as.data.table(rbind(prevalence_preg_vacc_atc,a))
      #   rm(no_users_present,a)
      # }else{rm(no_users_present)}
      prevalence_preg_vacc_atc[,no_exposed_pregnancies:=as.numeric(no_exposed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
      
      #calculate prevalence
      prevalence_preg_vacc_atc[,prevalence_vaccine_exposure:=round((no_exposed_pregnancies/no_pregnancies)*100,1)]
      prevalence_preg_vacc_atc[, data_access_provider:=data_access_provider_name]
      prevalence_preg_vacc_atc[, data_source:=data_source_name]
      
      #remove all files in poi tmp
      for(i in 1:length(prevalence_preg_vacc_atc_fl)){
        file.remove(paste0(poi_tmp, prevalence_preg_vacc_atc_fl[i]))
      }
      rm(prevalence_preg_vacc_atc_fl)
      
      setnames(prevalence_preg_vacc_atc,"atc_code_4", "vaccine_indicator")
      #save results to g_output/PREGNANCY_VACCINES
      if(subpopulations_present=="Yes"){
        fwrite(prevalence_preg_vacc_atc, paste0(poi_dir, subpopulations_names[s], "/PREGNANCY_VACCINES/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_vacc_prevalence_atc.csv"), row.names = F)
      } else {
        fwrite(prevalence_preg_vacc_atc, paste0(poi_dir,"PREGNANCY_VACCINES/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_prevalence_atc.csv"), row.names = F)
      }
      
      prevalence_preg_vacc_atc[,no_exposed_pregnancies:=as.numeric(no_exposed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
      prevalence_preg_vacc_atc[records_categories, u_range := i.range[no_exposed_pregnancies >= min & no_exposed_pregnancies <= max], on = .(no_exposed_pregnancies >= min, no_exposed_pregnancies <= max)]
      prevalence_preg_vacc_atc[records_categories, s_range := i.range[no_pregnancies >= min & no_pregnancies <= max], on = .(no_pregnancies >= min, no_pregnancies <= max)]
      
      gdpr_file<-prevalence_preg_vacc_atc[,-c("no_exposed_pregnancies","no_pregnancies"),with=F]
      setnames(gdpr_file,"u_range","no_exposed_pregnancies")
      setnames(gdpr_file,"s_range","no_pregnancies")
      setcolorder(gdpr_file,c("vaccine_indicator", "year","no_exposed_pregnancies", "no_pregnancies","prevalence_vaccine_exposure"))
      
      if(subpopulations_present=="Yes"){
        fwrite(gdpr_file, paste0(poi_dir,subpopulations_names[s], "/GDPR/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_vacc_prevalence_atc_masked.csv"), row.names = F)
      } else {
        fwrite(gdpr_file, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_prevalence_atc_masked.csv"), row.names = F)
      }
      rm(gdpr_file)
      
      prevalence_preg_vacc_atc[,u_range:=NULL][,s_range:=NULL]
      #apply masking
      prevalence_preg_vacc_atc[, no_exposed_pregnancies:= as.character(no_exposed_pregnancies)][as.numeric(no_exposed_pregnancies) > 0 & as.numeric(no_exposed_pregnancies) < 5, no_exposed_pregnancies := "<5"]
      prevalence_preg_vacc_atc[, no_pregnancies:= as.character(no_pregnancies)][as.numeric(no_pregnancies) > 0 & as.numeric(no_pregnancies) < 5, no_pregnancies := "<5"]
      prevalence_preg_vacc_atc[, prevalence_vaccine_exposure:= as.character(prevalence_vaccine_exposure)][no_exposed_pregnancies=="<5" | no_pregnancies=="<5", prevalence_vaccine_exposure := "N/A"]
      
      if(subpopulations_present=="Yes"){
        fwrite(prevalence_preg_vacc_atc, paste0(poi_dir,subpopulations_names[s], "/PREGNANCY_VACCINES/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_preg_vacc_prevalence_atc_masked.csv"), row.names = F)
        fwrite(prevalence_preg_vacc_atc, paste0(poi_dir,subpopulations_names[s], "/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_preg_vacc_prevalence_atc_masked.csv"), row.names = F)
        
              } else {
        fwrite(prevalence_preg_vacc_atc, paste0(poi_dir, "PREGNANCY_VACCINES/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_prevalence_atc_masked.csv"), row.names = F)
                fwrite(prevalence_preg_vacc_atc, paste0(poi_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_prevalence_atc_masked.csv"), row.names = F)
                
                      }
      
      rm(prevalence_preg_vacc_atc)
    }
    
    ####Prevalence of exposed pregnancies by stage of pregnancy,atc code_4 and year of pregnancy####
    if(length(list.files(poi_tmp,"prevalence_preg_vacc_s.rds"))>0){ 
      prevalence_preg_vacc_s_fl<-list.files(poi_tmp, "prevalence_preg_vacc_s.rds")#prevalence of medicines records by year and sex
      prevalence_preg_vacc_s<-lapply(paste0(poi_tmp,prevalence_preg_vacc_s_fl), readRDS)
      prevalence_preg_vacc_s<-as.data.table(rbindlist(prevalence_preg_vacc_s,fill = T))
      #make sure there are no zeros in no_subjects
      prevalence_preg_vacc_s<-prevalence_preg_vacc_s[!is.na(no_pregnancies)]
      #replace NA in no_users with 0
      prevalence_preg_vacc_s[is.na(no_exposed_pregnancies), no_exposed_pregnancies:=0]
      #apply sum to make sure all categories are combined
      prevalence_preg_vacc_s<-prevalence_preg_vacc_s[,lapply(.SD, sum), by=c("stage_of_pregnancy", "year"), .SDcols=c("no_exposed_pregnancies","no_pregnancies")]
      
      # #separate atc_code=="N/A"
      # no_users_present<-prevalence_preg_vacc_s[atc_code_4=="N/A"]
      # if(no_users_present[,.N]>0){
      #   no_users_present[,atc_code_4:=NULL][,no_exposed_pregnancies:=NULL]
      #   no_users_present[ , new := do.call(paste, c(.SD, sep = "_"))]
      #   events<-no_users_present[!duplicated(stage_of_pregnancy),stage_of_pregnancy]
      #   list<-vector(mode="list", length=length(events))
      #   for(ls in 1:length(list)){
      #     atc_codes_to_add<-prevalence_preg_vacc_s[stage_of_pregnancy==events[ls]][!duplicated(atc_code_4),atc_code_4]
      #     atc_codes_to_add<-atc_codes_to_add[!atc_codes_to_add %in% "N/A"]
      #     if(length(atc_codes_to_add)==0){
      #       a<-NULL
      #     }else{
      #       a<-as.data.table(expand.grid(no_users_present[stage_of_pregnancy==events[ls],new],atc_codes_to_add))
      #       names(a)<-c("combine", "atc_code_4")
      #       a[,c("stage_of_pregnancy","year","no_pregnancies"):=tstrsplit(combine,"_", fixed=T)]
      #       a[,combine:=NULL]
      #     }
      #     list[[ls]]<-a
      #     rm(a)
      #   }
      #   list<-rbindlist(list)
      #   list[,no_exposed_pregnancies:=0]
      #   prevalence_preg_vacc_s<-prevalence_preg_vacc_s[!atc_code_4 %in% "N/A"]
      #   prevalence_preg_vacc_s<-as.data.table(rbind(prevalence_preg_vacc_s,list))
      #   rm(list,no_users_present)
      # }else{rm(no_users_present)}
      prevalence_preg_vacc_s[,no_exposed_pregnancies:=as.numeric(no_exposed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
      
      #calculate prevalence
      prevalence_preg_vacc_s[,prevalence_vaccine_exposure:=round((no_exposed_pregnancies/no_pregnancies)*100,1)]
      prevalence_preg_vacc_s[, data_access_provider:=data_access_provider_name]
      prevalence_preg_vacc_s[, data_source:=data_source_name]
      
      
      #remove all files in poi tmp
      for(i in 1:length(prevalence_preg_vacc_s_fl)){
        file.remove(paste0(poi_tmp, prevalence_preg_vacc_s_fl[i]))
      }
      rm(prevalence_preg_vacc_s_fl)
      
      #setnames(prevalence_preg_vacc_s,"atc_code_4", "vaccine_indicator")
      #save results to g_output/PREGNANCY_VACCINES
      if(subpopulations_present=="Yes"){
        fwrite(prevalence_preg_vacc_s, paste0(poi_dir, subpopulations_names[s], "/PREGNANCY_VACCINES/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_vacc_prevalence_s.csv"), row.names = F)
      } else {
        fwrite(prevalence_preg_vacc_s, paste0(poi_dir,"PREGNANCY_VACCINES/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_prevalence_s.csv"), row.names = F)
      }
      
      prevalence_preg_vacc_s[,no_exposed_pregnancies:=as.numeric(no_exposed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
      prevalence_preg_vacc_s[records_categories, u_range := i.range[no_exposed_pregnancies >= min & no_exposed_pregnancies <= max], on = .(no_exposed_pregnancies >= min, no_exposed_pregnancies <= max)]
      prevalence_preg_vacc_s[records_categories, s_range := i.range[no_pregnancies >= min & no_pregnancies <= max], on = .(no_pregnancies >= min, no_pregnancies <= max)]
      
      gdpr_file<-prevalence_preg_vacc_s[,-c("no_exposed_pregnancies","no_pregnancies"),with=F]
      setnames(gdpr_file,"u_range","no_exposed_pregnancies")
      setnames(gdpr_file,"s_range","no_pregnancies")
      setcolorder(gdpr_file,c("stage_of_pregnancy", "year","no_exposed_pregnancies", "no_pregnancies","prevalence_vaccine_exposure"))
      
      if(subpopulations_present=="Yes"){
        fwrite(gdpr_file, paste0(poi_dir,subpopulations_names[s], "/GDPR/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_vacc_prevalence_s_masked.csv"), row.names = F)
      } else {
        fwrite(gdpr_file, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_prevalence_s_masked.csv"), row.names = F)
      }
      rm(gdpr_file)
      
      prevalence_preg_vacc_s[,u_range:=NULL][,s_range:=NULL]
      #apply masking
      prevalence_preg_vacc_s[, no_exposed_pregnancies:= as.character(no_exposed_pregnancies)][as.numeric(no_exposed_pregnancies) > 0 & as.numeric(no_exposed_pregnancies) < 5, no_exposed_pregnancies := "<5"]
      prevalence_preg_vacc_s[, no_pregnancies:= as.character(no_pregnancies)][as.numeric(no_pregnancies) > 0 & as.numeric(no_pregnancies) < 5, no_pregnancies := "<5"]
      prevalence_preg_vacc_s[, prevalence_vaccine_exposure:= as.character(prevalence_vaccine_exposure)][no_exposed_pregnancies=="<5" | no_pregnancies=="<5", prevalence_vaccine_exposure := "N/A"]
      
      if(subpopulations_present=="Yes"){
        fwrite(prevalence_preg_vacc_s, paste0(poi_dir,subpopulations_names[s], "/PREGNANCY_VACCINES/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_preg_vacc_prevalence_s_masked.csv"), row.names = F)
        fwrite(prevalence_preg_vacc_s, paste0(poi_dir,subpopulations_names[s], "/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_preg_vacc_prevalence_s_masked.csv"), row.names = F)
        
              } else {
        fwrite(prevalence_preg_vacc_s, paste0(poi_dir, "PREGNANCY_VACCINES/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_prevalence_s_masked.csv"), row.names = F)
                fwrite(prevalence_preg_vacc_s, paste0(poi_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_prevalence_s_masked.csv"), row.names = F)
                
                      }
      
      rm(prevalence_preg_vacc_s)
    }
    
    ####Counts of vaccine records by year####
    if(length(list.files(poi_tmp,"preg_vacc_y.rds"))>0){ 
      preg_vacc_y_fl<-list.files(poi_tmp, "preg_vacc_y.rds")#counts of medicines records by year and sex
      preg_vacc_y<-lapply(paste0(poi_tmp,preg_vacc_y_fl), readRDS)
      preg_vacc_y<-as.data.table(rbindlist(preg_vacc_y,fill = T))
      #make sure there are no zeros in no_subjects
      preg_vacc_y<-preg_vacc_y[!is.na(total_records)]
      #apply sum to make sure all categories are combined
      preg_vacc_y<-preg_vacc_y[,lapply(.SD, sum), by=c("stage_of_pregnancy","year","atc_code_4"), .SDcols=c("no_records","total_records")]
      #calculate percentage
      preg_vacc_y[,percentage:=round((no_records/total_records)*100,1)]
      preg_vacc_y[, data_access_provider:=data_access_provider_name]
      preg_vacc_y[, data_source:=data_source_name]
      
      #remove all files in poi tmp
      for(i in 1:length(preg_vacc_y_fl)){
        file.remove(paste0(poi_tmp, preg_vacc_y_fl[i]))
      }
      rm(preg_vacc_y_fl)
      #save results to g_output/PREGNANCY_VACCINES
      setnames(preg_vacc_y,"atc_code_4", "vaccine_indicator")
      if(subpopulations_present=="Yes"){
        fwrite(preg_vacc_y, paste0(poi_dir, subpopulations_names[s], "/PREGNANCY_VACCINES/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_vacc_counts_ys.csv"), row.names = F)
      } else {
        fwrite(preg_vacc_y, paste0(poi_dir,"PREGNANCY_VACCINES/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_counts_ys.csv"), row.names = F)
      }
      
      preg_vacc_y[,no_records:=as.numeric(no_records)][,total_records:=as.numeric(total_records)]
      preg_vacc_y[records_categories, u_range := i.range[no_records >= min & no_records <= max], on = .(no_records >= min, no_records <= max)]
      preg_vacc_y[records_categories, s_range := i.range[total_records >= min & total_records <= max], on = .(total_records >= min, total_records <= max)]
      
      gdpr_file<-preg_vacc_y[,-c("no_records","total_records"),with=F]
      setnames(gdpr_file,"u_range","no_records")
      setnames(gdpr_file,"s_range","total_records")
      setcolorder(gdpr_file,c("stage_of_pregnancy", "vaccine_indicator","no_records", "total_records","percentage"))
      
      if(subpopulations_present=="Yes"){
        fwrite(gdpr_file, paste0(poi_dir,subpopulations_names[s], "/GDPR/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_vacc_counts_ys_masked.csv"), row.names = F)
      } else {
        fwrite(gdpr_file, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_counts_ys_masked.csv"), row.names = F)
      }
      rm(gdpr_file)
      
      preg_vacc_y[,u_range:=NULL][,s_range:=NULL]
      #apply masking
      preg_vacc_y[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
      preg_vacc_y[, total_records:= as.character(total_records)][as.numeric(total_records) > 0 & as.numeric(total_records) < 5, total_records := "<5"]
      preg_vacc_y[, percentage:= as.character(percentage)][no_records=="<5" | total_records=="<5", percentage := "N/A"]
      
      if(subpopulations_present=="Yes"){
        fwrite(preg_vacc_y, paste0(poi_dir,subpopulations_names[s], "/PREGNANCY_VACCINES/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_preg_vacc_counts_ys_masked.csv"), row.names = F)
        fwrite(preg_vacc_y, paste0(poi_dir,subpopulations_names[s], "/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_preg_vacc_counts_ys_masked.csv"), row.names = F)
        
              } else {
        fwrite(preg_vacc_y, paste0(poi_dir, "PREGNANCY_VACCINES/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_counts_ys_masked.csv"), row.names = F)
                fwrite(preg_vacc_y, paste0(poi_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_counts_ys_masked.csv"), row.names = F)
                
                      }
      
      rm(preg_vacc_y)
    }
    ####Counts of vaccine records by year and meaning####
    if(length(list.files(poi_tmp,"preg_vacc_ym.rds"))>0){ 
      preg_vacc_ym_fl<-list.files(poi_tmp, "preg_vacc_ym.rds")#counts of medicines records by year and sex
      preg_vacc_ym<-lapply(paste0(poi_tmp,preg_vacc_ym_fl), readRDS)
      preg_vacc_ym<-as.data.table(rbindlist(preg_vacc_ym,fill = T))
      #make sure there are no zeros in no_subjects
      preg_vacc_ym<-preg_vacc_ym[!is.na(total_records)]
      #apply sum to make sure all categories are combined
      preg_vacc_ym<-preg_vacc_ym[,lapply(.SD, sum), by=c("stage_of_pregnancy","meaning", "year","atc_code_4"), .SDcols=c("no_records","total_records")]
      #calculate percentage
      preg_vacc_ym[,percentage:=round((no_records/total_records)*100,1)]
      preg_vacc_ym[, data_access_provider:=data_access_provider_name]
      preg_vacc_ym[, data_source:=data_source_name]
      
      #remove all files in poi tmp
      for(i in 1:length(preg_vacc_ym_fl)){
        file.remove(paste0(poi_tmp, preg_vacc_ym_fl[i]))
      }
      rm(preg_vacc_ym_fl)
      #save results to g_output/PREGNANCY_VACCINES
      setnames(preg_vacc_ym,"atc_code_4", "vaccine_indicator")
      if(subpopulations_present=="Yes"){
        fwrite(preg_vacc_ym, paste0(poi_dir, subpopulations_names[s], "/PREGNANCY_VACCINES/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_vacc_counts_ym.csv"), row.names = F)
      } else {
        fwrite(preg_vacc_ym, paste0(poi_dir,"PREGNANCY_VACCINES/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_counts_ym.csv"), row.names = F)
      }
      
      preg_vacc_ym[,no_records:=as.numeric(no_records)][,total_records:=as.numeric(total_records)]
      preg_vacc_ym[records_categories, u_range := i.range[no_records >= min & no_records <= max], on = .(no_records >= min, no_records <= max)]
      preg_vacc_ym[records_categories, s_range := i.range[total_records >= min & total_records <= max], on = .(total_records >= min, total_records <= max)]
      
      gdpr_file<-preg_vacc_ym[,-c("no_records","total_records"),with=F]
      setnames(gdpr_file,"u_range","no_records")
      setnames(gdpr_file,"s_range","total_records")
      setcolorder(gdpr_file,c("stage_of_pregnancy","meaning","year", "vaccine_indicator","no_records", "total_records","percentage"))

      if(subpopulations_present=="Yes"){
        fwrite(gdpr_file, paste0(poi_dir,subpopulations_names[s], "/GDPR/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_vacc_counts_ym_masked.csv"), row.names = F)
      } else {
        fwrite(gdpr_file, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_counts_ym_masked.csv"), row.names = F)
      }
      rm(gdpr_file)
      
      preg_vacc_ym[,u_range:=NULL][,s_range:=NULL]
      #apply masking
      preg_vacc_ym[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
      preg_vacc_ym[, total_records:= as.character(total_records)][as.numeric(total_records) > 0 & as.numeric(total_records) < 5, total_records := "<5"]
      preg_vacc_ym[, percentage:= as.character(percentage)][no_records=="<5" | total_records=="<5", percentage := "N/A"]
      
      if(subpopulations_present=="Yes"){
        fwrite(preg_vacc_ym, paste0(poi_dir,subpopulations_names[s], "/PREGNANCY_VACCINES/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_preg_vacc_counts_ym_masked.csv"), row.names = F)
        fwrite(preg_vacc_ym, paste0(poi_dir,subpopulations_names[s], "/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_preg_vacc_counts_ym_masked.csv"), row.names = F)
        
              } else {
        fwrite(preg_vacc_ym, paste0(poi_dir, "PREGNANCY_VACCINES/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_counts_ym_masked.csv"), row.names = F)
                fwrite(preg_vacc_ym, paste0(poi_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_counts_ym_masked.csv"), row.names = F)
                
                      }
      
      rm(preg_vacc_ym)
    }
    ####Combine results for excluded records dating prior to preg####
    if(length(list.files(poi_tmp,"preg_vacc_prior_date_excluded.rds"))>0){ 
      preg_vacc_prior_date_excluded_fl<-list.files(poi_tmp, "preg_vacc_prior_date_excluded.rds")
      preg_vacc_prior_date_excluded<-lapply(paste0(poi_tmp,preg_vacc_prior_date_excluded_fl), readRDS)
      preg_vacc_prior_date_excluded<-as.data.table(rbindlist(preg_vacc_prior_date_excluded,fill = T))
      preg_vacc_prior_date_excluded<-preg_vacc_prior_date_excluded[,lapply(.SD, sum), .SDcols="excluded_records", by=c("stage_of_pregnancy","year")]
      
      #remove all files in poi tmp
      for(i in 1:length(preg_vacc_prior_date_excluded_fl)){
        file.remove(paste0(poi_tmp, preg_vacc_prior_date_excluded_fl[i]))
      }
      rm(preg_vacc_prior_date_excluded_fl)
      
      #save results to g_output/PREGNANCY_VACCINES
      if(subpopulations_present=="Yes"){
        fwrite(preg_vacc_prior_date_excluded, paste0(poi_dir, subpopulations_names[s], "/PREGNANCY_VACCINES/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_vacc_prior_removed.csv"), row.names = F)
      } else {
        fwrite(preg_vacc_prior_date_excluded, paste0(poi_dir,"PREGNANCY_VACCINES/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_prior_removed.csv"), row.names = F)
      }
      
      if(subpopulations_present=="Yes"){
        fwrite(preg_vacc_prior_date_excluded, paste0(poi_dir, subpopulations_names[s], "/GDPR/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_vacc_prior_removed.csv"), row.names = F)
        fwrite(preg_vacc_prior_date_excluded, paste0(poi_dir, subpopulations_names[s], "/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_vacc_prior_removed.csv"), row.names = F)
        
              } else {
        fwrite(preg_vacc_prior_date_excluded, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_prior_removed.csv"), row.names = F)
                fwrite(preg_vacc_prior_date_excluded, paste0(poi_dir,"Masked/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_prior_removed.csv"), row.names = F)
                
                     }
      
      rm(preg_vacc_prior_date_excluded)
    }
    ####Combine results for duplicated vaccine records####
    if(length(list.files(poi_tmp,"vacc_duplicated_rec_excluded.rds"))>0){ 
      vaccines_duplicated_rec_excluded_fl<-list.files(poi_tmp, "vacc_duplicated_rec_excluded.rds")
      vaccines_duplicated_rec_excluded<-lapply(paste0(poi_tmp,vaccines_duplicated_rec_excluded_fl), readRDS)
      vaccines_duplicated_rec_excluded<-as.data.table(rbindlist(vaccines_duplicated_rec_excluded,fill = T))
      vaccines_duplicated_rec_excluded[,year:=substr(vaccine_group,3,6)]
      vaccines_duplicated_rec_excluded[,vaccine_group:=NULL]
      vaccines_duplicated_rec_excluded<-vaccines_duplicated_rec_excluded[,lapply(.SD, sum), .SDcols="duplicated_rec", by=c("stage_of_pregnancy", "year")]
      
      #remove all files in poi tmp
      for(i in 1:length(vaccines_duplicated_rec_excluded_fl)){
        file.remove(paste0(poi_tmp, vaccines_duplicated_rec_excluded_fl[i]))
      }
      rm(vaccines_duplicated_rec_excluded_fl)
      
      #save results to g_output/PREGNANCY_VACCINES
      if(subpopulations_present=="Yes"){
        fwrite(vaccines_duplicated_rec_excluded, paste0(poi_dir, subpopulations_names[s], "/PREGNANCY_VACCINES/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_vacc_dup_removed.csv"), row.names = F)
      } else {
        fwrite(vaccines_duplicated_rec_excluded, paste0(poi_dir,"PREGNANCY_VACCINES/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_dup_removed.csv"), row.names = F)
      }
      
      if(subpopulations_present=="Yes"){
        fwrite(vaccines_duplicated_rec_excluded, paste0(poi_dir, subpopulations_names[s], "/GDPR/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_vacc_dup_removed.csv"), row.names = F)
        fwrite(vaccines_duplicated_rec_excluded, paste0(poi_dir, subpopulations_names[s], "/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_vacc_dup_removed.csv"), row.names = F)
        
              } else {
        fwrite(vaccines_duplicated_rec_excluded, paste0(poi_dir,"GDPR/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_dup_removed.csv"), row.names = F)
                fwrite(vaccines_duplicated_rec_excluded, paste0(poi_dir,"Masked/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_dup_removed.csv"), row.names = F)
                
                      }
      
      
      rm(vaccines_duplicated_rec_excluded)
    } 
    
  } 
}

cat(green(print("The script pregnancy primary is finished.")))