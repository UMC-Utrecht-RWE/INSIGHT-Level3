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
    preg_file<-do.call(rbind,preg_file)
    preg_file<-as.data.table(preg_file)
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
          #create list to save files
          preg_med_prior_dates<-list()
          preg_med_prevalence_stage_total<-list()
          preg_med_prevalence_total<-list()
          preg_medicines_total<-list()
          preg_medicines_meaning<-list()
          
          index<-1
          print("Calculate counts of medicines use after a pregnancy record")
          for(medicine_ind in 1:length(med_files_to_search)){
            print(paste0("Calculating rates for pregnancy and medicine use:", names(pregnancy_files)[preg_ind], "_", med_files_to_search[medicine_ind]))
            med_file<-lapply(paste0(populations_dir,"MEDICINES/", medicines_files[names(medicines_files)==med_files_to_search[[medicine_ind]]][[1]]), readRDS)
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
              if(removed_rec[,.N]>0){
                preg_med_prior_dates[[index]]<-data.table(stage_of_pregnancy=preg_file[!duplicated(condition), condition],  removed_rec)
              }
              rm(removed_rec)
              #remove records
              med_file<-med_file[remove==0]
              med_file[,remove:=NULL]
              if(med_file[,.N]>0){
                #create a pregnancy identifier by combining person id and preganncy code date
                med_file[,rowid:=rowid(person_id,pregnancy_code_date)]#only rowid==1 means its a new pregnancy record, other number means the same pregnancy was exposed to more than one prescription
                #remove unecessary columns
                med_file[,start_follow_up_preg:=NULL][,end_follow_up_preg:=NULL][,min_time_period:=NULL][,max_time_period:=NULL][,preg_id:=NULL][,end_year:=NULL]
                
                
                #save the diag_preg file to diag_preg populations folder to be used in events_pregnancy_medicines and events_pregnancy_vaccines
                #saveRDS(med_file, paste0(populations_dir,"PREGNANCY_MEDICINES/", names(pregnancy_files)[preg_ind], "_", med_files_to_search[medicine_ind], ".rds"))
                
                #calculate prevalence of medicine use:number of preg rec having a prescription/dispensing by stage and year of medication/total number of pregnancy records by stage of pregnancy and year
                preg_med_prev_stage_y<-med_file[!is.na(year_medicines)][rowid==1, .N, by=c("condition", "year_pregnancy", "medicinal_product_atc_code")]
                setnames(preg_med_prev_stage_y,"year_pregnancy","year")
                setnames(preg_med_prev_stage_y,"N","no_exposed_pregnancies")
                setnames(preg_med_prev_stage_y,"condition","stage_of_pregnancy")
                setnames(preg_med_prev_stage_y,"medicinal_product_atc_code","atc_code_4")
                if(preg_med_prev_stage_y[,.N]>0){
                  preg_med_prevalence_stage_total[[index]]<-preg_med_prev_stage_y
                }
                rm(preg_med_prev_stage_y)
                
                #calculate prevalence of medicine use:number of preg rec having a prescription/dispensing by year of medication/total number of pregnancy records by year
                preg_med_prev_y<-med_file[!is.na(year_medicines)][rowid==1, .N, by=c("year_pregnancy","medicinal_product_atc_code")]
                setnames(preg_med_prev_y,"year_pregnancy","year")
                setnames(preg_med_prev_y,"N","no_exposed_pregnancies")
                setnames(preg_med_prev_y,"medicinal_product_atc_code","atc_code_4")
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
                if(preg_med_y_meaning[,.N]>0){
                  preg_medicines_meaning[[index]]<-preg_med_y_meaning
                }
                rm(preg_med_y_meaning,preg_med_y_meaning_t)
              }
              index<-index+1
              rm(med_file)
              gc()
            }
          }
          rm(med_files_to_search)
          
          ####combine results medicines####
          #prevalenece by stage and year
          preg_med_prevalence_stage_total<-rbindlist(preg_med_prevalence_stage_total)
          if(preg_med_prevalence_stage_total[,.N]>0){
            preg_med_prevalence_stage_total<-preg_med_prevalence_stage_total[,lapply(.SD, sum), .SDcols=c("no_exposed_pregnancies"), by=c("stage_of_pregnancy","atc_code_4", "year")]
          }else{preg_med_prevalence_stage_total<-NULL}
          
          #prevalenece by year
          preg_med_prevalence_total<-rbindlist(preg_med_prevalence_total)
          if(preg_med_prevalence_total[,.N]>0){
            preg_med_prevalence_total<-preg_med_prevalence_total[,lapply(.SD, sum), .SDcols=c("no_exposed_pregnancies"), by=c("atc_code_4", "year")]
          }else{preg_med_prevalence_total<-NULL}
          
          #counts by stage, year, atc
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
          if(!is.null(preg_med_prevalence_stage_total)){
            preg_med_prevalence_total_s<-merge.data.table(preg_med_prevalence_stage_total,counts_preg_stage_y, by=c("stage_of_pregnancy","year"), all=T, allow.cartesian = T)
            preg_med_prevalence_total<-merge.data.table(preg_med_prevalence_total,counts_preg_y, by=c("year"), all=T, allow.cartesian = T)
            saveRDS(preg_med_prevalence_total_s,paste0(poi_tmp,names(pregnancy_files)[preg_ind], "_prevalence_preg_med_s.rds"))
            saveRDS(preg_med_prevalence_total,paste0(poi_tmp,names(pregnancy_files)[preg_ind], "_prevalence_preg_med_y.rds"))
          }else{
            preg_med_prevalence_total_s<-data.table(stage_of_pregnancy=names(pregnancy_files)[preg_ind],atc_code_4="N/A", year="N/A",no_exposed_pregnancies=0)
            preg_med_prevalence_total<-data.table(atc_code_4="N/A", counts_preg_y, no_exposed_pregnancies=0)
            saveRDS(preg_med_prevalence_total_s,paste0(poi_tmp,names(pregnancy_files)[preg_ind], "_prevalence_preg_med_s.rds"))
            saveRDS(preg_med_prevalence_total,paste0(poi_tmp,names(pregnancy_files)[preg_ind], "_prevalence_preg_med_y.rds"))
          }
          
          rm(preg_med_prevalence_total,preg_med_prevalence_total_s)
          
          #combine and export the excluded records
          if(!is.null(preg_med_prior_dates[,.N])){
            #remove empty cells
            preg_med_prior_dates<-preg_med_prior_dates[excluded_records!=0]
            saveRDS(preg_med_prior_dates,paste0(poi_tmp,names(pregnancy_files)[preg_ind], "_preg_med_prior_date_excluded.rds"))
          }
          rm(preg_med_prior_dates)
          
        }else{rm(med_files_to_search)}
        
      }
      
      ####vaccines analysis####
      if(!is.null(vaccines_files)){
        #define vaccines files to be searched based on the time lag
        vacc_files_to_search<-names(vaccines_files)[substr(names(vaccines_files),3,6) %in% year_filter]
        
        if(length(vacc_files_to_search)>0){
          #create list to save files
          preg_vacc_prior_dates<-list()
          preg_vacc_prevalence_stage_total<-list()
          preg_vacc_prevalence_total<-list()
          preg_vaccines_total<-list()
          preg_vaccines_meaning<-list()
          duplicated_vacc_rec<-list()
          
          index<-1
          print("Calculate counts of vaccines exposure after a pregnancy record")
          for(vaccine_ind in 1:length(vacc_files_to_search)){
            print(paste0("Calculating rates for pregnancy and vaccine exposure:", names(pregnancy_files)[preg_ind], "_", vacc_files_to_search[vaccine_ind]))
            vacc_file<-lapply(paste0(populations_dir,"VACCINES/", vaccines_files[names(vaccines_files)==vacc_files_to_search[[vaccine_ind]]][[1]]), readRDS)
            vacc_file<-rbindlist(vacc_file, fill = T)
            vacc_file<-as.data.table(vacc_file)
            #remove duplicated vaccine records
            vacc_file[,dup_ind:=rowid(person_id,vaccines_date)]
            duplicated_vacc_rec[[vacc_ind]]<-data.table(stage_of_pregnancy=preg_file[!duplicated(condition),condition],vaccine_group=names(vaccines_files)[vaccine_ind], duplicated_rec=vacc_file[dup_ind>1,.N])
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
            vacc_file<-vacc_file[,vx_atc:=substr(vx_atc,1,4)]
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
              if(removed_rec[,.N]>0){
                preg_vacc_prior_dates[[index]]<-data.table(stage_of_pregnancy=preg_file[!duplicated(condition), condition],  removed_rec)
              }
              rm(removed_rec)
              #remove records
              vacc_file<-vacc_file[remove==0]
              vacc_file[,remove:=NULL]
              if(vacc_file[,.N]>0){
                #create a pregnancy identifier by combining person id and preganncy code date
                vacc_file[,rowid:=rowid(person_id,pregnancy_code_date)]#only rowid==1 means its a new pregnancy record, other number means the same pregnancy was exposed to more than one prescription
                #remove unecessary columns
                vacc_file[,start_follow_up_preg:=NULL][,end_follow_up_preg:=NULL][,min_time_period:=NULL][,max_time_period:=NULL][,preg_id:=NULL][,end_year:=NULL]
                
                
                #save the diag_preg file to diag_preg populations folder to be used in events_pregnancy_vaccines and events_pregnancy_vaccines
                #saveRDS(vacc_file, paste0(populations_dir,"PREGNANCY_VACCINES/", names(pregnancy_files)[preg_ind], "_", vacc_files_to_search[vaccine_ind], ".rds"))
                
                #calculate prevalence of vaccine use:number of preg rec having a prescription/dispensing by stage and year of medication/total number of pregnancy records by stage of pregnancy and year
                preg_vacc_prev_stage_y<-vacc_file[!is.na(year_vaccines)][rowid==1, .N, by=c("condition", "year_pregnancy", "vx_atc")]
                setnames(preg_vacc_prev_stage_y,"year_pregnancy","year")
                setnames(preg_vacc_prev_stage_y,"N","no_exposed_pregnancies")
                setnames(preg_vacc_prev_stage_y,"condition","stage_of_pregnancy")
                setnames(preg_vacc_prev_stage_y,"vx_atc","atc_code_4")
                if(preg_vacc_prev_stage_y[,.N]>0){
                  preg_vacc_prevalence_stage_total[[index]]<-preg_vacc_prev_stage_y
                }
                rm(preg_vacc_prev_stage_y)
                
                #calculate prevalence of vaccine use:number of preg rec having a prescription/dispensing by year of medication/total number of pregnancy records by year
                preg_vacc_prev_y<-vacc_file[!is.na(year_vaccines)][rowid==1, .N, by=c("year_pregnancy","vx_atc")]
                setnames(preg_vacc_prev_y,"year_pregnancy","year")
                setnames(preg_vacc_prev_y,"N","no_exposed_pregnancies")
                setnames(preg_vacc_prev_y,"vx_atc","atc_code_4")
                if(preg_vacc_prev_y[,.N]>0){
                  preg_vacc_prevalence_total[[index]]<-preg_vacc_prev_y
                }
                rm(preg_vacc_prev_y)
                
                
                #calculate counts of vaccine use:number of prescriptions/dispensing after a pregnancy/total number of records by year
                preg_vacc_y<-vacc_file[!is.na(year_vaccines)][, .N, by=c("year_vaccines","vx_atc","condition")]
                setnames(preg_vacc_y,"year_vaccines","year")
                setnames(preg_vacc_y,"vx_atc","atc_code_4")
                setnames(preg_vacc_y,"N","no_records")
                setnames(preg_vacc_y,"condition","stage_of_pregnancy")
                
                preg_vacc_y_t<-vacc_file[!is.na(year_vaccines)][, .N, by=c("year_vaccines","condition")]
                setnames(preg_vacc_y_t,"year_vaccines","year")
                setnames(preg_vacc_y_t,"N","total_records")
                setnames(preg_vacc_y_t,"condition","stage_of_pregnancy")
                preg_vacc_y<-merge.data.table(preg_vacc_y,preg_vacc_y_t,all=T, by=c("stage_of_pregnancy","year"))
                if(preg_vacc_y[,.N]>0){
                  preg_vaccines_total[[index]]<-preg_vacc_y
                }
                rm(preg_vacc_y,preg_vacc_y_t)
                
                #calculate counts of vaccine use:number of prescriptions/dispensing after a pregnancy/total number of records by year and meaning
                vacc_file[,meaning:=paste(meaning_pregnancy, meaning_vaccines, sep=":")]
                preg_vacc_y_meaning<-vacc_file[!is.na(year_vaccines)][, .N, by=c("condition","meaning","year_vaccines","vx_atc")]
                setnames(preg_vacc_y_meaning,"year_vaccines","year")
                setnames(preg_vacc_y_meaning,"vx_atc","atc_code_4")
                setnames(preg_vacc_y_meaning,"N","no_records")
                setnames(preg_vacc_y_meaning,"condition","stage_of_pregnancy")
                
                preg_vacc_y_meaning_t<-vacc_file[!is.na(year_vaccines)][, .N, by=c("condition","meaning","year_vaccines")]
                setnames(preg_vacc_y_meaning_t,"year_vaccines","year")
                setnames(preg_vacc_y_meaning_t,"N","total_records")
                setnames(preg_vacc_y_meaning_t,"condition","stage_of_pregnancy")
                preg_vacc_y_meaning<-merge.data.table(preg_vacc_y_meaning,preg_vacc_y_meaning_t,all=T, by=c("stage_of_pregnancy","meaning", "year"))
                if(preg_vacc_y_meaning[,.N]>0){
                  preg_vaccines_meaning[[index]]<-preg_vacc_y_meaning
                }
                rm(preg_vacc_y_meaning,preg_vacc_y_meaning_t)
              }
              index<-index+1
              rm(vacc_file)
              gc()
            }
          }
          rm(vacc_files_to_search)
          
          ####combine results vaccines####
          #prevalenece by stage and year
          preg_vacc_prevalence_stage_total<-rbindlist(preg_vacc_prevalence_stage_total)
          if(preg_vacc_prevalence_stage_total[,.N]>0){
            preg_vacc_prevalence_stage_total<-preg_vacc_prevalence_stage_total[,lapply(.SD, sum), .SDcols=c("no_exposed_pregnancies"), by=c("stage_of_pregnancy","atc_code_4", "year")]
          }else{preg_vacc_prevalence_stage_total<-NULL}
          
          #prevalenece by year
          preg_vacc_prevalence_total<-rbindlist(preg_vacc_prevalence_total)
          if(preg_vacc_prevalence_total[,.N]>0){
            preg_vacc_prevalence_total<-preg_vacc_prevalence_total[,lapply(.SD, sum), .SDcols=c("no_exposed_pregnancies"), by=c("atc_code_4", "year")]
          }else{preg_vacc_prevalence_total<-NULL}
          
          #counts by stage, year, atc
          preg_vaccines_total<-rbindlist(preg_vaccines_total)
          if(preg_vaccines_total[,.N]>0){
            preg_vaccines_total<-preg_vaccines_total[,lapply(.SD, sum), .SDcols=c("no_records","total_records"), by=c("stage_of_pregnancy","year","atc_code_4")]
          }else{preg_vaccines_total<-NULL}
          
          preg_vaccines_meaning<-rbindlist(preg_vaccines_meaning)
          if(preg_vaccines_meaning[,.N]>0){
            preg_vaccines_meaning<-preg_vaccines_meaning[,lapply(.SD, sum), .SDcols=c("no_records","total_records"), by=c("stage_of_pregnancy","meaning", "year","atc_code_4")]
          }else{preg_vaccines_meaning<-NULL}
          
          preg_vacc_prior_dates<-rbindlist(preg_vacc_prior_dates)
          if(preg_vacc_prior_dates[,.N]>0){
            preg_vacc_prior_dates<-preg_vacc_prior_dates[,lapply(.SD, sum), .SDcols=c("excluded_records"), by=c("stage_of_pregnancy","year")]
          }else{preg_vacc_prior_dates<-NULL}
          
          duplicated_vacc_rec<-rbindlist(duplicated_vacc_rec)
          if(duplicated_vacc_rec[,.N]>0){
            duplicated_vacc_rec<-duplicated_vacc_rec[,lapply(.SD, sum), .SDcols=c("duplicated_rec"), by=c("stage_of_pregnancy", "vaccine_group")]
          }else{duplicated_vacc_rec<-NULL}
          
          #export vaccines counts
          if(!is.null(preg_vaccines_total)){saveRDS(preg_vaccines_total,paste0(poi_tmp,names(diagnoses_files)[condition_ind], "_preg_vacc_y.rds"))}
          rm(preg_vaccines_total)
          if(!is.null(preg_vaccines_meaning)){saveRDS(preg_vaccines_meaning,paste0(poi_tmp,names(diagnoses_files)[condition_ind], "_preg_vacc_ym.rds"))}
          rm(preg_vaccines_meaning)
          
          #combine the prevalence data
          #total by stage of pregnancy prevalence
          if(!is.null(preg_vacc_prevalence_stage_total)){
            preg_vacc_prevalence_total_s<-merge.data.table(preg_vacc_prevalence_stage_total,counts_preg_stage_y, by=c("stage_of_pregnancy","year"), all=T, allow.cartesian = T)
            preg_vacc_prevalence_total<-merge.data.table(preg_vacc_prevalence_total,counts_preg_y, by=c("year"), all=T, allow.cartesian = T)
            saveRDS(preg_vacc_prevalence_total_s,paste0(poi_tmp,names(pregnancy_files)[preg_ind], "_prevalence_preg_vacc_s.rds"))
            saveRDS(preg_vacc_prevalence_total,paste0(poi_tmp,names(pregnancy_files)[preg_ind], "_prevalence_preg_vacc_y.rds"))
          }else{
            preg_vacc_prevalence_total_s<-data.table(stage_of_pregnancy=names(pregnancy_files)[preg_ind],atc_code_4="N/A", year="N/A",no_exposed_pregnancies=0)
            preg_vacc_prevalence_total<-data.table(atc_code_4="N/A", counts_preg_y, no_exposed_pregnancies=0)
            saveRDS(preg_vacc_prevalence_total_s,paste0(poi_tmp,names(pregnancy_files)[preg_ind], "_prevalence_preg_vacc_s.rds"))
            saveRDS(preg_vacc_prevalence_total,paste0(poi_tmp,names(pregnancy_files)[preg_ind], "_prevalence_preg_vacc_y.rds"))
          }
          
          rm(preg_vacc_prevalence_total,preg_vacc_prevalence_total_s)
          
          #combine and export the excluded records
          if(!is.null(preg_vacc_prior_dates[,.N])){
            #remove empty cells
            preg_vacc_prior_dates<-preg_vacc_prior_dates[excluded_records!=0]
            saveRDS(preg_vacc_prior_dates,paste0(poi_tmp,names(pregnancy_files)[preg_ind], "_preg_vacc_prior_date_excluded.rds"))
          }
          rm(preg_vacc_prior_dates)
          
          #combine and export the excluded duplicated records
          if(!is.null(duplicated_vacc_rec[,.N])){
            #remove empty cells
            duplicated_vacc_rec<-duplicated_vacc_rec[duplicated_rec!=0]
            saveRDS(duplicated_vacc_rec,paste0(poi_tmp,names(pregnancy_files)[preg_ind], "_preg_vacc_duplicated_rec_excluded.rds"))
          }
          rm(duplicated_vacc_rec)
          
          
        }else{rm(vacc_files_to_search)}
        
      }
    }
  }
  
  #####Combine results for summary PREGNANCY#####
  summary_preg_files<-list.files(poi_tmp,"summary_preg_m.rds")
  if(length(summary_preg_files>0)){
    summary_preg<-lapply(paste0(poi_tmp,summary_preg_files), readRDS)
    summary_preg<-rbindlist(summary_preg,fill = T)
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
    
    rm(summary_preg)
  }
  
  #####Combine results for medicines use after a pregnancy record#####
  medicines_files<-length(list.files(poi_tmp,"preg_med_y|preg_med_ym|prevalence_preg_med_s|prevalence_preg_med_y|preg_med_prior_date_excluded"))
  
  if(medicines_files>0){
    ####Prevalence of exposed pregnancies by atc code_4 and year of pregnancy####
    if(length(list.files(poi_tmp,"prevalence_preg_med_y"))>0){ 
      prevalence_preg_med_y_fl<-list.files(poi_tmp, "prevalence_preg_med_y")#prevalence of medicines records by year
      prevalence_preg_med_y<-lapply(paste0(poi_tmp,prevalence_preg_med_y_fl), readRDS)
      prevalence_preg_med_y<-rbindlist(prevalence_preg_med_y,fill = T)
      #make sure there are no zeros in no_subjects
      prevalence_preg_med_y<-prevalence_preg_med_y[!is.na(no_pregnancies)]
      #replace NA in atc_code_4 with N/A
      prevalence_preg_med_y[is.na(atc_code_4), atc_code_4:="N/A"]
      #replace NA in no_users with 0
      prevalence_preg_med_y[is.na(no_exposed_pregnancies), no_exposed_pregnancies:=0]
      #apply sum to make sure all categories are combined
      prevalence_preg_med_y<-prevalence_preg_med_y[,lapply(.SD, sum), by=c("atc_code_4","year"), .SDcols=c("no_exposed_pregnancies","no_pregnancies")]
      #separate atc_code=="N/A"
      no_users_present<-prevalence_preg_med_y[atc_code_4=="N/A"]
      if(no_users_present[,.N]>0){
        no_users_present[,atc_code_4:=NULL][,no_exposed_pregnancies:=NULL]
        no_users_present[ , new := do.call(paste, c(.SD, sep = "_"))]
          atc_codes_to_add<-prevalence_preg_med_y[!duplicated(atc_code_4),atc_code_4]
          atc_codes_to_add<-atc_codes_to_add[!atc_codes_to_add %in% "N/A"]
          if(length(atc_codes_to_add)==0){
            a<-NULL
          }else{
            a<-as.data.table(expand.grid(no_users_present[,new],atc_codes_to_add))
            names(a)<-c("combine", "atc_code_4")
            a[,c("year","no_pregnancies"):=tstrsplit(combine,"_", fixed=T)]
            a[,combine:=NULL]
            a[,no_exposed_pregnancies:=0]
          }
          prevalence_preg_med_y<-prevalence_preg_med_y[!atc_code_4 %in% "N/A"]
          prevalence_preg_med_y<-as.data.table(rbind(prevalence_preg_med_y,a))
        rm(no_users_present,a)
      }else{rm(no_users_present)}
      prevalence_preg_med_y[,no_exposed_pregnancies:=as.numeric(no_exposed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
      
      #calculate prevalence
      prevalence_preg_med_y[,prevalence_medicine_use:=round((no_exposed_pregnancies/no_pregnancies)*100,1)]
      
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
      
      #apply masking
      prevalence_preg_med_y[, no_exposed_pregnancies:= as.character(no_exposed_pregnancies)][as.numeric(no_exposed_pregnancies) > 0 & as.numeric(no_exposed_pregnancies) < 5, no_exposed_pregnancies := "<5"]
      prevalence_preg_med_y[, no_pregnancies:= as.character(no_pregnancies)][as.numeric(no_pregnancies) > 0 & as.numeric(no_pregnancies) < 5, no_pregnancies := "<5"]
      prevalence_preg_med_y[, prevalence_medicine_use:= as.character(prevalence_medicine_use)][no_exposed_pregnancies=="<5" | no_pregnancies=="<5", prevalence_medicine_use := "N/A"]
      
      if(subpopulations_present=="Yes"){
        fwrite(prevalence_preg_med_y, paste0(poi_dir,subpopulations_names[s], "/PREGNANCY_MEDICINES/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_preg_med_prevalence_y_masked.csv"), row.names = F)
      } else {
        fwrite(prevalence_preg_med_y, paste0(poi_dir, "PREGNANCY_MEDICINES/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_med_prevalence_y_masked.csv"), row.names = F)
      }
      
      rm(prevalence_preg_med_y)
    }
    
    ####Prevalence of exposed pregnancies by stage of pregnancy,atc code_4 and year of pregnancy####
    if(length(list.files(poi_tmp,"prevalence_preg_med_s"))>0){ 
      prevalence_preg_med_s_fl<-list.files(poi_tmp, "prevalence_preg_med_s")#prevalence of medicines records by year and sex
      prevalence_preg_med_s<-lapply(paste0(poi_tmp,prevalence_preg_med_s_fl), readRDS)
      prevalence_preg_med_s<-rbindlist(prevalence_preg_med_s,fill = T)
      #make sure there are no zeros in no_subjects
      prevalence_preg_med_s<-prevalence_preg_med_s[!is.na(no_pregnancies)]
      #replace NA in atc_code_4 with N/A
      prevalence_preg_med_s[is.na(atc_code_4), atc_code_4:="N/A"]
      #replace NA in no_users with 0
      prevalence_preg_med_s[is.na(no_exposed_pregnancies), no_exposed_pregnancies:=0]
      #apply sum to make sure all categories are combined
      prevalence_preg_med_s<-prevalence_preg_med_s[,lapply(.SD, sum), by=c("stage_of_pregnancy","atc_code_4", "year"), .SDcols=c("no_exposed_pregnancies","no_pregnancies")]
      
      #separate atc_code=="N/A"
      no_users_present<-prevalence_preg_med_s[atc_code_4=="N/A"]
      if(no_users_present[,.N]>0){
        no_users_present[,atc_code_4:=NULL][,no_exposed_pregnancies:=NULL]
        no_users_present[ , new := do.call(paste, c(.SD, sep = "_"))]
        events<-no_users_present[!duplicated(stage_of_pregnancy),stage_of_pregnancy]
        list<-vector(mode="list", length=length(events))
        for(ls in 1:length(list)){
          atc_codes_to_add<-prevalence_preg_med_s[stage_of_pregnancy==events[ls]][!duplicated(atc_code_4),atc_code_4]
          atc_codes_to_add<-atc_codes_to_add[!atc_codes_to_add %in% "N/A"]
          if(length(atc_codes_to_add)==0){
            a<-NULL
          }else{
            a<-as.data.table(expand.grid(no_users_present[stage_of_pregnancy==events[ls],new],atc_codes_to_add))
            names(a)<-c("combine", "atc_code_4")
            a[,c("stage_of_pregnancy","year","no_pregnancies"):=tstrsplit(combine,"_", fixed=T)]
            a[,combine:=NULL]
          }
          list[[ls]]<-a
          rm(a)
        }
        list<-rbindlist(list)
        list[,no_exposed_pregnancies:=0]
        prevalence_preg_med_s<-prevalence_preg_med_s[!atc_code_4 %in% "N/A"]
        prevalence_preg_med_s<-as.data.table(rbind(prevalence_preg_med_s,list))
        rm(list,no_users_present)
      }else{rm(no_users_present)}
      prevalence_preg_med_s[,no_exposed_pregnancies:=as.numeric(no_exposed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
      
      #calculate prevalence
      prevalence_preg_med_s[,prevalence_medicine_use:=round((no_exposed_pregnancies/no_pregnancies)*100,1)]
      
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
      
      #apply masking
      prevalence_preg_med_s[, no_exposed_pregnancies:= as.character(no_exposed_pregnancies)][as.numeric(no_exposed_pregnancies) > 0 & as.numeric(no_exposed_pregnancies) < 5, no_exposed_pregnancies := "<5"]
      prevalence_preg_med_s[, no_pregnancies:= as.character(no_pregnancies)][as.numeric(no_pregnancies) > 0 & as.numeric(no_pregnancies) < 5, no_pregnancies := "<5"]
      prevalence_preg_med_s[, prevalence_medicine_use:= as.character(prevalence_medicine_use)][no_exposed_pregnancies=="<5" | no_pregnancies=="<5", prevalence_medicine_use := "N/A"]
      
      if(subpopulations_present=="Yes"){
        fwrite(prevalence_preg_med_s, paste0(poi_dir,subpopulations_names[s], "/PREGNANCY_MEDICINES/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_preg_med_prevalence_s_masked.csv"), row.names = F)
      } else {
        fwrite(prevalence_preg_med_s, paste0(poi_dir, "PREGNANCY_MEDICINES/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_med_prevalence_s_masked.csv"), row.names = F)
      }
      
      rm(prevalence_preg_med_s)
    }
    
    ####Counts of medicine records by year####
    if(length(list.files(poi_tmp,"preg_med_y"))>0){ 
      preg_med_y_fl<-list.files(poi_tmp, "preg_med_y")#counts of medicines records by year and sex
      preg_med_y<-lapply(paste0(poi_tmp,preg_med_y_fl), readRDS)
      preg_med_y<-rbindlist(preg_med_y,fill = T)
      #make sure there are no zeros in no_subjects
      preg_med_y<-preg_med_y[!is.na(total_records)]
      #apply sum to make sure all categories are combined
      preg_med_y<-preg_med_y[,lapply(.SD, sum), by=c("stage_of_pregnancy","year","atc_code_4"), .SDcols=c("no_records","total_records")]
      #calculate percentage
      preg_med_y[,percentage:=round((no_records/total_records)*100,1)]
      
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
      
      #apply masking
      preg_med_y[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
      preg_med_y[, total_records:= as.character(total_records)][as.numeric(total_records) > 0 & as.numeric(total_records) < 5, total_records := "<5"]
      preg_med_y[, percentage:= as.character(percentage)][no_records=="<5" | total_records=="<5", percentage := "N/A"]
      
      if(subpopulations_present=="Yes"){
        fwrite(preg_med_y, paste0(poi_dir,subpopulations_names[s], "/PREGNANCY_MEDICINES/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_preg_med_counts_ys_masked.csv"), row.names = F)
      } else {
        fwrite(preg_med_y, paste0(poi_dir, "PREGNANCY_MEDICINES/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_med_counts_ys_masked.csv"), row.names = F)
      }
      
      rm(preg_med_y)
    }
    ####Counts of medicine records by year and meaning####
    if(length(list.files(poi_tmp,"preg_med_ym"))>0){ 
      preg_med_ym_fl<-list.files(poi_tmp, "preg_med_ym")#counts of medicines records by year and sex
      preg_med_ym<-lapply(paste0(poi_tmp,preg_med_ym_fl), readRDS)
      preg_med_ym<-rbindlist(preg_med_ym,fill = T)
      #make sure there are no zeros in no_subjects
      preg_med_ym<-preg_med_ym[!is.na(total_records)]
      #apply sum to make sure all categories are combined
      preg_med_ym<-preg_med_ym[,lapply(.SD, sum), by=c("stage_of_pregnancy","meaning", "year","atc_code_4"), .SDcols=c("no_records","total_records")]
      #calculate percentage
      preg_med_ym[,percentage:=round((no_records/total_records)*100,1)]
      
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
      
      #apply masking
      preg_med_ym[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
      preg_med_ym[, total_records:= as.character(total_records)][as.numeric(total_records) > 0 & as.numeric(total_records) < 5, total_records := "<5"]
      preg_med_ym[, percentage:= as.character(percentage)][no_records=="<5" | total_records=="<5", percentage := "N/A"]
      
      if(subpopulations_present=="Yes"){
        fwrite(preg_med_ym, paste0(poi_dir,subpopulations_names[s], "/PREGNANCY_MEDICINES/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_preg_med_counts_ym_masked.csv"), row.names = F)
      } else {
        fwrite(preg_med_ym, paste0(poi_dir, "PREGNANCY_MEDICINES/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_med_counts_ym_masked.csv"), row.names = F)
      }
      
      rm(preg_med_ym)
    }
    ####Combine results for excluded records dating prior to preg####
    if(length(list.files(poi_tmp,"preg_med_prior_date_excluded"))>0){ 
      preg_med_prior_date_excluded_fl<-list.files(poi_tmp, "preg_med_prior_date_excluded")
      preg_med_prior_date_excluded<-lapply(paste0(poi_tmp,preg_med_prior_date_excluded_fl), readRDS)
      preg_med_prior_date_excluded<-rbindlist(preg_med_prior_date_excluded,fill = T)
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
      
      rm(preg_med_prior_date_excluded)
    }
  } 
  
  #####Combine results for vaccine use after a pregnancy record#####
  vaccines_files<-length(list.files(poi_tmp,"preg_vacc_y|preg_vacc_ym|prevalence_preg_vacc_s|prevalence_preg_vacc_y|preg_vacc_prior_date_excluded|vacc_duplicated_rec_excluded"))
  
  if(vaccines_files>0){
    ####Prevalence of exposed pregnancies by atc code_4 and year of pregnancy####
    if(length(list.files(poi_tmp,"prevalence_preg_vacc_y"))>0){ 
      prevalence_preg_vacc_y_fl<-list.files(poi_tmp, "prevalence_preg_vacc_y")#prevalence of medicines records by year
      prevalence_preg_vacc_y<-lapply(paste0(poi_tmp,prevalence_preg_vacc_y_fl), readRDS)
      prevalence_preg_vacc_y<-rbindlist(prevalence_preg_vacc_y,fill = T)
      #make sure there are no zeros in no_subjects
      prevalence_preg_vacc_y<-prevalence_preg_vacc_y[!is.na(no_pregnancies)]
      #replace NA in atc_code_4 with N/A
      prevalence_preg_vacc_y[is.na(atc_code_4), atc_code_4:="N/A"]
      #replace NA in no_users with 0
      prevalence_preg_vacc_y[is.na(no_exposed_pregnancies), no_exposed_pregnancies:=0]
      #apply sum to make sure all categories are combined
      prevalence_preg_vacc_y<-prevalence_preg_vacc_y[,lapply(.SD, sum), by=c("atc_code_4","year"), .SDcols=c("no_exposed_pregnancies","no_pregnancies")]
      #calculate prevalence
      prevalence_preg_vacc_y[,prevalence_medicine_use:=round((no_exposed_pregnancies/no_pregnancies)*100,1)]
      
      #remove all files in poi tmp
      for(i in 1:length(prevalence_preg_vacc_y_fl)){
        file.remove(paste0(poi_tmp, prevalence_preg_vacc_y_fl[i]))
      }
      rm(prevalence_preg_vacc_y_fl)
      #save results to g_output/PREGNANCY_VACCINES
      if(subpopulations_present=="Yes"){
        fwrite(prevalence_preg_vacc_y, paste0(poi_dir, subpopulations_names[s], "/PREGNANCY_VACCINES/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_vacc_prevalence_y.csv"), row.names = F)
      } else {
        fwrite(prevalence_preg_vacc_y, paste0(poi_dir,"PREGNANCY_VACCINES/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_prevalence_y.csv"), row.names = F)
      }
      
      #apply masking
      prevalence_preg_vacc_y[, no_exposed_pregnancies:= as.character(no_exposed_pregnancies)][as.numeric(no_exposed_pregnancies) > 0 & as.numeric(no_exposed_pregnancies) < 5, no_exposed_pregnancies := "<5"]
      prevalence_preg_vacc_y[, no_pregnancies:= as.character(no_pregnancies)][as.numeric(no_pregnancies) > 0 & as.numeric(no_pregnancies) < 5, no_pregnancies := "<5"]
      prevalence_preg_vacc_y[, prevalence_medicine_use:= as.character(prevalence_medicine_use)][no_exposed_pregnancies=="<5" | no_pregnancies=="<5", prevalence_medicine_use := "N/A"]
      
      if(subpopulations_present=="Yes"){
        fwrite(prevalence_preg_vacc_y, paste0(poi_dir,subpopulations_names[s], "/PREGNANCY_VACCINES/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_preg_vacc_prevalence_y_masked.csv"), row.names = F)
      } else {
        fwrite(prevalence_preg_vacc_y, paste0(poi_dir, "PREGNANCY_VACCINES/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_prevalence_y_masked.csv"), row.names = F)
      }
      
      rm(prevalence_preg_vacc_y)
    }
    
    ####Prevalence of exposed pregnancies by stage of pregnancy,atc code_4 and year of pregnancy####
    if(length(list.files(poi_tmp,"prevalence_preg_vacc_s"))>0){ 
      prevalence_preg_vacc_s_fl<-list.files(poi_tmp, "prevalence_preg_vacc_s")#prevalence of medicines records by year and sex
      prevalence_preg_vacc_s<-lapply(paste0(poi_tmp,prevalence_preg_vacc_s_fl), readRDS)
      prevalence_preg_vacc_s<-rbindlist(prevalence_preg_vacc_s,fill = T)
      #make sure there are no zeros in no_subjects
      prevalence_preg_vacc_s<-prevalence_preg_vacc_s[!is.na(no_pregnancies)]
      #replace NA in atc_code_4 with N/A
      prevalence_preg_vacc_s[is.na(atc_code_4), atc_code_4:="N/A"]
      #replace NA in no_users with 0
      prevalence_preg_vacc_s[is.na(no_exposed_pregnancies), no_exposed_pregnancies:=0]
      #apply sum to make sure all categories are combined
      prevalence_preg_vacc_s<-prevalence_preg_vacc_s[,lapply(.SD, sum), by=c("stage_of_pregnancy","atc_code_4", "year"), .SDcols=c("no_exposed_pregnancies","no_pregnancies")]
      #calculate prevalence
      prevalence_preg_vacc_s[,prevalence_medicine_use:=round((no_exposed_pregnancies/no_pregnancies)*100,1)]
      
      #remove all files in poi tmp
      for(i in 1:length(prevalence_preg_vacc_s_fl)){
        file.remove(paste0(poi_tmp, prevalence_preg_vacc_s_fl[i]))
      }
      rm(prevalence_preg_vacc_s_fl)
      #save results to g_output/PREGNANCY_VACCINES
      if(subpopulations_present=="Yes"){
        fwrite(prevalence_preg_vacc_s, paste0(poi_dir, subpopulations_names[s], "/PREGNANCY_VACCINES/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_vacc_prevalence_s.csv"), row.names = F)
      } else {
        fwrite(prevalence_preg_vacc_s, paste0(poi_dir,"PREGNANCY_VACCINES/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_prevalence_s.csv"), row.names = F)
      }
      
      #apply masking
      prevalence_preg_vacc_s[, no_exposed_pregnancies:= as.character(no_exposed_pregnancies)][as.numeric(no_exposed_pregnancies) > 0 & as.numeric(no_exposed_pregnancies) < 5, no_exposed_pregnancies := "<5"]
      prevalence_preg_vacc_s[, no_pregnancies:= as.character(no_pregnancies)][as.numeric(no_pregnancies) > 0 & as.numeric(no_pregnancies) < 5, no_pregnancies := "<5"]
      prevalence_preg_vacc_s[, prevalence_medicine_use:= as.character(prevalence_medicine_use)][no_exposed_pregnancies=="<5" | no_pregnancies=="<5", prevalence_medicine_use := "N/A"]
      
      if(subpopulations_present=="Yes"){
        fwrite(prevalence_preg_vacc_s, paste0(poi_dir,subpopulations_names[s], "/PREGNANCY_VACCINES/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_preg_vacc_prevalence_s_masked.csv"), row.names = F)
      } else {
        fwrite(prevalence_preg_vacc_s, paste0(poi_dir, "PREGNANCY_VACCINES/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_prevalence_s_masked.csv"), row.names = F)
      }
      
      rm(prevalence_preg_vacc_s)
    }
    
    ####Counts of vaccine records by year####
    if(length(list.files(poi_tmp,"preg_vacc_y"))>0){ 
      preg_vacc_y_fl<-list.files(poi_tmp, "preg_vacc_y")#counts of medicines records by year and sex
      preg_vacc_y<-lapply(paste0(poi_tmp,preg_vacc_y_fl), readRDS)
      preg_vacc_y<-rbindlist(preg_vacc_y,fill = T)
      #make sure there are no zeros in no_subjects
      preg_vacc_y<-preg_vacc_y[!is.na(total_records)]
      #apply sum to make sure all categories are combined
      preg_vacc_y<-preg_vacc_y[,lapply(.SD, sum), by=c("stage_of_pregnancy","year","atc_code_4"), .SDcols=c("no_records","total_records")]
      #calculate percentage
      preg_vacc_y[,percentage:=round((no_records/total_records)*100,1)]
      
      #remove all files in poi tmp
      for(i in 1:length(preg_vacc_y_fl)){
        file.remove(paste0(poi_tmp, preg_vacc_y_fl[i]))
      }
      rm(preg_vacc_y_fl)
      #save results to g_output/PREGNANCY_VACCINES
      if(subpopulations_present=="Yes"){
        fwrite(preg_vacc_y, paste0(poi_dir, subpopulations_names[s], "/PREGNANCY_VACCINES/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_vacc_counts_ys.csv"), row.names = F)
      } else {
        fwrite(preg_vacc_y, paste0(poi_dir,"PREGNANCY_VACCINES/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_counts_ys.csv"), row.names = F)
      }
      
      #apply masking
      preg_vacc_y[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
      preg_vacc_y[, total_records:= as.character(total_records)][as.numeric(total_records) > 0 & as.numeric(total_records) < 5, total_records := "<5"]
      preg_vacc_y[, percentage:= as.character(percentage)][no_records=="<5" | total_records=="<5", percentage := "N/A"]
      
      if(subpopulations_present=="Yes"){
        fwrite(preg_vacc_y, paste0(poi_dir,subpopulations_names[s], "/PREGNANCY_VACCINES/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_preg_vacc_counts_ys_masked.csv"), row.names = F)
      } else {
        fwrite(preg_vacc_y, paste0(poi_dir, "PREGNANCY_VACCINES/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_counts_ys_masked.csv"), row.names = F)
      }
      
      rm(preg_vacc_y)
    }
    ####Counts of vaccine records by year and meaning####
    if(length(list.files(poi_tmp,"preg_vacc_ym"))>0){ 
      preg_vacc_ym_fl<-list.files(poi_tmp, "preg_vacc_ym")#counts of medicines records by year and sex
      preg_vacc_ym<-lapply(paste0(poi_tmp,preg_vacc_ym_fl), readRDS)
      preg_vacc_ym<-rbindlist(preg_vacc_ym,fill = T)
      #make sure there are no zeros in no_subjects
      preg_vacc_ym<-preg_vacc_ym[!is.na(total_records)]
      #apply sum to make sure all categories are combined
      preg_vacc_ym<-preg_vacc_ym[,lapply(.SD, sum), by=c("stage_of_pregnancy","meaning", "year","atc_code_4"), .SDcols=c("no_records","total_records")]
      #calculate percentage
      preg_vacc_ym[,percentage:=round((no_records/total_records)*100,1)]
      
      #remove all files in poi tmp
      for(i in 1:length(preg_vacc_ym_fl)){
        file.remove(paste0(poi_tmp, preg_vacc_ym_fl[i]))
      }
      rm(preg_vacc_ym_fl)
      #save results to g_output/PREGNANCY_VACCINES
      if(subpopulations_present=="Yes"){
        fwrite(preg_vacc_ym, paste0(poi_dir, subpopulations_names[s], "/PREGNANCY_VACCINES/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_preg_vacc_counts_ym.csv"), row.names = F)
      } else {
        fwrite(preg_vacc_ym, paste0(poi_dir,"PREGNANCY_VACCINES/", format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_counts_ym.csv"), row.names = F)
      }
      
      #apply masking
      preg_vacc_ym[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
      preg_vacc_ym[, total_records:= as.character(total_records)][as.numeric(total_records) > 0 & as.numeric(total_records) < 5, total_records := "<5"]
      preg_vacc_ym[, percentage:= as.character(percentage)][no_records=="<5" | total_records=="<5", percentage := "N/A"]
      
      if(subpopulations_present=="Yes"){
        fwrite(preg_vacc_ym, paste0(poi_dir,subpopulations_names[s], "/PREGNANCY_VACCINES/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_preg_vacc_counts_ym_masked.csv"), row.names = F)
      } else {
        fwrite(preg_vacc_ym, paste0(poi_dir, "PREGNANCY_VACCINES/Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name, "_preg_vacc_counts_ym_masked.csv"), row.names = F)
      }
      
      rm(preg_vacc_ym)
    }
    ####Combine results for excluded records dating prior to preg####
    if(length(list.files(poi_tmp,"preg_vacc_prior_date_excluded"))>0){ 
      preg_vacc_prior_date_excluded_fl<-list.files(poi_tmp, "preg_vacc_prior_date_excluded")
      preg_vacc_prior_date_excluded<-lapply(paste0(poi_tmp,preg_vacc_prior_date_excluded_fl), readRDS)
      preg_vacc_prior_date_excluded<-rbindlist(preg_vacc_prior_date_excluded,fill = T)
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
      
      rm(preg_vacc_prior_date_excluded)
    }
    ####Combine results for duplicated vaccine records####
    if(length(list.files(poi_tmp,"vacc_duplicated_rec_excluded"))>0){ 
      vaccines_duplicated_rec_excluded_fl<-list.files(poi_tmp, "vacc_duplicated_rec_excluded")
      vaccines_duplicated_rec_excluded<-lapply(paste0(poi_tmp,vaccines_duplicated_rec_excluded_fl), readRDS)
      vaccines_duplicated_rec_excluded<-rbindlist(vaccines_duplicated_rec_excluded,fill = T)
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
      
      rm(vaccines_duplicated_rec_excluded)
    } 
    
  } 
}

cat(green(print("The script pregnancy primary is finished.")))