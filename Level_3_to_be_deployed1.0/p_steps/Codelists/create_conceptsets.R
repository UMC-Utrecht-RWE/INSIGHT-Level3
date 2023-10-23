#Author: Vjola Hoxhaj Drs.
#email: v.hoxhaj@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 06/07/2021

`%!in%` = Negate(`%in%`)

####Read excel file####
parameter_file_fl<-list.files(paste0(projectFolder,"/p_parameters/"),"study_parameters")
parameter_file<-as.data.table(read_excel(paste0(projectFolder,"/p_parameters/",parameter_file_fl),col_types = "text", sheet = "study_parameters"))
#Set parameters basic parameters
study_name_codelist <- parameter_file[variable=="study_name_codelist",value]
recurrent_event_analysis<-parameter_file[variable=="recurrent_event_analysis",value]

codelist_file<-list.files(paste0(pre_dir,"Codelists/"), "full_codelist")
#codelist_file_study<-codelist_file[str_detect(codelist_file,study_name_codelist)]

if(study_name_codelist=="ALL"){
  codelist<-fread(paste0(pre_dir,"Codelists/",codelist_file), colClasses = "character")
  codelist[,event_definition:=gsub("-"," ",event_definition)]
  codelist[,event_definition:=gsub(","," ",event_definition)]
}
if(study_name_codelist=="Conception"){
  codelist<-fread(paste0(pre_dir,"Codelists/",codelist_file), colClasses = "character")
  codelist[,event_definition:=gsub("-"," ",event_definition)]
  codelist[,event_definition:=gsub(","," ",event_definition)]
  conception_file_fl<-list.files(paste0(projectFolder,"/p_steps/parameters/"),"conception_event_definition")
  conception_file<-as.data.table(read_excel(paste0(projectFolder,"/p_steps/parameters/",conception_file_fl),col_types = "text"))
  codelist[,variable_name:=paste(system,event_abbreviation,type,sep="_")]
  codelist<-codelist[variable_name %in% conception_file[,variable_name]]
  codelist[variable_name=="O_NEUROPATHICPAIN_COV", event_definition:="Pain"]
  codelist[variable_name=="O_CANCERPAIN_COV", event_definition:="Pain"]
  codelist[variable_name=="O_CHRONICPAIN_COV", event_definition:="Pain"]
  codelist[,variable_name:=NULL]
  }
if(study_name_codelist !="ALL" & study_name_codelist !="Conception"){
  codelist<-fread(paste0(pre_dir,"Codelists/",codelist_file), colClasses = "character")
  codelist[,event_definition:=gsub("-"," ",event_definition)]
  codelist[,event_definition:=gsub(","," ",event_definition)]
  
}

#select only necessary columns
codelist<-codelist[,c("event_definition", "coding_system", "code")]
codelist<-codelist[,coding_system:=gsub("/","",coding_system)]
#remove all special characters from the code variable
codelist<-codelist[,code:=gsub("[^[:alnum:]]", "", code)]
codelist<-codelist[,code:=iconv(code, "UTF-8", "UTF-8",sub='')]


#remove duplicates
codelist[,comb:=paste(event_definition,coding_system,code,sep="^")]
codelist<-codelist[!duplicated(comb)]
codelist[,comb:=NULL]


conditions_vocabularies<-codelist[!duplicated(coding_system),coding_system]
conditions_to_start_with<-c(conditions_vocabularies[str_detect(conditions_vocabularies, "^ICD")], 
                            conditions_vocabularies[str_detect(conditions_vocabularies, "^ICPC")], 
                            conditions_vocabularies[str_detect(conditions_vocabularies, "^MTHICD")])
conditions_rcd<-conditions_vocabularies[str_detect(conditions_vocabularies, "^RCD")]
conditions_snomed_codes<-conditions_vocabularies[str_detect(conditions_vocabularies, "^SNOMED")]
conditions_other_codes<-conditions_vocabularies[!(conditions_vocabularies %in% c(conditions_to_start_with,conditions_rcd,conditions_snomed_codes))]

#remove dots for read codes
codelist<-codelist[coding_system %in% conditions_rcd, code:=str_replace_all(code,"[.]","")]
while (codelist[coding_system %in% conditions_rcd & str_detect(code,"[.]"),.N]>0){
  codelist<-codelist[coding_system %in% conditions_rcd, code:=str_replace_all(code,"[.]","")]
}

#Create variable dot_present
codelist[,dot_present:=str_detect(codelist[,code],"\\.")]
#Create variable code_no_dot by removing dot from all codes
codelist[,code_no_dot:=gsub("\\.","",code)]
vocabularies_list<-codelist[!duplicated(coding_system), coding_system]
#put all information in a list
conditions<-vector(mode="list", length=length(unique(na.omit(codelist[,event_definition]))))
names(conditions)<-unique(na.omit(codelist[,event_definition]))
for (i in 1:length(conditions)){
  vocabularies<-vector(mode="list", length=length(unique(na.omit(codelist[,coding_system]))))
  names(vocabularies)<-unique(na.omit(codelist[,coding_system]))
  for (j in 1:length(vocabularies)){
    vocabularies[[j]]<-codelist[event_definition==names(conditions)[i] & coding_system==names(vocabularies)[j], code]
  }
  conditions[[i]]<-list.append(conditions[[i]],vocabularies)
  rm(vocabularies)
}

#remove empty vocabularies
conditions<-lapply(conditions, function(x) Filter(length, x))

#################################################################################################################
#Rule: start with
#Coding system: ICD9, ICD9CM, ICD10, ICD10CM, ICPC
#################################################################################################################
#vocabularies that will be filtered with start with
conditions_start<-list()
for(i in 1:length(conditions)){
  conditions_start[[i]]<-conditions[[i]][names(conditions[[i]]) %in% conditions_to_start_with]
}

names(conditions_start)<-names(conditions)

for(i in 1:length(conditions_start)){
  lapply(conditions_start[[i]], function(x) x[names(x) %in% c("code")])
}
conditions_start<-lapply(conditions_start, function(x) Filter(length, x))
conditions_start<-Filter(function(k) length(k)>0, conditions_start)
################################################################################################################
#Rule:Remove dot, start with
#Coding system: Read codes v2
###############################################################################################################
conditions_read<-list()

for(i in 1:length(conditions)){
  conditions_read[[i]]<-conditions[[i]][names(conditions[[i]]) %in% conditions_rcd]
}

names(conditions_read)<-names(conditions)
conditions_read<-lapply(conditions_read, function(x) Filter(length, x))
conditions_read<-Filter(function(k) length(k)>0, conditions_read)
################################################################################################################
#Rule: match exactly
#Coding system: SNOMEDCT_US
#################################################################################################################
#SNOMED codes
conditions_snomed<-list()
for(i in 1:length(conditions)){
  conditions_snomed[[i]]<-conditions[[i]][names(conditions[[i]]) %in% conditions_snomed_codes]
}
names(conditions_snomed)<-names(conditions)

conditions_snomed<-lapply(conditions_snomed, function(x) Filter(length, x))
conditions_snomed<-Filter(function(k) length(k)>0, conditions_snomed)
################################################################################################################
#Rule: match exactly
#Coding system: other codes
#################################################################################################################
#other codes
conditions_other<-list()
for(i in 1:length(conditions)){
  conditions_other[[i]]<-conditions[[i]][names(conditions[[i]]) %in% conditions_other_codes]
}
names(conditions_other)<-names(conditions)

conditions_other<-lapply(conditions_other, function(x) Filter(length, x))
conditions_other<-Filter(function(k) length(k)>0, conditions_other)
################################################################################################################
#output folder for Info report in g_output
if ("Info" %in% list.files(output_dir)){
  info_dir<-paste(output_dir, "Info/",sep="")
  do.call(file.remove, list(list.files(info_dir, full.names = T)))
} else {
  #Create the Info folder in the output dir
  dir.create(paste(output_dir, "Info", sep=""))
  info_dir<-paste(output_dir, "Info/", sep="")
}

codelist[,comb:=paste(event_definition, coding_system,"_")]
codelist[,dot_present:=NULL][,code_no_dot:=NULL]
codelist[,codes:=paste0(code, collapse = ", "), by="comb"]
codelist<-codelist[!duplicated(comb)]
codelist[,code:=NULL][,comb:=NULL]
codelist<-codelist[order(event_definition,coding_system)]

write.csv(codelist, paste0(info_dir, "data_characterisation_codelist.csv"), row.names = F)
rm(codelist)
