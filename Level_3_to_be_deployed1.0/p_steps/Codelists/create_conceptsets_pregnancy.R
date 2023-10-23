#####Pregnancy codelist####
info_dir<-paste(output_dir, "Info/",sep="")
#load codelist
####Read excel file####
codelist_preg_file<-list.files(paste0(pre_dir,"Codelists/"), "pregnancyL3_codelist")
#codelist_file_study<-codelist_file[str_detect(codelist_file,study_name_codelist)]
codelist_pregnancy<-fread(paste0(pre_dir,"Codelists/",codelist_preg_file), colClasses = "character")#remove "/" if present
codelist_pregnancy[,coding_system:=gsub("/","",coding_system)]
setnames(codelist_pregnancy,"event_definition","original_event_definition")
setnames(codelist_pregnancy,"conception_categories","event_definition")

#Create variable dot_present
codelist_pregnancy[,dot_present:=str_detect(codelist_pregnancy[,code],"[.]")]
#remove duplicates
codelist_pregnancy[,comb:=paste(event_definition,coding_system,code,sep="^")]
codelist_pregnancy<-codelist_pregnancy[!duplicated(comb)]
codelist_pregnancy[,comb:=NULL]
codelist_pregnancy[,code_original:=code]
codelist_pregnancy[,code:=as.character(gsub("\\.","", code))]

vocabularies_list_pregnancy<-codelist_pregnancy[!duplicated(coding_system), coding_system]

pregnancy_to_start_with<-c(vocabularies_list_pregnancy[str_detect(vocabularies_list_pregnancy, "^ICD")], 
                           vocabularies_list_pregnancy[str_detect(vocabularies_list_pregnancy, "^ICPC")], 
                           vocabularies_list_pregnancy[str_detect(vocabularies_list_pregnancy, "^MTHICD")])
pregnancy_rcd<-vocabularies_list_pregnancy[str_detect(vocabularies_list_pregnancy, "^RCD")]
pregnancy_snomed_codes<-vocabularies_list_pregnancy[str_detect(vocabularies_list_pregnancy, "^SNOMED")]
pregnancy_other_codes<-vocabularies_list_pregnancy[!(vocabularies_list_pregnancy %in% c(pregnancy_to_start_with,pregnancy_rcd,pregnancy_snomed_codes))]

#Create variable code_no_dot by removing dot from all codes
#remove dots for read codes

codelist_pregnancy<-codelist_pregnancy[coding_system %in% pregnancy_rcd, code:=str_replace_all(code,"[.]","")]
while (codelist_pregnancy[coding_system %in% pregnancy_rcd & str_detect(code,"[.]"),.N]>0){
  codelist_pregnancy<-codelist_pregnancy[coding_system %in% pregnancy_rcd, code:=str_replace_all(code,"[.]","")]
}

#put all information in a list
stage_pregnancy<-vector(mode="list", length=length(unique(na.omit(codelist_pregnancy[,event_definition]))))
names(stage_pregnancy)<-unique(na.omit(codelist_pregnancy[,event_definition]))
for (i in 1:length(stage_pregnancy)){
  vocabularies_pregnancy<-vector(mode="list", length=length(unique(na.omit(codelist_pregnancy[,coding_system]))))
  names(vocabularies_pregnancy)<-unique(na.omit(codelist_pregnancy[,coding_system]))
  for (j in 1:length(vocabularies_pregnancy)){
    vocabularies_pregnancy[[j]]<-codelist_pregnancy[event_definition==names(stage_pregnancy)[i] & coding_system==names(vocabularies_pregnancy)[j], code]
  }
  stage_pregnancy[[i]]<-list.append(stage_pregnancy[[i]],vocabularies_pregnancy)
  rm(vocabularies_pregnancy)
}

#remove empty vocabularies
stage_pregnancy<-lapply(stage_pregnancy, function(x) Filter(length, x))

#################################################################################################################
#Rule: start with
#Coding system: ICD9CM, ICD10CM, ICPC2P
#################################################################################################################
#vocabularies that will be filtered with start with
stage_pregnancy_start<-list()
for(i in 1:length(stage_pregnancy)){
  stage_pregnancy_start[[i]]<-stage_pregnancy[[i]][names(stage_pregnancy[[i]]) %in% pregnancy_to_start_with]
}

names(stage_pregnancy_start)<-names(stage_pregnancy)

for(i in 1:length(stage_pregnancy_start)){
  lapply(stage_pregnancy_start[[i]], function(x) x[names(x) %in% c("code")])
}
stage_pregnancy_start<-lapply(stage_pregnancy_start, function(x) Filter(length, x))
################################################################################################################
#Rule:Remove dot, start with
#Coding system: RCD, RCD2
###############################################################################################################
stage_pregnancy_read<-list()
for(i in 1:length(stage_pregnancy)){
  stage_pregnancy_read[[i]]<-stage_pregnancy[[i]][names(stage_pregnancy[[i]]) %in% pregnancy_rcd]
}
names(stage_pregnancy_read)<-names(stage_pregnancy)

for(i in 1:length(stage_pregnancy_read)){
  lapply(stage_pregnancy_read[[i]], function(x) x[names(x) %in% c("code")])
}
stage_pregnancy_read<-lapply(stage_pregnancy_read, function(x) Filter(length, x))

################################################################################################################
#Rule: match exactly
#Coding system: SNOMEDCT_US
#################################################################################################################
#SNOMED codes
stage_pregnancy_snomed<-list()
for(i in 1:length(stage_pregnancy)){
  stage_pregnancy_snomed[[i]]<-stage_pregnancy[[i]][names(stage_pregnancy[[i]]) %in% pregnancy_snomed_codes]
}
names(stage_pregnancy_snomed)<-names(stage_pregnancy)
stage_pregnancy_snomed<-lapply(stage_pregnancy_snomed, function(x) Filter(length, x))
################################################################################################################
#Rule: match exactly
#Coding system: other codes
#################################################################################################################
#other codes
stage_pregnancy_other<-list()
for(i in 1:length(stage_pregnancy)){
  stage_pregnancy_other[[i]]<-stage_pregnancy[[i]][names(stage_pregnancy[[i]]) %in% pregnancy_other_codes]
}
names(stage_pregnancy_other)<-names(stage_pregnancy)
stage_pregnancy_other<-lapply(stage_pregnancy_other, function(x) Filter(length, x))
################################################################################################################

codelist_pregnancy[,dot_present:=NULL][,code_issue_dot:=NULL]
#setnames(codelist_pregnancy, "code_original", "code")

write.csv(codelist_pregnancy, paste0(info_dir, "data_characterisation_codelist_pregnancy.csv"), row.names = F)
rm(codelist_pregnancy)


if(subpopulations_present=="Yes"){
  subpop_names_export<-data.table(DAP=data_access_provider_name, data_source=data_source_name, subpopulations_names=subpopulations_names)
  write.csv(subpop_names_export, paste0(info_dir, "subpopulations_names.csv"), row.names = F)
}