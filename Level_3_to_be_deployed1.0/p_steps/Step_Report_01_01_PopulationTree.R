
#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021

std_source_pop_dir<-paste0(projectFolder,"/g_output/STUDY_SOURCE_POPULATION/")
std_source_pop_dir_rds<-paste0(std_source_pop_dir,"RDS/")
#SOURCE_POPULATION <- readRDS(file = paste0(std_pop_tmp,"ALL_source_population.rds"))[age_start_study >= 0 & sex_at_instance_creation %in% c("F","M"),.(person_id,sex_at_instance_creation,age_start_study)]
SOURCE_POPULATION <- readRDS(file = paste0(std_pop_tmp,"ALL_source_population.rds"))[sex_at_instance_creation %in% c("F","M"),.(person_id,sex_at_instance_creation,age_start_study)]
SOURCE_POPULATION[age_start_study<0,age_start_study:=-1]

Agebands <- CreateBands(seq(from = 0, to = 125, by = 5))
#Add not born yet category
not_born<-data.table(band="-1",INT=-1,Order=-1)
Agebands<-rbind(not_born,Agebands)

SOURCE_POPULATION <- merge(x = SOURCE_POPULATION, y = Agebands,by.x = "age_start_study", by.y = "INT", all.x = F, all.y = F)
setorder(SOURCE_POPULATION,Order)

if(nrow(SOURCE_POPULATION) > 0){

TEMP <- INPUTMATRIX(
  d = SOURCE_POPULATION,
  value = "person_id",
  type = "count",
  var = "band",
  var.v = unique(SOURCE_POPULATION[["band"]]),
  cat = "sex_at_instance_creation",
  cat.v = c("F","M"),
  per = T
)

TEMP2 <- INPUTMATRIX(
  d = SOURCE_POPULATION,
  value = "person_id",
  type = "count",
  var = "band",
  var.v = unique(SOURCE_POPULATION[["band"]]),
  cat = "sex_at_instance_creation",
  cat.v = c("F","M"),
  per = F
)

}else{
  TEMP <- matrix(NA, nrow = 0, ncol = length(unique(Agebands[["band"]])), dimnames = list(c(),unique(Agebands[["band"]])))
  TEMP2 <- matrix(NA, nrow = 0, ncol = length(unique(Agebands[["band"]])), dimnames = list(c(),unique(Agebands[["band"]])))
  
  }

saveRDS(TEMP, file = paste0(std_source_pop_dir_rds,"R_01_01_POPTREE.rds"))
fwrite(as.data.table(TEMP2, keep.rownames = T), file = paste0(std_source_pop_dir,"R_01_01_POPTREE.csv"),sep = ";" )

rm(TEMP,Agebands,SOURCE_POPULATION)







