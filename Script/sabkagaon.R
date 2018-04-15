library(dplyr)
library(stringr)
##Merging Gaon Vikas 


Sabka_Gaon <- read.csv(file = "E:\\Google Drive\\R\\Raw_Data\\Raw_Files\\Sabka_Gaon.csv", header = TRUE,sep = ",",stringsAsFactors = FALSE)
Sabka_Gaon <- rename(Sabka_Gaon, vill_census_code = Village.Code)


NIC_Master <- read.csv(file = "E:\\Google Drive\\R\\Raw_Data\\Raw_Files\\NIC_Geog_Master\\NIC_Geog_Master.csv", header = TRUE,sep = ",")
NIC_Master_vill_Census <- filter(NIC_Master, vill_census_code != "NA")

NIC_Master_vill_Census <- distinct(NIC_Master, vill_census_code, .keep_all = TRUE)




NIC_Master_vill_Census$vill_census_code <- as.integer(as.character(NIC_Master_vill_Census$vill_census_code))
NIC_Master_vill_Census <- filter(NIC_Master_vill_Census, vill_census_code != "NA")
NIC_Master_vill_Census <- distinct(NIC_Master_vill_Census,vill_census_code, .keep_all = TRUE)


Sabka_Gaon_exact <- left_join(Sabka_Gaon,NIC_Master_vill_Census, by = "vill_census_code")
Sabka_Gaon_exact <- filter(Sabka_Gaon_exact, village_code != "NA")
Sabka_Gaon_exact$flag <- "census_code"
Sabka_Gaon_fuzzy <- anti_join(Sabka_Gaon,Sabka_Gaon_exact, by = "vill_census_code")



##By NAme 




Sabka_Gaon_fuzzy$name_concat <- str_to_lower(paste0(str_trim(Sabka_Gaon_fuzzy$State.Name),str_trim(Sabka_Gaon_fuzzy$District.Name),str_trim(Sabka_Gaon_fuzzy$Sub.District.Name),str_trim(Sabka_Gaon_fuzzy$Village.Name)))
NIC_Master$name_concat  <-str_to_lower(paste0(str_trim(NIC_Master$state_name),str_trim(NIC_Master$district_name),str_trim(NIC_Master$block_name),str_trim(NIC_Master$village_name)))



Sabka_Gaon_merge <- left_join(Sabka_Gaon_fuzzy,NIC_Master,"name_concat")

Sabka_Gaon_merge_distinct <- distinct(Sabka_Gaon_merge, vill_census_code.x, .keep_all = TRUE)

Sabka_Gaon_merge_distinct <- filter(Sabka_Gaon_merge_distinct, state_name != "NA")

temp1 <- Sabka_Gaon_merge_distinct

temp1 <- select(temp1, - vill_census_code.y,-name_concat)

master_temp <- rbind(Sabka_Gaon_exact,setNames(temp1, names(Sabka_Gaon_exact)))


Sabka_Gaon_fuzzy <- anti_join(Sabka_Gaon, master_temp, by = "vill_census_code")


NIC_Master$name_concat  <- str_trim(str_to_lower(paste0(NIC_Master$state_name,NIC_Master$block_name,NIC_Master$village_name)))
Sabka_Gaon_fuzzy$name_concat <- str_trim(str_to_lower(paste0(Sabka_Gaon_fuzzy$State.Name,Sabka_Gaon_fuzzy$Sub.District.Name,Sabka_Gaon_fuzzy$Village.Name)))

Sabka_Gaon_merge <- left_join(Sabka_Gaon_fuzzy,NIC_Master,"name_concat")

Sabka_Gaon_merge_distinct <- distinct(Sabka_Gaon_merge, vill_census_code.x, .keep_all = TRUE)

Sabka_Gaon_merge_distinct <- filter(Sabka_Gaon_merge_distinct, state_name != "NA")

Sabka_Gaon_merge_dup <- anti_join(Sabka_Gaon_merge,Sabka_Gaon_merge_distinct, by = "vill_census_code.x")



