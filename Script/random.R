library(RPostgreSQL)
library(dplyr)


drv <- dbDriver("PostgreSQL")
db <- "Village_Master"  
host_db <- "localhost"
db_port <- "5432"
db_user <- "postgres"
db_password <- '123456'


con <- dbConnect(drv, dbname=db, host=host_db, port=db_port, user=db_user, password=db_password)


state_mst <- dbGetQuery(con, "select * from mst_state")
dist_mst <- dbGetQuery(con, "select * from mst_district")
block_mst <- dbGetQuery(con, "select * from mst_block")
GP_mst <- dbGetQuery(con, "select * from mst_grampanchayat")
village_mst <- dbGetQuery(con, "select * from mst_village")

state_mst_edit <- select(state_mst,state_name,state_code,census2011)
state_mst_edit <- rename(state_mst_edit, state_census_code = census2011)

dist_mst_edit <- select(dist_mst,state_code,district_name,district_code,census2011)
dist_mst_edit <- rename(dist_mst_edit, dist_census_code = census2011)

master <- left_join(dist_mst_edit,state_mst_edit,by = "state_code")


block_mst_edit <- select(block_mst,district_code,block_name,block_code,census2011)
block_mst_edit <- rename(block_mst_edit, block_census_code = census2011)

master <- left_join(block_mst_edit,master,by = "district_code")

GP_mst_edit <- select(GP_mst,block_code,grampanchayat_name,grampanchayat_code)

master <- left_join(GP_mst_edit,master,by = "block_code")


vill_mst_edit <- select(village_mst,grampanchayat_code,village_name,village_code,census2011)
vill_mst_edit <- rename(vill_mst_edit, vill_census_code = census2011)

master <- left_join(vill_mst_edit, master, by = "grampanchayat_code")

NIC_geog_master <- select (master,state_name,state_code,state_census_code,district_name,district_code,dist_census_code,block_name,block_code,block_census_code,grampanchayat_name,grampanchayat_code,village_name,village_code,vill_census_code) 

write.csv(NIC_geog_master,"E:\\Google Drive\\R\\Raw_Data\\Raw_Files\\NIC_Geog_Master\\NIC_Geog_Master.csv")



