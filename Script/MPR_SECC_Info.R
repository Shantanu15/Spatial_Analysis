library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)


#Path for Raw Data File --Change it to the location of your file
MPR_Data <- read.csv("E:\\Google Drive\\R\\Maps\\Data\\MPR_Raw_070418.csv",header = TRUE, sep=",", stringsAsFactors = FALSE)

#Aggregating MPR Data
MPR_Data_Agg <- MPR_Data %>%
  group_by(state_name,
           district_name) %>%
  
  summarize(
    no_blocks = n(),
    earliest_block_date = min(progress_date),
    GP_Entered = sum(no_gp_intensivestrategy_in_progress),
    Villages_Entered = sum(no_village_intensivestrategy_in_progress),
    New_SHG = sum(new_shg_promoted_nrlmtargethousehold),
    Other_SHG = sum(pre_nrlm_shg_nrlmfold),
    Defunct_SHG = sum(no_shg_defaunct),
    SC_HH = sum(sc_member_mobilized_in_shg),
    ST_HH = sum(st_member_mobilized_in_shg),
    Min_HH = sum(minority_member_mobilized_in_shg),
    PwD_HH = sum(pwd_member_mobilized_in_shg),
    Other_HH = sum(other_member_mobilized_in_shg),
    Eligible_Bank_Ac = sum(no_new_shg_three_month_old, pre_nrlm_shg_nrlmfold),
    SHG_Bank_Ac = sum(no_new_shg_three_month_old_bank_account,no_pre_nrlm_shg_bank_account),
    New_SHG_eligible_RF = sum(no_new_shg_three_month_old_bank_account),
    Old_SHG_eligible_RF = sum (no_pre_nrlm_shg_eligible_for_rf),
    SC_SHG_RF = sum(no_sc_shg_provided_rf),
    ST_SHG_RF = sum(no_st_shg_provided_rf),
    Min_SHG_RF = sum(no_minority_shg_provided_rf),
    Other_SHG_RF = sum(no_other_shg_provided_rf),
    PWD_SHG_RF = sum(no_pwd_shg_provided_rf),
    Amt_SC_RF = sum(amount_of_rf_provided_to_sc_shg_rf),
    Amt_ST_RF = sum(amount_of_rf_provided_to_st_shg_rf),
    Amt_Min_RF = sum(amount_of_rf_provided_to_minority_shg_rf),
    Amt_Other_RF = sum(amount_of_rf_provided_to_other_shg_rf),
    Amt_PwD_RF = sum(amount_of_rf_provided_to_pwd_shg_rf),
    SC_SHG_CIF = sum(no_sc_shg_provided_cif_vrf),
    ST_SHG_CIF = sum(no_st_shg_provided_cif_vrf),
    Min_SHG_CIF = sum(no_minority_shg_provided_cif_vrf),
    Other_SHG_CIF = sum(no_other_shg_provided_cif_vrf),
    Pwd_SHG_CIF = sum(no_pwd_shg_provided_cif_vrf),
    Amt_SC_CIF = sum(amount_of_cif_vrf_provided_to_sc_shg_rf),
    Amt_ST_CIF = sum(amount_of_cif_vrf_provided_to_st_shg_rf),
    Amt_Min_CIF = sum(amount_of_cif_vrf_provided_to_minority_shg_rf),
    Amt_Other_CIF = sum(amount_of_cif_vrf_provided_to_other_shg_rf),
    Amt_PwD_CIF = sum(amount_of_cif_vrf_provided_to_pwd_shg_rf),
    six_month_SHG = sum(no_shg_six_month_old),
    MCP_prep = sum(no_shg_mip_mcf),
    VO_formed = sum(no_vo_formed),
    SHG_in_VO = sum(no_shg_membership_vo),
    CLF_formed = sum(no_clf_formed),
    Savings_Mob = sum(amount_mobilized_in_shg),
    Total_SHGs = New_SHG+Other_SHG-Defunct_SHG,
    Total_HHs_Mob = SC_HH+ST_HH+Min_HH+Other_HH,
    SC_ST_HHs_Mob= SC_HH+ST_HH,
    RF_Eligible = New_SHG_eligible_RF+Old_SHG_eligible_RF,
    RF_Provided_Num = SC_SHG_RF+ST_SHG_RF+Min_SHG_RF+Other_SHG_RF,
    RF_Amt = round(((Amt_SC_RF+Amt_ST_RF+Amt_Min_RF+Amt_Other_RF)/100000),1),
    CIF_Provided_Num = SC_SHG_CIF+ST_SHG_CIF+Min_SHG_CIF+Other_SHG_CIF,
    CIF_Amt = round(((Amt_SC_CIF+Amt_ST_CIF+Amt_Min_CIF+Amt_Other_CIF)/100000),1)
  )

MPR_Data_Agg$Total_CIS <- round((MPR_Data_Agg$RF_Amt+MPR_Data_Agg$CIF_Amt),1)
MPR_Data_Agg$earliest_block_date <-  dmy(MPR_Data_Agg$earliest_block_date)
MPR_Data_Agg$Today <- (Sys.Date())
MPR_Data_Agg$Age <- round (((MPR_Data_Agg$Today-MPR_Data_Agg$earliest_block_date)/365),1)



MPR_Data_Agg <- select(MPR_Data_Agg,state_name,district_name,earliest_block_date,Age,Total_HHs_Mob,Total_SHGs,RF_Eligible,RF_Provided_Num,RF_Amt,CIF_Provided_Num,CIF_Amt,Total_CIS)
MPR_Data_Agg$concat <- str_to_lower(paste0(MPR_Data_Agg$state_name,sep=",",MPR_Data_Agg$district_name))



Census_Dist <- read.csv("E:\\Google Drive\\R\\Maps\\Data\\Census_Dist.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
Census_Dist$concat <- str_to_lower(paste0(Census_Dist$state_name,sep=",",Census_Dist$district_name))




MPR_Data_Agg_merge <- left_join(MPR_Data_Agg,Census_Dist, by = "concat")
MPR_Data_Agg_merge <- select(MPR_Data_Agg_merge,-X,-concat,-state_name.y,-district_name.y,-num_blocks)

MPR_Data_Agg_merge <- rename(MPR_Data_Agg_merge, State = state_name.x, District = district_name.x, censuscode = Dist2011)
MPR_Data_Agg_merge <- filter(MPR_Data_Agg_merge,censuscode != "NA")

write.csv(MPR_Data_Agg_merge,"E:\\Google Drive\\R\\Maps\\Data\\MPR_Data_Agg.csv")

SECC_List <- read.csv("E:\\Google Drive\\R\\Raw_Data\\Census_MPR_SECC\\SECC_Block_List.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

SECC_List_Agg <- SECC_List %>%
  group_by(statename,state_secc_code,districtname,dist_secc_code) %>%
  summarize(tot_pop = sum(totPop), tot_HHs = sum(totHH),targetHHs = sum(TargetHHs))

SECC_List_Agg$concat <- str_to_lower(paste0(SECC_List_Agg$statename, sep = ",", SECC_List_Agg$districtname))
SECC_List_Agg <- left_join(SECC_List_Agg,Census_Dist, by = "concat")
