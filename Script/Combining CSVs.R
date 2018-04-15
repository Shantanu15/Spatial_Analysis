##Combining CSVs
library(dplyr)

setwd("E:\\Google Drive\\R\\Maps\\Data\\State_CSV")

file <- list.files("E:\\Google Drive\\R\\Maps\\Data\\State_CSV")

endval <- length(file)
comb_state_file <- NULL

for (i in 1:endval)
{
  temp <- read.csv(file[i],header = TRUE,sep = ",", stringsAsFactors = FALSE)
  comb_state_file <- rbind(temp,comb_state_file)
}
rm(temp)




write.csv(comb_state_file, "E:\\Google Drive\\R\\Maps\\Data\\combined_states.csv")