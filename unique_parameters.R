setwd('~/OneDrive/Desktop/DCDM_group_project/')
data = read.csv('filepath/analysis_data_clean.csv', header=T)

# Extracting the rows with unique parameter_id values
unique_rows = data[!duplicated(data$parameter_id), ]

# Dropping all rows except parameter_id and parameter_name
uniq_para = unique_rows[ -c(1:4, 6, 8) ]

# Exporting CSV so that parameter groups can be assigned
write.csv(uniq_para,"unique_parameters.csv", row.names = FALSE)
