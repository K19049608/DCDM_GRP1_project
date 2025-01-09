data = read.csv('analysis_data_clean', header=T)

# Read in parameter groups (this is the id_names google sheet, but saved as a CSV with excel)
parameter_groups = read.csv('parameters_to_groups.csv', header=T)

# Dropping parameter_name column from group data, to avoid replicating it during merge
parameter_groups = parameter_groups[-2]

# Assigning group names to cleaned data
data_grouped <- merge(data, parameter_groups, by="parameter_id")

write.csv(data_grouped,"analysis_data_grouped.csv", row.names = FALSE)
