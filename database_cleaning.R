install.packages('dplyr')
install.packages('readr')
install.packages('tidyr')

library(dplyr)
library(readr)
library(tidyr)



procedure_csv=read.csv('filepath/IMPC_procedure_clean.csv',header = T) #load in all the cleaned metadata and data files
disease_information_csv=read.csv('filepath/Disease_information_clean.csv',header = T)
impc_parameter_csv=read.csv('filepath/IMPC_parameter_description_clean.csv',header = T)
clean_data=read.csv('../group1/outputs/analysis_data_clean.csv',header = T)
parameter_groupings=read.csv('../group1/metadata/parameters_to_groups.csv',header = T) # we made a csv assigning groups to all the parameters that appeared in the clean data 


gene_table=clean_data[,1:2] #these tables are the tables that will be converted to csv to put in SQL database,e.g this one has gene info
analysis_table=clean_data[,c(8,1,3:6)] #analysis info
disease_table=disease_information_csv[,c(1,2,4,3)]
parameter_table=impc_parameter_csv[,c(5,3,4,2)]


#the impc_parameter_descriptions only contains IMPC parameter, there were more parameters in the analysis that didn't have IMPC tag
diff_set=setdiff(parameter_groupings$parameter_id,parameter_table$parameterID)#here we gather all the parameter IDs that weren't IMPC (were in grouping csv but not impc parameter list)

filtered_data <- parameter_groupings[(parameter_groupings$parameter_id %in% diff_set),1:2] # we gather the data for these parameters specifically parameter name and ID
filtered_data$description = filtered_data$parameter_name #the descriptions for all the values is just the name, continuing this to avoid NA



colnames(filtered_data) = c('parameterID','name','description') #give the df containing extra parameters the same column names as ones in parameter list making binding easier
filtered_data$description <- gsub(' ', '_', filtered_data$description) #none of the descriptions have spaces instead have "_" following the convention
parameter_table=bind_rows(parameter_table,filtered_data) #join those two df adding the non IMPC parameters


parameter_table$grouping <- parameter_groupings$group[match(#we get all the indexes matches between parameter_table name and parameter_grouping name and gets all the groups for that indexes and assigns it parameter table 
  parameter_table$name,
  parameter_groupings$parameter_name
)]


parameter_table=parameter_table[!(is.na(parameter_table$grouping)),] # gets rid of parameters that weren't assigned a group (save parameter space)
#note there are 20 more parameters than those in the groupings (meaning 20 more than in actual analysis), these are parameters that have exact same name as one of the parameter groupings so were also assigned group
# e.g IMPC_PAT_049_002 has name body weight which is identical to M-G-P_009_001_003 name meaning they both get grouped into 'weight'


parameter_table$procedure_mandatory <- procedure_csv$isMandatory[match(#we get all the indexes matches between parameter_table$impcParameterOrigId and procedure_csv$impcParameterOrigId and gets all the ismandatory for that indexes and assigns it parameter table
  parameter_table$impcParameterOrigId,
  procedure_csv$impcParameterOrigId
)]
unique(procedure_csv$name)
#making list of unique procedures, also adding procedureID to parameter, procedureID has new ID even for repeated procedures, need to fix

procedures_list=procedure_csv[,2:5] # make copy of procedures without original ProcedureID (they are not assigned only to unique procedures)
unique_procedures=distinct(procedures_list) # get all the unique  procedures
unique_procedures$procedureID = 1:nrow(unique_procedures) # assigns new ID (for each  unique)
unique_procedures=unique_procedures[,c(4,1,2)]#rearrange columns

procedures_list$procedureID <- match(procedures_list$name, unique(procedures_list$name)) #assigns new ID (for the list of procedures), by finding indexes where unique list and procedure df match
#after this repeated procedures would have the same ID

parameter_table$procedureID <- procedures_list$procedureID[match( #add procedureID to parameter by mathcing iimpcParameterOrigId in parameter table and procedure csv
  parameter_table$impcParameterOrigId,
  procedure_csv$impcParameterOrigId
)]

parameter_table=parameter_table %>% rename_with(~c('procedure_id', 'parameter_id'),c(procedureID,parameterID)) #renaming columns
unique_procedures=unique_procedures %>% rename_with(~c('procedure_id'),c(procedureID))
#parameter and procedure table done and ready for export

unique(gene_table$gene_accession_id)
unique(disease_table$gene_accession_id)

gene_table <- gene_table[!duplicated(gene_table$gene_accession_id), ]
# get rid of all repeats, just have list of all unique genes for its own table
# in order to make Gene foreign key in diseasegene table, have to include all genes tested within disease analysis(not in original gene list,have to manually add)
# have to manually add gene name and look up gene symbol
diseasegeneID_list =unique(disease_table$gene_accession_id)
diseasegene_list=c('Wt','Nnt','Coro1a','Sord','Malrd1','Rsxr','Tnfsf4','Asmt','Abcc6','Acads') # list of all gene_symbol for genes found in disease and not in analysis
diseasegene_df= data.frame(diseasegeneID_list,diseasegene_list) #make it a dataframe
diseasegene_df=diseasegene_df %>% rename_with(~c('gene_accession_id','gene_symbol'),c(diseasegeneID_list,diseasegene_list))
gene_table=rbind(gene_table,diseasegene_df)


unique_disease_table[duplicated(unique_disease_table$disease_term),]
#get just list of unique disease ID (disease and gene have many to many relationship)

disease_table = disease_table[,c(1,3,4)]

class(analysis_table$pvalue)

Group_list=unique(parameter_groupings$group) #making table for listed groups
unique(analysis_table$analysis_id)

unique(unique_disease_table$disease_id)

write.csv(analysis_table,'filepath/analysis_table.csv',row.names = F)
write.csv(parameter_table,'filepath/parameter_table.csv',row.names = F)
write.csv(gene_table,'filepath/gene_table.csv',row.names = F)
write.csv(unique_procedures,'filepath/procedures_table.csv',row.names = F)
write.csv(disease_table,'filepath/diseaseGene_table.csv',row.names = T)
write.csv(unique_disease_table,'filepath/disease_table.csv',row.names = F)
write.csv(Group_list,'filepath/group_list.csv',row.names = F)
