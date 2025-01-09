library('tidyr')
library('dplyr')
library('readr')



raw_text=read.csv('filepath/IMPC_procedure.txt',header = T) #load in with header, doesn't matter as later manipulations get rid of it, add it back later
# procedure ID values don't have comma sep,
proc_text_comma=sub(' ',',',raw_text$line_number.procedureId..name..description..isMandatory..impcParameterOrigId)#this replaces empty space with comma but only on first occurence (between Procedure ID and name)
proc_text_comma=as.data.frame(proc_text_comma)
headings=list('procedureId', 'name', 'description', 'isMandatory', 'impcParameterOrigId')
proc_text_comma = strsplit(proc_text_comma[,1], ',') #splits all rows by delim, creating vector of lists
clean_data = data.frame()
for (i in proc_text_comma){ # iteration variable (i) is individual lists of split characters
  if(length(i)>5){ # 5 columns, so any lists longer than 5 have had disease term split
    combined = do.call(paste, c(as.list(i[3:(length(i)-2)]), sep = ', ')) # combines split characters for procedure description, reinserts ', ' that was removed
    i=list(i[[1]],i[[2]],combined, i[[length(i)-1]],i[[length(i)]]) # rebuilding i variable with combined strings
  }
  clean_data= rbind(clean_data, i) # adding i as new row to empty data frame
}

colnames(clean_data)=headings# add back the headings

#cleaning up descriptions (some descriptions are missing or truncated for the same name)
for (i in clean_data$name){#for every name in the procedure
  filtered_rows <- clean_data[clean_data$name == i, ] #selects every row with this
  longest_value <- filtered_rows$description[which.max(nchar(filtered_rows$description))] #chooses the longest description (the non empty, or cut off ones)
  filtered_rows$description <- longest_value #gives them all the same description
  clean_data[clean_data$name == i, ] <- filtered_rows #update original
}

write_csv(clean_data,'filepath/IMPC_procedure_clean.csv')
