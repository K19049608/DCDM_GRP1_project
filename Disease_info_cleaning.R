library(dplyr)

# Import disease info, skips the first row (with no values), then uses 1st non-skipped row as header
# This leaves all required data in col2, all in one string per row, with values separated by ', '
dis_raw = read.table('Disease_information.txt', skip=1, header=T)

# Splitting the col2 data, to remove separators
# First split column names, with '..' separator, fixed prevents conflict with '.' in regex patterns
dis_cols = strsplit(colnames(dis_raw)[2], '..', fixed=T)

# Then split the actual data, using separator ', ', outputting a list of character factors
dis_split = strsplit(dis_raw[,2], ', ')

# Some character values in disease_term have ', ' within them, resulting in too many splits (factor length > 4)
# This loop re-combines split disease_term values, then combines all values into a data frame
dis_df = data.frame()
for (i in dis_split){ # iteration variable (i) is individual lists of split characters
  if(length(i)>4){ # 4 columns, so any lists longer than 4 have had disease term split
    combined = do.call(paste, c(as.list(i[2:(length(i)-2)]), sep = ', ')) # combines split characters for disease term, reinserts ', ' that was removed
    i=list(i[[1]], combined, i[[length(i)-1]],i[[length(i)]]) # rebuilding i variable with combined strings
  }
  dis_df= rbind(dis_df, i) # adding i as new row to empty data frame
}
# Adding column names isolated earlier
colnames(dis_df) = dis_cols[[1]]

# Removing duplicated rows
dis_df <- dis_df[!duplicated(dis_df), ]

# Writing df as csv
write.csv(dis_df,"Disease_information_clean.csv", row.names = FALSE)



