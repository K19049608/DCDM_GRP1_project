#import packages
library(tidyverse)
library(data.table)
library(dplyr)

#reading all csv files into a list
setwd('path/to/the/csvs/folder') #local directory location
filenames = list.files(pattern="\\.csv$") #reading file names into a list
print('file names read into list')
files = lapply(filenames, read.csv, header=FALSE) #applying read.csv onto list of names
print('files read into list')

#transposing all dataframes in the list
t_data= lapply(files, t)
print('transposed dataframes')
t_data= lapply(t_data, as.data.frame) #t() turns dataframes into lists, convert back to dataframe
print('transposed lists converted into dataframes')


#setting function for replacing column names with 1st row
set_colnames = function(input_dataframe){
  x <- input_dataframe[1,] #sets list with column names
  colnames(input_dataframe) <- x #replaces column names with list
  input_dataframe <- input_dataframe[-1,] #removes 1st row
}
print('setting column names function')

#applying function to list of dataframes
new_data = lapply(t_data, set_colnames)
print('setting column names of listed dataframes')

#binding rows of dataframes in list together
collated_data = bind_rows(new_data)
print('binding rows of all dataframes')

#writes data into a csv file for later use
write.csv(collated_raw_data,'filepath/collated_raw_data.csv', row.names = FALSE)
print('collated raw data file outputted')