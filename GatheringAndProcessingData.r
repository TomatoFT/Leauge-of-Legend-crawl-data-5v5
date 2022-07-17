rm(list = ls())

# readable in HTML format
library(xml2)
library(httr)
library(rvest)
library(stringr)
library(tidyverse)
library(dplyr)
library(readr)
library(plyr)

#===============================================================================
# base


data <- GET("https://www.metasrc.com/stats")

content <- content(data, as = "text")
html_data <- read_xml(content, as_html = TRUE)

#==============================================================================
# get all of values of filter
attr_value_mode <- html_data %>% html_nodes("#mode option") %>% html_attr("value")
mode <- html_data %>% html_nodes("#mode option") %>% html_text()

attr_value_patch <- html_data %>% html_nodes("#patch option") %>% html_attr("value")
patch <- html_data %>% html_nodes("#patch option") %>% html_text()

attr_value_region <- html_data %>% html_nodes("#region option") %>% html_attr("value")
region <- html_data %>% html_nodes("#region option") %>% html_text()

df_mode = cbind(attr_value_patch, patch)
df_mode = data.frame(df_mode)
# patch <- substr(dataset$Name[index],6,nchar(dataset$Name[index]))

setwd("D:/DS103-Project/dataset")

for (i in 1:length(df_mode$attr_value_patch)){
  if (df_mode$attr_value_patch[i] == "6.24" || df_mode$attr_value_patch[i] == "7.24" || df_mode$attr_value_patch[i] == "8.24") {
  next
  }
  else {
  print(df_mode$attr_value_patch[i])
  url <- sprintf("https://www.metasrc.com/5v5/%s/stats",df_mode$attr_value_patch[i])
  print(url)
  data <- GET(url)
  
 # df_mode$patch[i]=substr(df_mode$patch[i],6,nchar(dataset$Name[index]))
  
  print(df_mode$patch[i])
  patch_column = df_mode$attr_value_patch[i]
  name_csv = df_mode$attr_value_patch[i]
  
  content <- content(data, as = "text")
  html_data <- read_xml(content, as_html = TRUE)
  
  table_head <- xml_text(xml_find_all(html_data, "//*[@id='table-scroll']/table/thead/tr/th"))
  
  table_data <- xml_text(xml_find_all(html_data,"//*[@id='table-scroll']/table/tbody/tr/td"))
  # So cot o day tuong ung so luong table_head
  # So dong o day tuong ung so luong table_data
  
  dataset <- matrix(ncol=length(table_head), nrow = 
                      (length(table_data)/ length(table_head) ))
  data_col = length(table_head)
  # Lan luot lay tung dong trong table_data bo vao dataset
  
  count = 1
  i = 1
  while(i<length(table_data)-data_col){
    dataset[count,] <- c(table_data[i: (i-1+data_col)])
    i = i+ data_col
    count = count +1
  }
  
  # Chuyen ma tran dataset thanh dang dataframe
  
  dataset <- as.data.frame(dataset)
  dataset <- na.omit(dataset)
  
  # gan tieu de cho dataframe
  
  names(dataset) <- c(table_head)
  dataset$Patch = patch_column
  
  for (index in 1:length(dataset$Name)) {
    dataset$Name[index] <- substr(dataset$Name[index],1,nchar(dataset$Name[index])/2)
  }
  
  for (index in 1:length(dataset$Tier)) {
    dataset$Tier[index] <- substr(dataset$Tier[index],1,nchar(dataset$Tier[index])-4)
  }
  
  for (index in 1:length(dataset$Trend)) {
    if(dataset$Trend[index] == "999NEW") {
      dataset$Trend[index] <- "0"
    }
    dataset$Trend[index] <- as.numeric(dataset$Trend[index])
  }
  
  for (index in 1:length(dataset$`Win %`)) {
    dataset$`Win %`[index] <- as.double(substr(dataset$`Win %`[index],1,nchar(dataset$`Win %`[index])-1))
    dataset$`Role %`[index] <- as.double(substr(dataset$`Role %`[index],1 ,nchar(dataset$`Role %`[index])-1))
    dataset$`Ban %`[index] <- as.double(substr(dataset$`Ban %`[index],1,nchar(dataset$`Ban %`[index])-1))
    dataset$`Pick %`[index] <- as.double(substr(dataset$`Pick %`[index],1 ,nchar(dataset$`Pick %`[index])-1))
    dataset$Score[index] <- as.double(dataset$Score[index])
  }
  
  name = sprintf("data_LoL_%s_stats.csv",name_csv)

  write.csv(dataset, name, row.names = FALSE)
  }}
print("-------------------Done with full data-------------------")

#Delete the file to update

if (file.exists("Data_LoL_Full_patches.csv")) {
  file.remove("Data_LoL_Full_patches.csv")
  print("The file is deleted")
}

#import and merge all three CSV files into one data frame

  df <- list.files(path='D:/DS103-Project/dataset/') %>% 
    lapply(read_csv, show_col_types = FALSE) %>% 
    bind_rows 

df <- df[order(df$Patch),]

write.csv(df,file = "Data_LoL_Full_patches.csv")

