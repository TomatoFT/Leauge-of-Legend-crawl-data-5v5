# readable in HTML format
library(xml2)
# manipulate the GET or POST methods
library(httr)

library(rvest)
#===============================================================================
rm(list = ls())

#==============================================================================
# get all of values of filter
attr_value_mode <- html_data %>% html_nodes("#mode option") %>% html_attr("value")
mode <- html_data %>% html_nodes("#mode option") %>% html_text()

attr_value_patch <- html_data %>% html_nodes("#patch option") %>% html_attr("value")
patch <- html_data %>% html_nodes("#patch option") %>% html_text()

attr_value_region <- html_data %>% html_nodes("#region option") %>% html_attr("value")
region <- html_data %>% html_nodes("#region option") %>% html_text()


df_mode = cbind(attr_value_mode, mode)
df_mode = data.frame(df_mode)



for (i in 1:length(df_mode$attr_value_mode)){

  url <- sprintf("https://www.metasrc.com/%s/stats",df_mode$attr_value_mode[i])
  data <- GET(url)
  mode_column = df_mode$mode[i]
  name_csv = df_mode$attr_value_mode[i]
  
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
  dataset$mode = mode_column
  
  name = sprintf("data_LoL_%s_stats",name_csv)
  write.csv(dataset, name)
}









