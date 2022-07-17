rm(list = ls())
library(ggplot2)
data <- read.csv("D:/DS103-Project/dataset/Data_LoL_Full_patches.csv")
data_Champ <- data$Name
pd = data.frame(Champions=data$Role, KDA=count(data$Name))
print(typeof(data_Champ))
# Custom a few word of the title only:
p <- ggplot(data=pd, aes(x=Champions, y=KDA)) +
            geom_bar(stat="identity") + 
            theme_minimal() + 
    ggtitle("Leauge of Legend Data Exploration")
table(data$Role)

count(data$Name)

