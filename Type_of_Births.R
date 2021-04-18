library(ggplot2)
library(dplyr)
library(cowplot)
theme_set(theme_gray())
library(vcd)
library(GGally)
library(tidyr)
library(Hmisc)
library(lmtest)
library(ggmosaic)
library(readxl)
library(data.table)
library(scales)

setwd("C:\\Users\\srisa\\Documents\\Assignments\\Sem2\\Data Visualization\\Assignment 2")

birthMethod <- read_excel("aihw-per-91-supplementary-data-tables.xls",sheet = "Table 2.32", skip=5)
names(birthMethod)[names(birthMethod) == "Method of birth(a)"] <- "Birth_Method"
names(birthMethod)[names(birthMethod) == "Tas(b)"] <- "Tas"
names(birthMethod)[names(birthMethod) == "ACT(c)"] <- "ACT"

csec_byState_percent <- birthMethod[15,] %>% 
                         select(Birth_Method:NT) %>%
                           filter(Birth_Method %like% "Caesarean")

csec_byState_percent$NSW <- as.double(csec_byState_percent$NSW)
csec_byState_percent <- gather(csec_byState_percent,State,Percent,NSW:NT)

csec_byState_percent$State <- factor(csec_byState_percent$State)






plot_birthMethod<-ggplot(csec_byState_percent,aes(x = State, y = Percent))
plot_birthMethod<- plot_birthMethod+geom_bar(stat="identity",fill = "dodgerblue3" ) +  theme_minimal() +
coord_cartesian(ylim=c(25,40))+

 #scale_y_continuous(limits=c(25,40),oob = rescale_none) +
  labs(title = "Caesarian Section births across Australia in 2015",
       y = "Percentage of caesarian births ",
       x = "State or Teritorry")+
  theme(plot.title = element_text(face="bold"))+
  theme(axis.title = element_text(face = "bold"))+
  geom_text(aes(label=round(Percent,2)), vjust = -0.5,size = 4)



csecByAge <- read_excel("aihw-per-91-supplementary-data-tables.xls",sheet = "Table 2.33", skip=5)

names(csecByAge)[names(csecByAge) == "Age (years)"] <- "Age"
names(csecByAge)[names(csecByAge) == "ACT(a)"] <- "ACT"

csecByAge_percent <- csecByAge[11:16,] %>% 
  select(Age:NT)

csecByAge_percent$NSW <- as.double(csecByAge_percent$NSW)
csecByAge_percent <- gather(csecByAge_percent,State,Percent,NSW:NT)

csecByAge_percent$State <- factor(csecByAge_percent$State)

plot_cSecAge<-ggplot(csecByAge_percent,aes(x = State, y = Percent, fill = Age))
plot_cSecAge <- plot_cSecAge+geom_bar(stat="identity", position = "dodge" ) +  theme_minimal() +
  scale_fill_brewer(palette = "Set3")+
  labs(title = "Caesarian section births across Australia categorised in age groups",
       y = "Percentage of caesarian births",
       x = "State or Teritorry")+
  theme(plot.title = element_text(face="bold"))+
  theme(axis.title = element_text(face = "bold"))+
  theme(legend.position ="bottom" )+
  geom_text(aes(label=round(Percent,2)), position=position_dodge(width=0.9), vjust = -0.5,size = 4)

 
plot_title <- ggdraw() + draw_label("Caesarian section births across all States/Territories in Australia in 2015", 
                                    fontface = "bold", size = 15) 
# both plots to share centered y-axis label 
common_yaxis <- ggdraw() + draw_label("Percentage of caesarian section births", angle=90)





grid <- plot_grid(plot_title, 
                  # hide existing y-axis labels 
                  plot_birthMethod + theme(axis.title.y=element_blank()), 
                  plot_cSecAge + theme(axis.title.y=element_blank())+theme(axis.title.x = element_blank()), 
                  ncol=1, align="v", rel_heights = c(0.1,0.7,1.1)) 
# add title to juxtaposed plots 
grid1 <- plot_grid(common_yaxis, grid, ncol=2, rel_widths = c(0.025,1)) 
grid1

