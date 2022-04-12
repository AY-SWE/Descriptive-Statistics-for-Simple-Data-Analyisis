#install.packages("ggplot2")  
#install.packages("dplyr")
#install.packages("gmodels")
library("ggplot2")
library("dplyr") #data wrangling

#getwd()    copy and paste it then modify the directory from there, a hack
#setwd("C:/Users/ANDYYANG/CSE323") set to wd successful
#getwd()  

df = read.csv("CSE323_HW2_dataset.csv")
#View(df)
#summary(df)
#head(df)
#tail(df)
#names(df)
#df$"attribute"   # returns all the data values of that particular attribute

###################### PART 1 ##########################
df_KeyA= filter(df, Keyboard_Type=="A")
#print(df_KeyA)
df_KeyB= filter(df, Keyboard_Type=="B")
#print(df_KeyA)
df_KeyC= filter(df, Keyboard_Type=="C")
#print(df_KeyA)


library(gmodels)

df1 = df_KeyA %>% group_by(Repetition_Order) %>% summarise(mean = ci(WPM)[1], 
              lowCI = ci(WPM)[2],
              hiCI = ci(WPM)[3], 
              sd = ci(WPM)[4])
df2 = df_KeyB %>% group_by(Repetition_Order) %>% summarise(mean = ci(WPM)[1], 
                                                           lowCI = ci(WPM)[2],
                                                           hiCI = ci(WPM)[3], 
                                                           sd = ci (WPM)[4])
df3 = df_KeyC %>% group_by(Repetition_Order) %>% summarise(mean = ci(WPM)[1], 
                                                           lowCI = ci(WPM)[2],
                                                           hiCI = ci(WPM)[3], 
                                                           sd = ci(WPM)[4])

ggplot(data = df, mapping = aes(x= Repetition_Order)) + geom_line(data = df1, aes(y= mean, color  = "red")) + geom_line(data = df2, aes(y= mean), color  = "green") + geom_line(data = df3, aes(y= mean), color  = "blue") +geom_point(data = df1, aes(y= mean), color  = "red")+ geom_point(data = df2, aes(y= mean), color  = "green")+geom_point(data = df3, aes(y= mean), color  = "blue") + geom_errorbar(data = df1, aes(ymin=mean-1.96*sd, ymax=mean+1.96*sd), color  = "red", width= 0.1)+ geom_errorbar(data = df2, aes(ymin=mean-1.96*sd, ymax=mean+1.96*sd), color  = "green", width= 0.1)+ geom_errorbar(data = df3, aes(ymin=mean-1.96*sd, ymax=mean+1.96*sd), color  = "blue", width= 0.1)+ggtitle("Mean and 95% Confidence Interval of WPM")+ ylab("WPM")+ theme(plot.title = element_text(hjust = 0.5), legend.justification = c("right","bottom"), legend.position = c(0.9,0.05), legend.direction="horizontal", legend.key = element_rect(fill = "white")) + scale_colour_manual("", breaks = c("A", "B", "C"), values= c("A"="red","B"="green","C"="blue" )) + scale_y_continuous(breaks=seq(0, 70,10), expand=c(0,0), limits= c(0, 75)) 
# scale y continous expand c (0,0 ) removes that gap and touches the x-axis

###################### PART 2 #################################

df_last3repetition = df %>% filter(Repetition_Order >= 3)

df_bar = df_last3repetition %>% group_by(Keyboard_Type) %>% summarise(mean = ci(WPM)[1],
                                                      lowCI = ci(WPM)[2],
                                                      hiCI = ci(WPM)[3],
                                                      sd = ci(WPM)[4])

ggplot(data = df_bar, mapping = aes(x= Keyboard_Type, y= mean, fill = Keyboard_Type)) + geom_bar(stat="identity") + ggtitle("Mean and 95% Confidence Interval of WPM") + theme(plot.title = element_text(hjust = 0.5)) + ylab("WPM") + geom_errorbar(stat="identity", aes(ymin=mean-1.96*sd, ymax=mean+1.96*sd), width = 0.3) + geom_text(aes(label=sprintf("%0.2f", round(mean, digits = 2))), position=position_dodge(width=0.9), vjust=10) + scale_y_continuous(breaks=seq(0, 70, 10), expand=c(0,0), limits= c(0, 75)) 


###################### PART 3 #################################
df_bar_WER = df_last3repetition %>% group_by(Keyboard_Type) %>% summarise(mean = ci(WER)[1],
                                                                      lowCI = ci(WER)[2],
                                                                      hiCI = ci(WER)[3],
                                                                      sd = ci(WER)[4]) %>% mutate(mean_pct = mean * 100)

ggplot(data = df_bar_WER, mapping = aes(x= Keyboard_Type, y= mean, fill = Keyboard_Type)) + geom_bar(stat="identity") + ggtitle("Mean and 95% Confidence Interval of WPM") + theme(plot.title = element_text(hjust = 0.5)) + ylab("WER") + geom_errorbar(stat="identity", aes(ymin=mean-1.96*sd, ymax=mean+1.96*sd), width = 0.3) +geom_text(aes(label=sprintf("%.2f%%", mean_pct)), position=position_dodge(width=0.9), vjust=3) + scale_y_continuous(limits=c(0, .016),labels = scales::percent, expand=c(0,0))


###################### PART 4 #################################
df_plot4_1 = filter(df_last3repetition, ID=="1")   
df_plot4_2 = filter(df_last3repetition, ID=="2")   
df_plot4_3 = filter(df_last3repetition, ID=="3")   
df_plot4_4 = filter(df_last3repetition, ID=="4")   
df_plot4_5 = filter(df_last3repetition, ID=="5")   
df_plot4_6 = filter(df_last3repetition, ID=="6")   
df_plot4_7 = filter(df_last3repetition, ID=="7")   
df_plot4_8 = filter(df_last3repetition, ID=="8")   
df_plot4_9 = filter(df_last3repetition, ID=="9")   
df_plot4_10 = filter(df_last3repetition, ID=="10") 

df_plot4_bar1 = df_plot4_1 %>% group_by(Keyboard_Type) %>% summarise(mean = ci(WPM)[1], 
                                                           lowCI = ci(WPM)[2],
                                                           hiCI = ci(WPM)[3], 
                                                           sd = ci(WPM)[4])
df_plot4_bar2 = df_plot4_2 %>% group_by(Keyboard_Type) %>% summarise(mean = ci(WPM)[1], 
                                                                    lowCI = ci(WPM)[2],
                                                                    hiCI = ci(WPM)[3], 
                                                                    sd = ci(WPM)[4])
df_plot4_bar3 = df_plot4_3 %>% group_by(Keyboard_Type) %>% summarise(mean = ci(WPM)[1], 
                                                                    lowCI = ci(WPM)[2],
                                                                    hiCI = ci(WPM)[3], 
                                                                    sd = ci(WPM)[4])
df_plot4_bar4 = df_plot4_4 %>% group_by(Keyboard_Type) %>% summarise(mean = ci(WPM)[1], 
                                                                    lowCI = ci(WPM)[2],
                                                                    hiCI = ci(WPM)[3], 
                                                                    sd = ci(WPM)[4])
df_plot4_bar5 = df_plot4_5 %>% group_by(Keyboard_Type) %>% summarise(mean = ci(WPM)[1], 
                                                                    lowCI = ci(WPM)[2],
                                                                    hiCI = ci(WPM)[3], 
                                                                    sd = ci(WPM)[4])
df_plot4_bar6 = df_plot4_6 %>% group_by(Keyboard_Type) %>% summarise(mean = ci(WPM)[1], 
                                                                    lowCI = ci(WPM)[2],
                                                                    hiCI = ci(WPM)[3], 
                                                                    sd = ci(WPM)[4])
df_plot4_bar7 = df_plot4_7 %>% group_by(Keyboard_Type) %>% summarise(mean = ci(WPM)[1], 
                                                                    lowCI = ci(WPM)[2],
                                                                    hiCI = ci(WPM)[3], 
                                                                    sd = ci(WPM)[4])
df_plot4_bar8 = df_plot4_8 %>% group_by(Keyboard_Type) %>% summarise(mean = ci(WPM)[1], 
                                                                    lowCI = ci(WPM)[2],
                                                                    hiCI = ci(WPM)[3], 
                                                                    sd = ci(WPM)[4])
df_plot4_bar9 = df_plot4_9 %>% group_by(Keyboard_Type) %>% summarise(mean = ci(WPM)[1], 
                                                                    lowCI = ci(WPM)[2],
                                                                    hiCI = ci(WPM)[3], 
                                                                    sd = ci(WPM)[4])
df_plot4_bar10 = df_plot4_10 %>% group_by(Keyboard_Type) %>% summarise(mean = ci(WPM)[1], 
                                                                    lowCI = ci(WPM)[2],
                                                                    hiCI = ci(WPM)[3], 
                                                                    sd = ci(WPM)[4])
df_plot4_ID1_merge = df_plot4_1 %>% merge(df_plot4_bar1, by.x="Keyboard_Type")
df_plot4_ID2_merge = df_plot4_2 %>% merge(df_plot4_bar2, by.x="Keyboard_Type")
df_plot4_ID3_merge = df_plot4_3 %>% merge(df_plot4_bar3, by.x="Keyboard_Type")
df_plot4_ID4_merge = df_plot4_4 %>% merge(df_plot4_bar4, by.x="Keyboard_Type")
df_plot4_ID5_merge = df_plot4_5 %>% merge(df_plot4_bar5, by.x="Keyboard_Type")
df_plot4_ID6_merge = df_plot4_6 %>% merge(df_plot4_bar6, by.x="Keyboard_Type")
df_plot4_ID7_merge = df_plot4_7 %>% merge(df_plot4_bar7, by.x="Keyboard_Type")
df_plot4_ID8_merge = df_plot4_8 %>% merge(df_plot4_bar8, by.x="Keyboard_Type")
df_plot4_ID9_merge = df_plot4_9 %>% merge(df_plot4_bar9, by.x="Keyboard_Type")
df_plot4_ID10_merge = df_plot4_10 %>% merge(df_plot4_bar10, by.x="Keyboard_Type")

df_plot4_final_merge = bind_rows(df_plot4_ID1_merge,df_plot4_ID2_merge,df_plot4_ID3_merge,df_plot4_ID4_merge,df_plot4_ID5_merge,df_plot4_ID6_merge,df_plot4_ID7_merge,df_plot4_ID8_merge,df_plot4_ID9_merge,df_plot4_ID10_merge)

ggplot(data = df_plot4_final_merge, mapping = aes(x= ID, y= mean, group = Keyboard_Type, fill = Keyboard_Type)) +  geom_bar(position = 'dodge', stat = "identity") + ggtitle("A vs. B vs. C")+ theme(plot.title = element_text(hjust = 0.5)) + ylab("WPM") + xlab("WPM by User and Keyboard Type") + scale_y_continuous(breaks=seq(0, 100, 20), expand=c(0,0), limits= c(0, 100)) + scale_x_continuous(breaks=seq(0, 10, 1)) + geom_errorbar(aes(ymin=mean-1.96*sd, ymax=mean+1.96*sd), width = 0.5, position = position_dodge(0.9))