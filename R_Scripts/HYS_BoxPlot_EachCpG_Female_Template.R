setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\Female\\GeneCpG\\STAIS")
df = read.csv("Common9_STAIS_Data_FemaleHYS.csv",header = T)
df <- df  %>% select(-c(1)) #Removing PATNO Column
df <- df[!(df$HYS == 4), ] #Removing HYS4
df$APPRDX[df$APPRDX == 1] <- 'Patient'
df$APPRDX[df$APPRDX == 2] <- 'Control'

#### 1. cg19829375 ####
df_1 <- df  %>% select(c(1,2,3))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")

df_long$HYS = as.character(df_long$HYS) #STAIS_Methyl_HYS_wise_inPatCont

ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.81, 0.15), axis.text = element_text(face="bold"),text = element_text(size = 16, color = "black"),
        legend.background = element_rect(fill="azure",linewidth=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text=element_text(color="black"))+  theme(plot.title = element_text(face="bold", size=18)) +
  theme(axis.title = element_text(face="bold")) + 
  xlab("\nHYS") + ylab("Methylation Value\n") +
  ggtitle ("State Anxiety - cg01600124 Methylation - Female")

#### 2. cg23149728 ####
df_1 <- df  %>% select(c(1,2,4))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.81, 0.85), axis.text = element_text(face="bold"),text = element_text(size = 16, color = "black"),
        legend.background = element_rect(fill="azure",linewidth=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text=element_text(color="black"))+  theme(plot.title = element_text(face="bold", size=18)) +
  theme(axis.title = element_text(face="bold")) + 
  xlab("\nHYS") + ylab("Methylation Value\n") +
  ggtitle ("State Anxiety - cg08271803 Methylation - Female")

#### 3. cg18174834 ####
df_1 <- df  %>% select(c(1,2,5))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.81, 0.15), axis.text = element_text(face="bold"),text = element_text(size = 16, color = "black"),
        legend.background = element_rect(fill="azure",linewidth=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text=element_text(color="black"))+  theme(plot.title = element_text(face="bold", size=18)) +
  theme(axis.title = element_text(face="bold")) + 
  xlab("\nHYS") + ylab("Methylation Value\n") +
  ggtitle ("State Anxiety - cg16516429 Methylation - Female")

#### 4. cg06704773 ####
df_1 <- df  %>% select(c(1,2,6))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.81, 0.85), axis.text = element_text(face="bold"),text = element_text(size = 16, color = "black"),
        legend.background = element_rect(fill="azure",linewidth=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text=element_text(color="black"))+  theme(plot.title = element_text(face="bold", size=18)) +
  theme(axis.title = element_text(face="bold")) + 
  xlab("\nHYS") + ylab("Methylation Value\n") +
  ggtitle ("State Anxiety - cg26955850 Methylation - Female")

#### 5. cg11865360 ####
df_1 <- df  %>% select(c(1,2,7))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.81, 0.85), axis.text = element_text(face="bold"),text = element_text(size = 16, color = "black"),
        legend.background = element_rect(fill="azure",linewidth=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text=element_text(color="black"))+  theme(plot.title = element_text(face="bold", size=18)) +
  theme(axis.title = element_text(face="bold")) + 
  xlab("\nHYS") + ylab("Methylation Value\n") +
  ggtitle ("State Anxiety - cg23881278 Methylation - Female")

#### 6. cg03605226 ####
df_1 <- df  %>% select(c(1,2,8))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.81, 0.15), axis.text = element_text(face="bold"),text = element_text(size = 16, color = "black"),
        legend.background = element_rect(fill="azure",linewidth=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text=element_text(color="black"))+  theme(plot.title = element_text(face="bold", size=18)) +
  theme(axis.title = element_text(face="bold")) + 
  xlab("\nHYS") + ylab("Methylation Value\n") +
  ggtitle ("State Anxiety - cg16234718 Methylation - Female")

#### 7. cg03984780 ####
df_1 <- df  %>% select(c(1,2,9))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.81, 0.15), axis.text = element_text(face="bold"),text = element_text(size = 16, color = "black"),
        legend.background = element_rect(fill="azure",linewidth=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text=element_text(color="black"))+  theme(plot.title = element_text(face="bold", size=18)) +
  theme(axis.title = element_text(face="bold")) + 
  xlab("\nHYS") + ylab("Methylation Value\n") +
  ggtitle ("State Anxiety - cg21105972 Methylation - Female")

#### 8. cg05791411 ####
df_1 <- df  %>% select(c(1,2,10))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.81, 0.85), axis.text = element_text(face="bold"),text = element_text(size = 16, color = "black"),
        legend.background = element_rect(fill="azure",linewidth=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text=element_text(color="black"))+  theme(plot.title = element_text(face="bold", size=18)) +
  theme(axis.title = element_text(face="bold")) + 
  xlab("\nHYS") + ylab("Methylation Value\n") +
  ggtitle ("State Anxiety - cg23974023 Methylation - Female")

#### 9. cg07777378 ####
df_1 <- df  %>% select(c(1,2,11))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.81, 0.15), axis.text = element_text(face="bold"),text = element_text(size = 16, color = "black"),
        legend.background = element_rect(fill="azure",linewidth=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text=element_text(color="black"))+  theme(plot.title = element_text(face="bold", size=18)) +
  theme(axis.title = element_text(face="bold")) + 
  xlab("\nHYS") + ylab("Methylation Value\n") +
  ggtitle ("State Anxiety - cg22930773 Methylation - Female")
