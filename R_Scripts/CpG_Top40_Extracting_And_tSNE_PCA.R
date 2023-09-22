setwd("Z:\\")
getwd()

crfl= read.csv('CpGList_FromRandomForestResult_AfterLog2FoldChange.csv', header = T)
crfl_trim40 = crfl[-c(41:353),]

x <- table(crfl_trim40$Run_1)

library(dplyr)
library(tidyr)
longPivot <- crfl_trim40 %>% pivot_longer(cols = -c(Position),names_to = "Run_Id", values_to = "CPG_ID")
freqTbl <- as.data.frame(table(longPivot$CPG_ID)) %>% arrange(desc(Freq))

# length(freqTbl$Var1)
# length(unique(longPivot$CPG_ID))

top_cpg_trim40 = as.data.frame(freqTbl[-c(41:249),1])
write.csv(top_cpg_trim40, 'CpG_Top40_FromRandomForest_AfterPCA.csv', row.names = F)




CpgTop40 = read.csv('CpG_Top40_FromRandomForest_withDataAndAPPRDX.csv', header = T)

#tSNE
install.packages("Rtsne")
IR_data <- CpgTop40[ ,3:42]
IR_target <- data.frame(CpgTop40[ ,43])

tsne_results <- Rtsne(IR_data, perplexity=100, max_iter = 6000, pca = T, check_duplicates = T)
#plot(tsne_results$Y,col=IR_target$chrd...355., asp=1)
#par(mfrow=c(1,2)) # To plot two images side-by-side

data <- as.data.frame(cbind(tsne_results$Y,as.factor(CpgTop40[ ,43])))

data[,"V3"] <- as.character(data$V3)  
plot(V2~V1, data)
ggplot(data, aes(V1,V2, colour=V3)) + geom_point() + 
  scale_color_discrete(name = 'Patient ID', labels = c("PD Patient", "Healthy Control", "SWEDD")) + 
  labs(title = waiver(), x="Dim_1" ,y="Dim_2") + ggtitle("tSNE_Top40_CpG_PatientID") +
  geom_smooth(method = "lm", formula = y~x, se = F)

ggplot(data, aes(V1,V2, colour=V3)) + geom_point() + 
  scale_color_discrete(name = 'Patient ID', labels = c("PD Patient", "Healthy Control", "SWEDD")) + 
  labs(title = waiver(), x="Dim_1" ,y="Dim_2") + ggtitle("tSNE_Top40_CpG_PatientID_Separate") + facet_wrap(~V3)



##############################################################

chrpca = read.csv("CpG_Top40_FromRandomForest_withDataAndAPPRDX.csv",header = T)
head(chrpca)

rownames(chrpca)= chrpca$CpG
chrpca11 = chrpca[,-c(1,2,43)]
head(chrpca11)

merapca = prcomp(chrpca11, retx = T, scale = F)
TABLE=data.frame(merapca$x)
out=summary(merapca)
out1=(out$importance[3,])*100

meradata = data.frame(cbind(chrpca11, merapca$x[,1:2]))

df= data.frame("PC1"= merapca$x[,1], "PC2"= merapca$x[,2], "class"= as.factor(chrpca$APPRDX))
head(df)
#meradata[,"APPRDX"] <- as.factor(meradata$APPRDX)  
plot(PC2~PC1, data=df)
ggplot(df, aes(PC1, PC2, color = class)) +
  geom_point(position = position_jitter()) +
  scale_color_discrete(name = 'Patient ID', labels = c("PD Patient", "Healthy Control", "SWEDD")) +
  labs(x = "PC1 (23.2%)", y= "PC2 (17.3%)") + 
  ggtitle("PCA_Top40_CpG_PatientID") +
  geom_smooth(method = "lm", formula = y~x)


print(ggbiplot(merapca, obs.scale = 1, var.scale = 1, groups = chrpca$APPRDX, ellipse = TRUE, circle = TRUE))









