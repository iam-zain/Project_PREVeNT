setwd("Z:\\PPMI_Data\\Excel_Data\\ChromosomeWise")
getwd()

library(ggplot2)
library(Rtsne)

chrd= read.csv("Chr_21_CpG_Apprdx.csv")
chr <- data.matrix(chrd)

IR_data <- chr[ ,2:6706]
IR_target <- chr[ ,6707]

tsne_results <- (Rtsne(IR_data, perplexity=100, max_iter = 6000, pca = F, check_duplicates = FALSE))

#par(mfrow=c(1,2)) # To plot two images side-by-side

data <- as.data.frame(cbind(tsne_results$Y,as.factor(chr[ ,6707])))

data[,"V3"] <- as.character(data$V3)  
plot(V2~V1, data)
ggplot(data, aes(V1,V2, colour=V3)) + geom_point() + 
  scale_color_discrete(name = 'Patient ID', labels = c("PD Patient", "Healthy Control", "SWEDD")) + 
  labs(title = waiver(), x="Dim_1" ,y="Dim_2") + ggtitle("Chr_21_tSNE")+
  geom_smooth(method = "lm", formula = y~x, se = F)

ggplot(data, aes(V1,V2, colour=V3)) + geom_point() + 
  scale_color_discrete(name = 'Patient ID', labels = c("PD Patient", "Healthy Control", "SWEDD")) + 
  labs(title = waiver(), x="Dim_1" ,y="Dim_2") + ggtitle("Chr_22_var08_tSNE_Separate") + facet_wrap(~V3)




###########################################################################


library(ggplot2)

chrpca = read.csv("Chr_21_CpG_Apprdx.csv",header = T)
head(chrpca)

rownames(chrpca)= chrpca$CpG
chrpca11 = chrpca[,-c(1,6706)]
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
  labs(x = "PC1 (9.6%)", y= "PC2 (8.0%)") + 
  ggtitle("Chr_21_PCA") +
  geom_smooth(method = "lm", formula = y~x, se = F)


print(ggbiplot(merapca, obs.scale = 1, var.scale = 1, groups = chrpca$APPRDX, ellipse = TRUE, circle = TRUE))





