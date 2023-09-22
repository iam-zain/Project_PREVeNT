setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250")
options(scipen = 999) #Viewing output values as decimal values

####  NMI #### 

Feat_Data = vroom("Methylome_Male.csv")
head(Feat_Data)[1:3,1:8]
FeatData <- Feat_Data %>% select(-c(1,2,3,5,6,7))
colnames(FeatData)[1]

{
  cl <- makeCluster(3)
  registerDoParallel(cl)
  dat_dsc <- discretize(FeatData)
  test_result <- 
    foreach(elem = 1:length(dat_dsc), .combine = rbind) %dopar%
    {
      tempdf1 <- as.data.frame(infotheo::mutinformation(dat_dsc[,1],dat_dsc[,elem], method = "emp"))
      rownames(tempdf1) <- colnames(dat_dsc[elem])
      tempdf1
    }
  stopCluster(cl)
}

colnames(test_result) <- ("MutInf_APPRDX")
test_result$Normalized <- (test_result$MutInf_APPRDX)/(0.615201807)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_Methylome_Male.csv")



setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\Male\\GeneCpG\\Xtras\\NMI")

# NMI
df_NMI <- read.csv('NMI_Methylome_Male07.csv')
CpgNames <- df_NMI$CpG
SelectCpGsRaw = Feat_Data[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$PATNO <- Feat_Data$PATNO
SelectCpGsRaw$APPRDX <- Feat_Data$APPRDX
write.csv(SelectCpGsRaw, 'NMI_Methylome_Male07_Data.csv', row.names = F)

# NMI
df_NMI <- read.csv('NMI_Methylome_Male06.csv')
CpgNames <- df_NMI$CpG
SelectCpGsRaw = Feat_Data[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$PATNO <- Feat_Data$PATNO
SelectCpGsRaw$APPRDX <- Feat_Data$APPRDX
write.csv(SelectCpGsRaw, 'NMI_Methylome_Male06_Data.csv', row.names = F)

# NMI
df_NMI <- read.csv('NMI_Methylome_Male055.csv')
CpgNames <- df_NMI$CpG
SelectCpGsRaw = Feat_Data[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$PATNO <- Feat_Data$PATNO
SelectCpGsRaw$APPRDX <- Feat_Data$APPRDX
write.csv(SelectCpGsRaw, 'NMI_Methylome_Male055_Data.csv', row.names = F)

