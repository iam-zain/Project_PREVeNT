setwd ("Z:\\PPMI_Data\\Excel_Data\\SocioBehaviour_Data\\Feature_Gene_Disease_Association\\Depression_Processed&Results_Files")

library(Boruta)
library(dplyr)
library(data.table)
library(vroom)

cl <- makeCluster(3)
registerDoParallel(cl)
df = vroom("DepressionCateg_Methylome_APPRDX.csv")

df = subset(df, select = -c(PATNO,Sentrix,Gender)) %>% mutate_all(as.factor)
dfp <-df[which(df$APPRDX == 1),]
dfh <-df[which(df$APPRDX == 2),]
dfc <- subset(df, select = -c(MDS_Depress)) %>% mutate_all(as.factor)

##### APPRDX1 #####
res_bor <- Boruta (MDS_Depress ~ ., data = dfp, doTrace = 2, maxRuns = 555) #Running Boruta
stopCluster(cl)

dfh$MDS_Depress <- as.factor(dfh$MDS_Depress)
sel_bor <- TentativeRoughFix (res_bor)
attStats (res_bor)
imp_CpG <- getNonRejectedFormula(res_bor) #Shows confirmed & tentative features
imp_Conf_CpG <- getConfirmedFormula(res_bor) #Shows only confirmed important features

imp_CpG_Data <- df %>% select(APPRDX,MDS_Depress, cg00040455)
fwrite (imp_CpG_Data, "CpG_Depress_Boruta_APPRDX1.csv")


##### APPRDX2 #####
res_bor <- Boruta (MDS_Depress ~ ., data = dfh, doTrace = 2, maxRuns = 555) #Running Boruta
stopCluster(cl)

dfh$MDS_Depress <- as.factor(dfh$MDS_Depress)
sel_bor <- TentativeRoughFix (res_bor)
attStats (res_bor)
imp_CpG <- getNonRejectedFormula(res_bor) #Shows confirmed & tentative features
imp_Conf_CpG <- getConfirmedFormula(res_bor) #Shows only confirmed important features

imp_CpG_Data <- df %>% select(APPRDX,MDS_Depress, cg00040455,cg26737636)
write.csv(imp_CpG_Data, "CpG_Depress_Boruta_APPRDX2.csv")


##### Patient vs Healthy #####
res_bor <- Boruta (APPRDX ~ ., data = dfc, doTrace = 2, maxRuns = 555) #Running Boruta
stopCluster(cl)

dfh$APPRDX <- as.factor(dfh$APPRDX)
print (res_bor)
#plot (res_bor, las = 2, cex.axis = 0.6) #Visualize importance of each feature 
#plotImpHistory (res_bor)
sel_bor <- TentativeRoughFix (res_bor)
attStats (res_bor)
imp_CpG <- getNonRejectedFormula(res_bor) #Shows confirmed & tentative features
imp_Conf_CpG <- getConfirmedFormula(res_bor) #Shows only confirmed important features

imp_CpG_Data <- df %>% select(APPRDX,MDS_Depress, cg26737636)
write.csv(imp_CpG_Data, "CpG_Depress_Boruta_APPRDX1vs2.csv")



####### Alternative, written in a way that it can be used as server code  #######
df1= as.data.frame(t(df))
df1$CpG=rownames(df1)
df1=df1[-1,]
head(df1)
res_bor = Boruta::Boruta(Smell_Category ~., data = df, doTrace = 2, maxRuns = 55)
print(res_bor)
df$Smell_Category <- as.factor(df$Smell_Category)

sel_bor <- Boruta::TentativeRoughFix (res_bor)
attrib <- Boruta::attStats(res_bor)

CpG_select <- attrib [which(attrib$decision != "Rejected"),]

rown=data.frame("CpG"=(rownames(CpG_select)))
fff <- merge(df1,rown, by = "CpG")

write.csv(fff, "./temp/temp_UP_Bor.csv", row.names = F)
