library(stringr)

setwd("Z:\\PPMI_Data\\Notepad_Texts")
PatSen <- read.delim('PPMI_Meth_n524_for_LONI030718.txt')
head(PatSen)
PatSen$Sentrix <- str_c(PatSen$Sentrix.ID, '_', PatSen$Sentrix.Position)
PatSen = subset(PatSen, select = -c(2,3))
class(PatSen)

setwd("Z:\\PPMI_Data\\Excel_Data")
MetSen <- read.csv('MethylPatientApprdx_ZeroReplaceBlank.csv')

PatMetSen <- merge(x = PatSen, y = MetSen, by = 'Sentrix', all = TRUE)
write.csv(PatMetSen, "Sentrix_PATNO_APPRDX_of524Methyl.csv")

#Patient 3794 (3794A) has two rows with missing values, both are merged into one row 
#...in the above saved file.
