# Download the csv file
df <- read.csv("kidney_disease.csv", stringsAsFactors = FALSE, header = TRUE)
# View the first 6 rows of the data
head(df)
# View the last 6 rows of the data
tail(df)
# View the whole data set
View(df)
# View the structure of the data
str(df)
# View the  statistical summary of the data
summary(df)
# View the number of rows and columns
dim(df)

# After checking the dataset, I found that there are some problem with the data type of the dataset. such as Pcv, Wc,
#R c, Htn, dm, cad,pe, ane, type are not in the correct format.So I changed the pcv, wc, rc data type from character to numeric

# Changing the data type of pcv, wc, rc character to numeric
df$pcv <- as.numeric(df$pcv)
df$wc <- as.numeric(df$wc)
df$rc <- as.numeric(df$rc)
# checking the data type of the columns again to confirm the changes
str(df$pcv)
str(df$wc)
str(df$rc)

#Now changing the data type boolean values to factor with levels yes and no Levels are 1 and 0 respectively in the colum
#of dm, cad, pe, ane 

df$dm <- as.factor(df$dm)
df$htn <- as.factor(df$htn)
df$cad <- as.factor(df$cad)
df$appet <- as.factor(df$appet)
df$pe <- as.factor(df$pe)
df$ane <- as.factor(df$ane)

# checking the data type of the columns again to confirm the changes
str(df$htn)
str(df$dm)
str(df$cad)
str(df$pe)
str(df$ane)

# Checking the statical summary again to see
summary(df)
View(df)
table(df$cad)
table(df$rbc)
table(df$pc)

# checking the missing values
colSums(is.na(df))

#The missing vale is age, bp,sg, al, su, bgr, bu, sc, sod,pot, hemo, pcv,wc and rc.Now for missing value need to check 
#the column type. If the column type is numeric then we can replace the missing value with the average of the column. 
#If the column type is character then we can replace the missing value with the mode of the column.

# Checking all info of all missing columns
summary(df$age)
summary(df$bp)
summary(df$sg)
summary(df$al)
summary(df$su)
summary(df$bgr)
summary(df$bu)
summary(df$sc)
summary(df$sod)
summary(df$pot)
summary(df$hemo)
summary(df$pcv)
summary(df$wc)
summary(df$rc)
summary(df$htn)
summary(df$dm)
summary(df$cad)
summary(df$pe)
summary(df$ane)

# Assign the mean of the column to the variable mean_column than replace the missing value with column.

# age
age_mean <- mean(df$age, na.rm = TRUE)
df$age[is.na(df$age)] <- age_mean
# bp
bp_mean <- mean(df$bp, na.rm = TRUE)
df$bp[is.na(df$bp)] <- bp_mean
# sg
sg_mean <- mean(df$sg, na.rm = TRUE)
df$sg[is.na(df$sg)] <- sg_mean
# al
al_mean <- mean(df$al, na.rm = TRUE)
df$al[is.na(df$al)] <- al_mean
# su
su_mean <- mean(df$su, na.rm = TRUE)
df$su[is.na(df$su)] <- su_mean
# bgr
bgr_mean <- mean(df$bgr, na.rm = TRUE)
df$bgr[is.na(df$bgr)] <- bgr_mean
# bu
bu_mean <- mean(df$bu, na.rm = TRUE)
df$bu[is.na(df$bu)] <- bu_mean
# sc
sc_mean <- mean(df$sc, na.rm = TRUE)
df$sc[is.na(df$sc)] <- sc_mean
# sod
sod_mean <- mean(df$sod, na.rm = TRUE)
df$sod[is.na(df$sod)] <- sod_mean
# pot
pot_mean <- mean(df$pot, na.rm = TRUE)
df$pot[is.na(df$pot)] <- pot_mean
# hemo
hemo_mean <- mean(df$hemo, na.rm = TRUE)
df$hemo[is.na(df$hemo)] <- hemo_mean
# pcv
pcv_mean <- mean(df$pcv, na.rm = TRUE)
df$pcv[is.na(df$pcv)] <- pcv_mean
# wc
wc_mean <- mean(df$wc, na.rm = TRUE)
df$wc[is.na(df$wc)] <- wc_mean
# rc
rc_mean <- mean(df$rc, na.rm = TRUE)
df$rc[is.na(df$rc)] <- rc_mean

# View the summary of the data again
summary(df)
View(df)
str(df)

# more info about the rbc column 
summary(df$rbc)
table(df$rbc)


# rbc column has 152 missing values and 47% of the data is missing
# and there is only two values in the column, normal and abnormal
# so we will fill the missing value with "unknown" value
df$rbc <- factor(df$rbc, levels = c("normal", "abnormal", ""), 
                 labels = c("normal", "abnormal", "unknown"))
df$rbc[is.na(df$rbc)] <- "unknown"

# checking the value count of the rbc column 
# for the unknown value is assigned or not
table(df$rbc)

# Now we will check the pc column
summary(df$pc)
table(df$pc)


# pc column has 65 missing values and 21% of the data is missing and there is only two values in the column, normal and abnormal
# so we will fill the missing value with "unknown" value

df$pc <- factor(df$pc, levels = c("normal", "abnormal", ""), 
                labels = c("normal", "abnormal", "unknown"))
df$pc[is.na(df$pc)] <- "unknown"

# now cheek one more time the value count of pc column
table(df$pc)

# Now we will check the pcc column
summary(df$pcc)
table(df$pcc)


# pcc column has 4 missing values and 1% of the data is missing
# and there is only two values in the column, present and notpresent
# so we will fill the missing value with "unknown" value

df$pcc <- factor(df$pcc, levels = c("present", "notpresent", ""), 
                 labels = c("present", "notpresent", "unknown"))
df$pcc[is.na(df$pcc)] <- "unknown"

# Check the fixing pcc column
table(df$pcc)

# Now we will check the ba column
summary(df$ba)
table(df$ba)

# ba column has 4 missing values and 1% of the data is missing
# and there is only two values in the column, present and notpresent
# so we will fill the missing value with "unknown" value

df$ba <- factor(df$ba, levels = c("present", "notpresent", ""), 
                labels = c("present", "notpresent", "unknown"))
df$ba[is.na(df$ba)] <- "unknown"

# now cheek one moretime the value count of ba column
table(df$ba)

# Now we will check the htn column
summary(df$htn)
table(df$htn)

# After checking the summary of the htn column, we can
# see that there are 2 missing values and 1% of the
# data is missing and there are two values in the column
# yes and no, so we will fill the missing value with "unknown" value

df$htn <- factor(df$htn, levels = c("yes", "no", ""), 
                 labels = c("yes", "no", "unknown"))
df$htn[is.na(df$htn)] <- "unknown"

# now cheek one moretime the value count of htn column
table(df$htn)

# Now we will check the dm column
summary(df$dm)
table(df$dm)

# After checking the summary of the dm column, we can
# see that there are 2 missing values and 1% of the
# data is missing and there are two values in the column
# yes and no, so we will fill the missing value with "unknown" value

df$dm <- factor(df$dm, levels = c("yes", "no", ""), 
                labels = c("yes", "no", "unknown"))
df$dm[is.na(df$dm)] <- "unknown"

# now cheek one moretime the value count of dm column
table(df$dm)

# Now we will check the cad column
table(df$cad)
summary(df$cad)

# After checking the summary of the cad column, we can
# see that there are 2 missing values and 1% of the
# data is missing and there are two values in the column
# yes and no, so we will fill the missing value with "unknown" value

df$cad <- factor(df$cad, levels = c("yes", "no", ""), 
                 labels = c("yes", "no", "unknown"))
df$cad[is.na(df$cad)] <- "unknown"

# now cheek one more time the value count of cad column
table(df$cad)

# Now we will check the appet column
table(df$appet)
summary(df$appet)

# After checking the summary of the appet column, we can
# see that there are 1 missing values and 0.3% of the
# data is missing and there are two values in the column
# good and poor, so we will fill the missing value with "unknown" value

df$appet <- factor(df$appet, levels = c("good", "poor", ""), 
                   labels = c("good", "poor", "unknown"))
df$appet[is.na(df$appet)] <- "unknown"  

# now cheek one moretime the value count of appet column
table(df$appet)

# Now we will check the pe column
table(df$pe)
summary(df$pe)

# After checking the summary of the pe column, we can
# see that there are 1 missing values and 0.3% of the
# data is missing and there are two values in the column
# yes and no, so we will fill the missing value with "unknown" value

df$pe <- factor(df$pe, levels = c("yes", "no", ""), 
                labels = c("yes", "no", "unknown"))
df$pe[is.na(df$pe)] <- "unknown"

# now cheek one moretime the value count of pe column
table(df$pe)

# Now we will check the ane column
table(df$ane)
summary(df$ane)

# After checking the summary of the ane column, we can
# see that there are 1 missing values and 0.3% of the
# data is missing and there are two values in the column
# yes and no, so we will fill the missing value with "unknown" value

df$ane <- factor(df$ane, levels = c("yes", "no", ""), 
                 labels = c("yes", "no", "unknown"))
df$ane[is.na(df$ane)] <- "unknown"

# now cheek one moretime the value count of ane column
table(df$ane)

# Now we will check the classification column
table(df$classification)
summary(df$classification)

# After checking the summary of the classification column, we can
# see that there are 2 missing input and 0.3% of the
# data is missing and there are two values in the column
# ckd and notckd, so we will fill the missing value with "unknown" value

df$classification <- factor(df$classification, levels = c("ckd", "notckd", ""), 
                            labels = c("ckd", "notckd", "unknown"))
df$classification[is.na(df$classification)] <- "unknown"

# now cheek one moretime the value count of classification column
table(df$classification)

# after completing the missing value treatment, we will 
# import the data into the new csv file
# this new csv file we can apply for the model building
# And we can Do more visualization and analysis on 
# this Clean data.
write.csv(df, "clean_kidney_disease.csv", row.names = FALSE)




