# demographic table fro heritability paper

library(stats)

# Read in and format data -------------------------------------------------

setwd("~/Actual Documents/UPenn 4th Year/Scripts and Sheets/heritability") #set working directory
cnp <- read.csv("phenotypeFile_CNP_11.4.csv") #read in remote CNP data
self <- read.csv("phenotypeFile_sr_raw_11.4.csv") #read in self-report scores
inf <- read.csv("phenotypeFile_ir_raw_11.4.csv") #read in informant-report scores and informant information
dem <- read.csv("Dem_12.1.20.csv") #read in demographic information

dem$recruit_type <- as.factor(dem$recruit_type) #convert recruit_type to a meaningful factor
levels(dem$recruit_type) <- c("proband","family member")
dem$part_sex <- as.factor(dem$part_sex) #convert sex to a meaningful factor
levels(dem$part_sex) <- c("Female", "Male")
colnames(dem)[1] <- "ID"

# filter out participants without data
cnp.na <- na.omit(cnp[,1:3])
self.na <- na.omit(self[,1:3])
inf.na <- na.omit(inf[,1:3])

#merge each source's DF with participants with data with dem info
merged.cnp.na <- merge(cnp.na, dem, by = "ID")
merged.self.na <- merge(self.na, dem, by = "ID")
merged.inf.na <- merge(inf.na, dem, by = "ID")

#get n for each source by recruit type
table(merged.cnp.na$recruit_type)
table(merged.self.na$recruit_type)
table(merged.inf.na$recruit_type)

#develop list of participant ids from SR, IR, and CNP (separate for probands and family members)
cnp.id.list <- merged.cnp.na$ID
self.id.list <- merged.self.na$ID
inf.id.list <- merged.inf.na$ID

id.list <- union (cnp.id.list, self.id.list)
id.list <- union (id.list, inf.id.list)

#use this list of study ids to cut down dem DF
dem.filtered <- dem[dem$ID %in% id.list, ] 

# Calculate demographics --------------------------------------------------

#make DF to fill values into
DF <- data.frame(Probands = integer(), AllFamily = integer())


dem.p <- subset (dem.filtered, dem.filtered$recruit_type == "proband") #subset probands with demographic information
dem.f <- subset (dem.filtered, dem.filtered$recruit_type == "family member") #subset family members with demographic information


DF[1,1] <- min(na.omit(dem.p$part_age_screen)) #add min proband age to DF
DF[2,1] <- max(na.omit(dem.p$part_age_screen)) #add max proband age to DF
DF[1,2] <- min(na.omit(dem.f$part_age_screen)) #add min all fam age to DF
DF[2,2] <- max(na.omit(dem.f$part_age_screen)) #add max all fam age to DF


DF[3,1] <- mean(na.omit(dem.p$part_age_screen)) #add mean proband age to DF
DF[4,1] <- sd(na.omit(dem.p$part_age_screen)) #add sd proband age to DF
DF[3,2] <- mean(na.omit(dem.f$part_age_screen)) #add mean all fam age to DF
DF[4,2] <- sd(na.omit(dem.f$part_age_screen)) #add sd all fam age to DF

DF[5,1] <- (table(dem.p$part_sex)[1]/(table(dem.p$part_sex)[1]+
                                       table(dem.p$part_sex)[2]))*100 #add % female probands to DF
DF[5,2] <- (table(dem.f$part_sex)[1]/(table(dem.f$part_sex)[1]+
                                       table(dem.f$part_sex)[2]))*100 #add % female all family to DF

#take out values with multiple race boxes checked
race.p <- subset (dem.p, dem.p$mult_race == 0)
race.f <- subset (dem.f, dem.f$mult_race == 0)

#note: part_race_v2_1 is White, _2 is Black, _3 is Asian, _4 is American Indian / Alaska Native
# _5 Hawaiian / Pacific Islander, _8 Middle Eastern, _6 Other, _7 Decline to Answer

DF[6,1] <- (table(race.p$part_race_v2___4)[2]/nrow(dem.p))*100 #percent American Indian proband
DF[6,2] <- (table(race.f$part_race_v2___4)[2]/nrow(dem.f))*100 #percent American Indian all fam
DF[7,1] <- (table(race.p$part_race_v2___3)[2]/nrow(dem.p))*100 #percent Asian proband
DF[7,2] <- (table(race.f$part_race_v2___3)[2]/nrow(dem.f))*100 #percent Asian all fam
DF[8,1] <- (table(race.p$part_race_v2___2)[2]/nrow(dem.p))*100 #percent Black proband
DF[8,2] <- (table(race.f$part_race_v2___2)[2]/nrow(dem.f))*100 #percent Black all fam
DF[9,1] <- (table(race.p$part_race_v2___5)[2]/nrow(dem.p))*100 #percent Hawaiian proband
DF[9,2] <- (table(race.f$part_race_v2___5)[2]/nrow(dem.f))*100 #percent Hawaiian all fam
DF[10,1] <- (table(race.p$part_race_v2___8)[2]/nrow(dem.p))*100 #percent Middle Eastern proband
DF[10,2] <- (table(race.f$part_race_v2___8)[2]/nrow(dem.f))*100 #percent Middle Eastern all fam
DF[11,1] <- (table(dem.p$mult_race)[2]/nrow(dem.p))*100 #percent Multiracial proband
DF[11,2] <- (table(dem.f$mult_race)[2]/nrow(dem.f))*100 #percent Multiracial all fam
DF[12,1] <- (table(race.p$part_race_v2___6)[2]/nrow(dem.p))*100 #percent Other proband
DF[12,2] <- (table(race.f$part_race_v2___6)[2]/nrow(dem.f))*100 #percent Other all fam
DF[13,1] <- (table(race.p$part_race_v2___1)[2]/nrow(dem.p))*100 #percent White proband
DF[13,2] <- (table(race.f$part_race_v2___1)[2]/nrow(dem.f))*100 #percent White all fam



row.names(DF) <- c("Age Min", "Age Max", "Age Mean", "Age SD", 
                   "% Female","% American Indian / Alaska Native","% Asian",
                   "% Black","% Hawaiian / Pacific Islander","% Middle Eastern",
                   "% Multiracial","%Other","% White")

write.csv(DF, "DemTable_1.19.21.csv")