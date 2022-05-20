#make figures for distribution of each of the measures in probands and family members for heritability paper

library(ggplot2)
library(ggpubr)

# Read in and format data -------------------------------------------------

setwd("~/Actual Documents/UPenn 4th Year/Scripts and Sheets/heritability") #set working directory
cnp <- read.csv("phenotypeFile_CNP_11.4.csv") #read in remote CNP data
self <- read.csv("phenotypeFile_sr_raw_11.4.csv") #read in self-report scores
inf <- read.csv("phenotypeFile_ir_raw_11.4.csv") #read in informant-report scores and informant information
dem <- read.csv("Dem_1.11.21.csv") #read in demographic information
dem$part_sex <- as.factor(dem$part_sex) #convert sex to a meaningful factor
levels(dem$part_sex) <- c("Female", "Male")
colnames(dem)[1] <- "ID"

#dem$recruit_type_deg_collapsed <- dem$recruit_type_deg
for (i in 1:nrow(dem)){
  if (dem[i,"recruit_type_deg"] == "1" | dem[i,"recruit_type_deg"] == "2A"){
    dem[i,"recruit_type_deg_collapsed"] <- dem[i,"recruit_type_deg"]
  }
  else{
    dem[i,"recruit_type_deg_collapsed"] <- "other family member"
  }
}

dem$recruit_type_deg <- as.factor(dem$recruit_type_deg) #convert recruit_type to a meaningful factor
levels(dem$recruit_type_deg) <- c("proband","first degree family member",
                                  "second degree family member", 
                                  "third degree family member",
                                  "fourth degree family member",
                                  "spouse of proband",
                                  "spouse of proband family member",
                                  "fifth and above degree family member")
dem$recruit_type_deg_collapsed <- as.factor(dem$recruit_type_deg_collapsed) #convert recruit_type to a meaningful factor
levels(dem$recruit_type_deg_collapsed) <- c("proband","1st deg. relative",
                                  "other relative")


#merge each source's DF with participants with data with dem info
merged.cnp <- merge(cnp, dem, by = "ID")
merged.self <- merge(self, dem, by = "ID")
merged.inf <- merge(inf, dem, by = "ID")


# CNP Violin plots ------------------------------------------------------------

p1 <- ggplot(merged.cnp, aes(x=recruit_type_deg_collapsed, y=KER40ER40CR, 
                             color = recruit_type_deg_collapsed, fill = recruit_type_deg_collapsed)) + 
  geom_jitter(position=position_jitter(0.2))+
  geom_violin(trim = FALSE, alpha = 0.3) + labs(x="Participant Status", y = "ER40 CR") + theme_bw()

p2 <- ggplot(merged.cnp, aes(x=recruit_type_deg_collapsed, y=KER40ER40CRT, 
                             color = recruit_type_deg_collapsed, fill = recruit_type_deg_collapsed)) + 
  geom_jitter(position=position_jitter(0.2))+
  geom_violin(trim = FALSE, alpha = 0.3) + labs(x="Participant Status", y = "ER40 RT") + theme_bw()

p3 <- ggplot(merged.cnp, aes(x=recruit_type_deg_collapsed, y=PMAT24APMAT24ACR, 
                             color = recruit_type_deg_collapsed, fill = recruit_type_deg_collapsed)) + 
  geom_jitter(position=position_jitter(0.2))+
  geom_violin(trim = FALSE, alpha = 0.3) + labs(x="Participant Status", y = "PMAT CR") + theme_bw()

p4 <- ggplot(merged.cnp, aes(x=recruit_type_deg_collapsed, y=PMAT24APMAT24ARTCR, 
                             color = recruit_type_deg_collapsed, fill = recruit_type_deg_collapsed)) + 
  geom_jitter(position=position_jitter(0.2)) +
  geom_violin(trim = FALSE, alpha = 0.3) + labs(x="Participant Status", y = "PMAT RT") + theme_bw()

p5 <- ggplot(merged.cnp, aes(x=recruit_type_deg_collapsed, y= PCETPCETACC2, 
                             color = recruit_type_deg_collapsed, fill = recruit_type_deg_collapsed)) + 
  geom_jitter(position=position_jitter(0.2))+
  geom_violin(trim = FALSE, alpha = 0.3) + labs(x="Participant Status", y = "PCET ACC") + theme_bw()

p6 <- ggplot(merged.cnp, aes(x=recruit_type_deg_collapsed, y= PCETPCETRTCR, 
                             color = recruit_type_deg_collapsed, fill = recruit_type_deg_collapsed)) + 
  geom_jitter(position=position_jitter(0.2)) +
  geom_violin(trim = FALSE, alpha = 0.3) + labs(x="Participant Status", y = "PCET RT") + theme_bw()

ggarrange(p1,p3,p5,p2,p4,p6,
  ncol = 3,
  nrow = 2,
  labels = "AUTO",
  legend = "none")

# SR Violin plots ------------------------------------------------------------

p1 <- ggplot(merged.self, aes(x=recruit_type_deg_collapsed, y=SRSsrTotal, 
                              color = recruit_type_deg_collapsed, fill = recruit_type_deg_collapsed)) + 
  geom_jitter(position=position_jitter(0.2))+
  geom_violin(trim = FALSE, alpha = 0.3) + labs(x="Participant Status", y = "SRS Total") + theme_bw()

p2 <- ggplot(merged.self, aes(x=recruit_type_deg_collapsed, y=SRSsrCog, 
                              color = recruit_type_deg_collapsed, fill = recruit_type_deg_collapsed)) + 
  geom_jitter(position=position_jitter(0.2))+
  geom_violin(trim = FALSE, alpha = 0.3) + labs(x="Participant Status", y = "SRS Cog") + theme_bw()

p3 <- ggplot(merged.self, aes(x=recruit_type_deg_collapsed, y=BAPQtotal, 
                              color = recruit_type_deg_collapsed, fill = recruit_type_deg_collapsed)) + 
  geom_jitter(position=position_jitter(0.2))+
  geom_violin(trim = FALSE, alpha = 0.3) + labs(x="Participant Status", y = "BAPQ Total") + theme_bw()

p4 <- ggplot(merged.self, aes(x=recruit_type_deg_collapsed, y=BAPQaloof, 
                              color = recruit_type_deg_collapsed, fill = recruit_type_deg_collapsed)) + 
  geom_jitter(position=position_jitter(0.2)) +
  geom_violin(trim = FALSE, alpha = 0.3) + labs(x="Participant Status", y = "BAPQ Aloof") + theme_bw()

p5 <- ggplot(merged.self, aes(x=recruit_type_deg_collapsed, y= BRIEFsr, 
                              color = recruit_type_deg_collapsed, fill = recruit_type_deg_collapsed)) + 
  geom_jitter(position=position_jitter(0.2))+
  geom_violin(trim = FALSE, alpha = 0.3) + labs(x="Participant Status", y = "BRIEF Total") + theme_bw()

p6 <- ggplot(merged.self, aes(x=recruit_type_deg_collapsed, y= LSAS, 
                              color = recruit_type_deg_collapsed, fill = recruit_type_deg_collapsed)) + 
  geom_jitter(position=position_jitter(0.2)) +
  geom_violin(trim = FALSE, alpha = 0.3) + labs(x="Participant Status", y = "LSAS") + theme_bw()

p7 <- ggplot(merged.self, aes(x=recruit_type_deg_collapsed, y= SRSsrRRB, 
                              color = recruit_type_deg_collapsed, fill = recruit_type_deg_collapsed)) + 
  geom_jitter(position=position_jitter(0.2)) +
  geom_violin(trim = FALSE, alpha = 0.3) + labs(x="Participant Status", y = "SRS RRB") + theme_bw()

p8 <- ggplot(merged.self, aes(x=recruit_type_deg_collapsed, y= AQtotal, 
                              color = recruit_type_deg_collapsed, fill = recruit_type_deg_collapsed)) + 
  geom_jitter(position=position_jitter(0.2)) +
  geom_violin(trim = FALSE, alpha = 0.3) + labs(x="Participant Status", y = "AQ Total") + theme_bw()


ggarrange(p1,p8,p3,
          p2,p4,p6,
          p7,p5,
          ncol = 3,
          nrow = 3,
          labels = "AUTO",
          legend = "none")

ggarrange(p1,p8,p3,
          p2,p4,p6,
          ncol = 3,
          nrow = 2,
          labels = "AUTO",
          legend = "none")
ggarrange(p7,p5,
          ncol = 3,
          nrow = 1,
          labels = c("G","H"),
          legend = "none")

# IR Violin plots ------------------------------------------------------------

p1 <- ggplot(merged.inf, aes(x=recruit_type_deg_collapsed, y=SRSirTotal, 
                             color = recruit_type_deg_collapsed, fill = recruit_type_deg_collapsed)) + 
  geom_jitter(position=position_jitter(0.2))+
  geom_violin(trim = FALSE, alpha = 0.3) + labs(x="Participant Status", y = "SRS Total") + theme_bw()

p2 <- ggplot(merged.inf, aes(x=recruit_type_deg_collapsed, y=SRSirCog, 
                             color = recruit_type_deg_collapsed, fill = recruit_type_deg_collapsed)) + 
  geom_jitter(position=position_jitter(0.2))+
  geom_violin(trim = FALSE, alpha = 0.3) + labs(x="Participant Status", y = "SRS Cog") + theme_bw()

p3 <- ggplot(merged.inf, aes(x=recruit_type_deg_collapsed, y= BRIEFir, 
                             color = recruit_type_deg_collapsed, fill = recruit_type_deg_collapsed)) + 
  geom_jitter(position=position_jitter(0.2))+
  geom_violin(trim = FALSE, alpha = 0.3) + labs(x="Participant Status", y = "BRIEF Total") + theme_bw()

p4 <- ggplot(merged.inf, aes(x=recruit_type_deg_collapsed, y= SRSirRRB, 
                             color = recruit_type_deg_collapsed, fill = recruit_type_deg_collapsed)) + 
  geom_jitter(position=position_jitter(0.2)) +
  geom_violin(trim = FALSE, alpha = 0.3) + labs(x="Participant Status", y = "SRS RRB") + theme_bw()

p5 <- ggplot(merged.inf, aes(x=recruit_type_deg_collapsed, y= ABCstereotypy, 
                             color = recruit_type_deg_collapsed, fill = recruit_type_deg_collapsed)) + 
  geom_jitter(position=position_jitter(0.2)) +
  geom_violin(trim = FALSE, alpha = 0.3) + labs(x="Participant Status", y = "ABC Sterotypy") + theme_bw()


ggarrange(p1,p2,p3,
          p4,p5,
          ncol = 3,
          nrow = 2,
          labels = "AUTO",
          legend = "none")