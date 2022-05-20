# bivariate heritability graphing
# incorporate p-values by greying squares for values now passing significance

library(readxl)
library(Hmisc)
library(corrplot)
library(ggplot2)
library(ggpubr)

# Read in and format data -------------------------------------------------
setwd("~/Sheets & Scripts/Heritability figures") #set working directory
df <- read_excel("heritability_results_bivar_11.19.20.xlsx")

df$RhoGoriginal <- df$RhoG
for(i in 1:nrow(df)){
  if (df[i,"BH_sig"] == "N"){
    df[i,"RhoG"] <- NA
  }
}

cnp <- subset(df, df$Source=="CNP")
#cut out traits that are not heritable
cnp <- subset(cnp, Trait1 != "PCETACC" & Trait1 != "PMATCR" & 
                Trait1 != "PMATRT" & Trait2 != "PCETACC" & 
                Trait2 != "PMATCR" & Trait2 != "PMATRT" )

sr <- subset(df,df$Source=="Self Report")
ir <- subset(df,df$Source=="Informant Report")

# ggplot heat map ---------------------------------------------------------
#maybe deepskyblue for the high
p1 <- ggplot(data = cnp, aes(x=Trait1, y=Trait2, fill=RhoG)) + 
  geom_tile()+theme_bw() +
  geom_text(aes(Trait1, Trait2, label = RhoGoriginal), color = "black", size = 2.5) +
  scale_fill_gradient2(high = "forestgreen", low = "red3", mid = "yellow", 
                       midpoint = 0, limit = c(-1,1), na.value = "grey80")+
  theme_bw(base_size = 8)+
  theme(axis.text.x = element_text(angle = 90)) 

p2 <- ggplot(data = sr, aes(x=Trait1, y=Trait2, fill=RhoG)) + 
  geom_tile()+theme_bw() +
  geom_text(aes(Trait1, Trait2, label = RhoGoriginal), color = "black", size = 2.5) +
  scale_fill_gradient2(high = "forestgreen", low = "red3", mid = "yellow", 
                       midpoint = 0, limit = c(-1,1), na.value = "grey80") +
  theme_bw(base_size = 8)+
  theme(axis.text.x = element_text(angle = 90))

p3 <- ggplot(data = ir, aes(x=Trait1, y=Trait2, fill=RhoG)) + 
  geom_tile()+theme_bw() +
  geom_text(aes(Trait1, Trait2, label = RhoGoriginal), color = "black", size = 2.5) +
  scale_fill_gradient2(high = "forestgreen", low = "red3", mid = "yellow", 
                       midpoint = 0, limit = c(-1,1), na.value = "grey80")+
  theme_bw(base_size = 8)+
  theme(axis.text.x = element_text(angle = 90))

png("Figure5.png", width = 1800, height = 1200, units = 'px', res = 300)
ggarrange(p2,p1,p3,
          ncol = 2,
          nrow = 2,
          widths = c(1.2,0.6),
          heights = c(1.1,1),
          labels = "AUTO",
          common.legend = TRUE, 
          legend = "right")
dev.off()