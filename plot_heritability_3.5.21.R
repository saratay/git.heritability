
library(readxl)
library(ggplot2)
library(gridExtra)

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

setwd("~/Sheets & Scripts/Heritability figures") #set working directory

df <- read_excel("heritability_results_univar_11.6.20.xlsx")
df$stdmin <- df$H2r - df$H2r_stderr
df$stdmax <- df$H2r + df$H2r_stderr


#reorder Variables for x-axis
# order by domain
df$Variable <- factor(df$Variable, levels = c("AQ","SRSirTotal","SRSsrTotal",
                                              "BAPQtotal",
                                              "BAPQaloof","ER40CR","ER40RT",
                                              "LSAS","SRSirCog","SRSsrCog",      
                                              "ABC","SRSirRRB","SRSsrRRB",
                                              "BRIEFir","BRIEFsr",
                                              "PCETACC","PCETRT",
                                              "PMATCR","PMATRT"))   

#create column for labeling each behavioral domain
df$Domain <- c("Social","Social",
               "Executive Function","Executive Function",
               "Executive Function","Executive Function",
               "Total ASD","Social","Total BAP","Social",
               "Executive Function","Social",
               "Restricted, Repetive Behavior","Total ASD",
               "Total ASD","Social","Executive Function",
               "Restricted, Repetive Behavior",
               "Restricted, Repetive Behavior")

png("Figure4.png", width = 6, height = 3, units = 'in', res = 300)
p.byDomain <- ggplot(data=df, aes(x=Variable, y=H2r, fill = Domain, label = ifelse(p < 0.05, "*", "NS"))) +
  geom_bar(stat="identity", position=position_dodge()) +
  ylim (-0.15, 1.1) +
  geom_errorbar(aes(ymin=stdmin, ymax=stdmax), width=.2,position=position_dodge(.9)) +
  geom_text(vjust = -7, size = 2) +
  theme_bw(base_size = 8)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0))+
  ylab("Heritability")+
  xlab(" ")
p.byDomain
dev.off()