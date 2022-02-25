library(extrafont)
library(readr)
library(ggplot2)
loadfonts()


df <- read_csv("../data/rq3_data/grades_ds.csv")

df$group_name <- as.factor(df$group_name)


p <- ggplot(df, aes(x=group_name, y=`Final Grade`, fill = group_name)) + 
  geom_violin(trim=FALSE)+
  scale_fill_manual(values=c("#E8999A", "#72a2e8"))+
  scale_y_continuous(breaks = seq(0, 10, by=1), limits=c(4,10))+
  labs(x="Tutoring", y = "Final Grade")+
  geom_boxplot(width=0.1) + 
  theme_classic() + theme(axis.text.x = element_text(colour="black"), 
                          axis.text.y = element_text(colour="black"), 
                          text = element_text(size=18, family = 'serif', colour="black"),
                          axis.ticks.y = element_line(color = c(NA, NA, NA, NA)),
                          axis.line.y.left = element_line(color = "white"),
                          panel.grid.major.y = element_line(colour = 'gray85'), 
                          panel.background = element_rect(colour = "white"),
                          legend.position = "top",
                          legend.title=element_blank()
                          ) 


sod <- read_csv("../data/rq3_data/grades_sod.csv")
ds <- read_csv("../data/rq3_data/grades_ds.csv")

sod_online <- subset(sod, group_name == "Online")
sod_inperson <- subset(sod, group_name == "In-person")

ds_online <- subset(ds, group_name == "Online")
ds_inperson <- subset(ds, group_name == "In-person")

qqnorm(sod$`Final Grade`)
qqline(sod$`Final Grade`)
shapiro.test(sod$`Final Grade`)

qqnorm(ds$`Final Grade`)
qqline(ds$`Final Grade`)
shapiro.test(ds$`Final Grade`)

# Use bootstrap K-S test to calculate exact p-values while ties are present across distributions
ks.boot(sod_inperson$`Final Grade`, sod_online$`Final Grade`)
ks.boot(ds_inperson$`Final Grade`, ds_online$`Final Grade`)