#upload libraries
library(survival)
library(survminer)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(ggpubr)
library(ggsci)
library(readxl)
library(showtext)
library(lubridate)
library(cowplot)
library(grid)
library(egg)

data_analysis <- read_excel("C:/Users/lblan/OneDrive/Escritorio/CEAB/2022/First_chapter/data_analysis_cens.xlsx",
                            col_types = c("numeric", "date","numeric", "date",
                                          "numeric", "numeric", "numeric", "numeric"))

clima_field <- read_excel("C:/Users/lblan/OneDrive/Escritorio/PrimerCap/Chapter-Survival/PrimerCap/clima_field.xlsx", 
                          col_types = c("date", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric"))

#Dichotomize location and method, and change data labels
data_analysis$`hl/bg` <- factor(data_analysis$`hl/bg`, 
                                  levels = c("1", "2"), 
                                  labels = c("HLC", "BG"))
data_analysis$location <- factor(data_analysis$location, 
                                   levels = c("1", "2", "3"), 
                                   labels = c("Vegetated", "Urban", "Laboratory"))


clima_field$location <- factor(clima_field$location, 
                                 levels = c("1", "2"), 
                                 labels = c("Vegetated", "Urban"))

datos_field <- filter(data_analysis, !location == "Laboratory")
datos_field <- inner_join(data_analysis, clima_field, by = c("start_date", "location"), all= TRUE) #Combining temperature of each location with the data

datos_field <- datos_field %>%
  rename(method = 'hl/bg')

data_analysis <- data_analysis %>%
  rename(method = 'hl/bg')

################################################################3
#Fit survival with Kaplan-Meier method
##LOCATION (LAB AND FIELD)
survfit(Surv(total_lived, censored) ~ location, data = data_analysis, type = "kaplan-meier") 

ggsurvplot(survfit(Surv(total_lived, censored) ~ location, data = data_analysis, type = "kaplan-meier"),pval = TRUE)


Fig1 <-  survfit(Surv(total_lived, censored) ~ location, data_analysis, conf.type = "log-log") %>%
  ggsurvplot(
    conf.int = T,
    legend.title = "",
    legend.labs = c("Vegetated", "Urban", "Laboratory"),
    ylab="Survival probability", xlab="Total of days lived",
    legend= c(0.7,0.7),
    surv.plot.heigh = 1.30,
    surv.median.line = "hv", # Specify median survival
    break.x.by = 10,
    font.tickslab = c(30),
    font.y = c(30),
    font.x = c(30),
    font.family = "Lato",
    palette =c("#0C4A95", "#E38B00", "#00956C"),
    ggtheme = theme_bw((base_size=30)))
Fig1

Fig1$plot <- Fig1$plot+ 
  ggplot2::annotate("text", 
                    x = 80, y = 1, # x and y coordinates of the text
                    label = "a", size = 15, fontface = "bold")
Fig1$plot

##METHOD (FIELD)
data_analysis <- data_analysis %>% 
 rename(method = `hl/bg`)

survfit(Surv(total_lived, censored) ~ method, data = data_analysis, type = "kaplan-meier")

ggsurvplot(survfit(Surv(total_lived, censored) ~ method, data = data_analysis, type = "kaplan-meier"),pval = TRUE)

Fig2 <-  survfit(Surv(total_lived, censored) ~ method, data = data_analysis, type = "kaplan-meier") %>%
  ggsurvplot(
    surv.median.line = "hv",
    ylab="Survival probability", xlab="Total of days lived",
    surv.plot.heigh = 1.30,
    break.x.by = 10,
    font.tickslab = c(16),
    pval = T,
    pval.method = T,
    font.y = c(16),
    font.x = c(16),
    conf.int = TRUE,
    risk.table = TRUE,
    tables.col = "black",
    risk.table.pos="out",
    risk.table.title="",
    risk.table.fontsize = 4.75,
    font.family = "Lato",
    tables.height = 0.25,
    tables.theme = theme_bw(),
    palette =c("#FDE725FF", "#414487FF"),
    ggtheme = theme_bw((base_size=16)))

Fig2


#This is a plot about the survival between locations by method of capture#
Fig3<- survfit(Surv(total_lived, censored)~ method + location, datos_field, conf.type="log-log")%>%
  ggsurvplot(
    conf.int = T, 
    facet.by = "location",
    legend.title = "Method of capture", 
    short.panel.labs = T,
    legend.labs = c("BGT method", "HLC method"),
    ylab="Survival probability", xlab="Total of days lived ",
    legend= c(0.83,0.65),
    surv.plot.heigh = 1.30,
    surv.median.line = "hv", # Specify median survival
    break.x.by = 10,
    font.tickslab = c(22),
    font.y = c(22),
    font.x = c(22),
    font.family = "Lato",
    palette =c("#0C4A95","#E38B00"),
    ggtheme = theme_bw(base_size=22),
  )
Fig3 <- tag_facet(Fig3, open = "", close = "", fontface = "bold", size = 16, 
                  hjust = c(-20, -18))
Fig3<- Fig3 + theme(strip.text.x = element_blank())
Fig3 <- ggarrange(Fig3 + rremove("ylab") + rremove("xlab"))

#This is a plot about the survival between same methods of capture in the different locations#
Fig4<- survfit(Surv(total_lived, censored)~ location + method, data_analysis, conf.type="log-log")%>%
  ggsurvplot(
    conf.int = T, 
    facet.by = "method",
    legend.title = "Location", 
    short.panel.labs = T,
    legend.labs = c("Vegetated", "Urban"),
    ylab="Survival probability", xlab="Total of days lived",
    legend= c(0.83,0.65),
    surv.plot.heigh = 1.30,
    surv.median.line = "hv", # Specify median survival
    break.x.by = 10,
    font.tickslab = c(22),
    font.y = c(22),
    font.x = c(22),
    font.family = "Lato",
    palette =c( "#E35200","#00956C"),
    ggtheme = theme_bw(base_size=22),
  )
my_tag <- c("c", "d")

Fig4
Fig4 <- tag_facet(Fig4, open = "", close = "", fontface = "bold", size = 16, 
                  tag_pool = my_tag, hjust = c(-20, -18))
Fig4<- Fig4 + theme(strip.text.x = element_blank())
Fig4 <- ggarrange(Fig4 + rremove("ylab") + rremove("xlab"))

FIG5<-plot_grid(Fig3, Fig4, nrow = 2)

FIG5<- annotate_figure(FIG5, left = textGrob("Survival Probability", rot = 90, vjust = 0.4, gp = gpar(cex = 1.8)),
                                  bottom = textGrob("Total of days lived", gp = gpar(cex = 1.8)))
FIG5
ggsave("C:\\Users\\lblan\\OneDrive\\Escritorio\\PrimerCap\\Chapter-Survival\\PrimerCap\\Kaplan-Meier y Parametrico\\Kaplan-Meier\\FIG5.pdf", FIG5, width = 430, height = 300, units = "mm")

survdiff(Surv(total_lived, censored)~location + strata(method), data = datos_field)
ggsurvplot(survfit(Surv(total_lived, censored)~location + strata(method), data = datos_field, type = "kaplan-meier"),pval = TRUE)

Figp<- survfit(Surv(total_lived, censored)~location + strata(method), data = datos_field, conf.type="log-log")%>%
  ggsurvplot(
    conf.int = F, 
    censor = F,
    legend.title= "",
    legend.labs = c("Vegetated with HLC", "Vegetated with BGT", "Urban with HLC", "Urban with BGT"),
    ylab="Survival probability", xlab="Total of days lived",
    legend= c(0.7,0.7),
    surv.plot.heigh = 1.30,
    surv.median.line = "hv", # Specify median survival
    break.x.by = 10,
    linetype = c("solid", "dashed", "solid", "dashed"),
    size = 1.25,
    font.tickslab = c(28),
    font.y = c(28),
    font.x = c(28),
    font.family = "Lato",
    palette =c("#0C4A95", "#E38B00", "#E35200", "#00956C"),
    ggtheme = theme_bw(base_size=28),
  )
Figp<- ggpar(Figp, font.legend = 25)
Figp
Figp$plot <- Figp$plot+ 
  ggplot2::annotate("text", 
                    x = 80, y = 1, # x and y coordinates of the text
                    label = "b", size = 15, fontface = "bold")
Figp$plot

splots <- list(Fig1, Figp)
Fig_KMc <- arrange_ggsurvplots(splots, ncol = 2)
ggsave("C:\\Users\\lblan\\OneDrive\\Escritorio\\PrimerCap\\Chapter-Survival\\PrimerCap\\Kaplan-Meier y Parametrico\\Kaplan-Meier\\Fig_KMc.pdf", Fig_KMc, width = 430, height = 200, units = "mm")



#ESTIMATION OF THE MEAN, MEDIAN AND PERCENTILES
survloc <- survfit(Surv(total_lived, censored) ~ location, data_analysis) #location 
survmet <- survfit(Surv(total_lived, censored) ~ method, data_analysis) #capture method
survlocfi <- survfit(Surv(total_lived, censored) ~ location, datos_field) #location field
survlocmetfi <- survfit(Surv(total_lived, censored) ~ method + location, datos_field) #location field


print(survloc, print.rmean = TRUE)
print(survmet, print.rmean = TRUE)
print(survlocfi, print.rmean = TRUE)

quantile(survloc, c(0.05, 0.5, 0.95))
quantile(survmet, c(0.05, 0.5, 0.95))
quantile(survlocfi, c(0.05, 0.5, 0.95))

#LOG-RANK TEST
survdiff(Surv(total_lived, censored) ~ location, data = data_analysis, rho = 0)  #Prueba log-rank LOCATION
survdiff(Surv(total_lived, censored) ~ location, data = data_analysis, rho = 1)  #Prueba log-rank LOCATION

survdiff(Surv(total_lived, censored) ~ method, data = datos_field, rho = 0) #Prueba log-rank METHOD
survdiff(Surv(total_lived, censored) ~ method, data = datos_field, rho = 1)  #Prueba log-rank METHOD

survdiff(Surv(total_lived, censored) ~ location, data = datos_field, rho = 0) #Prueba log-rank LOCATION FIELD
survdiff(Surv(total_lived, censored) ~ location, data = datos_field, rho = 1)  #Prueba log-rank LOCATION FIELD

survdiff(Surv(total_lived, censored) ~ location + method, data = datos_field, rho = 0) #Prueba log-rank LOCATION FIELD
survdiff(Surv(total_lived, censored) ~ method + location, data = datos_field, rho = 0) #Prueba log-rank LOCATION FIELD


#ESTIMATION OF THE CUMULATIVE RISK FUNCTION
R <- survloc %>% fortify %>% group_by(strata) %>% mutate(CumHaz = cumsum(n.event/n.risk))
print(R, 60)
ggsurvplot(survloc, fun = "cumhaz", xlab = "time (days)", censor = T, 
           ylab = "cumulative risk", title = "cumulative risk", legend.title = "Location")

S <- survmet %>% fortify %>% group_by(strata) %>% mutate(CumHaz = cumsum(n.event/n.risk))
print(S, 60)
ggsurvplot(survmet, fun = "cumhaz", xlab = "time (days)", censor = T, 
           ylab = "cumulative risk", title = "cumulative risk", legend.title = "Method")

SF <- survlocfi %>% fortify %>% group_by(strata) %>% mutate(CumHaz = cumsum(n.event/n.risk))
print(S, 60)
ggsurvplot(survlocfi, fun = "cumhaz", xlab = "time (days)", censor = T, 
           ylab = "cumulative risk", title = "cumulative risk", legend.title = "Method")


