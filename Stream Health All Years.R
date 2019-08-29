#To open excel in R
library(openxlsx)
# To plot using ggplot2
library(ggplot2)
#To plot side by side or on top of each other
library(gridExtra)
#To use date_break functinoallity
library(scales)
library(lubridate)
library(rkt)
library(EnvStats)
library(zoo)
library(dplyr)
library(Kendall)
library(boot)
library(magrittr)
library(ggpubr)
library(ggthemes)
library(plotly)
library(psych)
library(shiny)

#path of spreadsheet
dbPath1 <- "TMDL_Stream_2010-2019_V3.xlsx"

# load the workbook

wb <- loadWorkbook(dbPath1)

#load the worksheets
SARU_Excel <-  read.xlsx(dbPath1, "SARU", detectDates = T)
LS_1_Excel <-  read.xlsx(dbPath1, "LS_1", detectDates = T)
JO_1_Excel <-  read.xlsx(dbPath1, "JO_1", detectDates = T)
PM_1_Excel <-  read.xlsx(dbPath1, "PM_1", detectDates = T)
NCLD_Excel <-  read.xlsx(dbPath1, "NCLD", detectDates = T)
HC_1_Excel <-  read.xlsx(dbPath1, "HC_1", detectDates = T)
HC_2_Excel <-  read.xlsx(dbPath1, "HC_2", detectDates = T)
NC_1_Excel <-  read.xlsx(dbPath1, "NC_1", detectDates = T)
WC_1_Excel <-  read.xlsx(dbPath1, "WC_1", detectDates = T)

# Combine Worksheets
AllCountsAmbient <- rbind(SARU_Excel, LS_1_Excel, JO_1_Excel, PM_1_Excel,
                          NCLD_Excel, HC_1_Excel,HC_2_Excel, NC_1_Excel, WC_1_Excel)
#Creat Year and Month Vectors

AllCountsAmbient$Month <- month(AllCountsAmbient$Date, label = TRUE, abbr = TRUE)

#get objects in correct form
AllCountsAmbient$pH <- as.numeric(AllCountsAmbient$pH)
AllCountsAmbient$Turbidity <- as.numeric(AllCountsAmbient$Turbidity)

AllCountsAmbient$Temp.Type <- ifelse(AllCountsAmbient$TEMP < 16, "below", "above")
AllCountsAmbient$DO.Type <- ifelse(AllCountsAmbient$DO > 9.5, "below", "above")
AllCountsAmbient$Turbidity.Type <- ifelse(AllCountsAmbient$Turbidity < 4, "below", "above")

#______________________________________________FINAL GRAPHS_____________________________________________________________________________________________________________________


#saving image as tiff file for high resolution....Publication Worthy!
#Temperature________
tiff("In-Situ_Temp_Ambient.tiff", units="in", width=12, height=8, res=500)
ggplot(subset(AllCountsAmbient, Site %in% c("HC-1", "HC-2", "NC-1", "WC-1")), aes(x=Date, y=TEMP, group = Site, colour = Temp.Type)) + geom_point(size=3, shape=21, fill="white") + geom_hline(yintercept = 16, color = "red",) + 
  geom_line(size=1) + geom_smooth(method = lm, se= FALSE) + facet_wrap(~Site, ncol = 1) + labs(title=NULL, subtitle=NULL, y="Temperature (Degrees Celcius)", x= "Date", caption = "Ambient Locations, 2010-2018") +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%b-%y")) +  theme_bw() + theme(axis.title.y = element_text(size=22), title = element_text(size = 18), axis.text.x = element_text(size = 15, angle = 65, vjust = 0.6),
                                                                                                    axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=16), axis.title.x = element_text(size = 18),
                                                                                                    legend.position = "none",  strip.text.x = element_text(size=12, colour = "steelblue3"))
dev.off()

tiff("In-Situ_Temp_TMDL.tiff", units="in", width=12, height=8, res=500)
ggplot(subset(AllCountsAmbient, Site %in% c("LS-1", "JO-1", "NCLD-1", "PM-1", "SARU")), aes(x=Date, y=TEMP, group = Site, colour = Temp.Type)) + geom_point(size=3, shape=21, fill="white") + geom_hline(yintercept = 16, color = "red",) + 
  geom_line(size=1) + geom_smooth(method = lm, se= FALSE) + facet_wrap(~Site, ncol = 1) + labs(title=NULL, subtitle=NULL, y="Temperature (Degrees Celcius)", x= "Date", caption = "TMDL Locations, 2010-2018") +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%b-%y")) + theme_bw() + theme(axis.title.y = element_text(size=22), title = element_text(size = 18), axis.text.x = element_text(size = 15, angle = 65, vjust = 0.6),
                                                                                                   axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=16), axis.title.x = element_text(size = 18),
                                                                                                   legend.position = "none",  strip.text.x = element_text(size=12, colour = "steelblue3"))
dev.off()

# Other temperature graphs found in Temp Loggers and Ambient Air folders.....

#Dissolved Oxygen______

tiff("DO_Over_Time.tiff", units="in", width=13, height=8, res=300)
ggplot(AllCountsAmbient, aes(x=Date, y=DO, group = Site, colour = Site)) + geom_point(size=3, shape=21, fill="white") + geom_hline(yintercept = 9.5, color = "red",) + 
  geom_line(size=1) + labs(title=NULL, subtitle=NULL, y="Dissolved Oxygen (mg/L)", x= "Date", caption = "Source: In-Situ Monitoring, 2010-2018") +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%b-%y")) + theme_bw() + theme(axis.title.y = element_text(size=22), axis.title.x = element_text(size=22), title = element_text(size = 18), axis.text.x = element_text(size = 18, angle = 65, vjust = 0.6),
                                                                                                   axis.text.y = element_text(size = 18), legend.title = element_text(size=22), legend.text = element_text(size=18)) +
  scale_y_continuous(breaks=c(5, 9, 13))

dev.off()

require(gridExtra)



tiff("DO_Over_Time_TMDL.tiff", units="in", width=12, height=6, res=300)
ggplot(subset(AllCountsAmbient, Site %in% c("LS-1", "JO-1", "NCLD-1", "PM-1", "SARU")) , aes(x=Date, y=DO, group = Site, colour = Site)) + geom_point(size=3, shape=21, fill="white") + geom_hline(yintercept = 9.5, color = "red",) + 
  geom_line(size=1) + labs(title=NULL, subtitle=NULL, y=NULL, x= NULL) +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%b-%y")) + theme_bw() + theme(axis.title.y = element_text(size=22), axis.title.x = element_text(size=22), title = element_text(size = 18), axis.text.x = element_blank(),
                                                                                                   axis.text.y = element_text(size = 18), legend.title = element_text(size=22), legend.text = element_text(size=18)) +
  scale_y_continuous(breaks=c(5, 9, 13))
dev.off()

tiff("DO_Over_Time_Ambient.tiff", units="in", width=12, height=6, res=300)
ggplot(subset(AllCountsAmbient, Site %in% c("HC-1", "HC-2", "NC-1", "WC-1")) , aes(x=Date, y=DO, group = Site, colour = Site)) + geom_point(size=3, shape=21, fill="white") + geom_hline(yintercept = 9.5, color = "red") + 
  geom_line(size=1) + labs(title=NULL, subtitle=NULL, y=NULL, x= "Date", caption = "Source: In-Situ Monitoring, 2010-2018") +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%b-%y")) + theme_bw() + theme(axis.title.y = element_text(size=22), axis.title.x = element_text(size=22), title = element_text(size = 18), axis.text.x = element_text(size = 18, angle = 65, vjust = 0.6),
                                                                                                   axis.text.y = element_text(size = 18), legend.title = element_text(size=22), legend.text = element_text(size=18)) +
  scale_y_continuous(breaks=c(5, 9, 13))
dev.off()

tiff("DO_Over_Summer_Boxplot.tiff", units="in", width=12, height=6, res=300)
ggplot(subset(AllCountsAmbient, Season %in% "Summer"), aes(Site, DO)) + geom_boxplot(aes(fill=Site), alpha=0.3) + geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = .5, fill="red") + 
  labs(title = NULL, subtitle=NULL, y="Summer Dissolved Oxygen (mg/L)", x=NULL) + geom_hline(yintercept = 9.5, color="red", size=1) +
  theme_bw() + theme(axis.title.y = element_text(size=22), axis.title.x = element_text(size=22), title = element_text(size = 18), axis.text.x = element_text(size = 18, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 18), legend.title = element_text(size=22), legend.text = element_text(size=18), legend.position = "none")
dev.off()

tiff("DO_facet.tiff", units="in", width=10, height=8, res=300)
ggplot(AllCountsAmbient[!is.na(AllCountsAmbient$DO),], aes(x=Date, y=DO, group = Site, color = DO.Type)) + geom_point(size=3, shape=21, fill = "white") + geom_hline(yintercept = 9.5, color = "red") +
  geom_line(size=1)  + labs(y="Dissolved Oxygen (mg/L)", x=NULL, caption="Source: In-Situ Ambient Monitoring, 2010-2017") + facet_wrap(~Site, ncol=2) +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%b-%y")) + theme(axis.text.x = element_text(angle = 90)) + theme_bw() + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
        axis.text.y = element_text(size = 15), legend.title = element_text(size=10), legend.text = element_text(size=10), legend.position = "none",  strip.text.x = element_text(size=12, colour = "steelblue3")) + scale_y_continuous(breaks=c(5, 9, 13))
dev.off()


#SpC___________
SCdf <- data.frame(Site = c("HC-1", "HC-2", "JO-1", "LS-1", "NC-1", "NCLD-1", "PM-1", "SARU", "WC-1"), SpC.Mean = c(191.9, 191.1, 179.8, 155.5, 163.7, 160.7, 181, 204.9, 243.7), 
                   std.dev = c(35.198, 31.771, 21.952, 43.018, 31.072, 28.843, 21.407, 28.385, 48.508))
SCdf$SpC.Type <- ifelse(SCdf$SpC.Mean < 186, "below", "above")
SCdf <- SCdf[order(SCdf$SpC.Mean), ]
SCdf$Site <- factor(SCdf$Site, levels = SCdf$Site)

tiff("SpC_Bar.tiff", units="in", width=8, height=4, res=300)
ggplot(SCdf, aes(x=Site, y=SpC.Mean, label=SpC.Mean)) + 
  geom_bar(stat='identity', aes(fill=SpC.Type), width=.5) + scale_fill_manual(name = "Specific Conductivity",
                                                                              labels = c("Above Average", "Below Average"),
                                                                              values = c("above"="#f8766d", "below"="#00ba38")) + 
  geom_errorbar(aes(ymin=SpC.Mean-std.dev, ymax=SpC.Mean+std.dev), width=.2,
                position=position_dodge(.9))  + coord_flip() +
  labs(title=NULL, subtitle=NULL, y="Specific Conductivity (uS/cm)") + theme_bw() + 
  theme(axis.title.y = element_text(size=12), axis.title.x = element_text(size = 12), title = element_text(size = 18), axis.text.x = element_text(size = 10, angle = 65, vjust = 0.6),
        axis.text.y = element_text(size = 12), legend.title = element_text(size=10), legend.text = element_text(size=10))
dev.off()

SpC_Bar_error <- summaryStats(SpC.Mean ~ Site, data = SCdf, digits = 2, p.value = TRUE, stats.in.rows = TRUE, test = "nonparametric")

tiff("SpC_facet.tiff", units="in", width=10, height=8, res=300)

ggplot(AllCountsAmbient[!is.na(AllCountsAmbient$Specific.Cond),], aes(x=Date, y=Specific.Cond, group = Site, colour = Site)) + geom_point(size=2, shape=21, fill="white") + 
  geom_line(size=1) + geom_smooth(method = "lm", se=TRUE) + labs(y="Specific Conductivity (uS)", x=NULL, caption="Source: Ambient Monitoring, 2010-2017") + facet_wrap(~Site, ncol=2) +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%b-%y")) + theme(axis.text.x = element_text(angle = 90)) + theme_bw() + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
        axis.text.y = element_text(size = 13), legend.title = element_text(size=10), legend.text = element_text(size=10), legend.position = "none", strip.text.x = element_text(size =12, colour = "steelblue3"))
dev.off()

tiff("SpC_Seasonal.tiff", units="in", width=11, height=6, res=300)
ggplot(AllCountsAmbient, aes(Site, Specific.Cond, na.rm=TRUE)) + geom_boxplot(aes(fill=Season), alpha=0.4) + 
  labs(title = NULL, subtitle=NULL, y="Specific Conductivity (uS/cm)", caption = "In-Situ Monitoring, 2010-2018") +
  theme_bw() + theme(axis.title.y = element_text(size=18), axis.title.x = element_text(size = 18), title = element_text(size = 18), axis.text.x = element_text(size = 15, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) + scale_fill_manual( breaks = c("Autumn","Spring","Summer","Winter"),
                                                                                                                                                            values = scales::hue_pal()(4) )
dev.off()

#Turbidity
tiff("Turbidity_Over_time.tiff", units="in", width=12, height=9, res=300)
ggplot(AllCountsAmbient, aes(x=Date, y=Turbidity, group = Site)) + geom_point(size=2.2, shape=21, aes(col = Turbidity.Type, fill= Turbidity.Type)) + geom_hline(yintercept = 4, color = "orange2", size = 0.7) + geom_hline(yintercept = 70, color = "red", size = 0.7) +
  geom_line(size=0.8, color = "turquoise3") + facet_wrap(~Site, ncol = 2) + labs(title=NULL, subtitle=NULL, y="Turbidity (NTU)", x= "Date") +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%b-%y")) + theme_bw() + theme(axis.title.y = element_text(size=20), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                                                                                                   axis.text.y = element_text(size = 10), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none", strip.text.x = element_text(size = 13, colour = "steelblue3")) + 
  scale_y_log10(breaks=c(1,5, 10, 30, 100, 500))
dev.off()

tiff("Turbidity_in_wet_and_dry.tiff", units="in", width=10, height=6, res=300)
ggplot(AllCountsAmbient, aes(Site, Turbidity, na.rm=TRUE)) + geom_boxplot(aes(fill=Precipitation), alpha=0.4,na.rm=TRUE) + geom_hline(yintercept = 4, color = "orange2", size = 1) + geom_hline(yintercept = 70, color = "red", size=1) +
  labs(title = NULL, subtitle=NULL, caption = "Source: In-Situ Monitoring, 2010-2018", y="Turbidity (NTU)") + scale_y_log10(breaks=c(0, 1, 2, 5, 10, 20, 50, 100, 200)) +
  theme_bw() + theme(axis.title.y = element_text(size=18), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=15), legend.text = element_text(size=15))

dev.off()

#Fecal Coliform______
tiff("FC_Sites.tiff", units="in", width=10, height=6, res=300)
ggplot(subset(AllCountsAmbient, Site %in% c("LS-1", "JO-1", "NCLD-1", "PM-1", "SARU")), aes(Site, Fecal.Coliform)) + geom_boxplot(aes(fill=Site), alpha=0.4) + geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = .4, fill = "red", binwidth = .12) + 
  labs(title = NULL, subtitle=NULL, y="Fecal Coliform (CFU)", caption = "Source: In-Situ Monitoring 2010-2018") + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) + scale_y_log10(breaks=c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")
dev.off()

tiff("FC_wetvsdry.tiff", units="in", width=10, height=6, res=300)
ggplot(subset(AllCountsAmbient, Site %in% c("LS-1", "JO-1", "NCLD-1", "PM-1", "SARU")), aes(Site, Fecal.Coliform)) + geom_boxplot(aes(fill=Precipitation), alpha=0.4) + 
  labs(title = "Fecal Coliform Across TMDL Sites in Wet and Dry Conditions", subtitle="2010-2018", y="Fecal Coliform (CFU)") + geom_hline(yintercept = 50, color="red", size=1) + scale_y_log10(breaks=c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=15), legend.text = element_text(size=15))
dev.off()

tiff("FC_stripchart.tiff", units="in", width=8, height=6, res=300)
stripChart(Fecal.Coliform ~ Precipitation, data = subset(AllCountsAmbient, Site %in% c("LS-1", "JO-1", "NCLD-1", "PM-1", "SARU")),
           col = c("red", "blue"), p.value = TRUE,
           ci.and.test = "nonparametric", conf.level = 0.95, ylab = "Fecal Coliform (CFU)")
dev.off()

#Water Quality index and final graph? do we need it? Maybe not......but still coool


tiff("FC_grid2.tiff", units="in", width=8, height=2, res=300)
ggplot(subset(AllCountsAmbient, Site %in% c("LS-1", "JO-1", "NCLD-1", "PM-1", "SARU")), aes(Site, Fecal.Coliform)) + 
  stat_boxplot(geom = 'errorbar', width = 0.4, color = "cyan4") +
  geom_boxplot(fill = "gray94", width = 0.5, color = "cyan4")  +
  labs(title = "a. Fecal Coliform (CFU)", subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 50, color="red", size=1) + scale_y_log10(breaks=c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)) +
  theme(axis.title.y = element_text(size=12), title = element_text(size = 16), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_few() + theme(axis.title.y = element_text(size=10), title = element_text(size = 8), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                      axis.text.y = element_text(size = 10), legend.title = element_text(size=18), legend.text = element_text(size=15)) + coord_flip()

dev.off()


tiff("Temp_grid2.tiff", units="in", width=8, height=2.4, res=300)
ggplot(subset(AllCountsAmbient, Season == "Summer"), aes(Site, TEMP)) + 
  stat_boxplot(geom = 'errorbar', width = 0.4, color = "cyan4") +
  geom_boxplot(fill = "gray94", width = 0.5, color = "cyan4")  +
  labs(title = "b. Summer Temperature (Degrees Celsius)", subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 16, color="red", size=1) +
  theme(axis.title.y = element_text(size=7), title = element_text(size = 15), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_few() + theme(axis.title.y = element_text(size=4), title = element_text(size = 8), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                      axis.text.y = element_text(size = 10), legend.title = element_text(size=18), legend.text = element_text(size=15)) + coord_flip()


dev.off()


tiff("DO_grid2.tiff", units="in", width=8, height=2.4, res=300)
ggplot(subset(AllCountsAmbient, Season == "Summer"), aes(Site, DO)) + 
  stat_boxplot(geom = 'errorbar', width = 0.4, color = "cyan4") +
  geom_boxplot(fill = "gray94", width = 0.5, color = "cyan4")  +
  labs(title = "c. Summer Dissolved Oxygen (mg/L)", subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 9.5, color="red", size=1) +
  theme(axis.title.y = element_text(size=7), title = element_text(size = 15), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_few() + theme(axis.title.y = element_text(size=4), title = element_text(size = 8), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                      axis.text.y = element_text(size = 10), legend.title = element_text(size=18), legend.text = element_text(size=15)) + coord_flip()
dev.off()

tiff("pH_grid2.tiff", units="in", width=8, height=2.4, res=300)

ggplot(AllCountsAmbient, aes(Site, pH)) + 
  stat_boxplot(geom = 'errorbar', width = 0.4, color = "cyan4") +
  geom_boxplot(fill = "gray94", width = 0.5, color = "cyan4")  +
  labs(title = "d. pH", subtitle=NULL, y=NULL, x=NULL) +
  theme(axis.title.y = element_text(size=7), title = element_text(size = 15), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_few() + theme(axis.title.y = element_text(size=7), title = element_text(size = 8), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                      axis.text.y = element_text(size = 10), legend.title = element_text(size=18), legend.text = element_text(size=15)) + coord_flip()

dev.off()


tiff("Turbidity_grid2.tiff", units="in", width=8, height=2.4, res=300)
ggplot(AllCountsAmbient, aes(Site, Turbidity)) + 
  stat_boxplot(geom = 'errorbar', width = 0.4, color = "cyan4") +
  geom_boxplot(fill = "gray94", width = 0.5, color = "cyan4")  +
  geom_hline(yintercept = 4, color = "yellow2", size = 1) + geom_hline(yintercept = 70, color = "red", size = 1) +
  labs(title = "e. Turbidity (NTU)", subtitle=NULL, y=NULL, x=NULL) + scale_y_log10(breaks=c(0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100, 150)) +
  theme(axis.title.y = element_text(size=7), title = element_text(size = 15), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_few() + theme(axis.title.y = element_text(size=7), title = element_text(size = 8), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                      axis.text.y = element_text(size = 10), legend.title = element_text(size=18), legend.text = element_text(size=15)) + coord_flip()

dev.off()

#newgraphspart2

#Fecals
fecalmeans <- aggregate(Fecal.Coliform ~  Site, AllCountsAmbient, mean)

fecalgeo <- function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

gm_mean <- exp(mean(log(x)))

is.num <- sapply(fecalmeans, is.numeric)
fecalmeans[is.num] <- lapply(fecalmeans[is.num], round, 2)

tiff("FC_Sites2.tiff", units="in", width=10, height=6, res=300)
ggplot(subset(AllCountsAmbient, Site %in% c("LS-1", "JO-1", "NCLD-1", "PM-1", "SARU")), aes(Site, Fecal.Coliform), label = sprintf("%0.2f", round(fecalmeans, digits = 2))) + 
  stat_boxplot(geom = 'errorbar', width = 0.25, color = "darkcyan") +
  geom_boxplot(fill = "gray97", width = 0.4, color = "darkcyan")  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + 
  labs(title = NULL, subtitle=NULL, y="Fecal Coliform (CFU)") + geom_hline(yintercept = 50, color="red", size=1) + 
  geom_hline(yintercept = 200, color="blue", size=1) + scale_y_log10(breaks=c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

dev.off()
#DO

tiff("DO_Over_Summer_Boxplot2.tiff", units="in", width=12, height=6, res=300)
ggplot(subset(AllCountsAmbient, Season %in% "Summer"), aes(Site, DO)) + stat_boxplot(geom = 'errorbar', width = 0.25, color = "darkcyan") +
  geom_boxplot(aes(fill = Site), width = 0.4, color = "darkcyan") + 
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + 
  labs(title = NULL, subtitle=NULL, y="Summer Dissolved Oxygen (mg/L)", x=NULL) + geom_hline(yintercept = 9.5, color="red", size=1) +
  theme_bw() + theme(axis.title.y = element_text(size=22), axis.title.x = element_text(size=22), title = element_text(size = 18), axis.text.x = element_text(size = 18, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 18), legend.title = element_text(size=22), legend.text = element_text(size=18), legend.position = "none")

dev.off()

tiff("DO_Over_Summer_Boxplot3.tiff", units="in", width=12, height=6, res=300)
f <- list(
  family = "helvetica",
  size = 18,
  color = "Black"
)
y <- list(
  title = "Summer Dissolved Oxygen (mg/L)",
  titlefont = f
)
x <- list(
  title = "Temperature (Degrees Celcius)",
  titlefont = f
)

plot_ly(subset(AllCountsAmbient, Season %in% "Summer"), y = ~DO, color = ~Site, type = "box", jitter = 0.3, pointpos = -1.8, boxpoints = 'all') %>%
  layout( yaxis = y)

plot_ly(AllCountsAmbient, y = ~DO, x = ~TEMP, color = ~Site, type = "scatter", jitter = 0.3) %>% 
  layout(yaxis = y) %>%
  layout(xaxis = x)



plot_ly(AllCountsAmbient, y = ~Turbidity, x = ~Site, color = ~Precipitation, type = "scatter", jitter = 0.3) %>% 
  layout(yaxis = y) %>%
  layout(xaxis = x)
dev.off()
#to add dot plot geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = .2, fill="red")

#plotly

pturb <- ggplot(AllCountsAmbient, aes(Site, Turbidity)) + 
  stat_boxplot(geom = 'errorbar', width = 0.4, color = "cyan4") +
  geom_boxplot(fill = "gray94", width = 0.5, color = "cyan4")  +
  labs(title = "e. Turbidity (NTU)", subtitle=NULL, y=NULL, x=NULL) + scale_y_log10(breaks=c(0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100, 150)) +
  theme(axis.title.y = element_text(size=7), title = element_text(size = 15), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_few() + theme(axis.title.y = element_text(size=7), title = element_text(size = 8), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                      axis.text.y = element_text(size = 10), legend.title = element_text(size=18), legend.text = element_text(size=15)) + coord_flip()

pturb <- ggplotly(pturb)

pturb


# 2019

ggplot(subset(AllCountsAmbient, Year == "2019"), aes(x=Date, y=TEMP, group = Site, colour = Temp.Type)) + geom_point(size=3, shape=21, fill="white") + geom_hline(yintercept = 16, color = "red",) + 
  geom_line(size=1) + facet_wrap(~Site, ncol = 2) + labs(title=NULL, subtitle=NULL, y="Temperature (Degrees Celcius)", x= "Date", caption = "Ambient Locations, 2010-2018") +
  scale_x_date(breaks = date_breaks("1 month"), labels = date_format("%b-%y")) +  theme_bw() + theme(axis.title.y = element_text(size=22), title = element_text(size = 18), axis.text.x = element_text(size = 15, angle = 65, vjust = 0.6),
                                                                                                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=16), axis.title.x = element_text(size = 18),
                                                                                                     legend.position = "none",  strip.text.x = element_text(size=12, colour = "steelblue3"))

cols <- c(below = "red", above = "blue")
DO_2019 <- ggplot(subset(AllCountsAmbient, Year == "2019"), aes(x=Date, y=DO, group = Site, colour = DO.Type)) + geom_point(size=3, shape=21, fill = factor(AllCountsAmbient$DO.Type)) + geom_hline(yintercept = 9.5, color = "red",) + 
  geom_line(size=1) + facet_wrap(~Site, ncol = 2) + labs(title=NULL, subtitle=NULL, y="Temperature (Degrees Celcius)", x= "Date", caption = "Ambient Locations, 2010-2018") +
  scale_x_date(breaks = date_breaks("1 month"), labels = date_format("%b-%y")) +  theme_bw() + theme(axis.title.y = element_text(size=22), title = element_text(size = 18), axis.text.x = element_text(size = 15, angle = 65, vjust = 0.6),
                                                                                                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=16), axis.title.x = element_text(size = 18),
                                                                                                     legend.position = "none",  strip.text.x = element_text(size=12, colour = "steelblue3"))

DO_2019 + scale_colour_manual(values = cols)
