# Line with Points, no curved smoothing line
Ambient_Temp <- ggplot(AllCountsAmbient, aes(x=date, y=TEMP, group = site, colour = site)) + geom_point(size=2, shape=21, fill="white") + geom_hline(yintercept = 16) + 
  geom_line(size=1)
plot(Ambient_Temp)

Ambient_DO <-ggplot(AllCountsAmbient, aes(x=date, y=DO, group = site, colour = site)) + geom_point(size=2, shape=21, fill="white") + geom_hline(yintercept = 9.5) + 
  geom_line(size=1)
plot(Ambient_DO)

#plot multiple plots together? With wicked cool labels

require(gridExtra)


Ambient_Temp <- ggplot(AllCountsAmbient, aes(x=date, y=TEMP, group = site, colour = site)) + geom_point(size=2, shape=21, fill="white") + geom_hline(yintercept = 16) + 
  geom_line(size=1) + labs(title="Temperature and Dissolved Oxygen Over Time", subtitle="From 2010-2017", y="Temperature (Degrees Celcius)", x=NULL)
plot(Ambient_Temp)

Ambient_DO <-ggplot(AllCountsAmbient, aes(x=date, y=DO, group = site, colour = site)) + geom_point(size=2, shape=21, fill="white") + geom_hline(yintercept = 9.5) + 
  geom_line(size=1) + labs(y="Dissolved Oxygen (mg/l", x="Time", caption="Source: Ambient Monitoring, 2010-2017")
plot(Ambient_DO)

grid.arrange(Ambient_Temp, Ambient_DO)

# Setting x Tick Marks for date!

Ambient_Temp <- ggplot(AllCountsAmbient, aes(x=date, y=TEMP, group = site, colour = site)) + geom_point(size=2, shape=21, fill="white") + geom_hline(yintercept = 16, color = "red",) + 
  geom_line(size=1) + labs(title="Temperature and Dissolved Oxygen Over Time", subtitle="From 2010-2017", y="Temperature (Degrees Celcius)", x=NULL) + 
  scale_x_date(breaks = date_breaks("3 months"), labels = date_format("%b-%y")) + theme(axis.text.x = element_text(angle = 90)) + theme(axis.ticks.x = element_blank(), axis.title.x = element_blank(),
                                                                                                                                        axis.text.x = element_blank()) + 
  theme(plot.background = element_rect(fill = "light yellow")) + theme(legend.title = element_text(size = 12, color = "Forestgreen"))

plot(Ambient_Temp)

Ambient_DO <-ggplot(AllCountsAmbient, aes(x=date, y=DO, group = site, colour = site)) + geom_point(size=2, shape=21, fill="white") + geom_hline(yintercept = 9.5, color = "red") + 
  geom_line(size=1) + labs(y="Dissolved Oxygen (mg/l", x="Time", caption="Source: Ambient Monitoring, 2010-2017") +
  scale_x_date(breaks = date_breaks("3 months"), labels = date_format("%b-%y")) + theme(axis.text.x = element_text(angle = 90)) + theme(plot.background = element_rect(fill = "light yellow")) +
  theme(legend.title = element_text(size = 12, color = "Forestgreen"))
plot(Ambient_DO)

grid.arrange(Ambient_Temp, Ambient_DO)

#Plotting DO, Temp, and SpC over Time in one graph!

Ambient_Temp <- ggplot(AllCountsAmbient, aes(x=date, y=TEMP, group = site, colour = site)) + geom_point(size=2, shape=21, fill="white") + geom_hline(yintercept = 16, color = "red",) + 
  geom_line(size=1) + labs(title="In-Situ Temperature Over Time at Ambient Monitoring Locations", subtitle="From 2010-2018", y="Temperature (Degrees Celcius)", x= "Date") +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%b-%y")) + theme(axis.title.y = element_text(size=15), title = element_text(size = 25), axis.text.x = element_text(size = 20), 
                                                                                      axis.text.y = element_text(size = 20), legend.title = element_text(size=25), legend.text = element_text(size=20)) + 
  theme(plot.background = element_rect(fill = "light blue"))

plot(Ambient_Temp)

Ambient_DO <-ggplot(AllCountsAmbient, aes(x=date, y=DO, group = site, colour = site)) + geom_point(size=2, shape=21, fill="white") + geom_hline(yintercept = 9.5, color = "red") + 
  geom_line(size=1) + labs(y="Dissolved Oxygen (mg/l", x="Date", title = "In-Situ Dissolved Oxygen Over Time at Ambient Monitoring Locations", subtitle = "From 2010-2018") +
  scale_x_date(breaks = date_breaks("3 months")) + theme(plot.background = element_rect(fill = "light blue")) +
  theme(axis.text.x = element_text(angle = 90), axis.title.y = element_text(size=15), title = element_text(size = 20))
plot(Ambient_DO)

Ambient_SpC <-ggplot(AllCountsAmbient[!is.na(AllCountsAmbient$Specific.Cond),], aes(x=date, y=Specific.Cond, group = site, colour = site)) + geom_point(size=2, shape=21, fill="white") + 
  geom_line(size=1) + labs(y="Specific Conductivity (uS)", x=NULL, caption="Source: Ambient Monitoring, 2010-2017") +
  scale_x_date(breaks = date_breaks("3 months"), labels = date_format("%b-%y")) + theme(axis.text.x = element_text(angle = 90)) + theme(plot.background = element_rect(fill = "light yellow"))
plot(Ambient_SpC)


grid.arrange(Ambient_Temp, Ambient_DO, Ambient_SpC)

# facetgrid to look at groups side by side of one parameter
Ambient_SpC_facet <-ggplot(AllCountsAmbient[!is.na(AllCountsAmbient$Specific.Cond),], aes(x=date, y=Specific.Cond, group = site, colour = site)) + geom_point(size=2, shape=21, fill="white") + 
  geom_line(size=1) + labs(y="Specific Conductivity (uS)", x=NULL, caption="Source: Ambient Monitoring, 2010-2017") +
  scale_x_date(breaks = date_breaks("6 months"), labels = date_format("%b-%y")) + theme(axis.text.x = element_text(angle = 90)) + theme(plot.background = element_rect(fill = "light yellow")) + facet_grid(~site)
plot(Ambient_SpC_facet)


plot(Ambient_DO)

#To run histogram, need to turn DO into a factor
AllCountsAmbient$DOFactor <- AllCountsAmbient %>%
  mutate(DO = cut(DO, breaks = c(3, 5, 7, 9, 11, 13, 15)))
# histogram of DO values
ggplot(AllCountsAmbient) + aes(x = DO) + geom_bar() +facet_grid(~site)




#looking atSpC (and filling in lines with missing data)

Ambient_SpC <-ggplot(AllCountsAmbient[!is.na(AllCountsAmbient$Specific.Cond),], aes(x=date, y=Specific.Cond, group = site, colour = site)) + geom_point(size=2, shape=21, fill="white") + 
  geom_line(size=1) + labs(y="Specific Conductivity (uS)", x="Time", caption="Source: Ambient Monitoring, 2010-2017") +
  scale_x_date(breaks = date_breaks("3 months"), labels = date_format("%b-%y")) + theme(axis.text.x = element_text(angle = 90)) + theme(plot.background = element_rect(fill = "light yellow"))
plot(Ambient_SpC)

# Statistics!



# pH by year for Waynita Creek, while filling in lines from missing data!
ggplot(WC_1_Excel[!is.na(WC_1_Excel$pH),], aes(x=date, y=pH)) + geom_point(size=2, shape=21, fill="white") + geom_line() + labs(y="pH", x="Time", caption="Source: Ambient Monitoring, 2010-2016") + 
  scale_x_date(breaks = date_breaks("3 months"), labels = date_format("%b-%y")) + theme(axis.text.x = element_text(angle = 90)) + theme(plot.background = element_rect(fill = "light yellow")) +geom_smooth(method = "lm")

#goodness of fit test
attach(AllCountsAmbient)
qqPlot(DO[site == "HC-1"], dist = "lnorm",
       add.line = TRUE, points.col = "blue", ylab = "Quantiles of HC-1 Dissolved Oxygen")
# results from distributtion paremeter estimation

HC_1.DO <- with(AllCountsAmbient, DO[site == "HC-1"])
enorm(HC_1.DO, ci = TRUE)

Summer.DO <- with(AllCountsAmbient, DO[Season == "Summer"])
enorm(Summer.DO, ci = TRUE, ci.param = "mean")$interval

#Testing goodness of fit test...
attach(AllCountsAmbient)
Summer_DO_gof <- DO[Season == "Summer"]
sw.norm <- gofTest(Summer_DO_gof, dist = "norm")
sw.norm

#histogram of Summer DO values
hist(Summer.DO, freq = FALSE, xlim = c(4, 14),
     col = "blue", xlab = "Summer DO (mg/L)",
     ylab = "Relative Frequency", main = "")
epdfPlot(Summer.DO, epdf.col = "red", add = TRUE)



# Wilcoxen Rank Sum Test to test statistical differences among groups...not helpful with DO but could be used for TSS or Metals with %imperviousness
summaryStats(DO ~ site  , data = AllCountsAmbient, digits = 2, p.value = TRUE, stats.in.rows = TRUE, test = "nonparametric")
summaryStats(TEMP ~ site, data = AllCountsAmbient, digits = 2, p.value = TRUE, stats.in.rows = TRUE, test = "nonparametric")
# Quantile method for determining confidence intervals
attach(AllCountsAmbient)
eqnorm(DO[site == "SARU"], p = 0.75, ci = TRUE,conf.level = 0.95)

#stripChart
stripChart(TEMP ~ site, data = AllCountsAmbient, col = c("red", "blue", "green", "yellow", "orange", "black", "purple"), p.value = TRUE,
           ci.and.test = "nonparametric", conf.level = 0.95, ylab = "Temperature (degree C)")
stripChart(DO ~ Site, data = AllCountsAmbient, col = c("red", "blue", "green", "yellow", "orange", "black", "purple"), p.value = TRUE,
           ci.and.test = "nonparametric", conf.level = 0.95, ylab = "Temperature (degree C)")



#isitdifferent?

# running data for new report

# In-Situ Temperature data # Trend data from 2010-2018


AllCountsAmbient_NotTMDL <- subset(AllCountsAmbient, Site %in% c("HC-1", "HC-2", "NC-1", "WC-1"))
AllCountsAmbient_NotTMDL_odd <- subset(AllCountsAmbient_NotTMDL, Year %in% c("2011", "2013", "2015", "2017"))
AllCountsAmbient_NotTMDL_odd$Month <- month(AllCountsAmbient_NotTMDL_odd$Date, label = TRUE, abbr = TRUE)


#Temperature at sites for total time for TMDL & Ambient with lines


ggplot(subset(AllCountsAmbient, Site %in% c("HC-1", "HC-2", "NC-1", "WC-1")), aes(x=Date, y=TEMP, group = Site, colour = Site)) + geom_point(size=3, shape=21, fill="white") + geom_hline(yintercept = 16, color = "red",) + 
  geom_line(size=1) + geom_smooth(method = lm, se= FALSE) + facet_wrap(~Site, ncol = 1) + labs(title="In-Situ Temperature Over Time", subtitle="Ambient Locations, From 2010-2018", y="Temperature (Degrees Celcius)", x= "Date") +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%b-%y")) +  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                                                                                                    axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15))


ggplot(subset(AllCountsAmbient, Site %in% c("LS-1", "JO-1", "NCLD-1", "PM-1", "SARU")), aes(x=Date, y=TEMP, group = Site, colour = Site)) + geom_point(size=3, shape=21, fill="white") + geom_hline(yintercept = 16, color = "red",) + 
  geom_line(size=1) + geom_smooth(method = lm, se= FALSE) + facet_wrap(~Site, ncol = 1) + labs(title="In-Situ Temperature Over Time", subtitle="TMDL Locations, From 2010-2018", y="Temperature (Degrees Celcius)", x= "Date") +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%b-%y")) + theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                                                                                                   axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15))



#fecals

# fecals geom mean by year

fecalsub <- subset(AllCountsAmbient, select = c(Fecal.Coliform, Site, Year))

NCLD1F <- aggregate(Fecal.Coliform~Year, data=subset(fecalsub, Site %in% "NCLD-1"), FUN = "median")
NCLD1F$Geo.Mean <- c(105.5, 112.5, 157, 136.3)
rkt(NCLD1F$Year, NCLD1F$Geo.Mean, correct = TRUE, rep = "a")
rkt(NCLD_Excel$Year, NCLD_Excel$Fecal.Coliform, correct = TRUE, rep = "a" )

LS1F <- aggregate(Fecal.Coliform~Year, data=subset(fecalsub, Site %in% "LS-1"), FUN = "median")
LS1F$GeoMean <- c(67.7, 91.8, 143, 41.3, 128, 191, 300, 327, 303)
rkt(LS_1_Excel$Year, LS_1_Excel$Fecal.Coliform, correct = TRUE, rep = "a" )

PM1F <- aggregate(Fecal.Coliform~Year, data=subset(fecalsub, Site %in% "PM-1"), FUN = "median")
PM1F$GeoMean <- c(65.5, 44.5, 79.2, 45.2, 83.9, 80.3, 178, 188, 232)
rkt(PM_1_Excel$Year, PM_1_Excel$Fecal.Coliform, correct = TRUE, rep = "a" )

SARUF <- aggregate(Fecal.Coliform~Year, data=subset(fecalsub, Site %in% "SARU"), FUN = "median")
SARUF$GeoMean <- c(53.2, 62.6, 61, 94.8, 171, 85.3, 200, 251, 250)
rkt(SARU_Excel$Year, SARU_Excel$Fecal.Coliform, correct = TRUE, rep = "a" )

JO1F <- aggregate(Fecal.Coliform~Year, data=subset(fecalsub, Site %in% "JO-1"), FUN = "median")
JO1F$GeoMean <- c(23.7, 14, 40.8, 12.3, 12.4, 9.98, 11.5, 22.4, 19)
rkt(JO_1_Excel$Year, JO_1_Excel$Fecal.Coliform, correct = TRUE, rep = "a" )

LS1F <- subset(fecalsub, Site %in% "LS-1")
PM1F <- subset(fecalsub, Site %in% "PM-1")
SARUF <- subset(fecalsub, Site %in% "SARU")
JO1F <- subset(fecalsub, Site %in% "JO-1")



#Summary and fecals

summaryFull(Fecal.Coliform~Year, subset(fecalsub, Site %in% "JO-1"), digits=3)

fecalFacetPrecip <- ggplot(subset(AllCountsAmbient, Site %in% c("LS-1", "JO-1", "NCLD-1", "PM-1", "SARU")), aes(Site, Fecal.Coliform)) + geom_boxplot(aes(fill=Site), alpha=0.3) + geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = .5, fill="red", binwidth = .12) + 
  labs(title = "Fecal Coliform Accross TMDL Sites in Wet and Dry Conditions", subtitle="2010-2018", y="Fecal Coliform (CFU)") + geom_hline(yintercept = 50, color="red", size=1) + facet_wrap(~Precipitation, nrow = 1) + scale_y_log10(breaks=c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)) +
  theme(axis.title.y = element_text(size=15, color = "white"), title = element_text(size = 18, color = "white"), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6, color = "white"),axis.text.y = element_text(size = 15, color = "white"), legend.title = element_text(size=12, colour = "Black"), legend.text = element_text(size=20)) +
  theme(plot.background = element_rect(fill = "darkseagreen4")) + theme(legend.title = element_blank(), legend.background = element_blank(), legend.key = element_blank(),
                                                                        legend.text = element_blank())
fecalFacetPrecip

fecalbysite <- ggplot(subset(AllCountsAmbient, Site %in% c("LS-1", "JO-1", "NCLD-1", "PM-1", "SARU")), aes(Site, Fecal.Coliform)) + geom_boxplot(aes(fill=Site), alpha=0.3) + geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = .5, fill="red", binwidth = .12) + 
  labs(title = "Fecal Coliform at TMDL Sites", subtitle="2010-2018", y="Fecal Coliform (CFU)") + geom_hline(yintercept = 50, color="red", size=1) + scale_y_log10(breaks=c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15))

fecalbysite

fecalBP <- ggplot(subset(AllCountsAmbient, Site %in% c("LS-1", "JO-1", "NCLD-1", "PM-1", "SARU")), aes(Site, Fecal.Coliform)) + geom_boxplot(aes(fill=Precipitation), alpha=0.3) + 
  labs(title = "Fecal Coliform Across TMDL Sites in Wet and Dry Conditions", subtitle="2010-2018", y="Fecal Coliform (CFU)") + geom_hline(yintercept = 50, color="red", size=1) + scale_y_log10(breaks=c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15))



fecalBP

#fecals on horizontal axis?



stripChart(Fecal.Coliform ~ Site, data = AllCountsAmbient, col = c("red", "blue", "green", "yellow", "purple"), p.value = TRUE,
           ci.and.test = "nonparametric", conf.level = 0.95, ylab = "Temperature (degree C)")
stripChart(Fecal.Coliform ~ Season, data = subset(TMDL, Site %in% "LS-1"), col = c("red", "blue", "green", "yellow"), p.value = TRUE,
           ci.and.test = "nonparametric", conf.level = 0.95, ylab = "Temperature (degree C)")

stripChart(Fecal.Coliform ~ Precipitation, data = subset(TMDL, Site %in% c("LS-1", "JO-1", "NCLD-1", "PM-1", "SARU")),
           col = c("red", "blue"), p.value = TRUE,
           ci.and.test = "nonparametric", conf.level = 0.95, ylab = "Fecal Coliform (CFU)")

Fecal_Table <- summaryStats(Fecal.Coliform ~ Site, data = subset(AllCountsAmbient, Site %in% c("LS-1", "JO-1", "NCLD-1", "PM-1", "SARU")) , digits=3, p.value=TRUE, stats.in.rows=TRUE,
                            test.arg.list=list(var.equal = FALSE, test="nonparametric"))

Fecal_Table

write.table(Fecal_Table, file = "Fecal_Table.txt", sep = ",", quote = FALSE, row.names = TRUE)

TMDL <- subset(AllCountsAmbient, Site %in% c("LS-1", "JO-1", "NCLD-1", "PM-1", "SARU"))
summaryFull(Fecal.Coliform~Season, subset(TMDL, Site %in% "LS-1"), stats="all", na.rm=TRUE)
summaryFull(Fecal.Coliform~Site, subset(AllCountsAmbient, Site %in% c("LS-1", "JO-1", "NCLD-1", "PM-1", "SARU")), stats="all", na.rm=TRUE)

#by site
summaryStats(Fecal.Coliform~Season, data = TMDL, digits=3, p.value=TRUE, stats.in.rows=TRUE)

summaryStats(Fecal.Coliform~Season, subset(TMDL, Site %in% "LS-1"), digits=3, na.rm=TRUE, p.value=TRUE, stats.in.rows=TRUE,
             test.arg.list=list(var.equal = FALSE))

summaryStats(Fecal.Coliform~Season, subset(TMDL, Site %in% "JO-1"), digits=3, na.rm=TRUE, p.value=TRUE, stats.in.rows=TRUE,
             test.arg.list=list(var.equal = FALSE))

summaryStats(Fecal.Coliform~Precipitation, subset(TMDL, Site %in% c("LS-1", "JO-1", "NCLD-1", "PM-1", "SARU")), digits=3, na.rm=TRUE, p.value=TRUE, stats.in.rows=TRUE, test="nonparametric")





#dissolved Oxygen
ggplot(subset(AllCountsAmbient, Site %in% c("HC-1", "HC-2", "NC-1", "WC-1")), aes(x=Date, y=DO, group = Site, colour = Site)) + geom_point(size=3, shape=21, fill="white") + geom_hline(yintercept = 9.5, color = "red",) + 
  geom_line(size=1) + geom_smooth(method = lm, se= FALSE) + facet_wrap(~Site, ncol = 1) + labs(title="In-Situ Dissolved Oxygen Over Time", subtitle="Ambient Locations, From 2010-2018", y="Dissolved Oxygen (mg/L)", x= "Date") +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%b-%y")) + theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                                                                                                   axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  scale_y_continuous(breaks=c(5, 9, 13))

ggplot(subset(AllCountsAmbient, Site %in% c("LS-1", "JO-1", "NCLD-1", "PM-1", "SARU")), aes(x=Date, y=DO, group = Site, colour = Site)) + geom_point(size=3, shape=21, fill="white") + geom_hline(yintercept = 9.5, color = "red",) + 
  geom_line(size=1) + geom_smooth(method = lm, se= TRUE) + facet_wrap(~Site, ncol = 1) + labs(title="In-Situ Dissolved Oxygen Over Time", subtitle="TMDL Locations, From 2010-2018", y="Dissolved Oxygen (mg/L)", x= "Date") +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%b-%y")) + theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                                                                                                   axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  scale_y_continuous(breaks=c(5, 9, 13))

ggplot(AllCountsAmbient, aes(x=Date, y=DO, group = Site, colour = Site)) + geom_point(size=3, shape=21, fill="white") + geom_hline(yintercept = 9.5, color = "red",) + 
  geom_line(size=1) + labs(title="In-Situ Dissolved Oxygen Over Time", subtitle="From 2010-2018", y="Dissolved Oxygen (mg/L)", x= "Date") +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%b-%y")) + theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                                                                                                   axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  scale_y_continuous(breaks=c(5, 9, 13))

g <- ggplot(subset(AllCountsAmbient, Season %in% "Summer"), aes(Site, DO)) + geom_boxplot(aes(fill=Site), alpha=0.3) + geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = .5, fill="red") + 
  labs(title = "Dissolved Oxygen Accross All Sites During Summer", subtitle="2010-2018", y="DO (mg/L)") + geom_hline(yintercept = 9.5, color="red", size=1) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")
g


ggplot(AllCountsAmbient[!is.na(AllCountsAmbient$DO),], aes(x=Date, y=DO, group = Site, colour = Site)) + geom_point(size=2, shape=21, fill="white") + 
  geom_line(size=1) + geom_smooth(method = "lm", se=TRUE) + labs(y="Dissolved Oxygen (mg/L)", x=NULL, caption="Source: In-Situ Ambient Monitoring, 2010-2017") + facet_wrap(~Site, ncol=2) +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%b-%y")) + theme(axis.text.x = element_text(angle = 90)) + theme_bw() + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
        axis.text.y = element_text(size = 15), legend.title = element_text(size=10), legend.text = element_text(size=10), legend.position = "none",  strip.text.x = element_text(size=18, colour = "royalblue")) + scale_y_continuous(breaks=c(5, 9, 13))



DO_Table <-summaryStats(DO~Site, subset(AllCountsAmbient, Season %in% "Summer"), digits=3, na.rm=TRUE, p.value=TRUE, 
                        stats.in.rows=TRUE, test="nonparametric")
DO_Table

write.table(DO_Table, file = "DO_Table.txt", sep = ",", quote = FALSE, row.names = TRUE)

summaryFull(DO~Site, data = AllCountsAmbient)

#Specific Conductivity

summaryStats(Specific.Cond~Site, data = AllCountsAmbient, digits=3, p.value=TRUE, stats.in.rows=TRUE)
SCdf <- data.frame(Site = c("HC-1", "HC-2", "JO-1", "LS-1", "NC-1", "NCLD-1", "PM-1", "SARU", "WC-1"), SpC.Mean = c(191.9, 191.1, 179.8, 155.5, 163.7, 160.7, 181, 204.9, 243.7))

SCdf$SpC.Type <- ifelse(SCdf$SpC.Mean < 186, "below", "above")
SCdf <- SCdf[order(SCdf$SpC.Mean), ]
SCdf$Site <- factor(SCdf$Site, levels = SCdf$Site)
ggplot(SCdf, aes(x=Site, y=SpC.Mean)) + geom_bar()


ggplot(SCdf, aes(x=Site, y=SpC.Mean, label=SpC.Mean)) + 
  geom_bar(stat='identity', aes(fill=SpC.Type), width=.5) + scale_fill_manual(name = "Specific Conductivity",
                                                                              labels = c("Above Average", "Below Average"),
                                                                              values = c("above"="#f8766d", "below"="#00ba38")) + coord_flip() +
  labs(title="Mean Specific Conductivity by Site", subtitle="2010-2018", y="Specific Conductivity (uS/cm)") + theme_bw() + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
        axis.text.y = element_text(size = 15), legend.title = element_text(size=10), legend.text = element_text(size=10))


Ambient_SpC <-ggplot(AllCountsAmbient[!is.na(AllCountsAmbient$Specific.Cond),], aes(x=Date, y=Specific.Cond, group = Site, colour = Site)) + geom_point(size=2, shape=21, fill="white") + 
  geom_line(size=1) + geom_smooth(method = "lm", se=TRUE) + labs(y="Specific Conductivity (uS)", x=NULL, caption="Source: Ambient Monitoring, 2010-2017") + facet_wrap(~Site, ncol=2) +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%b-%y")) + theme(axis.text.x = element_text(angle = 90)) + theme_bw() + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
        axis.text.y = element_text(size = 15), legend.title = element_text(size=10), legend.text = element_text(size=10), legend.position = "none", strip.text.x = element_text(size =12, colour = "steelblue3"))
plot(Ambient_SpC)




SpCBP <- ggplot(AllCountsAmbient, aes(Site, Specific.Cond, na.rm=TRUE)) + geom_boxplot(aes(fill=Precipitation), alpha=0.3,na.rm=TRUE) + 
  labs(title = "Conductivity in Wet and Dry Conditions", subtitle="2010-2018", y="Specific Conductivity (uS/cm)") +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15))


SpCBP

SpCSeason <- ggplot(AllCountsAmbient, aes(Site, Specific.Cond, na.rm=TRUE)) + geom_boxplot(aes(fill=Season), alpha=0.3,na.rm=TRUE) + 
  labs(title = "Seasonal Variation in Conductivity", subtitle="2010-2018", y="Specific Conductivity (uS/cm)") +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15))

SpCSeason

SpCSeasons_Table <-summaryStats(Specific.Cond~Season, AllCountsAmbient, digits=3, na.rm=TRUE, p.value=TRUE, 
                                stats.in.rows=TRUE, test="nonparametric")
SpCSeasons_Table

write.table(SpCSeasons_Table, file = "SpCSeasons_Table.txt", sep = ",", quote = FALSE, row.names = TRUE)

Turb_Table <- summaryStats(Turbidity~Precipitation, AllCountsAmbient, digits=3, na.rm=TRUE, p.value=TRUE, stats.in.rows=TRUE, test="nonparametric")

#turbidity
AllCountsAmbient$Turbidity <- as.numeric(AllCountsAmbient$Turbidity)

ggplot(subset(AllCountsAmbient, Site %in% c("HC-1", "HC-2", "NC-1", "WC-1")), aes(x=Date, y=Turbidity, group = Site, colour = Site)) + geom_point(size=3, shape=21, fill="white") + geom_hline(yintercept = 9.5, color = "red",) + 
  geom_line(size=1) + geom_smooth(method = lm, se= TRUE) + facet_wrap(~Site, ncol = 1) + labs(title="In-Situ Turbidity Over Time", subtitle="Ambient Locations, From 2010-2018", y="Turbidity (NTU)", x= "Date") +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%b-%y")) + theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                                                                                                   axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  scale_y_continuous(limits = c(0,20), breaks = c(0,5,10,15))

ggplot(subset(AllCountsAmbient, Site %in% c("LS-1", "JO-1", "NCLD-1", "PM-1", "SARU")), aes(x=Date, y=Turbidity, group = Site, colour = Site)) + geom_point(size=3, shape=21, fill="white") + geom_hline(yintercept = 9.5, color = "red",) + 
  geom_line(size=1) + geom_smooth(method = lm, se= TRUE) + facet_wrap(~Site, ncol = 1) + labs(title="In-Situ Turbidity Over Time", subtitle="TMDL Locations, From 2010-2018", y="Turbidity (NTU)", x= "Date") +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%b-%y")) + theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                                                                                                   axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) + 
  scale_y_continuous(limits = c(0,20), breaks = c(0,5,10,15))

ggplot(AllCountsAmbient, aes(x=Date, y=Turbidity, group = Site, colour = Site)) + geom_point(size=3, shape=21, fill="white") + 
  geom_line(size=1) + geom_smooth(method = lm, se= TRUE) + facet_wrap(~Site, ncol = 2) + labs(title="In-Situ Turbidity Over Time", subtitle="From 2010-2018", y="Turbidity (NTU)", x= "Date") +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%b-%y")) + theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                                                                                                   axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) + 
  scale_y_continuous(limits = c(0,20), breaks = c(0,5,10,15))

TurbBP <- ggplot(AllCountsAmbient, aes(Site, Turbidity, na.rm=TRUE)) + geom_boxplot(aes(fill=Precipitation), alpha=0.3,na.rm=TRUE) + 
  labs(title = "Turbidity in Wet and Dry Conditions", subtitle="2010-2018", y="Turbidity (NTU)") + scale_y_log10(breaks=c(0, 1, 2, 5, 10, 20, 50, 100, 200)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15))


TurbBP

Turb_Table <- summaryStats(Turbidity~Precipitation, AllCountsAmbient, digits=3, na.rm=TRUE, p.value=TRUE, stats.in.rows=TRUE, test="nonparametric")

write.table(Turb_Table, file = "Turb_Table.txt", sep = ",", quote = FALSE, row.names = TRUE)

#watershed Differences!

#Temp
ggplot(subset(AllCountsAmbient, Season == "Summer"), aes(Watershed, TEMP)) + geom_boxplot(aes(fill=Watershed), alpha=0.3) + geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = .5, fill="red") + 
  labs(title = NULL, subtitle=NULL, y="Temperature (Degrees C)", caption = "Source: Summer In-situ Monitoring 2010-2018") + geom_hline(yintercept = 16, color="red", size=1) +
  theme_bw() + theme(axis.title.y = element_text(size=22), axis.title.x = element_text(size=22), title = element_text(size = 18), axis.text.x = element_text(size = 18, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 18), legend.title = element_text(size=22), legend.text = element_text(size=18), legend.position = "none")

#DO

ggplot(subset(AllCountsAmbient, Season == "Summer"), aes(Watershed, DO)) + geom_boxplot(aes(fill=Watershed), alpha=0.3) + geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = .5, fill="red") + 
  labs(title = NULL, subtitle=NULL, y="Dissolved Oxygen (mg/L)", caption = "Source: Summer In-situ Monitoring 2010-2018") + geom_hline(yintercept = 9.5, color="red", size=1) +
  theme_bw() + theme(axis.title.y = element_text(size=22), axis.title.x = element_text(size=22), title = element_text(size = 18), axis.text.x = element_text(size = 18, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 18), legend.title = element_text(size=22), legend.text = element_text(size=18), legend.position = "none")

#Turbidity

ggplot(AllCountsAmbient, aes(Watershed, Turbidity)) + geom_boxplot(aes(fill=Watershed), alpha=0.3) + geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = .3, fill="red") + 
  labs(title = NULL, subtitle=NULL, y="Turbidity (NTU)", caption = "In-situ Monitoring 2010-2018") + geom_hline(yintercept = 70, color="red", size=1) + geom_hline(yintercept = 4, color = "yellow2", size =1) +
  theme_bw() + theme(axis.title.y = element_text(size=22), axis.title.x = element_text(size=22), title = element_text(size = 18), axis.text.x = element_text(size = 18, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 18), legend.title = element_text(size=22), legend.text = element_text(size=18), legend.position = "none") + scale_y_log10(breaks=c(1,5, 10, 30, 100, 500))

