#2015-2020 Monitoring

AllCounts2015_now <- subset(AllCountsAmbient, Year %in% c(2015,2016,2017,2018,2019,2020))
AllCountsFecal2015 <- subset(AllCounts2015_now, !is.na(Fecal.Coliform))
AllCounts2009_now <- subset(AllCountsAmbient, Year %in% c(2009,2010,2011,2012,2013,2014))
AllCountsFecal2009 <- subset(AllCounts2009_now, !is.na(Fecal.Coliform))
AllCountsFecal2015 <- subset(AllCounts2015_now, !is.na(Fecal.Coliform))
FecalFocus <- subset(AllCountsFecal2015, Site %in% c("LS-1", "JO-1", "NCLD-1", "PM-1", "SARU"))
FecalFocus2 <- subset(AllCountsFecal2009, Site %in% c("LS-1", "JO-1", "NCLD-1", "PM-1", "SARU"))
AllCounts2020 <- subset(AllCountsAmbient, Year == 2020)
FecalFocus2015_2020 <- subset(FecalFocus, Year %in% c(2015,2016,2017,2018,2019,2020))
FecalFocus2009_2015 <- subset(FecalFocus2, Year %in% c(2009,2010,2011,2012,2013,2014))
FecalFocus2009_2015$Year.Range <- "2009-2014"
FecalFocus2015_2020$Year.Range <- "2015-2020"
#Fecal By Site

Fecal_HC2015 <- ggplot(subset(AllCountsFecal2015, Monitoring.Basin == "Horse Creek"), aes(Site, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y="Fecal Coliform Bacteria (CFU)", x=NULL) +  
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 12), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Fecal_LS2015 <- ggplot(subset(AllCountsFecal2015, Monitoring.Basin == "Little Swamp Creek"), aes(Site, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Fecal_LNC2015 <- ggplot(subset(AllCountsFecal2015, Monitoring.Basin == "Lower North Creek"), aes(Site, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Fecal_LSR2015 <- ggplot(subset(AllCountsFecal2015, Monitoring.Basin == "Lower Sammamish River"), aes(Site, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Fecal_PC2015 <- ggplot(subset(AllCountsFecal2015, Monitoring.Basin == "Perry Creek"), aes(Site, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Fecal_PA2015 <- ggplot(subset(AllCountsFecal2015, Monitoring.Basin == "Parr Creek"), aes(Site, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Fecal_UNC2015 <- ggplot(subset(AllCountsFecal2015, Monitoring.Basin == "Upper North Creek"), aes(Site, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

tiff("FC_By_Basin_2015.tiff", units="in", width=11, height=6, res=500)
grid.arrange(Fecal_HC2015, Fecal_LS2015, Fecal_PC2015, Fecal_LNC2015, Fecal_UNC2015, Fecal_LSR2015, Fecal_PA2015, nrow = 1)
dev.off()

FC_Table_Basin <- summaryStats(Fecal.Coliform ~ Monitoring.Basin, data = AllCountsAmbient2019, digits=3, p.value=FALSE, stats.in.rows=TRUE,
                               test.arg.list=list(var.equal = FALSE, test="nonparametric"))

FC_Table_Basin

write.table(FC_Table_Basin, file = "FC_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)

#fecals & Nutrients

grid.arrange(Fecal_HC, Fecal_LS, Fecal_PC, Fecal_LNC, Fecal_UNC, Fecal_LSR, Fecal_PA, TP_HC, TP_LS, TP_PC, TP_LNC, TP_UNC, TP_LSR, TP_PA, TN_HC, TN_LS,
             TN_PC, TN_LNC, TN_UNC, TN_LSR, TN_PA, nrow = 3)


ggplot(subset(AllCountsFecal2015, Site %in% c("LS-1", "JO-1", "NCLD-1", "PM-1", "SARU")), aes(Site, Fecal.Coliform)) + geom_boxplot(aes(fill=Site), alpha=0.4) +   stat_summary(fun.y= "mean", colour="black", geom="point", shape=18, size=3,show_guide = FALSE) +
  labs(title = NULL, subtitle=NULL, y="Fecal Coliform (CFU)", caption = "Source: In-Situ Monitoring 2010-2020") + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) + scale_y_log10(breaks=c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

FC_Table_Focus_Area <- summaryStats(Fecal.Coliform ~ Site, data = FecalFocus, digits=3, p.value=FALSE, stats.in.rows=TRUE,
                               test.arg.list=list(var.equal = FALSE, test="nonparametric"))

FC_Table_Focus_Area

write.table(FC_Table_Focus_Area, file = "FC_Table_Focus_Area.txt", sep = ",", quote = FALSE, row.names = TRUE)

#All Sites
ggplot(AllCountsFecal2015, aes(Site, Fecal.Coliform)) + geom_boxplot(aes(fill=Site), alpha=0.4) +   stat_summary(fun.y= "mean", colour="black", geom="point", shape=18, size=3,show_guide = FALSE) +
  labs(title = NULL, subtitle=NULL, y="Fecal Coliform (CFU)", caption = "Source: In-Situ Monitoring 2010-2020") + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) + scale_y_log10(breaks=c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Fecal_HC <- ggplot(subset(AllCountsFecal2015, Monitoring.Basin == "Horse Creek"), aes(Site, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y="Fecal Coliform Bacteria (CFU)", x=NULL) +  
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 12), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Fecal_LS <- ggplot(subset(AllCountsFecal2015, Monitoring.Basin == "Little Swamp Creek"), aes(Site, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Fecal_LNC <- ggplot(subset(AllCountsFecal2015, Monitoring.Basin == "Lower North Creek"), aes(Site, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Fecal_LSR <- ggplot(subset(AllCountsFecal2015, Monitoring.Basin == "Lower Sammamish River"), aes(Site, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Fecal_PC <- ggplot(subset(AllCountsFecal2015, Monitoring.Basin == "Perry Creek"), aes(Site, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Fecal_PA <- ggplot(subset(AllCountsFecal2015, Monitoring.Basin == "Parr Creek"), aes(Site, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Fecal_UNC <- ggplot(subset(AllCountsFecal2015, Monitoring.Basin == "Upper North Creek"), aes(Site, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

tiff("FC_By_Basin.tiff", units="in", width=11, height=6, res=500)
grid.arrange(Fecal_HC, Fecal_LS, Fecal_PC, Fecal_LNC, Fecal_UNC, Fecal_LSR, Fecal_PA, nrow = 1)
dev.off()

#Fecal TMDL 2010-2105 & 2015-2020

FecalFocus2015graph <- ggplot(FecalFocus2015_2020, aes(Site, Fecal.Coliform)) + geom_boxplot(aes(fill=Site), alpha=0.4) +   stat_summary(fun.y= "mean", colour="black", geom="point", shape=18, size=3,show_guide = FALSE) +
  facet_wrap(~Year.Range, ncol = 1) + labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) + scale_y_log10(limits=c(1, 100000)) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


FecalFOcus2010graph <- ggplot(FecalFocus2009_2015, aes(Site, Fecal.Coliform)) + geom_boxplot(aes(fill=Site), alpha=0.4) +   stat_summary(fun.y= "mean", colour="black", geom="point", shape=18, size=3,show_guide = FALSE) +
  facet_wrap(~Year.Range, ncol = 1) + labs(title = NULL, subtitle=NULL, y="Fecal Coliform (CFU)", x=NULL) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) + scale_y_log10(limits=c(1, 100000)) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

tiff("FC_Focus_Area_2009_2015.tiff", units="in", width=11, height=6, res=500)
grid.arrange(FecalFOcus2010graph, FecalFocus2015graph, nrow = 1)
dev.off()

FC_Table_Focus_Area2010 <- summaryStats(Fecal.Coliform ~ Site, data = FecalFocus2009_2015, digits=3, p.value=FALSE, stats.in.rows=TRUE,
                                    test.arg.list=list(var.equal = FALSE, test="nonparametric"))

write.table(FC_Table_Focus_Area2010, file = "FC_Table_Focus_Area2010.txt", sep = ",", quote = FALSE, row.names = TRUE)


FC_Table_Focus_Area2015 <- summaryStats(Fecal.Coliform ~ Site, data = FecalFocus2015_2020, digits=3, p.value=FALSE, stats.in.rows=TRUE,
                                    test.arg.list=list(var.equal = FALSE, test="nonparametric"))

write.table(FC_Table_Focus_Area2015, file = "FC_Table_Focus_Area2015.txt", sep = ",", quote = FALSE, row.names = TRUE)


#By Year

FecalFocus2015graphYear <- ggplot(FecalFocus2015_2020, aes(Site, Fecal.Coliform)) + geom_boxplot(aes(fill=Site), alpha=0.4) +   stat_summary(fun.y= "mean", colour="black", geom="point", shape=18, size=3,show_guide = FALSE) +
  facet_wrap(~Year, ncol = 6) + labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) + scale_y_log10(limits=c(1, 100000)) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


FecalFOcus2010graphYear <- ggplot(FecalFocus2009_2015, aes(Site, Fecal.Coliform)) + geom_boxplot(aes(fill=Site), alpha=0.4) +   stat_summary(fun.y= "mean", colour="black", geom="point", shape=18, size=3,show_guide = FALSE) +
  facet_wrap(~Year, ncol = 5) + labs(title = NULL, subtitle=NULL, y="Fecal Coliform (CFU)", x=NULL) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) + scale_y_log10(limits=c(1, 100000)) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

grid.arrange(FecalFOcus2010graphYear, FecalFocus2015graphYear, nrow = 2)