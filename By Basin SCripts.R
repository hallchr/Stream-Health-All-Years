#subsets necessary

AllCountsAmbientSubTN <- subset(AllCountsAmbient, !is.na(TN))
AllCountsAmbientSubTP <- subset(AllCountsAmbient, !is.na(TP))
AllCountsAmbientSubFecal <- subset(AllCountsAmbient, !is.na(Fecal.Coliform))
AllCountsAmbientSubTSS <- subset(AllCountsAmbient, !is.na(TSS))
AllCountsAmbientSubCu <- subset(AllCountsAmbient, !is.na(Cu))
AllCountsAmbientSubZn <- subset(AllCountsAmbient, !is.na(Zn))
AllCountsAmbientSubPb <- subset(AllCountsAmbient, !is.na(Pb))
AllCountsAmbientSubBIBI <- subset(AllCountsAmbient, !is.na(BIBI))
AllCounts2015_now <- subset(AllCountsAmbient, Year %in% c(2015,2016,2017,2018,2019,2020))
#by basin stats

#Temp by Site

Temp_HC <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Horse Creek"), aes(Site, TEMP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(10,18) +
  labs(title = NULL, subtitle=NULL, y="Temperature (Degrees Celsius)", x=NULL) + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 12), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Temp_LS <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Little Swamp Creek"), aes(Site, TEMP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(10,18) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Temp_LNC <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Lower North Creek"), aes(Site, TEMP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(10,18) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Temp_LSR <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Lower Sammamish River"), aes(Site, TEMP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(10,18) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Temp_PC <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Perry Creek"), aes(Site, TEMP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(10,18) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Temp_UNC <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Upper North Creek"), aes(Site, TEMP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(10,18) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

#Parr Creek didnt have a summer sample....same for DO
Temp_PA <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Parr Creek"), aes(Site, TEMP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(10,18) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Temp_USR <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Upper Sammamish River"), aes(Site, TEMP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(10,18) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

tiff("Temperature_By_Basin.tiff", units="in", width=11, height=6, res=500)
grid.arrange(Temp_HC, Temp_LS, Temp_PC, Temp_LNC, Temp_UNC, Temp_LSR, Temp_USR, Temp_PA, nrow = 1)
dev.off()


Temp_Table_Basin <- summaryStats(TEMP ~ Monitoring.Basin, data = AllCountsAmbientSummer, digits=3, p.value=TRUE, stats.in.rows=TRUE,
                                 test.arg.list=list(var.equal = FALSE, test="nonparametric"))

Temp_Table_Basin

write.table(Temp_Table_Basin, file = "Temp_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)

#Temp By Basin



Temp_HC <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Horse Creek"), aes(Monitoring.Basin, TEMP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(10,18) +
  labs(title = NULL, subtitle=NULL, y="Temperature (Degrees Celsius)", x=NULL) + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_text(size = 12), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Temp_LS <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Little Swamp Creek"), aes(Monitoring.Basin, TEMP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(10,18) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Temp_LNC <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Lower North Creek"), aes(Monitoring.Basin, TEMP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(10,18) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Temp_LSR <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Lower Sammamish River"), aes(Monitoring.Basin, TEMP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(10,18) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Temp_PC <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Perry Creek"), aes(Monitoring.Basin, TEMP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(10,18) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Temp_UNC <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Upper North Creek"), aes(Monitoring.Basin, TEMP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(10,18) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

#Parr Creek didnt have a summer sample....same for DO
Temp_PA <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Parr Creek"), aes(Monitoring.Basin, TEMP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(10,18) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Temp_USR <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Upper Sammamish River"), aes(Monitoring.Basin, TEMP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(10,18) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


tiff("Temperature_By_Basin_No_Site.tiff", units="in", width=11, height=6, res=500)
grid.arrange(Temp_HC, Temp_LS, Temp_PC, Temp_LNC, Temp_UNC, Temp_LSR, Temp_USR, Temp_PA, nrow = 1)
dev.off()


Temp_Table_Basin <- summaryStats(TEMP ~ Monitoring.Basin, data = AllCountsAmbientSummer, digits=3, p.value=TRUE, stats.in.rows=TRUE,
                                 test.arg.list=list(var.equal = FALSE, test="nonparametric"))

Temp_Table_Basin

write.table(Temp_Table_Basin, file = "Temp_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)

#DO by Site

DO_HC <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Horse Creek"), aes(Site, DO)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(2,12) +
  labs(title = NULL, subtitle=NULL, y="Dissolved Oxygen (mg/L)", x=NULL) + geom_hline(yintercept = 9.5, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 12), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

DO_LS <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Little Swamp Creek"), aes(Site, DO)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(2,12) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 9.5, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

DO_LNC <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Lower North Creek"), aes(Site, DO)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(2,12) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 9.5, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


DO_LSR <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Lower Sammamish River"), aes(Site, DO)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(2,12) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 9.5, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


DO_PC <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Perry Creek"), aes(Site, DO)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(2,12) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 9.5, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


DO_UNC <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Upper North Creek"), aes(Site, DO)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(2,12) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 9.5, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

DO_PA <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Parr Creek"), aes(Site, DO)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(2,12) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 9.5, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

DO_USR <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Upper Sammamish River"), aes(Site, DO)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(2,12) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 9.5, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


tiff("DO_By_Basin.tiff", units="in", width=11, height=6, res=500)
grid.arrange(DO_HC, DO_LS, DO_PC, DO_LNC, DO_UNC, DO_LSR, DO_USR, DO_PA, nrow = 1)
dev.off()

tiff("DOTemp_By_Basin.tiff", units="in", width=11, height=6, res=500)
grid.arrange(DO_HC, DO_LS, DO_PC, DO_LNC, DO_UNC, DO_LSR, DO_USR, DO_PA,
             Temp_HC, Temp_LS, Temp_PC, Temp_LNC, Temp_UNC, Temp_LSR, Temp_USR, Temp_PA, nrow = 2)
dev.off()

DO_Table_Basin <- summaryStats(DO ~ Monitoring.Basin, data = AllCountsAmbientSummer, digits=3, p.value=TRUE, stats.in.rows=TRUE,
                               test.arg.list=list(var.equal = FALSE, test="nonparametric"))

DO_Table_Basin

write.table(DO_Table_Basin, file = "DO_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)

#DO By Basin
DO_HC <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Horse Creek"), aes(Monitoring.Basin, DO)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(2,12) +
  labs(title = NULL, subtitle=NULL, y="Dossolved Oxygen (mg/L)", x=NULL) + geom_hline(yintercept = 9.5, color="red", size=1) + 
  theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_text(size = 12), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

DO_LS <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Little Swamp Creek"), aes(Monitoring.Basin, DO)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(2,12) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 9.5, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

DO_LNC <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Lower North Creek"), aes(Monitoring.Basin, DO)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(2,12) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 9.5, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


DO_LSR <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Lower Sammamish River"), aes(Monitoring.Basin, DO)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(2,12) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 9.5, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


DO_PC <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Perry Creek"), aes(Monitoring.Basin, DO)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(2,12) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 9.5, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


DO_UNC <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Upper North Creek"), aes(Monitoring.Basin, DO)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(2,12) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 9.5, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

DO_PA <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Parr Creek"), aes(Monitoring.Basin, DO)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(2,12) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 9.5, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

DO_USR <- ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Upper Sammamish River"), aes(Monitoring.Basin, DO)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(2,12) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 9.5, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


tiff("DO_By_Basin_No_Site.tiff", units="in", width=11, height=6, res=500)
grid.arrange(DO_HC, DO_LS, DO_PC, DO_LNC, DO_UNC, DO_LSR, DO_USR, DO_PA, nrow = 1)
dev.off()

tiff("DOTemp_By_Basin.tiff", units="in", width=11, height=6, res=500)
grid.arrange(DO_HC, DO_LS, DO_PC, DO_LNC, DO_UNC, DO_LSR, DO_USR, DO_PA,
             Temp_HC, Temp_LS, Temp_PC, Temp_LNC, Temp_UNC, Temp_LSR, Temp_USR, Temp_PA, nrow = 2)
dev.off()

DO_Table_Basin <- summaryStats(DO ~ Monitoring.Basin, data = AllCountsAmbientSummer, digits=3, p.value=TRUE, stats.in.rows=TRUE,
                               test.arg.list=list(var.equal = FALSE, test="nonparametric"))

DO_Table_Basin

write.table(DO_Table_Basin, file = "DO_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)



#SpC by site

SpC_HC <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Horse Creek"), aes(Site, Specific.Cond)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,500) +
  labs(title = NULL, subtitle=NULL, y="Specific Conductivity (uS/cm)", x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 12), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

SpC_LS <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Little Swamp Creek"), aes(Site, Specific.Cond)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,500) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) +  
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

SpC_LNC <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Lower North Creek"), aes(Site, Specific.Cond)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,500) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


SpC_LSR <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Lower Sammamish River"), aes(Site, Specific.Cond)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,500) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


SpC_PC <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Perry Creek"), aes(Site, Specific.Cond)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,500) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


SpC_UNC <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Upper North Creek"), aes(Site, Specific.Cond)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,500) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) +  
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

SpC_PA <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Parr Creek"), aes(Site, Specific.Cond)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,500) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

SpC_USR <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Upper Sammamish River"), aes(Site, Specific.Cond)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,500) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


tiff("SpC_By_Basin.tiff", units="in", width=11, height=6, res=500)
grid.arrange(SpC_HC, SpC_LS, SpC_PC, SpC_LNC, SpC_UNC, SpC_LSR, SpC_USR, SpC_PA, nrow = 1)
dev.off()

SpC_Table_Basin <- summaryStats(Specific.Cond ~ Monitoring.Basin, data = AllCountsAmbient, digits=3, p.value=TRUE, stats.in.rows=TRUE,
                               test.arg.list=list(var.equal = FALSE, test="nonparametric"))

SpC_Table_Basin

write.table(SpC_Table_Basin, file = "SpC_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)

#SpC by basin

SpC_HC <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Horse Creek"), aes(Monitoring.Basin, Specific.Cond)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,500) +
  labs(title = NULL, subtitle=NULL, y="Specific Conductivity (uS/cm)", x=NULL) +
  theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_text(size = 12), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

SpC_LS <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Little Swamp Creek"), aes(Monitoring.Basin, Specific.Cond)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,500) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) +  
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

SpC_LNC <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Lower North Creek"), aes(Monitoring.Basin, Specific.Cond)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,500) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


SpC_LSR <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Lower Sammamish River"), aes(Monitoring.Basin, Specific.Cond)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,500) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


SpC_PC <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Perry Creek"), aes(Monitoring.Basin, Specific.Cond)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,500) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


SpC_UNC <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Upper North Creek"), aes(Monitoring.Basin, Specific.Cond)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,500) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) +  
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

SpC_PA <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Parr Creek"), aes(Monitoring.Basin, Specific.Cond)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,500) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

SpC_USR <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Upper Sammamish River"), aes(Monitoring.Basin, Specific.Cond)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,500) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

tiff("SpC_By_Basin_No_Sites.tiff", units="in", width=11, height=6, res=500)
grid.arrange(SpC_HC, SpC_LS, SpC_PC, SpC_LNC, SpC_UNC, SpC_LSR, SpC_USR, SpC_PA, nrow = 1)
dev.off()



#Turbidity by Site

Turb_HC <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Horse Creek"), aes(Site, Turbidity)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) +ylim(0,50) +
  labs(title = NULL, subtitle=NULL, y="Turbidity (NTU)", x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 12), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Turb_LS <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Little Swamp Creek"), aes(Site, Turbidity)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,50) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) +  
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Turb_LNC <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Lower North Creek"), aes(Site, Turbidity)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) +ylim(0,50) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Turb_LSR <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Lower Sammamish River"), aes(Site, Turbidity)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE)  +ylim(0,50) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Turb_PC <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Perry Creek"), aes(Site, Turbidity)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE)  +ylim(0,50) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Turb_UNC <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Upper North Creek"), aes(Site, Turbidity)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE)  +ylim(0,50) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) +  
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Turb_PA <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Parr Creek"), aes(Site, Turbidity)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) +ylim(0,50) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Turb_USR <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Upper Sammamish River"), aes(Site, Turbidity)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,50) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


tiff("Turb_By_Basin.tiff", units="in", width=11, height=6, res=500)
grid.arrange(Turb_HC, Turb_LS, Turb_PC, Turb_LNC, Turb_UNC, Turb_LSR, Turb_USR, Turb_PA, nrow = 1)
dev.off()

Turb_Table_Basin <- summaryStats(Turbidity ~ Monitoring.Basin, data = AllCountsAmbient, digits=3, p.value=TRUE, stats.in.rows=TRUE,
                                test.arg.list=list(var.equal = FALSE, test="nonparametric"))

Turb_Table_Basin

write.table(Turb_Table_Basin, file = "Turb_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)

#Turbidity by Basin
Turb_HC <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Horse Creek"), aes(Monitoring.Basin, Turbidity)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) +ylim(0,50) +
  labs(title = NULL, subtitle=NULL, y="Turbidity (NTU)", x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Turb_LS <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Little Swamp Creek"), aes(Monitoring.Basin, Turbidity)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,50) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) +  
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Turb_LNC <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Lower North Creek"), aes(Monitoring.Basin, Turbidity)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) +ylim(0,50) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Turb_LSR <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Lower Sammamish River"), aes(Monitoring.Basin, Turbidity)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE)  +ylim(0,50) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Turb_PC <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Perry Creek"), aes(Monitoring.Basin, Turbidity)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE)  +ylim(0,50) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Turb_UNC <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Upper North Creek"), aes(Monitoring.Basin, Turbidity)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE)  +ylim(0,50) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) +  
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Turb_PA <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Parr Creek"), aes(Monitoring.Basin, Turbidity)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) +ylim(0,50) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Turb_USR <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Upper Sammamish River"), aes(Monitoring.Basin, Turbidity)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,50) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


tiff("Turb_By_Basin_No_Site.tiff", units="in", width=11, height=6, res=500)
grid.arrange(Turb_HC, Turb_LS, Turb_PC, Turb_LNC, Turb_UNC, Turb_LSR, Turb_USR, Turb_PA, nrow = 1)
dev.off()

#TSS by Site
TSS_HC <- ggplot(subset(AllCountsAmbientSubTSS, Monitoring.Basin == "Horse Creek"), aes(Site, TSS)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) +ylim(0,70) +
  labs(title = NULL, subtitle=NULL, y="TSS (mg/L)", x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 12), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TSS_LS <- ggplot(subset(AllCountsAmbientSubTSS, Monitoring.Basin == "Little Swamp Creek"), aes(Site, TSS)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,70) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) +  
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TSS_LNC <- ggplot(subset(AllCountsAmbientSubTSS, Monitoring.Basin == "Lower North Creek"), aes(Site, TSS)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) +ylim(0,70) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TSS_LSR <- ggplot(subset(AllCountsAmbientSubTSS, Monitoring.Basin == "Lower Sammamish River"), aes(Site, TSS)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE)  +ylim(0,70) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TSS_PC <- ggplot(subset(AllCountsAmbientSubTSS, Monitoring.Basin == "Perry Creek"), aes(Site, TSS)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE)  +ylim(0,70) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TSS_UNC <- ggplot(subset(AllCountsAmbientSubTSS, Monitoring.Basin == "Upper North Creek"), aes(Site, TSS)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE)  +ylim(0,70) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) +  
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TSS_PA <- ggplot(subset(AllCountsAmbientSubTSS, Monitoring.Basin == "Parr Creek"), aes(Site, TSS)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) +ylim(0,70) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TSS_USR <- ggplot(subset(AllCountsAmbientSubTSS, Monitoring.Basin == "Upper Sammamish River"), aes(Site, TSS)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,70) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


tiff("TSS_By_Basin.tiff", units="in", width=11, height=6, res=500)
grid.arrange(TSS_HC, TSS_LS, TSS_PC, TSS_LNC, TSS_UNC, TSS_LSR, TSS_PA, nrow = 1)
dev.off()

TSS_Table_Basin <- summaryStats(TSS ~ Monitoring.Basin, data = AllCountsAmbient, digits=3, p.value=TRUE, stats.in.rows=TRUE,
                                 test.arg.list=list(var.equal = FALSE, test="nonparametric"))

TSS_Table_Basin

write.table(Turb_Table_Basin, file = "Turb_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)

#TSS by Basin
TSS_HC <- ggplot(subset(AllCountsAmbientSubTSS, Monitoring.Basin == "Horse Creek"), aes(Monitoring.Basin, TSS)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) +ylim(0,70) +
  labs(title = NULL, subtitle=NULL, y="TSS (mg/L)", x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TSS_LS <- ggplot(subset(AllCountsAmbientSubTSS, Monitoring.Basin == "Little Swamp Creek"), aes(Monitoring.Basin, TSS)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,70) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) +  
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TSS_LNC <- ggplot(subset(AllCountsAmbientSubTSS, Monitoring.Basin == "Lower North Creek"), aes(Monitoring.Basin, TSS)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) +ylim(0,70) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TSS_LSR <- ggplot(subset(AllCountsAmbientSubTSS, Monitoring.Basin == "Lower Sammamish River"), aes(Monitoring.Basin, TSS)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE)  +ylim(0,70) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TSS_PC <- ggplot(subset(AllCountsAmbientSubTSS, Monitoring.Basin == "Perry Creek"), aes(Monitoring.Basin, TSS)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE)  +ylim(0,70) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TSS_UNC <- ggplot(subset(AllCountsAmbientSubTSS, Monitoring.Basin == "Upper North Creek"), aes(Monitoring.Basin, TSS)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE)  +ylim(0,70) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) +  
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TSS_PA <- ggplot(subset(AllCountsAmbientSubTSS, Monitoring.Basin == "Parr Creek"), aes(Monitoring.Basin, TSS)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) +ylim(0,70) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TSS_USR <- ggplot(subset(AllCountsAmbientSubTSS, Monitoring.Basin == "Upper Sammamish River"), aes(Monitoring.Basin, TSS)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,70) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


tiff("TSS_By_Basin_No_Site.tiff", units="in", width=11, height=6, res=500)
grid.arrange(TSS_HC, TSS_LS, TSS_PC, TSS_LNC, TSS_UNC, TSS_LSR, TSS_PA, nrow = 1)
dev.off()

#Nutrients by Site
TP_HC <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Horse Creek"), aes(Site, TP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,0.5) +
  labs(title = NULL, subtitle=NULL, y="Total Phosphorous (mg/L)", x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) +geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 12), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TP_LS <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Little Swamp Creek"), aes(Site, TP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,0.5) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TP_LNC <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Lower North Creek"), aes(Site, TP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,0.5) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TP_LSR <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Lower Sammamish River"), aes(Site, TP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,0.5) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TP_PC <- ggplot(subset(AllCountsAmbientSubTP, Monitoring.Basin == "Perry Creek"), aes(Site, TP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,0.5) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TP_UNC <- ggplot(subset(AllCountsAmbientSubTP, Monitoring.Basin == "Upper North Creek"), aes(Site, TP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,0.5) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TP_PA <- ggplot(subset(AllCountsAmbientSubTP, Monitoring.Basin == "Parr Creek"), aes(Site, TP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,.5) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TP_USR <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Upper Sammamish River"), aes(Site, TP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,.5) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

tiff("TP_By_Basin.tiff", units="in", width=11, height=6, res=500)
grid.arrange(TP_HC, TP_LS, TP_PC, TP_LNC, TP_UNC, TP_LSR, TP_PA, nrow = 1)

dev.off()


TN_HC <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Horse Creek"), aes(Site, TN)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3) +
  labs(title = NULL, subtitle=NULL, y="Total Nitrogen (mg/L)") + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.98, color = "red", size=1) +
  labs(title = NULL, subtitle=NULL, y="Total Nitrogen (mg/L)", x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 12), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TN_LS <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Little Swamp Creek"), aes(Site, TN)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.98, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TN_LNC <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Lower North Creek"), aes(Site, TN)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL)  + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.98, color = "red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TN_LSR <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Lower Sammamish River"), aes(Site, TN)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.98, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TN_PC <- ggplot(subset(AllCountsAmbientSubTN, Monitoring.Basin == "Perry Creek"), aes(Site, TN)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL)  + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.98, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TN_UNC <- ggplot(subset(AllCountsAmbientSubTN, Monitoring.Basin == "Upper North Creek"), aes(Site, TN)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.98, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TN_PA <- ggplot(subset(AllCountsAmbientSubTN, Monitoring.Basin == "Parr Creek"), aes(Site, TN)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.98, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TN_USR <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Upper Sammamish River"), aes(Site, TN)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.98, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


tiff("TN_By_Basin.tiff", units="in", width=11, height=6, res=500)
grid.arrange(TN_HC, TN_LS, TN_PC, TN_LNC, TN_UNC, TN_LSR, TN_PA, nrow = 1)

dev.off()

tiff("Nutrients_By_Basin.tiff", units="in", width=11, height=6, res=500)
grid.arrange(TP_HC, TP_LS, TP_PC, TP_LNC, TP_UNC, TP_LSR, TP_PA, TN_HC, TN_LS, TN_PC, TN_LNC, TN_UNC, TN_LSR, TN_PA, nrow = 2)

dev.off()




TP_Table_Basin <- summaryStats(TP ~ Monitoring.Basin, data = AllCountsAmbient2019, digits=3, p.value=TRUE, stats.in.rows=TRUE,
                               test.arg.list=list(var.equal = FALSE, test="nonparametric"))

TP_Table_Basin

write.table(TP_Table_Basin, file = "TP_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)

TN_Table_Basin <- summaryStats(TN ~ Monitoring.Basin, data = AllCountsAmbient2019, digits=3, p.value=TRUE, stats.in.rows=TRUE,
                               test.arg.list=list(var.equal = FALSE, test="nonparametric"))

TN_Table_Basin

write.table(TN_Table_Basin, file = "TN_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)

#Nutrients by Basin

TP_HC <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Horse Creek"), aes(Monitoring.Basin, TP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,0.5) +
  labs(title = NULL, subtitle=NULL, y="Total Phosphorous (mg/L)", x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) +geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TP_LS <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Little Swamp Creek"), aes(Monitoring.Basin, TP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,0.5) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TP_LNC <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Lower North Creek"), aes(Monitoring.Basin, TP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,0.5) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TP_LSR <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Lower Sammamish River"), aes(Monitoring.Basin, TP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,0.5) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TP_PC <- ggplot(subset(AllCountsAmbientSubTP, Monitoring.Basin == "Perry Creek"), aes(Monitoring.Basin, TP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,0.5) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TP_UNC <- ggplot(subset(AllCountsAmbientSubTP, Monitoring.Basin == "Upper North Creek"), aes(Monitoring.Basin, TP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,0.5) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TP_PA <- ggplot(subset(AllCountsAmbientSubTP, Monitoring.Basin == "Parr Creek"), aes(Monitoring.Basin, TP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,.5) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_blank(), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TP_USR <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Upper Sammamish River"), aes(Monitoring.Basin, TP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,.5) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_blank(), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

tiff("TP_By_Basin_No_Site.tiff", units="in", width=11, height=6, res=500)
grid.arrange(TP_HC, TP_LS, TP_PC, TP_LNC, TP_UNC, TP_LSR, TP_PA, nrow = 1)

dev.off()


TN_HC <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Horse Creek"), aes(Monitoring.Basin, TN)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3) +
  labs(title = NULL, subtitle=NULL, y="Total Nitrogen (mg/L)") + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.98, color = "red", size=1) +
  labs(title = NULL, subtitle=NULL, y="Total Nitrogen (mg/L)", x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TN_LS <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Little Swamp Creek"), aes(Monitoring.Basin, TN)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.98, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TN_LNC <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Lower North Creek"), aes(Monitoring.Basin, TN)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL)  + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.98, color = "red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TN_LSR <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Lower Sammamish River"), aes(Monitoring.Basin, TN)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.98, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TN_PC <- ggplot(subset(AllCountsAmbientSubTN, Monitoring.Basin == "Perry Creek"), aes(Monitoring.Basin, TN)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL)  + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.98, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


TN_UNC <- ggplot(subset(AllCountsAmbientSubTN, Monitoring.Basin == "Upper North Creek"), aes(Monitoring.Basin, TN)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.98, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TN_PA <- ggplot(subset(AllCountsAmbientSubTN, Monitoring.Basin == "Parr Creek"), aes(Monitoring.Basin, TN)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.98, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

TN_USR <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Upper Sammamish River"), aes(Monitoring.Basin, TN)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.98, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")




tiff("TN_By_Basin_No_Sites.tiff", units="in", width=11, height=6, res=500)
grid.arrange(TN_HC, TN_LS, TN_PC, TN_LNC, TN_UNC, TN_LSR, TN_PA,nrow = 1)
dev.off()

TP_Table_Basin <- summaryStats(TP ~ Monitoring.Basin, data = AllCountsAmbient, digits=3, p.value=TRUE, stats.in.rows=TRUE,
                               test.arg.list=list(var.equal = FALSE, test="nonparametric"))

TP_Table_Basin

write.table(TP_Table_Basin, file = "TP_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)

TN_Table_Basin <- summaryStats(TN ~ Monitoring.Basin, data = AllCountsAmbient, digits=3, p.value=TRUE, stats.in.rows=TRUE,
                               test.arg.list=list(var.equal = FALSE, test="nonparametric"))

TN_Table_Basin

write.table(TN_Table_Basin, file = "TN_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)


#Fecal By Site

Fecal_HC <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Horse Creek"), aes(Site, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y="Fecal Coliform Bacteria (CFU)", x=NULL) +  
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 12), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Fecal_LS <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Little Swamp Creek"), aes(Site, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Fecal_LNC <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Lower North Creek"), aes(Site, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Fecal_LSR <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Lower Sammamish River"), aes(Site, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Fecal_PC <- ggplot(subset(AllCountsAmbientSubFecal, Monitoring.Basin == "Perry Creek"), aes(Site, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Fecal_PA <- ggplot(subset(AllCountsAmbientSubFecal, Monitoring.Basin == "Parr Creek"), aes(Site, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Fecal_UNC <- ggplot(subset(AllCountsAmbientSubFecal, Monitoring.Basin == "Upper North Creek"), aes(Site, Fecal.Coliform)) + 
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

FC_Table_Basin <- summaryStats(Fecal.Coliform ~ Monitoring.Basin, data = AllCountsAmbient2019, digits=3, p.value=FALSE, stats.in.rows=TRUE,
                               test.arg.list=list(var.equal = FALSE, test="nonparametric"))

FC_Table_Basin

write.table(FC_Table_Basin, file = "FC_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)

#fecals & Nutrients

grid.arrange(Fecal_HC, Fecal_LS, Fecal_PC, Fecal_LNC, Fecal_UNC, Fecal_LSR, Fecal_PA, TP_HC, TP_LS, TP_PC, TP_LNC, TP_UNC, TP_LSR, TP_PA, TN_HC, TN_LS,
             TN_PC, TN_LNC, TN_UNC, TN_LSR, TN_PA, nrow = 3)

#Fecal by Basin
Fecal_HC <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Horse Creek"), aes(Monitoring.Basin, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y="Fecal Coliform Bacteria (CFU)", x=NULL) +  
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Fecal_LS <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Little Swamp Creek"), aes(Monitoring.Basin, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_blank(), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Fecal_LNC <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Lower North Creek"), aes(Monitoring.Basin, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_blank(), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Fecal_LSR <- ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Lower Sammamish River"), aes(Monitoring.Basin, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_blank(), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Fecal_PC <- ggplot(subset(AllCountsAmbientSubFecal, Monitoring.Basin == "Perry Creek"), aes(Monitoring.Basin, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_blank(), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Fecal_PA <- ggplot(subset(AllCountsAmbientSubFecal, Monitoring.Basin == "Parr Creek"), aes(Monitoring.Basin, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_blank(), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Fecal_UNC <- ggplot(subset(AllCountsAmbientSubFecal, Monitoring.Basin == "Upper North Creek"), aes(Monitoring.Basin, Fecal.Coliform)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3000) + geom_hline(yintercept = 50, color="red", size=1) + geom_hline(yintercept = 200, color="blue", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_blank(), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

tiff("FC_By_Basin_No_Site.tiff", units="in", width=11, height=6, res=500)
grid.arrange(Fecal_HC, Fecal_LS, Fecal_PC, Fecal_LNC, Fecal_UNC, Fecal_LSR, Fecal_PA, nrow = 1)
dev.off()

FC_Table_Basin <- summaryStats(Fecal.Coliform ~ Monitoring.Basin, data = AllCountsAmbient, digits=3, p.value=FALSE, stats.in.rows=TRUE,
                               test.arg.list=list(var.equal = FALSE, test="nonparametric"))

FC_Table_Basin

write.table(FC_Table_Basin, file = "FC_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)

#metals by site

Zn_HC <- ggplot(subset(AllCountsAmbientSubZn, Monitoring.Basin == "Horse Creek"), aes(Site, Zn)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,150) + 
  labs(title = NULL, subtitle=NULL, y="Zinc (ug/L)", x=NULL) + geom_hline(yintercept = 32, color = "orange2", size = 1) + geom_hline(yintercept = 35, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 12), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Zn_LS <- ggplot(subset(AllCountsAmbientSubZn, Monitoring.Basin == "Little Swamp Creek"), aes(Site, Zn)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,150) + 
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 32, color = "orange2", size = 1) + geom_hline(yintercept = 35, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Zn_LNC <- ggplot(subset(AllCountsAmbientSubZn, Monitoring.Basin == "Lower North Creek"), aes(Site, Zn)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,150) + 
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 32, color = "orange2", size = 1) + geom_hline(yintercept = 35, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Zn_LSR <- ggplot(subset(AllCountsAmbientSubZn, Monitoring.Basin == "Lower Sammamish River"), aes(Site, Zn)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,150) + 
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 32, color = "orange2", size = 1) + geom_hline(yintercept = 35, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Zn_PC <- ggplot(subset(AllCountsAmbientSubZn, Monitoring.Basin == "Perry Creek"), aes(Site, Zn)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,150) + 
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 32, color = "orange2", size = 1) + geom_hline(yintercept = 35, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Zn_UNC <- ggplot(subset(AllCountsAmbientSubZn, Monitoring.Basin == "Upper North Creek"), aes(Site, Zn)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,150) + 
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 32, color = "orange2", size = 1) + geom_hline(yintercept = 35, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Zn_PA <- ggplot(subset(AllCountsAmbientSubZn, Monitoring.Basin == "Parr Creek"), aes(Site, Zn)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,150) + 
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 32, color = "orange2", size = 1) + geom_hline(yintercept = 35, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

tiff("Zn_By_Basin.tiff", units="in", width=11, height=6, res=500)
grid.arrange(Zn_HC, Zn_LS, Zn_PC, Zn_LNC, Zn_UNC, Zn_LSR, Zn_PA, nrow = 1)
dev.off()

Cu_HC <- ggplot(subset(AllCountsAmbientSubCu, Monitoring.Basin == "Horse Creek"), aes(Site, Cu)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,6) +
  labs(title = NULL, subtitle=NULL, y="Copper (ug/L)") + geom_hline(yintercept = 3.47, color = "orange2", size = 1) + geom_hline(yintercept = 4.61, color = "red", size=1) +
  labs(title = NULL, subtitle=NULL, y="Copper (ug/L)", x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 12), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Cu_LS <- ggplot(subset(AllCountsAmbientSubCu, Monitoring.Basin == "Little Swamp Creek"), aes(Site, Cu)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,6) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 3.47, color = "orange2", size = 1) + geom_hline(yintercept = 4.61, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Cu_LNC <- ggplot(subset(AllCountsAmbientSubCu, Monitoring.Basin == "Lower North Creek"), aes(Site, Cu)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,6) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL)  + geom_hline(yintercept = 3.47, color = "orange2", size = 1) + geom_hline(yintercept = 4.61, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Cu_LSR <- ggplot(subset(AllCountsAmbientSubCu, Monitoring.Basin == "Lower Sammamish River"), aes(Site, Cu)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,6) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 3.47, color = "orange2", size = 1) + geom_hline(yintercept = 4.61, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Cu_PC <- ggplot(subset(AllCountsAmbientSubCu, Monitoring.Basin == "Perry Creek"), aes(Site, Cu)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,6) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL)  + geom_hline(yintercept = 3.47, color = "orange2", size = 1) + geom_hline(yintercept = 4.61, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Cu_UNC <- ggplot(subset(AllCountsAmbientSubCu, Monitoring.Basin == "Upper North Creek"), aes(Site, Cu)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,6) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 3.47, color = "orange2", size = 1) + geom_hline(yintercept = 4.61, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Cu_PA <- ggplot(subset(AllCountsAmbientSubCu, Monitoring.Basin == "Parr Creek"), aes(Site, Cu)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,6) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 3.47, color = "orange2", size = 1) + geom_hline(yintercept = 4.61, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

tiff("Cu_By_Basin.tiff", units="in", width=11, height=6, res=500)
grid.arrange(Cu_HC, Cu_LS, Cu_PC, Cu_LNC, Cu_UNC, Cu_LSR, Cu_PA, nrow = 1)
dev.off()


Pb_HC <- ggplot(subset(AllCountsAmbientSubPb, Monitoring.Basin == "Horse Creek"), aes(Site, Pb)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,2) +
  labs(title = NULL, subtitle=NULL, y="Lead (ug/L)", x=NULL) + geom_hline(yintercept = 0.541, color = "orange2", size = 1) + geom_hline(yintercept = 13.88, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Pb_LS <- ggplot(subset(AllCountsAmbientSubPb, Monitoring.Basin == "Little Swamp Creek"), aes(Site, Pb)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,2) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.541, color = "orange2", size = 1) + geom_hline(yintercept = 13.88, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Pb_LNC <- ggplot(subset(AllCountsAmbientSubPb, Monitoring.Basin == "Lower North Creek"), aes(Site, Pb)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,2) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL)  + geom_hline(yintercept = 0.541, color = "orange2", size = 1) + geom_hline(yintercept = 13.88, color = "red", size=1) +
  theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Pb_LSR <- ggplot(subset(AllCountsAmbientSubPb, Monitoring.Basin == "Lower Sammamish River"), aes(Site, Pb)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,2) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.541, color = "orange2", size = 1) + geom_hline(yintercept = 13.88, color = "red", size=1) +
  theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Pb_PC <- ggplot(subset(AllCountsAmbientSubPb, Monitoring.Basin == "Perry Creek"), aes(Site, Pb)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,2) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL)  + geom_hline(yintercept = 0.541, color = "orange2", size = 1) + geom_hline(yintercept = 13.88, color = "red", size=1) +
  theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Pb_UNC <- ggplot(subset(AllCountsAmbientSubPb, Monitoring.Basin == "Upper North Creek"), aes(Site, Pb)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,2) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.541, color = "orange2", size = 1) + geom_hline(yintercept = 13.88, color = "red", size=1) +
  theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Pb_PA <- ggplot(subset(AllCountsAmbientSubPb, Monitoring.Basin == "Parr Creek"), aes(Site, Pb)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,2) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.541, color = "orange2", size = 1) + geom_hline(yintercept = 13.88, color = "red", size=1) +
  theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")



tiff("Pb_By_Basin.tiff", units="in", width=11, height=5, res=500)
grid.arrange(Pb_HC, Pb_LS, Pb_PC, Pb_LNC, Pb_UNC, Pb_LSR, Pb_PA, nrow = 1)
dev.off()

Zn_Table_Basin <- summaryStats(Zn ~ Monitoring.Basin, data = AllCountsAmbient, digits=3, p.value=TRUE, stats.in.rows=TRUE,
                               test.arg.list=list(var.equal = FALSE, test="nonparametric"))

Zn_Table_Basin

write.table(Zn_Table_Basin, file = "Zn_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)


Pb_Table_Basin <- summaryStats(Pb ~ Monitoring.Basin, data = AllCountsAmbient, digits=3, p.value=TRUE, stats.in.rows=TRUE,
                               test.arg.list=list(var.equal = FALSE, test="nonparametric"))

Pb_Table_Basin

write.table(Pb_Table_Basin, file = "Pb_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)



#metals by basin

Zn_HC <- ggplot(subset(AllCountsAmbientSubZn, Monitoring.Basin == "Horse Creek"), aes(Monitoring.Basin, Zn)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,150) + 
  labs(title = NULL, subtitle=NULL, y="Zinc (ug/L)", x=NULL) + geom_hline(yintercept = 32, color = "orange2", size = 1) + geom_hline(yintercept = 35, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_text(size = 12), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Zn_LS <- ggplot(subset(AllCountsAmbientSubZn, Monitoring.Basin == "Little Swamp Creek"), aes(Monitoring.Basin, Zn)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,150) + 
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 32, color = "orange2", size = 1) + geom_hline(yintercept = 35, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Zn_LNC <- ggplot(subset(AllCountsAmbientSubZn, Monitoring.Basin == "Lower North Creek"), aes(Monitoring.Basin, Zn)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,150) + 
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 32, color = "orange2", size = 1) + geom_hline(yintercept = 35, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Zn_LSR <- ggplot(subset(AllCountsAmbientSubZn, Monitoring.Basin == "Lower Sammamish River"), aes(Monitoring.Basin, Zn)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,150) + 
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 32, color = "orange2", size = 1) + geom_hline(yintercept = 35, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Zn_PC <- ggplot(subset(AllCountsAmbientSubZn, Monitoring.Basin == "Perry Creek"), aes(Monitoring.Basin, Zn)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,150) + 
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 32, color = "orange2", size = 1) + geom_hline(yintercept = 35, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Zn_UNC <- ggplot(subset(AllCountsAmbientSubZn, Monitoring.Basin == "Upper North Creek"), aes(Monitoring.Basin, Zn)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,150) + 
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 32, color = "orange2", size = 1) + geom_hline(yintercept = 35, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Zn_PA <- ggplot(subset(AllCountsAmbientSubZn, Monitoring.Basin == "Parr Creek"), aes(Monitoring.Basin, Zn)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,150) + 
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 32, color = "orange2", size = 1) + geom_hline(yintercept = 35, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

tiff("Zn_By_BasinNo_Site.tiff", units="in", width=11, height=6, res=500)
grid.arrange(Zn_HC, Zn_LS, Zn_PC, Zn_LNC, Zn_UNC, Zn_LSR, Zn_PA, nrow = 1)
dev.off()

Cu_HC <- ggplot(subset(AllCountsAmbientSubCu, Monitoring.Basin == "Horse Creek"), aes(Monitoring.Basin, Cu)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,6) +
  labs(title = NULL, subtitle=NULL, y="Copper (ug/L)") + geom_hline(yintercept = 3.47, color = "orange2", size = 1) + geom_hline(yintercept = 4.61, color = "red", size=1) +
  labs(title = NULL, subtitle=NULL, y="Copper (ug/L)", x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_text(size = 12), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Cu_LS <- ggplot(subset(AllCountsAmbientSubCu, Monitoring.Basin == "Little Swamp Creek"), aes(Monitoring.Basin, Cu)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,6) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 3.47, color = "orange2", size = 1) + geom_hline(yintercept = 4.61, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Cu_LNC <- ggplot(subset(AllCountsAmbientSubCu, Monitoring.Basin == "Lower North Creek"), aes(Monitoring.Basin, Cu)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,6) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL)  + geom_hline(yintercept = 3.47, color = "orange2", size = 1) + geom_hline(yintercept = 4.61, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Cu_LSR <- ggplot(subset(AllCountsAmbientSubCu, Monitoring.Basin == "Lower Sammamish River"), aes(Monitoring.Basin, Cu)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,6) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 3.47, color = "orange2", size = 1) + geom_hline(yintercept = 4.61, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Cu_PC <- ggplot(subset(AllCountsAmbientSubCu, Monitoring.Basin == "Perry Creek"), aes(Monitoring.Basin, Cu)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,6) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL)  + geom_hline(yintercept = 3.47, color = "orange2", size = 1) + geom_hline(yintercept = 4.61, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Cu_UNC <- ggplot(subset(AllCountsAmbientSubCu, Monitoring.Basin == "Upper North Creek"), aes(Monitoring.Basin, Cu)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,6) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 3.47, color = "orange2", size = 1) + geom_hline(yintercept = 4.61, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Cu_PA <- ggplot(subset(AllCountsAmbientSubCu, Monitoring.Basin == "Parr Creek"), aes(Monitoring.Basin, Cu)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,6) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 3.47, color = "orange2", size = 1) + geom_hline(yintercept = 4.61, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

tiff("Cu_By_BasinNo_Sites.tiff", units="in", width=11, height=6, res=500)
grid.arrange(Cu_HC, Cu_LS, Cu_PC, Cu_LNC, Cu_UNC, Cu_LSR, Cu_PA, nrow = 1)
dev.off()



Pb_HC <- ggplot(subset(AllCountsAmbientSubPb, Monitoring.Basin == "Horse Creek"), aes(Monitoring.Basin, Pb)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,2) +
  labs(title = NULL, subtitle=NULL, y="Lead (ug/L)", x=NULL) + geom_hline(yintercept = 0.541, color = "orange2", size = 1) + geom_hline(yintercept = 13.88, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_text(size = 12), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Pb_LS <- ggplot(subset(AllCountsAmbientSubPb, Monitoring.Basin == "Little Swamp Creek"), aes(Monitoring.Basin, Pb)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,2) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.541, color = "orange2", size = 1) + geom_hline(yintercept = 13.88, color = "red", size=1) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

Pb_LNC <- ggplot(subset(AllCountsAmbientSubPb, Monitoring.Basin == "Lower North Creek"), aes(Monitoring.Basin, Pb)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,2) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL)  + geom_hline(yintercept = 0.541, color = "orange2", size = 1) + geom_hline(yintercept = 13.88, color = "red", size=1) +
  theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Pb_LSR <- ggplot(subset(AllCountsAmbientSubPb, Monitoring.Basin == "Lower Sammamish River"), aes(Monitoring.Basin, Pb)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,2) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.541, color = "orange2", size = 1) + geom_hline(yintercept = 13.88, color = "red", size=1) +
  theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Pb_PC <- ggplot(subset(AllCountsAmbientSubPb, Monitoring.Basin == "Perry Creek"), aes(Monitoring.Basin, Pb)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,2) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL)  + geom_hline(yintercept = 0.541, color = "orange2", size = 1) + geom_hline(yintercept = 13.88, color = "red", size=1) +
  theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Pb_UNC <- ggplot(subset(AllCountsAmbientSubPb, Monitoring.Basin == "Upper North Creek"), aes(Monitoring.Basin, Pb)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,2) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.541, color = "orange2", size = 1) + geom_hline(yintercept = 13.88, color = "red", size=1) +
  theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


Pb_PA <- ggplot(subset(AllCountsAmbientSubPb, Monitoring.Basin == "Parr Creek"), aes(Monitoring.Basin, Pb)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,2) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + geom_hline(yintercept = 0.541, color = "orange2", size = 1) + geom_hline(yintercept = 13.88, color = "red", size=1) +
  theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")



tiff("Pb_By_Basin_No_Sites.tiff", units="in", width=11, height=5, res=500)
grid.arrange(Pb_HC, Pb_LS, Pb_PC, Pb_LNC, Pb_UNC, Pb_LSR, Pb_PA, nrow = 1)
dev.off()

#BASIN SCRIPTS NOT BY SITE_______________________________________________________



#fecals & Nutrients

grid.arrange(Fecal_HC, Fecal_LS, Fecal_PC, Fecal_LNC, Fecal_UNC, Fecal_LSR, Fecal_PA, TP_HC, TP_LS, TP_PC, TP_LNC, TP_UNC, TP_LSR, TP_PA, TN_HC, TN_LS,
             TN_PC, TN_LNC, TN_UNC, TN_LSR, TN_PA, nrow = 3)

#BIBI by Site
BIBI_HC <- ggplot(subset(AllCountsAmbientSubBIBI, Monitoring.Basin == "Horse Creek"), aes(Site, BIBI)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,100) + geom_hline(yintercept = 20, color="red", size=1) + geom_hline(yintercept = 40, color="Yellow", size=1) + geom_hline(yintercept = 60, color= "palegreen1", size=1) +  geom_hline(yintercept = 80, color= "palegreen4", size=1) +
  labs(title = NULL, subtitle=NULL, y="BIBI", x=NULL) +  
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

BIBI_LS <- ggplot(subset(AllCountsAmbientSubBIBI, Monitoring.Basin == "Little Swamp Creek"), aes(Site, BIBI)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3100) + geom_hline(yintercept = 20, color="red", size=1) + geom_hline(yintercept = 40, color="Yellow", size=1) + geom_hline(yintercept = 60, color= "palegreen1", size=1) +  geom_hline(yintercept = 80, color= "palegreen4", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

BIBI_LNC <- ggplot(subset(AllCountsAmbientSubBIBI, Monitoring.Basin == "Lower North Creek"), aes(Site, BIBI)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,100) + geom_hline(yintercept = 20, color="red", size=1) + geom_hline(yintercept = 40, color="Yellow", size=1) + geom_hline(yintercept = 60, color= "palegreen1", size=1) +  geom_hline(yintercept = 80, color= "palegreen4", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


BIBI_LSR <- ggplot(subset(AllCountsAmbientSubBIBI, Monitoring.Basin == "Lower Sammamish River"), aes(Site, BIBI)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,100) + geom_hline(yintercept = 20, color="red", size=1) + geom_hline(yintercept = 40, color="Yellow", size=1) + geom_hline(yintercept = 60, color= "palegreen1", size=1) +  geom_hline(yintercept = 80, color= "palegreen4", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


BIBI_PC <- ggplot(subset(AllCountsAmbientSubBIBI, Monitoring.Basin == "Perry Creek"), aes(Site, BIBI)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,100) + geom_hline(yintercept = 20, color="red", size=1) + geom_hline(yintercept = 40, color="Yellow", size=1) + geom_hline(yintercept = 60, color= "palegreen1", size=1) +  geom_hline(yintercept = 80, color= "palegreen4", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

BIBI_PA <- ggplot(subset(AllCountsAmbientSubBIBI, Monitoring.Basin == "Parr Creek"), aes(Site, BIBI)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,100) + geom_hline(yintercept = 20, color="red", size=1) + geom_hline(yintercept = 40, color="Yellow", size=1) + geom_hline(yintercept = 60, color= "palegreen1", size=1) +  geom_hline(yintercept = 80, color= "palegreen4", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


BIBI_UNC <- ggplot(subset(AllCountsAmbientSubBIBI, Monitoring.Basin == "Upper North Creek"), aes(Site, BIBI)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,100) + geom_hline(yintercept = 20, color="red", size=1) + geom_hline(yintercept = 40, color="Yellow", size=1) + geom_hline(yintercept = 60, color= "palegreen1", size=1) +  geom_hline(yintercept = 80, color= "palegreen4", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL)  +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

tiff("BIBI_By_Basin.tiff", units="in", width=13, height=6, res=500)
grid.arrange(BIBI_HC, BIBI_LNC, BIBI_UNC, BIBI_LSR, BIBI_PA, nrow = 1)
dev.off()



#BIBI by Basin
BIBI_HC <- ggplot(subset(AllCountsAmbientSubBIBI, Monitoring.Basin == "Horse Creek"), aes(Monitoring.Basin, BIBI)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "red4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,100) + geom_hline(yintercept = 20, color="red", size=1) + geom_hline(yintercept = 40, color="Yellow", size=1) + geom_hline(yintercept = 60, color= "palegreen1", size=1) +  geom_hline(yintercept = 80, color= "palegreen4", size=1) +
  labs(title = NULL, subtitle=NULL, y="BIBI", x=NULL) +  
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_text(size = 12), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

BIBI_LS <- ggplot(subset(AllCountsAmbientSubBIBI, Monitoring.Basin == "Little Swamp Creek"), aes(Monitoring.Basin, BIBI)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "purple4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,3100) + geom_hline(yintercept = 20, color="red", size=1) + geom_hline(yintercept = 40, color="Yellow", size=1) + geom_hline(yintercept = 60, color= "palegreen1", size=1) +  geom_hline(yintercept = 80, color= "palegreen4", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

BIBI_LNC <- ggplot(subset(AllCountsAmbientSubBIBI, Monitoring.Basin == "Lower North Creek"), aes(Monitoring.Basin, BIBI)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "springgreen4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,100) + geom_hline(yintercept = 20, color="red", size=1) + geom_hline(yintercept = 40, color="Yellow", size=1) + geom_hline(yintercept = 60, color= "palegreen1", size=1) +  geom_hline(yintercept = 80, color= "palegreen4", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), element_blank(),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


BIBI_LSR <- ggplot(subset(AllCountsAmbientSubBIBI, Monitoring.Basin == "Lower Sammamish River"), aes(Monitoring.Basin, BIBI)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "slategray4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,100) + geom_hline(yintercept = 20, color="red", size=1) + geom_hline(yintercept = 40, color="Yellow", size=1) + geom_hline(yintercept = 60, color= "palegreen1", size=1) +  geom_hline(yintercept = 80, color= "palegreen4", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


BIBI_PC <- ggplot(subset(AllCountsAmbientSubBIBI, Monitoring.Basin == "Perry Creek"), aes(Monitoring.Basin, BIBI)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,100) + geom_hline(yintercept = 20, color="red", size=1) + geom_hline(yintercept = 40, color="Yellow", size=1) + geom_hline(yintercept = 60, color= "palegreen1", size=1) +  geom_hline(yintercept = 80, color= "palegreen4", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

BIBI_PA <- ggplot(subset(AllCountsAmbientSubBIBI, Monitoring.Basin == "Parr Creek"), aes(Monitoring.Basin, BIBI)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "turquoise4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,100) + geom_hline(yintercept = 20, color="red", size=1) + geom_hline(yintercept = 40, color="Yellow", size=1) + geom_hline(yintercept = 60, color= "palegreen1", size=1) +  geom_hline(yintercept = 80, color= "palegreen4", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


BIBI_UNC <- ggplot(subset(AllCountsAmbientSubBIBI, Monitoring.Basin == "Upper North Creek"), aes(Monitoring.Basin, BIBI)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(fill = "salmon4", width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,100) + geom_hline(yintercept = 20, color="red", size=1) + geom_hline(yintercept = 40, color="Yellow", size=1) + geom_hline(yintercept = 60, color= "palegreen1", size=1) +  geom_hline(yintercept = 80, color= "palegreen4", size=1) +
  labs(title = NULL, subtitle=NULL, y=NULL, x=NULL)  +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_blank(),
                     axis.text.y = element_blank(), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

tiff("BIBI_By_Basin_No_Site.tiff", units="in", width=13, height=6, res=500)
grid.arrange(BIBI_HC, BIBI_LNC, BIBI_UNC, BIBI_LSR, BIBI_PA, nrow = 1)
dev.off()

BIBI_Table_Basin <- summaryStats(BIBI ~ Monitoring.Basin, data = AllCountsAmbient, digits=3, p.value=FALSE, stats.in.rows=TRUE,
                               test.arg.list=list(var.equal = FALSE, test="nonparametric"))

BIBI_Table_Basin

write.table(BIBI_Table_Basin, file = "BIBI_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)


#Sediment
Turb_Table_Basin <- summaryStats(Turbidity ~ Monitoring.Basin, data = AllCountsAmbient, digits=3, p.value=FALSE, stats.in.rows=TRUE,
                                 test.arg.list=list(var.equal = FALSE, test="nonparametric"))

Turb_Table_Basin

write.table(Turb_Table_Basin, file = "Turb_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)

TSS_Table_Basin <- summaryStats(TSS ~ Monitoring.Basin, data = AllCountsAmbient, digits=3, p.value=FALSE, stats.in.rows=TRUE,
                                 test.arg.list=list(var.equal = FALSE, test="nonparametric"))

TSS_Table_Basin

write.table(TSS_Table_Basin, file = "TSS_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)

#metals
Zn_Table_Basin <- summaryStats(Zn ~ Monitoring.Basin, data = AllCountsAmbient, digits=3, p.value=FALSE, stats.in.rows=TRUE,
                                test.arg.list=list(var.equal = FALSE, test="nonparametric"))

Zn_Table_Basin

write.table(Zn_Table_Basin, file = "Zn_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)

Cu_Table_Basin <- summaryStats(Cu ~ Monitoring.Basin, data = AllCountsAmbient, digits=3, p.value=FALSE, stats.in.rows=TRUE,
                                test.arg.list=list(var.equal = FALSE, test="nonparametric"))

Cu_Table_Basin

write.table(Cu_Table_Basin, file = "Cu_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)

Pb_Table_Basin <- summaryStats(Pb ~ Monitoring.Basin, data = AllCountsAmbient, digits=3, p.value=FALSE, stats.in.rows=TRUE,
                                test.arg.list=list(var.equal = FALSE, test="nonparametric"))

Pb_Table_Basin

write.table(Pb_Table_Basin, file = "Pb_Table_Basin.txt", sep = ",", quote = FALSE, row.names = TRUE)


#LNC BIBI
tiff("BIBI_LNC.tiff", units="in", width=15, height=3.5, res=500)
ggplot(subset(AllCountsAmbientSubBIBI, Monitoring.Basin == "Lower North Creek"), aes(Site, BIBI)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(0,100) + geom_hline(yintercept = 20, color="red", size=1) + geom_hline(yintercept = 40, color="Yellow", size=1) + geom_hline(yintercept = 60, color= "palegreen1", size=1) +  geom_hline(yintercept = 80, color= "palegreen4", size=1) +
  labs(title = NULL, subtitle=NULL, y="BIBI", x=NULL) +  
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

dev.off()

#LNC TEMP/DO
tiff("DO_LNC.tiff", units="in", width=15, height=3.5, res=500)
ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Lower North Creek"), aes(Site, DO)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(3,12) +
  labs(title = NULL, subtitle=NULL, y="Dissolved Oxygen (mg/L)", x=NULL) + geom_hline(yintercept = 9.5, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 12), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")
dev.off()

tiff("Temp_LNC.tiff", units="in", width=15, height=3.5, res=500)
ggplot(subset(AllCountsAmbientSummer, Monitoring.Basin == "Lower North Creek"), aes(Site, TEMP)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + ylim(10,18) +
  labs(title = NULL, subtitle=NULL, y="Temperature (Degrees Celsius)", x=NULL) + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 12), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")
dev.off()
#LNC Turbidity
tiff("Turb_LNC.tiff", units="in", width=15, height=3.5, res=500)
ggplot(subset(AllCountsAmbient, Monitoring.Basin == "Lower North Creek"), aes(Site, Turbidity)) + 
  facet_wrap(~Monitoring.Basin, ncol = 1) + geom_boxplot(width = 0.4, color = "darkcyan", alpha = 0.7)  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) +ylim(0,10) +
  labs(title = NULL, subtitle=NULL, y="Turbidity (NTU)", x=NULL) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=12), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 12), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")
dev.off()
