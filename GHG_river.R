#### import libraries ####
# data wrangling

library(tidyverse)
library(reshape)
library(data.table)
library(xlsx)
library(gridExtra)
library(grid)
library(chron)
library(devtools)
library(SOfun)
library(usethis)
library(reprex)

# data visualization 

library(GGally)
library(RColorBrewer)
library(proj4)
library(leaflet)
library(leaflet.minicharts)
library(RColorBrewer)
library(mapview)
library(htmlwidgets)
library(corrplot)
library(mice)
library(VIM)
library(ggmosaic)
library(esquisse)
library(scales) # Make color gradient scales


# data analysis - machine learning - statistics

library(Rcpp)
library(vegan)
library(cluster)
library(MuMIn) # R2c and R2m
library(nlme)
library(gamm4)
library(plotrix)
library(lme4)
library(dunn.test)  # Kruskal Wallis test
library(car)
library(psych)
library(psycho)
library(remotes)
library(mlr)
library(randomForest)
library(Metrics)
library(Hmisc)
library(xgboost)
library(checkmate)
library(ranger)


#### import data #### 
river2 <- read.csv("20190516_River2.csv", header = TRUE)

river <- read.csv("20190516_River.csv", header = TRUE)
river_dis <- read.csv("20190516_river_dissolved.csv", header = TRUE)


#### processing data 4C's ####
# Converting

river$Shading <- as.factor(river$Shading)
river$Pool_class <- as.factor(river$Pool_class)

# Creating

proj4string <- "+proj=utm +zone=17S +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
pj <- project(river[, 44:45], proj4string, inverse = TRUE)
river$latitude <- pj$y
river$longitude <- pj$x

# Completing/imputing missing data by using mean

river <- river %>% select(-c("X18O2", "Note", "DO_sat"))

# Some visualization for missing values

md.pattern(river)
mice_plot <- aggr(river, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(river), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))
marginplot(river[c(5,9)])

# Some test with mice but doesn't work
# set.seed(1234)
# imputed_Data <- mice(river2, m=5, maxit = 5, method = 'polyreg')

# just replace with mean 

for(i in 1:ncol(river)){
    river[is.na(river[,i]), i] <- mean(river[,i], na.rm = TRUE)
}

#### Correcting dissolved gas concentrations ####

V.headspace = 6 # mL 
V.aq        = 6 # mL

R        = 1.98719   # gas constant in cal / K mol
T        = river$T_w        # assumed river temperature 
pressure = 2 # atm 

# Henry coefficients for N2O

A.n2o    = -180.95   
B.n2o    =  13205.8     
C.n2o    =  20.0399   
D.n2o    =  0.0238544


# HENRY's LAW
# H = 1/exp[{-a+b/(T+273)+C*ln(T+273)+d*(T+273)}]/R	
H.n2o    = 1/exp((A.n2o + B.n2o / (T + 273.15) + C.n2o * log(T + 273.15) + D.n2o * (T + 273.15)) / R)	

Cg.n2o   = river$Dis_N2O_1 * 10^-6 * pressure  # vol atm N2O / vol sample; Cg


Ca.n2o   = 55.5 * (Cg.n2o/H.n2o) * 44 * 10^3  # mg N2O/L H2O
Cah.n2o  = (V.headspace/V.aq * Cg.n2o * (44/22.4) * (273.15/(T + 273.15))*10^3) # mg N2O/L H2O

n2o.aq   = (Ca.n2o + Cah.n2o) * 10^3 # ug N2O/L H2O
n2o.aq

n2o.ppm = n2o.aq*24.45/(44.013)
n2o.ppm

river$Dis_N2O_cor <- n2o.aq

# Henry coefficients for ch4

A.ch4    = -365.183    
B.ch4    =  18106.7      
C.ch4    =  49.7554    
D.ch4    = -0.00028503 


# HENRY's LAW
# H = 1/exp[{-a+b/(T+273)+C*ln(T+273)+d*(T+273)}]/R	
H.ch4    = 1/exp((A.ch4 + B.ch4 / (T + 273.15) + C.ch4 * log(T + 273.15) + D.ch4 * (T + 273.15)) / R)	
Cg.ch4   = river$Dis_CH4_1 * 10^-6  * pressure # vol atm ch4 / vol sample; Cg



Ca.ch4   = 55.5 * (Cg.ch4/H.ch4) * 16 * 10^3  # mg ch4/L H2O
Cah.ch4  = (V.headspace/V.aq * Cg.ch4 * (16/22.4) * (273.15/(T + 273.15))*10^3) # mg ch4/L H2O

ch4.aq   = (Ca.ch4 + Cah.ch4)  * 10^3 # ug ch4/L H2O
ch4.aq

ch4.ppm = ch4.aq*24.45/(16.04)
ch4.ppm

river$Dis_CH4_cor <- ch4.aq

# Henry coefficients for co2

A.co2    = -317.658    
B.co2    =  17371.2     
C.co2    =  43.0607    
D.co2    = -0.000219107 

# HENRY's LAW
# H = 1/exp[{-a+b/(T+273)+C*ln(T+273)+d*(T+273)}]/R 
H.co2    = 1/exp((A.co2 + B.co2 / (T + 273.15) + C.co2 * log(T + 273.15) + D.co2 * (T + 273.15)) / R)   
Cg.co2   = river$Dis_CO2_1 * 10^-6 * pressure   # vol atm co2 / vol sample; Cg


Ca.co2   = 55.5 * (Cg.co2/H.co2) * 44 * 10^3  # mg co2/L H2O
Cah.co2  = (V.headspace/V.aq * Cg.co2 * (44/22.4) * (273.15/(T + 273.15))*10^3) # mg co2/L H2O

co2.aq   = (Ca.co2 + Cah.co2)*10^3  # ug co2/L H2O
co2.aq

co2.ppm    = 581.68 * co2.aq/1000 - 5.2707; co2.ppm # FINAL dissolved CO2 concentration in ppm.   
co2.ppm


river$Dis_CO2_cor <- co2.ppm

# river <- river %>% select(-c(38:43))
write_csv(river, "river_DC.csv")
#### Boxplot of all countinuous variables regarding rivers #### 

# This  is for make a list of graphs

plot_river <- function(data, column, column2){
    ggplot(data) +
        geom_boxplot(aes_string(x = column2 , y = column)) +
        xlab(column2) +
        ylab(column) 
}

plot_river_1 <- lapply(colnames(river)[6:14], plot_river, data = river, column2 = "River")
plot_river_2 <- lapply(colnames(river)[27:39], plot_river, data = river, column2 = "River")

# print the graphs

lapply(plot_river_1, print)
lapply(plot_river_2, print)

# save the graphs into folder

for (i in 1:length(plot_river_1)){
    tiff(filename = paste("Boxplot_river_",colnames(river)[6:14][i],".tiff", sep =""),units = 'px',height = 1800,width = 1800,res = 300,pointsize = 12)
    print(plot_river_1[[i]])
    dev.off()
}

for (i in 1:length(plot_river_2)){
    tiff(filename = paste("Boxplot_river_",colnames(river)[27:39][i],".tiff", sep =""),units = 'px',height = 1800,width = 1800,res = 300,pointsize = 12)
    print(plot_river_1[[i]])
    dev.off()
}

# Put graphs with similar variables together

ggsave("Boxplot_river_1.tiff",marrangeGrob(plot_river_1[1:4],nrow = 2, ncol= 2, top = ''),
       units = 'cm', height = 20, width = 20, dpi = 300)

ggsave("Boxplot_river_2.tiff",marrangeGrob(plot_river_1[5:9],nrow = 2, ncol= 3, top = ''),
       units = 'cm', height = 20, width = 30, dpi = 300)

ggsave("Boxplot_river_3.tiff",marrangeGrob(plot_river_2[1:3],nrow = 1, ncol= 3, top = ''),
       units = 'cm', height = 15, width = 30, dpi = 300)

ggsave("Boxplot_river_4.tiff",marrangeGrob(plot_river_2[4:9],nrow = 2, ncol= 3, top = ''),
       units = 'cm', height = 15, width = 30, dpi = 300)

ggsave("Boxplot_river_5.tiff",marrangeGrob(plot_river_2[10:13],nrow = 2, ncol= 2, top = ''),
       units = 'cm', height = 20, width = 20, dpi = 300)


#### Bar charts for categorical variables ####

bar_river <- function(data, column){
    ggplot(data) +
        geom_bar(aes_string(x = column)) +
        xlab(column) +
        facet_wrap(.~ River) 
}

bar_river_1 <- c(lapply(colnames(river)[15:19], bar_river, data = river),
                 lapply(colnames(river)[26], bar_river, data = river))

lapply(bar_river_1, print)

# save the graphs into folder

for (i in 3:5){
    tiff(filename = paste("Barchart_river_",colnames(river)[15:19][i],".tiff", sep =""),units = 'cm', height = 20, width = 20,res = 300,pointsize = 12)
    print(bar_river_1[[i]])
    dev.off()
}
ggsave("Barchart_river_Pool_class.tiff", bar_river_1[[6]],units = 'cm', height = 20, width = 20, dpi = 300)
ggsave("Barchart_river_LB.tiff", bar_river_1[[1]],units = 'cm', height = 20, width = 40, dpi = 300)
ggsave("Barchart_river_RB.tiff", bar_river_1[[2]],units = 'cm', height = 20, width = 40, dpi = 300)

#### Leaflet map for all sampling points in the river ####

colors <- brewer.pal(n = 7, name = "Dark2")
tilesURL <- '//{s}.tile.openstreetmap.se/hydda/full/{z}/{x}/{y}.png'
FUN_river <- function(x,x2){
    for (i in 1:length(colnames(x))){
        y <- colnames(x)[i]
        if(sum(is.na(x[,y])) > 0){ 
            z <- x2[complete.cases(x[,y]),y]
            z2 <- x2[complete.cases(x[,y]),]
        } else {
            z <- x2[,y]
            z2 <- x2
        }
        m <- leaflet(width = "100%", height = "1000px") %>%
            addTiles(tilesURL) %>%
            fitBounds(lng1 = -79.10, lat1 =  -2.83, lng2 = -78.85, lat2 = -2.963) %>%
            addMinicharts(z2$longitude, z2$latitude,
                          type = "pie",
                          chartdata = z,
                          colorPalette = colors[i],
                          opacity = 0.6,
                          showLabels = TRUE,
                          width = 60* sqrt(z)/sqrt(max(z)),
                          transitionTime = 0)
        mapshot(m, file = paste("river_",colnames(x)[i], '.png', sep = ""))
        saveWidget(m, file= paste("river_",colnames(x)[i], '.html', sep = ""))
    }
}
# Chemical components
FUN_river(river[,6:13], river)

# Pollutants
FUN_river(river[,26:33], river)

# Dissolved gas
FUN_river(river[,40:42], river)

# Fluxes
river <- river %>% arrange(No)
river2 <- bind_cols(river, river_flux_GWP[,43:45])
FUN_river(river2[,43:45], river2)

# Dissolved gases
colnames(river)[40:42] <- c("Dissovled N2O", "Dissovled CH4","Dissolved CO2")

m <- leaflet(width = "100%", height = "1000px") %>%
    addTiles(tilesURL) %>%
    fitBounds(lng1 = -79.10, lat1 =  -2.83, lng2 = -78.85, lat2 = -2.963) %>%
    addMinicharts(river$longitude, river$latitude,
                  # type = "pie",
                  chartdata = river[,40:42],
                  colorPalette = colors[1:3],
                  opacity = 0.6,
                  showLabels = TRUE,
                  width = 400* sqrt(river[,40:42])/sqrt(max(river[,40:42])),
                  transitionTime = 0)
# mapshot(m, "river_dissolved_gases.png")
saveWidget(m, "river_dissolved_gases.html")
# Dissolved gases in GWP 

river4 <- as.data.frame(river[,40:42])

river4$`Dissovled N2O` <- river4$`Dissovled N2O`*265
river4$`Dissovled CH4` <- river4$`Dissovled CH4`*28

m <- leaflet(width = "100%", height = "1000px") %>%
    addTiles(tilesURL) %>%
    fitBounds(lng1 = -79.10, lat1 =  -2.83, lng2 = -78.85, lat2 = -2.963) %>%
    addMinicharts(river$longitude, river$latitude,
                  # type = "pie",
                  chartdata = river4,
                  colorPalette = colors[1:3],
                  opacity = 0.6,
                  showLabels = TRUE,
                  width = 100* sqrt(river4)/sqrt(max(river4)),
                  transitionTime = 0)
# mapshot(m, "river_dissolved_gases.png")
saveWidget(m, "river_GWP.html")

#### Leaflet map for sampling points in each tributary #####
river_CU <- river %>% filter(River == "Cuenca")
river_TA <- river %>% filter(River == "Tarqui")
river_MA <- river %>% filter(River == "Machangara")
river_TO <- river %>% filter(River == "Tomebamba")
river_YA <- river %>% filter(River == "Yanuncay")
#*** Cuenca tributary #### 
FUN_river_CU <- function(x,x2){
    for (i in 1:length(colnames(x))){
        y <- colnames(x)[i]
        if(sum(is.na(x[,y])) > 0){ 
            z <- x2[complete.cases(x[,y]),y]
            z2 <- x2[complete.cases(x[,y]),]
        } else {
            z <- x2[,y]
            z2 <- x2
        }
        m <- leaflet(width = "100%", height = "1000px") %>%
            addTiles(tilesURL) %>%
            fitBounds(lng1 = -78.955479, lat1 =  -2.841429, lng2 = -78.857288, lat2 = -2.891148) %>%
            addMinicharts(z2$longitude, z2$latitude,
                          type = "pie",
                          chartdata = z,
                          colorPalette = colors[i],
                          opacity = 0.6,
                          showLabels = TRUE,
                          width = 60* sqrt(z)/sqrt(max(z)),
                          transitionTime = 0)
        mapshot(m, file = paste("river_CU_",colnames(x)[i], '.png', sep = ""))
        saveWidget(m, file= paste("river_CU_",colnames(x)[i], '.html', sep = ""))
    }
}
# Chemical components
FUN_river_CU(river_CU[,6:13], river_CU)

# Pollutants
FUN_river_CU(river_CU[,26:33], river_CU)

# Dissolved gas
FUN_river_CU(river_CU[,40:42], river_CU)

#*** Machangara tributary #### 
FUN_river_MA <- function(x,x2){
    for (i in 1:length(colnames(x))){
        y <- colnames(x)[i]
        if(sum(is.na(x[,y])) > 0){ 
            z <- x2[complete.cases(x[,y]),y]
            z2 <- x2[complete.cases(x[,y]),]
        } else {
            z <- x2[,y]
            z2 <- x2
        }
        m <- leaflet(width = "100%", height = "1000px") %>%
            addTiles(tilesURL) %>%
            fitBounds(lng1 = -79.001570, lat1 =  -2.844231, lng2 = -78.945265, lat2 = -2.898579) %>%
            addMinicharts(z2$longitude, z2$latitude,
                          type = "pie",
                          chartdata = z,
                          colorPalette = colors[2],
                          opacity = 0.6,
                          showLabels = TRUE,
                          width = 60* sqrt(z)/sqrt(max(z)),
                          transitionTime = 0)
        mapshot(m, file = paste("river_MA_",colnames(x)[i], '.png', sep = ""))
        saveWidget(m, file= paste("river_MA_",colnames(x)[i], '.html', sep = ""))
    }
}
# Chemical components
FUN_river_MA(river_MA[,6:13], river_MA)

# Pollutants
FUN_river_MA(river_MA[,26:33], river_MA)

# Dissolved gas
FUN_river_MA(river_MA[,40:42], river_MA)

#*** Tarqui tributary #### 
FUN_river_TA <- function(x,x2){
    for (i in 1:length(colnames(x))){
        y <- colnames(x)[i]
        if(sum(is.na(x[,y])) > 0){ 
            z <- x2[complete.cases(x[,y]),y]
            z2 <- x2[complete.cases(x[,y]),]
        } else {
            z <- x2[,y]
            z2 <- x2
        }
        m <- leaflet(width = "100%", height = "1000px") %>%
            addTiles(tilesURL) %>%
            fitBounds(lng1 = -79.079762, lat1 =  -2.910350, lng2 = -78.994617, lat2 = -3.005666) %>%
            addMinicharts(z2$longitude, z2$latitude,
                          type = "pie",
                          chartdata = z,
                          colorPalette = colors[2],
                          opacity = 0.6,
                          showLabels = TRUE,
                          width = 60* sqrt(z)/sqrt(max(z)),
                          transitionTime = 0)
        mapshot(m, file = paste("river_TA_",colnames(x)[i], '.png', sep = ""))
        saveWidget(m, file= paste("river_TA_",colnames(x)[i], '.html', sep = ""))
    }
}
# Chemical components
FUN_river_TA(river_TA[,6:13], river_TA)

# Pollutants
FUN_river_TA(river_TA[,26:33], river_TA)

# Dissolved gas
FUN_river_TA(river_TA[,40:42], river_TA)
#*** Tomebamba tributary #### 
FUN_river_TO <- function(x,x2){
    for (i in 1:length(colnames(x))){
        y <- colnames(x)[i]
        if(sum(is.na(x[,y])) > 0){ 
            z <- x2[complete.cases(x[,y]),y]
            z2 <- x2[complete.cases(x[,y]),]
        } else {
            z <- x2[,y]
            z2 <- x2
        }
        m <- leaflet(width = "100%", height = "1000px") %>%
            addTiles(tilesURL) %>%
            fitBounds(lng1 = -79.079418, lat1 =  -2.866853, lng2 = -78.948441, lat2 = -2.929532) %>%
            addMinicharts(z2$longitude, z2$latitude,
                          type = "pie",
                          chartdata = z,
                          colorPalette = colors,
                          opacity = 0.6,
                          showLabels = TRUE,
                          width = 60* sqrt(z)/sqrt(max(z)),
                          transitionTime = 0)
        mapshot(m, file = paste("river_TO_",colnames(x)[i], '.png', sep = ""))
        saveWidget(m, file= paste("river_TO_",colnames(x)[i], '.html', sep = ""))
    }
}
# Chemical components
FUN_river_TO(river_TO[,6:13], river_TO)

# Pollutants
FUN_river_TO(river_TO[,26:33], river_TO)

# Dissolved gas
FUN_river_TO(river_TO[,40:42], river_TO)
#*** Yanuncay tributary #### 
FUN_river_YA <- function(x,x2){
    for (i in 1:length(colnames(x))){
        y <- colnames(x)[i]
        if(sum(is.na(x[,y])) > 0){ 
            z <- x2[complete.cases(x[,y]),y]
            z2 <- x2[complete.cases(x[,y]),]
        } else {
            z <- x2[,y]
            z2 <- x2
        }
        m <- leaflet(width = "100%", height = "1000px") %>%
            addTiles(tilesURL) %>%
            fitBounds(lng1 = -79.234600, lat1 =  -2.881891, lng2 = -78.975391, lat2 = -2.966581) %>%
            addMinicharts(z2$longitude, z2$latitude,
                          type = "pie",
                          chartdata = z,
                          colorPalette = colors,
                          opacity = 0.6,
                          showLabels = TRUE,
                          width = 60* sqrt(z)/sqrt(max(z)),
                          transitionTime = 0)
        mapshot(m, file = paste("river_YA_",colnames(x)[i], '.png', sep = ""))
        saveWidget(m, file= paste("river_YA_",colnames(x)[i], '.html', sep = ""))
    }
}
# Chemical components
FUN_river_YA(river_YA[,6:13], river_YA)

# Pollutants
FUN_river_YA(river_YA[,26:33], river_YA)

# Dissolved gas
FUN_river_YA(river_YA[,40:42], river_YA)



#### Correlation coefficients #### 
variable_river <- cbind(river_sta[6:13], river_sta[26:35], river_sta$Solar)

corr_river <- cor(variable_river, use = 'pairwise')
p.mat <- cor.mtest(variable_river)$p
colnames(p.mat) <- colnames(corr_river)
row.names(p.mat) <- colnames(corr_river)

# GGally Not really nice
ggsave("Corr_coeff.tiff", ggpairs(variable_river,
                                  lower = list(continuous = wrap("smooth", color = "deepskyblue")),
                                  upper = list(continuous = wrap("cor", size = 3, color = "tomato"))
) + theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()),
units = 'cm', height = 50, width = 50, dpi = 300)


# corrplot nicer but cannot handle the categorical variables

tiff("corr_coeff_2.tiff",units = 'cm',height = 50,width = 50,res = 300, pointsize = 12)
corrplot(corr_river, p.mat = p.mat, method = "circle", type = "upper",
         sig.level = 0.05, insig = "blank", order = "alphabet")
dev.off()

# Mosaic plots ####
# Using mosaic plot to represent the relationship among two or more categorical variables 
# in this case, only for river and hydromorphological data

ggsave("Mosaic_river_LB.tiff", ggplot(river)+
           geom_mosaic(aes(x= product(River), fill = LB))+
           labs(x ="", y = "")+
           theme_bw()+
           theme(axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 text=element_text(size=13),
                 strip.text.x = element_text(size=13)),
       units = 'cm', height = 15, width = 20, dpi = 300)
ggsave("Mosaic_river_RB.tiff", ggplot(river)+
           geom_mosaic(aes(x= product(River), fill = RB))+
           labs(x ="", y = "")+
           theme_bw()+
           theme(axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 text=element_text(size=13),
                 strip.text.x = element_text(size=13)),
       units = 'cm', height = 15, width = 20, dpi = 300)

# no separation between left and right banks 

old_lu <- levels(as.factor(c(river$LB, river$RB)))
new_lu <- c("Agriculture", "Urban", "Industry", "Nature", "Industry", "Agriculture", "Urban",
            "Nature", "Road", "Urban")
new_landuse <- bind_cols(as.data.frame(old_lu), as.data.frame(new_lu))
new_landuse$LB <- as.character(new_landuse$LB)
new_landuse$`Left bank` <- as.character(new_landuse$`Left bank`)

colnames(new_landuse) <- c("LB", "Left bank")
river <- left_join(river, new_landuse, by = "LB")

colnames(new_landuse) <- c("RB", "Right bank")
river <- left_join(river, new_landuse, by = "RB")



ggsave("Mosaic_river_land_use.jpg", river %>% select(`Left bank`,`Right bank`, River) %>% pivot_longer(cols = -River, names_to = "Bank", values_to = "Land use") %>% 
    ggplot()+
    geom_mosaic(aes(x= product(River), fill = `Land use`))+
    labs(x ="", y = "")+
    theme_bw()+
    theme(axis.text.x = element_text(size=13),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
          text=element_text(size=13),
          strip.text.x = element_text(size=13)),
    units = 'cm', height = 15, width = 20, dpi = 300)


# Other mosaic plots

ggsave("Mosaic_river_FV.tiff",ggplot(river)+
           geom_mosaic(aes(x= product(River), fill = Flow_variation))+
           labs(x ="", y = "")+
           theme_bw()+
           theme(axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 text=element_text(size=13),
                 strip.text.x = element_text(size=13)),
       units = 'cm', height = 15, width = 20, dpi = 300)

ggsave("Mosaic_river_shading.tiff", ggplot(river)+
           geom_mosaic(aes(x= product(River), fill = Shading))+
           labs(x ="", y = "")+
           theme_bw()+
           theme(axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 text=element_text(size=13),
                 strip.text.x = element_text(size=13)),
       units = 'cm', height = 15, width = 20, dpi = 300)
ggsave("Mosaic_river_erosion.tiff",ggplot(river)+
           geom_mosaic(aes(x= product(River), fill = Erosion))+
           labs(x ="", y = "")+
           theme_bw()+
           theme(axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 text=element_text(size=13),
                 strip.text.x = element_text(size=13)),
       units = 'cm', height = 15, width = 20, dpi = 300)
ggsave("Mosaic_river_pool.tiff",ggplot(river)+
           geom_mosaic(aes(x= product(River), fill = Pool_class))+
           labs(x ="", y = "")+
           theme_bw()+
           theme(axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 text=element_text(size=13),
                 strip.text.x = element_text(size=13)),
       units = 'cm', height = 15, width = 20, dpi = 300)
#### Correct_GWP per river ####

river_flux_GWP <- read_csv("River2.csv")

river_flux_GWP$Date <- as.factor(river_flux_GWP$Date)
river_flux_GWP$Location <- as.factor(river_flux_GWP$Location)
river_flux_GWP$River <- as.factor(river_flux_GWP$River)
river_flux_GWP$LB <- as.factor(river_flux_GWP$LB)
river_flux_GWP$RB <- as.factor(river_flux_GWP$RB)
river_flux_GWP$Shading <- as.factor(river_flux_GWP$Shading)
river_flux_GWP$Erosion <- as.factor(river_flux_GWP$Erosion)
river_flux_GWP$Flow_variation <- as.factor(river_flux_GWP$Flow_variation)

# change the colnames

colnames(river_flux_GWP)[6:41] <- c("Water temperature", "pH", "DO", "EC", "TDS", "Salinity", "Turbility", "Chlorophyll", "Left Bank", 
                                    "Right Bank", "Shading", "Erosion", "Flow variability", "Average depth", "Average velocity",
                                    "Pool Class", "BOD", "COD", "TN" ,"NH4", "NO2", "NO3", "TP", "PO4", "Air temperature", "Wind velocity",
                                    "Rain", "Solar radiation", "Latitude", "Longitude", "Dissolved N2O", "Dissolved CH4", "Dissolved CO2",
                                    "Flux CO2_umol", "Flux CH4_umol", "Flux N2O_umol")

river_flux_GWP$`Flux CO2` <- river_flux_GWP$`Flux CO2_umol`*(12+16*2)/1000
river_flux_GWP$`Flux CH4` <- river_flux_GWP$`Flux CH4_umol`*(12+4)*28/1000
river_flux_GWP$`Flux N2O` <- river_flux_GWP$`Flux N2O_umol`*(14*2+16)*265/1000

river_flux_GWP2 <- river_flux_GWP %>% select(c(3:5,43:45))

river_flux_GWP3 <- river_flux_GWP2 %>% gather(key = "Flux_gases", value = "GWP", - Time, - Location, -River)

river_flux_GWP3$Flux_gases <- as.factor(river_flux_GWP3$Flux_gases)

river_flux_GWP3$Flux_gases <- relevel(river_flux_GWP3$Flux_gases,"Flux CO2")
river_flux_GWP3$Flux_gases <- factor(river_flux_GWP3$Flux_gases, 
                                     labels = c(expression("CO"["2"]), expression("CH"["4"]), 
                                                expression("N"["2"]*"O")))


# CO2 equivalent 

ggsave("Fluxes_GWP_Co2_equi.tiff", river_flux_GWP3 %>% ggplot(aes(x = River, y = GWP, fill = River)) +
           geom_boxplot() +
           stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
           theme_bw()+
           ylab(bquote("GWP (mg"~CO[2]*"-equivalent "*m^-2*" "*d^-1*")")) +
           facet_wrap(.~ Flux_gases, scales = "free", labeller = label_parsed) +
           scale_fill_brewer(palette = "Paired", name = "Tributaries")+
           theme(text=element_text(size=14),
                 strip.text.x = element_text(size=14),
                 axis.text.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.title.x = element_blank(),
                 legend.position="right",
                 legend.title = element_text(size = 14),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm')), 
       units = 'cm', height = 15, width = 30, dpi = 300
)

# no CO2 equivalent.

river_flux_GWP3$Concentration[river_flux_GWP3$Flux_gases == '"CO"["2"]'] <- river_flux_GWP3$GWP[river_flux_GWP3$Flux_gases == '"CO"["2"]']/1
river_flux_GWP3$Concentration[river_flux_GWP3$Flux_gases == '"CH"["4"]'] <- river_flux_GWP3$GWP[river_flux_GWP3$Flux_gases == '"CH"["4"]']/28
river_flux_GWP3$Concentration[river_flux_GWP3$Flux_gases == '"N"["2"] * "O"'] <- river_flux_GWP3$GWP[river_flux_GWP3$Flux_gases == '"N"["2"] * "O"']/265

river_flux_GWP3$River <- factor(river_flux_GWP3$River,
                                levels = c("Machangara", "Yanuncay", "Cuenca", "Tarqui", "Tomebamba"))

ggsave("Fluxes_GWP_no_Co2_equi.tiff", river_flux_GWP3 %>% 
           ggplot(aes(x = River, y = Concentration, fill = River)) +
           geom_boxplot() +
           stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
           theme_bw()+
           ylab(bquote("Fluxes (mg "*m^-2*" "*d^-1*")")) +
           facet_wrap(.~ Flux_gases, scales = "free", labeller = label_parsed) +
           scale_fill_brewer(palette = "Paired", name = "Tributaries")+
           theme(text=element_text(size=14),
                 strip.text.x = element_text(size=14),
                 axis.text.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.title.x = element_blank(),
                 legend.position="right",
                 legend.title = element_text(size = 14),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm')), 
       units = 'cm', height = 15, width = 30, dpi = 300
)


#### Correct_Mean+SEM of the total GWP ####


river_sem <- merge(aggregate(data = river_flux_GWP3, Concentration ~ River + Flux_gases, FUN =mean), 
                   aggregate(data = river_flux_GWP3, Concentration ~ River + Flux_gases, FUN =std.error), by = c("River", "Flux_gases"))
colnames(river_sem) <- c("Tributaries", "Fluxes", "Mean", "SEM")

river_sem$Mean2[river_sem$Fluxes == '"CO"["2"]'] <- river_sem$Mean[river_sem$Fluxes == '"CO"["2"]']*1
river_sem$Mean2[river_sem$Fluxes == '"CH"["4"]'] <- river_sem$Mean[river_sem$Fluxes == '"CH"["4"]']*28
river_sem$Mean2[river_sem$Fluxes == '"N"["2"] * "O"'] <- river_sem$Mean[river_sem$Fluxes == '"N"["2"] * "O"']*265

river_sem$SEM2[river_sem$Fluxes == '"CO"["2"]'] <- river_sem$SEM[river_sem$Fluxes == '"CO"["2"]']*1
river_sem$SEM2[river_sem$Fluxes == '"CH"["4"]'] <- river_sem$SEM[river_sem$Fluxes == '"CH"["4"]']*28
river_sem$SEM2[river_sem$Fluxes == '"N"["2"] * "O"'] <- river_sem$SEM[river_sem$Fluxes == '"N"["2"] * "O"']*265

ggsave("Total_fluxes_Co2_equi.tiff", river_sem %>% 
           ggplot(aes(x = Tributaries, y = Mean2, fill = Tributaries)) +
           geom_bar(stat = 'identity') +
           geom_errorbar(aes(ymin= Mean2 - SEM2, ymax=Mean2+SEM2), width=.2,
                         position=position_dodge(.9)) +
           theme_bw() +
           ylab("Mean of the fluxes (mg"~CO[2]*"-equivalent "*m^-2*" "*d^-1*")") +
           facet_wrap(.~ Fluxes, scales = "free", labeller = label_parsed) +
           scale_fill_brewer(palette = "Paired")+
           theme(text=element_text(size=14),
                 strip.text.x = element_text(size=14),
                 axis.text.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.title.x = element_blank(),
                 legend.position="right",
                 legend.title = element_text(size = 14),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm')),
       units = 'cm', height = 15, width = 30, dpi = 300
)

river_sem$Tributaries <- factor(river_sem$Tributaries,
                                levels = c("Machangara", "Yanuncay", "Cuenca", "Tarqui", "Tomebamba"))



ggsave("Total_fluxes_no_Co2_equi.tiff", river_sem %>% 
           ggplot(aes(x = Tributaries, y = Mean, fill = Tributaries)) +
           geom_bar(stat = 'identity') +
           geom_errorbar(aes(ymin= Mean - SEM, ymax=Mean+SEM), width=.2,
                         position=position_dodge(.9)) +
           theme_bw() +
           ylab(bquote("Mean of the fluxes (mg "*m^-2*" "*d^-1*")")) +
           facet_wrap(.~ Fluxes, scales = "free", labeller = label_parsed) +
           scale_fill_brewer(palette = "Paired")+
           theme(text=element_text(size=14),
                 strip.text.x = element_text(size=14),
                 axis.text.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.title.x = element_blank(),
                 legend.position="right",
                 legend.title = element_text(size = 14),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm')),
       units = 'cm', height = 15, width = 30, dpi = 300
)
# change unit to g CO2 equivalent.yr-1 

river_sem$Mean3[river_sem$Tributaries == 'Cuenca'] <- river_sem$Mean2[river_sem$Tributaries == 'Cuenca']*365/1000000*95.92
river_sem$Mean3[river_sem$Tributaries == 'Machangara'] <- river_sem$Mean2[river_sem$Tributaries == 'Machangara']*365/1000000*111.19
river_sem$Mean3[river_sem$Tributaries == 'Tarqui'] <- river_sem$Mean2[river_sem$Tributaries == 'Tarqui']*365/1000000*138.98
river_sem$Mean3[river_sem$Tributaries == 'Tomebamba'] <- river_sem$Mean2[river_sem$Tributaries == 'Tomebamba']*365/1000000*113.03
river_sem$Mean3[river_sem$Tributaries == 'Yanuncay'] <- river_sem$Mean2[river_sem$Tributaries == 'Yanuncay']*365/1000000*113.81


river_sem$SEM3[river_sem$Tributaries == 'Cuenca'] <- river_sem$SEM2[river_sem$Tributaries == 'Cuenca']*365/1000000*95.92
river_sem$SEM3[river_sem$Tributaries == 'Machangara'] <- river_sem$SEM2[river_sem$Tributaries == 'Machangara']*365/1000000*111.19
river_sem$SEM3[river_sem$Tributaries == 'Tarqui'] <- river_sem$SEM2[river_sem$Tributaries == 'Tarqui']*365/1000000*138.98
river_sem$SEM3[river_sem$Tributaries == 'Tomebamba'] <- river_sem$SEM2[river_sem$Tributaries == 'Tomebamba']*365/1000000*113.03
river_sem$SEM3[river_sem$Tributaries == 'Yanuncay'] <- river_sem$SEM2[river_sem$Tributaries == 'Yanuncay']*365/1000000*113.81

ggsave("Total_emission_per_year_Co2_equi.tiff", river_sem %>% 
           ggplot(aes(x = Tributaries, y = Mean3, fill = Tributaries)) +
           geom_bar(stat = 'identity') +
           geom_errorbar(aes(ymin= Mean3 - SEM3, ymax=Mean3+SEM3), width=.2,
                         position=position_dodge(.9)) +
           theme_bw() +
           ylab("Mean of the total emission per year (Gg"~CO[2]*"-equivalent "*yr^-1*")") +
           facet_wrap(.~ Fluxes, scales = "free", labeller = label_parsed) +
           scale_fill_brewer(palette = "Paired")+
           theme(text=element_text(size=14),
                 strip.text.x = element_text(size=14),
                 axis.text.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.title.x = element_blank(),
                 legend.position="right",
                 legend.title = element_text(size = 14),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm')),
       units = 'cm', height = 15, width = 30, dpi = 300
)
# no CO2 equivalent

river_sem$Mean4[river_sem$Tributaries == 'Cuenca'] <- river_sem$Mean[river_sem$Tributaries == 'Cuenca']*365/1000000*95.92
river_sem$Mean4[river_sem$Tributaries == 'Machangara'] <- river_sem$Mean[river_sem$Tributaries == 'Machangara']*365/1000000*111.19
river_sem$Mean4[river_sem$Tributaries == 'Tarqui'] <- river_sem$Mean[river_sem$Tributaries == 'Tarqui']*365/1000000*138.98
river_sem$Mean4[river_sem$Tributaries == 'Tomebamba'] <- river_sem$Mean[river_sem$Tributaries == 'Tomebamba']*365/1000000*113.03
river_sem$Mean4[river_sem$Tributaries == 'Yanuncay'] <- river_sem$Mean[river_sem$Tributaries == 'Yanuncay']*365/1000000*113.81


river_sem$SEM4[river_sem$Tributaries == 'Cuenca'] <- river_sem$SEM[river_sem$Tributaries == 'Cuenca']*365/1000000*95.92
river_sem$SEM4[river_sem$Tributaries == 'Machangara'] <- river_sem$SEM[river_sem$Tributaries == 'Machangara']*365/1000000*111.19
river_sem$SEM4[river_sem$Tributaries == 'Tarqui'] <- river_sem$SEM[river_sem$Tributaries == 'Tarqui']*365/1000000*138.98
river_sem$SEM4[river_sem$Tributaries == 'Tomebamba'] <- river_sem$SEM[river_sem$Tributaries == 'Tomebamba']*365/1000000*113.03
river_sem$SEM4[river_sem$Tributaries == 'Yanuncay'] <- river_sem$SEM[river_sem$Tributaries == 'Yanuncay']*365/1000000*113.81

ggsave("Total_emission_per_year_no_Co2_equi.tiff", river_sem %>% 
           ggplot(aes(x = Tributaries, y = Mean4, fill = Tributaries)) +
           geom_bar(stat = 'identity') +
           geom_errorbar(aes(ymin= Mean4 - SEM4, ymax=Mean4+SEM4), width=.2,
                         position=position_dodge(.9)) +
           theme_bw() +
           ylab(bquote("Mean of the total emissions per year (Gg "*yr^-1*")")) +
           facet_wrap(.~ Fluxes, scales = "free", labeller = label_parsed) +
           scale_fill_brewer(palette = "Paired")+
           theme(text=element_text(size=14),
                 strip.text.x = element_text(size=14),
                 axis.text.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.title.x = element_blank(),
                 legend.position="right",
                 legend.title = element_text(size = 14),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm')),
       units = 'cm', height = 15, width = 30, dpi = 300
)


# sum up the perentage --> Not correct as percentage of the mean values not the whole thing

river_sem2 <- river_sem %>% group_by(Fluxes) %>% mutate(Percentage = Mean4*100/ sum(Mean4), Percentage_SEM = SEM4*100/ sum(Mean4))

# sum up the results
# river_sem: Mean: Flux (no CO2), Mean2: Flux (CO2), Mean 3: per year(CO2), Mean 4: per year(no CO2)

river_sem_sum <- merge(aggregate(data = river_flux_GWP3, Concentration ~ Flux_gases, FUN =mean), 
                   aggregate(data = river_flux_GWP3, Concentration ~ Flux_gases, FUN =std.error), by = c("Flux_gases"))
colnames(river_sem_sum) <- c("Fluxes", "Mean", "SEM")

river_sem_sum$Mean2[river_sem_sum$Fluxes == '"CO"["2"]'] <- river_sem_sum$Mean[river_sem_sum$Fluxes == '"CO"["2"]']*1
river_sem_sum$Mean2[river_sem_sum$Fluxes == '"CH"["4"]'] <- river_sem_sum$Mean[river_sem_sum$Fluxes == '"CH"["4"]']*28
river_sem_sum$Mean2[river_sem_sum$Fluxes == '"N"["2"] * "O"'] <- river_sem_sum$Mean[river_sem_sum$Fluxes == '"N"["2"] * "O"']*265

river_sem_sum$SEM2[river_sem_sum$Fluxes == '"CO"["2"]'] <- river_sem_sum$SEM[river_sem_sum$Fluxes == '"CO"["2"]']*1
river_sem_sum$SEM2[river_sem_sum$Fluxes == '"CH"["4"]'] <- river_sem_sum$SEM[river_sem_sum$Fluxes == '"CH"["4"]']*28
river_sem_sum$SEM2[river_sem_sum$Fluxes == '"N"["2"] * "O"'] <- river_sem_sum$SEM[river_sem_sum$Fluxes == '"N"["2"] * "O"']*265

river_sem_sum$Mean3 <- river_sem_sum$Mean2*365*(95.92+111.19+138.98+113.03+113.81)/1000000 # convert to Gg
river_sem_sum$SEM3 <- river_sem_sum$SEM2*365*(95.92+111.19+138.98+113.03+113.81)/1000000 # convert to Gg
river_sem_sum$Mean4 <- river_sem_sum$Mean*365*(95.92+111.19+138.98+113.03+113.81)/1000000 # convert to Gg
river_sem_sum$SEM4 <- river_sem_sum$SEM*365*(95.92+111.19+138.98+113.03+113.81)/1000000 # convert to Gg





#### Correct_box plot WQ ####

river_WQI <- read_csv("River_WQI2.csv")

river_WQI$F_CO2_mg <- river_WQI$F_CO2*(12+16*2)/1000
river_WQI$F_CH4_mg <- river_WQI$F_CH4*(12+4)*28/1000
river_WQI$F_N2O_mg <- river_WQI$F_N2O*(14*2+16)*265/1000

river_WQI_v2 <- river_WQI[, c(5, 44, 47, 51:53)] %>% pivot_longer(c(-River,-Prati_WQI_1, -`OWQI-2`), names_to = "Fluxes", values_to = "Concentration")
river_WQI_v2$Fluxes <- as_factor(river_WQI_v2$Fluxes)
river_WQI_v2 <- river_WQI_v2[,-1]
old_Ore <- c("Good","Fair", "Very Poor", "Poor")
new_Ore <- c("Acceptable Quality", "Polluted", "Very Heavily Polluted", "Heavily Polluted")
for (i in 1:nrow(river_WQI_v2)){
    for (j in 1:length(new_Ore)){
        river_WQI_v2$`OWQI-2`[i] <- str_replace_all(river_WQI_v2$`OWQI-2`[i], old_Ore[j], new_Ore[j])
    }
}


colnames(river_WQI_v2) <- c("Prati Index", "Oregon Index", "Fluxes", "Concentration")

river_WQI_v2 <- river_WQI_v2 %>%
    gather(key = "WQI", value = "Water Quality", - `Fluxes`, - Concentration)

river_WQI_v2$Fluxes <- factor(river_WQI_v2$Fluxes, 
                              labels = c(expression("CO"["2"]), expression("CH"["4"]), 
                                         expression("N"["2"]*"O")))
river_WQI_v2$`Water Quality` <- as.factor(river_WQI_v2$`Water Quality`)
river_WQI_v2$`Water Quality` <- factor(river_WQI_v2$`Water Quality`, 
                                       levels = c("Good Quality", "Acceptable Quality",
                                                  "Polluted", "Heavily Polluted", "Very Heavily Polluted"))


# CO2 equivalent 

ggsave("WQI_final_CO2_equi_11_06.tiff", river_WQI_v2 %>% 
           ggplot(aes(y=Concentration, x=`Water Quality`,
                      fill = `Water Quality`)) +
           geom_boxplot() +
           stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="blue", fill="blue") +
           theme_bw() +
           labs(fill = "Prati Index") +
           ylab("GWP (mg"~CO[2]*"-equivalent "*m^-2*" "*d^-1*")") +
           facet_wrap(WQI~Fluxes, scales = "free"
                      , labeller = labeller(Fluxes = label_parsed)) +
           
           scale_fill_manual(
               values = c("blue","green","yellow", "orange","red"),
               name = "Prati/Oregon Index",
                             labels = c("Good Quality/Excellent", "Acceptable Quality/Good",
                                        "Polluted/Fair", "Heavily Polluted/Poor",  "Very Heavily Polluted/Very Poor")) + 
           theme(text=element_text(size=14),
                 strip.text.x =element_text(size=14),
                 axis.text.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.title.x = element_blank(),
                 legend.position = "right",
                 legend.title = element_text(size = 14),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm')),
       units = 'cm', height = 20, width = 30, dpi = 300
)

# no CO2 equivalent

river_WQI_v2$Concentration2[river_WQI_v2$Fluxes == '"CO"["2"]'] <- river_WQI_v2$Concentration[river_WQI_v2$Fluxes == '"CO"["2"]']/1
river_WQI_v2$Concentration2[river_WQI_v2$Fluxes == '"CH"["4"]'] <- river_WQI_v2$Concentration[river_WQI_v2$Fluxes == '"CH"["4"]']/28
river_WQI_v2$Concentration2[river_WQI_v2$Fluxes == '"N"["2"] * "O"'] <- river_WQI_v2$Concentration[river_WQI_v2$Fluxes == '"N"["2"] * "O"']/265

ggsave("WQI_final_no_CO2_equi.tiff", river_WQI_v2 %>% 
           ggplot(aes(y=Concentration2, x=`Water Quality`,
                      fill = factor(`Water Quality`, 
                                    levels=  c("Good Quality", "Acceptable Quality",
                                               "Polluted", "Heavily Polluted", "Very Heavily Polluted")))) +
           geom_boxplot() +
           stat_summary(fun =mean, geom="point", shape=20, size=5, color="blue", fill="blue") +
           theme_bw() +
           labs(fill = "Prati Index") +
           ylab(bquote("Fluxes (mg "*m^-2*" "*d^-1*")")) +
           facet_wrap(WQI~Fluxes, scales = "free"
                      , labeller = labeller(Fluxes = label_parsed)) +
           scale_fill_manual(values = c("blue","green","yellow","orange","red"), name = "Prati/Oregon Index", 
                             labels = c("Good Quality/Excellent", "Acceptable Quality/Good",
                                        "Polluted/Fair", "Heavily Polluted/Poor", "Very Heavily Polluted/Very Poor")) +
           # scale_x_discrete(labels =c(bquote("CO"[2]), bquote("CH"[4]), bquote("N"[2]*"O")))+
           theme(text=element_text(size=14),
                 strip.text.x =element_text(size=14),
                 axis.text.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.title.x = element_blank(),
                 legend.position = "right",
                 legend.title = element_text(size = 14),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm')),
       units = 'cm', height = 20, width = 30, dpi = 300
)

# sum up WQI

river_WQI_sum_Prati <- river_WQI_v2 %>% filter(WQI == "Prati Index")
river_WQI_sum_Prati <- merge(aggregate(data = river_WQI_sum_Prati, Concentration2 ~ `Water Quality` + Fluxes, FUN =mean), 
                             aggregate(data = river_WQI_sum_Prati, Concentration2 ~ `Water Quality` + Fluxes, FUN =std.error), 
                             by = c("Water Quality", "Fluxes"))

river_WQI_sum_Oregon <- river_WQI_v2 %>% filter(WQI == "Oregon Index")
river_WQI_sum_Oregon$`Water Quality` <- as.character(river_WQI_sum_Oregon$`Water Quality`)
for (i in 1:nrow(river_WQI_sum_Oregon)){
    for (j in 1:length(new_Ore)){
        river_WQI_sum_Oregon$`Water Quality`[i] <- str_replace_all(river_WQI_sum_Oregon$`Water Quality`[i], new_Ore[j], old_Ore[j])
    }
}
river_WQI_sum_Oregon$`Water Quality` <- as.factor(river_WQI_sum_Oregon$`Water Quality`)
river_WQI_sum_Oregon <- merge(aggregate(data = river_WQI_sum_Oregon, Concentration2 ~ `Water Quality` + Fluxes, FUN =mean), 
                             aggregate(data = river_WQI_sum_Oregon, Concentration2 ~ `Water Quality` + Fluxes, FUN =std.error), 
                             by = c("Water Quality", "Fluxes")) # Fair and Good of Oregon contain only one variable --> No SEM

# sum up WQI 11.06.2020 CO2 equivalent

river_WQI_sum_Prati_CO2 <- river_WQI_v2 %>% filter(WQI == "Prati Index")
river_WQI_sum_Prati_CO2 <- merge(aggregate(data = river_WQI_sum_Prati_CO2, Concentration ~ `Water Quality` + Fluxes, FUN =mean), 
                             aggregate(data = river_WQI_sum_Prati_CO2, Concentration ~ `Water Quality` + Fluxes, FUN =std.error), 
                             by = c("Water Quality", "Fluxes"))
river_WQI_sum_Prati_CO2 <- merge(aggregate(data = river_WQI_sum_Prati_CO2, Concentration.x ~ `Water Quality`, FUN = sum),
                                 aggregate(data = river_WQI_sum_Prati_CO2, Concentration.y ~ `Water Quality`, FUN = sum),
                                 by = c("Water Quality"))


river_WQI_sum_Oregon_CO2 <- river_WQI_v2 %>% filter(WQI == "Oregon Index")
river_WQI_sum_Oregon_CO2$`Water Quality` <- as.character(river_WQI_sum_Oregon_CO2$`Water Quality`)
for (i in 1:nrow(river_WQI_sum_Oregon_CO2)){
    for (j in 1:length(new_Ore)){
        river_WQI_sum_Oregon_CO2$`Water Quality`[i] <- str_replace_all(river_WQI_sum_Oregon_CO2$`Water Quality`[i], new_Ore[j], old_Ore[j])
    }
}
river_WQI_sum_Oregon_CO2$`Water Quality` <- as.factor(river_WQI_sum_Oregon_CO2$`Water Quality`)
river_WQI_sum_Oregon_CO2 <- merge(aggregate(data = river_WQI_sum_Oregon_CO2, Concentration ~ `Water Quality` + Fluxes, FUN =mean), 
                              aggregate(data = river_WQI_sum_Oregon_CO2, Concentration ~ `Water Quality` + Fluxes, FUN =std.error), 
                              by = c("Water Quality", "Fluxes")) # Fair and Good of Oregon_CO2 contain only one variable --> No SEM

river_WQI_sum_Oregon_CO2 <- merge(aggregate(data = river_WQI_sum_Oregon_CO2, Concentration.x ~ `Water Quality`, FUN =sum), 
                                  aggregate(data = river_WQI_sum_Oregon_CO2, Concentration.y ~ `Water Quality`, FUN =sum, na.rm=TRUE, na.action=NULL), 
                                  by = c("Water Quality"))


#### Correct_box plot Land-use #####

river_LS <- river_WQI[, c(15, 16, 48:50)] %>% pivot_longer(c(-LB, -RB), names_to = "Fluxes", values_to = "Concentration") %>% 
    pivot_longer(c(- `Fluxes`, - Concentration), names_to = "Bank", values_to = "Land use")
river_LS$Bank <- str_replace_all(river_LS$Bank, "LB", "Left Bank")
river_LS$Bank <- str_replace_all(river_LS$Bank, "RB", "Right Bank")
old_lu <- as.character(levels(as.factor(river_LS$`Land use`)))
new_lu <- c("Agriculture", "Urban", "Industry", "Nature", "Industry", "Agriculture", "Urban",
            "Nature", "Road", "Urban")
for (i in 1:nrow(river_LS)){
    for (j in 1:length(new_lu)){
        river_LS$`Land use`[i] <- str_replace_all(river_LS$`Land use`[i], old_lu[j], new_lu[j])
    }
}

river_LS$Fluxes <- as.factor(river_LS$Fluxes)
river_LS$Fluxes <- relevel(river_LS$Fluxes,"F_CO2")

river_LS$Fluxes <- factor(river_LS$Fluxes, 
                              labels = c(expression("CO"["2"]), expression("CH"["4"]), 
                                         expression("N"["2"]*"O")))

# CO2 equivalent and separate the banks

ggsave("LS_final_CO2_equi.tiff", river_LS %>% 
           ggplot(aes(x = `Land use`, y = Concentration, fill = `Land use`)) +
           geom_boxplot() +
           stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="blue", fill="blue") +
           theme_bw() +
           ylab("GWP (mg"~CO[2]*"-equivalent "*m^-2*" "*d^-1*")") +
           facet_wrap(Bank~Fluxes, scales = "free"
                      , labeller = labeller(Fluxes = label_parsed)) +
           scale_fill_brewer(palette = "Paired", name = "Land use category")+
           theme(text=element_text(size=14),
                 strip.text.x = element_text(size=14),
                 axis.text.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.title.x = element_blank(),
                 legend.position="right",
                 legend.title = element_text(size = 14),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm')),
       units = 'cm', height = 20, width = 30, dpi = 300
)


# CO2 equivalent and no separation

river_LS$`Land use` <- as.factor(river_LS$`Land use`)
river_LS$`Land use` <- factor(river_LS$`Land use`, 
                              levels = c("Nature", "Industry", "Agriculture", "Road", "Urban"))

ggsave("LS_final_no_CO2_equi_no_bank.tiff", river_LS %>%
           # filter(Bank == "Right Bank") %>% 
           ggplot(aes(x = `Land use`, y = Concentration, fill = `Land use`)) +
           geom_boxplot() +
           stat_summary(fun=mean, geom="point", shape=20, size=5, color="blue", fill="blue") +
           theme_bw() +
           ylab("GWP (mg"~CO[2]*"-equivalent "*m^-2*" "*d^-1*")") +
           facet_wrap(.~Fluxes, scales = "free"
                      , labeller = labeller(Fluxes = label_parsed)) +
           scale_fill_manual(
               values = c("blue","green","yellow", "orange","red"), name = "Land use category")+
           theme(text=element_text(size=14),
                 strip.text.x = element_text(size=14),
                 axis.text.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.title.x = element_blank(),
                 legend.position="right",
                 legend.title = element_text(size = 14),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm')),
       units = 'cm', height = 15, width = 30, dpi = 300
)

# no CO2 equivalent and no separation

river_LS$Concentration2[river_LS$Fluxes == '"CO"["2"]'] <- river_LS$Concentration[river_LS$Fluxes == '"CO"["2"]']/1
river_LS$Concentration2[river_LS$Fluxes == '"CH"["4"]'] <- river_LS$Concentration[river_LS$Fluxes == '"CH"["4"]']/28
river_LS$Concentration2[river_LS$Fluxes == '"N"["2"] * "O"'] <- river_LS$Concentration[river_LS$Fluxes == '"N"["2"] * "O"']/265

river_LS$`Land use` <- as.factor(river_LS$`Land use`)
river_LS$`Land use` <- factor(river_LS$`Land use`, 
                              levels = c("Nature", "Industry", "Agriculture", "Road", "Urban"))

ggsave("LS_final_no_CO2_equi_no_bank.tiff", river_LS %>% 
           ggplot(aes(x = `Land use`, y = Concentration2, fill = `Land use`)) +
           geom_boxplot() +
           stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="blue", fill="blue") +
           theme_bw() +
           ylab(bquote("Fluxes (mg "*m^-2*" "*d^-1*")")) +
           facet_wrap(.~Fluxes, scales = "free"
                      , labeller = labeller(Fluxes = label_parsed)) +
           scale_fill_manual(
               values = c("blue","green","yellow", "orange","red"), name = "Land use category")+
           theme(text=element_text(size=14),
                 strip.text.x = element_text(size=14),
                 # axis.text.x = element_blank(),
                 # axis.ticks.x = element_blank(),
                 # axis.title.x = element_blank(),
                 legend.position="none",
                 legend.title = element_text(size = 14),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm')),
       units = 'cm', height = 15, width = 30, dpi = 300
)
# sum up LS

river_LS_sum <- merge(aggregate(data = river_LS, Concentration2 ~ `Land use` + Fluxes, FUN =mean), 
                      aggregate(data = river_LS, Concentration2 ~ `Land use` + Fluxes, FUN =std.error), 
                      by = c("Land use", "Fluxes"))
# sum up LS CO2 equivalent

river_LS_v2_sum <- merge(aggregate(data = river_LS, Concentration ~ `Land use` + Fluxes, FUN =mean), 
                      aggregate(data = river_LS, Concentration ~ `Land use` + Fluxes, FUN =std.error), 
                      by = c("Land use", "Fluxes"))

river_LS_v2_sum <- merge(aggregate(data = river_LS_v2_sum, Concentration.x ~ `Land use`, FUN =sum), 
                         aggregate(data = river_LS_v2_sum, Concentration.y ~ `Land use`, FUN =sum), 
                         by = c("Land use"))



#### Correct_summarise flux in each river ####

river_flux_each <- river_flux_GWP %>% # mg.m-2.d-1 not CO2 equivalent
    select(River, `Flux CO2`, `Flux CH4`, `Flux N2O`) %>%
    group_by(River) %>% summarise_each(funs = sum) %>%
    mutate_at(vars(`Flux CO2`, `Flux CH4`, `Flux N2O`), funs("percent" = ./sum(.)))
river_flux_each$Area <- c(95.92, 111.19, 138.98, 113.03, 113.81) # km2
river_flux_each <- river_flux_each %>% mutate_at(vars(`Flux CO2`, `Flux CH4`, `Flux N2O`), funs("area" = .*365*Area)) # g.year-1
river_flux_each <- river_flux_each %>% mutate_at(vars(`Flux CO2_area`, `Flux CH4_area`, `Flux N2O_area`), funs("percent" = .*100/ sum(.))) # percentage of area

river_flux_stacked <- river_flux_each[,c(1, 12:14)] %>% pivot_longer(cols = -River, names_to = "GHG", values_to = "Flux_area")
river_flux_stacked$GHG <- as.factor(river_flux_stacked$GHG)
river_flux_stacked$GHG <- relevel(river_flux_stacked$GHG,"Flux CO2_area_percent")
river_flux_stacked$GHG <- factor(river_flux_stacked$GHG, 
                                 labels = c(expression("Flux CO"["2"]), expression("Flux CH"["4"]), expression("Flux N"["2"]*"O")))

river_flux_stacked$River <- factor(river_flux_stacked$River,
                                   levels = c("Machangara", "Yanuncay", "Cuenca", "Tarqui", "Tomebamba"))

ggsave("Per_river_total_emissions.tiff", river_flux_stacked %>% ggplot() +
           geom_bar(aes(y=Flux_area, x=GHG,fill = `River`), stat = 'identity')+
           theme_bw() +
           # xlab("Year") +
           ylab("Fraction of the total emissions per year (%)") +
           # facet_grid(.~Bank) +
           scale_fill_brewer(palette = "Paired") +
           scale_x_discrete(labels =c(bquote("CO"[2]), bquote("CH"[4]), bquote("N"[2]*"O")))+
           theme(text=element_text(size=14),
                 strip.text.x = element_text(size=14),
                 axis.text.x = element_text(size = 14),
                 axis.ticks.x = element_blank(),
                 axis.title.x = element_blank(),
                 legend.position="bottom",
                 legend.title = element_blank(),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm')),
       units = 'cm', height = 20, width = 20, dpi = 300
)
#### Correlation variables including fluxes ####


variable_river_GWP <- cbind(river_flux_GWP[6:13], river_flux_GWP[22:33], river_flux_GWP[43:45]) %>% select(-Rain)
corr_river_GWP <- cor(variable_river_GWP, use = 'pairwise', method = "spearman")
p.mat <- cor.mtest(variable_river_GWP)

pmat <- as.matrix(p.mat[[1]])

jpeg("corr_coeff_3.jpeg",units = 'cm',height = 20,width = 20,res = 300, pointsize = 16)
corrplot(corr_river_GWP, p.mat = p.mat$p, method = "circle", type = "upper",
         sig.level = 0.05, insig = "blank", order = "alphabet")
dev.off()


#### REVISION NOTES #############
#### Correct_dissolved gas concentrations per river ####

river_DG <- river %>% select(c(5,46:48))
river_DG$Dis_CO2_cor <- river_DG$Dis_CO2_cor/1000

river_DG <- river_DG %>% pivot_longer(cols = -River, names_to = "GHGs", values_to = "Concentrations")
river_DG$GHGs <- as.factor(river_DG$GHGs)
river_DG$GHGs <- factor(river_DG$GHGs, levels = c("Dis_CO2_cor", "Dis_CH4_cor", "Dis_N2O_cor"),   
                        labels = c(expression("CO"["2"]*" (matm)"), expression("CH"["4"]*" ("*mu*"g "*L^-1*")"), 
                                   expression("N"["2"]*"O"*" ("*mu*"g "*L^-1*")")))

river_DG$River <- factor(river_DG$River,
                                levels = c("Machangara", "Yanuncay", "Cuenca", "Tarqui", "Tomebamba"))

ggsave("Dissolved GHG conc_river.jpeg",
    river_DG %>% 
        ggplot(aes(x = River, y = Concentrations, fill = River)) +
        geom_boxplot() +
        stat_summary(fun=mean, geom="point", shape=15, size=3, color="black", fill="black") +
        theme_bw()+
        ylab(bquote("Dissolved concentration")) +
        facet_wrap(.~ GHGs, scales = "free", labeller = label_parsed) +
        scale_fill_brewer(palette = "Paired", name = "Tributaries")+
        theme(text=element_text(size=14),
              strip.text.x = element_text(size=14),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.x = element_blank(),
              legend.position="right",
              legend.title = element_text(size = 14),
              legend.text = element_text(size = 12),
              legend.spacing.x = unit(0.5, 'cm'))
, units = 'cm', height = 15, width = 30, dpi = 300)

# sum up the GHG dissolved concentrations from the rivers

river_DG_sum <-  merge(aggregate(data = river_DG, Concentrations ~  GHGs + River, FUN =mean), 
                              aggregate(data = river_DG, Concentrations ~  GHGs + River, FUN =std.error), 
                              by = c("GHGs", "River"))
colnames(river_DG_sum)[3:4] <- c("Mean", "SE")


# convert to CO2 equivalent

river_DG_CO <- river_DG 
river_DG_CO[river_DG_CO$GHGs == '"CH"["4"]',] <- river_DG_CO %>% filter(GHGs == '"CH"["4"]') %>% mutate(Concentrations = Concentrations*28)
river_DG_CO <- river_DG_CO %>% arrange(GHGs)
river_DG_CO$Concentrations[73:108] <- river_DG_CO$Concentrations[73:108]*265

river_DG_CO_sum <-  merge(aggregate(data = river_DG_CO, Concentrations ~  GHGs + River, FUN =mean), 
                       aggregate(data = river_DG_CO, Concentrations ~  GHGs + River, FUN =std.error), 
                       by = c("GHGs", "River"))
colnames(river_DG_CO_sum)[3:4] <- c("Mean", "SE")

# sum of CO2 equivalent

river_DG_CO_sum2 <- merge(aggregate(data = river_DG_CO, Concentrations ~   River, FUN =mean), 
                       aggregate(data = river_DG_CO, Concentrations ~   River, FUN =std.error), 
                       by = c("River"))
colnames(river_DG_CO_sum2)[2:3] <- c("Mean", "SE")

#### Correct_dissolved gas concentraitons per WQI ####

river <- river %>% arrange(No)
river_WQI_DG <- river_WQI[, c(5, 44, 47)] 
river_WQI_DG <- bind_cols(river_WQI_DG, river[,c(46:48)])
river_WQI_DG$Dis_CO2_cor <- river_WQI_DG$Dis_CO2_cor/1000

river_WQI_DG <- river_WQI_DG %>% pivot_longer(c(-River,-Prati_WQI_1, -`OWQI-2`), names_to = "GHGs", values_to = "Concentration")


river_WQI_DG$GHGs <- as_factor(river_WQI_DG$GHGs)
river_WQI_DG <- river_WQI_DG[,-1]
old_Ore <- c("Good","Fair", "Very Poor", "Poor")
new_Ore <- c("Acceptable Quality", "Polluted", "Very Heavily Polluted", "Heavily Polluted")
for (i in 1:nrow(river_WQI_DG)){
    for (j in 1:length(new_Ore)){
        river_WQI_DG$`OWQI-2`[i] <- str_replace_all(river_WQI_DG$`OWQI-2`[i], old_Ore[j], new_Ore[j])
    }
}


colnames(river_WQI_DG) <- c("Prati Index", "Oregon Index", "Fluxes", "Concentration")

river_WQI_DG <- river_WQI_DG %>%
    gather(key = "WQI", value = "Water Quality", - `Fluxes`, - Concentration)

river_WQI_DG$Fluxes <- factor(river_WQI_DG$Fluxes, levels = c("Dis_CO2_cor", "Dis_CH4_cor", "Dis_N2O_cor"), 
                              labels = c(expression("CO"["2"]*""), expression("CH"["4"]*""), 
                                         expression("N"["2"]*"O"*"")))
river_WQI_DG$`Water Quality` <- as.factor(river_WQI_DG$`Water Quality`)
river_WQI_DG$`Water Quality` <- factor(river_WQI_DG$`Water Quality`, 
                                       levels = c("Good Quality", "Acceptable Quality",
                                                  "Polluted", "Heavily Polluted", "Very Heavily Polluted"))

ggsave("Dissolved GHG conc_WQI.jpeg", 
       river_WQI_DG %>% 
           ggplot(aes(y=Concentration, x=`Water Quality`,
                      fill = factor(`Water Quality`, 
                                    levels=  c("Good Quality", "Acceptable Quality",
                                               "Polluted", "Heavily Polluted", "Very Heavily Polluted")))) +
           geom_boxplot() +
           stat_summary(fun =mean, geom="point", shape=15, size=2.5, color="black", fill="black") +
           theme_bw() +
           labs(fill = "Prati Index") +
           ylab(bquote("Dissolved concentration (ppm)")) +
           facet_wrap(WQI~Fluxes, scales = "free"
                      , labeller = labeller(Fluxes = label_parsed)) +
           scale_fill_manual(values = c("blue","green","yellow","orange","red"), name = "Prati/Oregon Index", 
                             labels = c("Good Quality/Excellent", "Acceptable Quality/Good",
                                        "Polluted/Fair", "Heavily Polluted/Poor", "Very Heavily Polluted/Very Poor")) +
           # scale_x_discrete(labels =c(bquote("CO"[2]), bquote("CH"[4]), bquote("N"[2]*"O")))+
           theme(text=element_text(size=14),
                 strip.text.x =element_text(size=14),
                 axis.text.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.title.x = element_blank(),
                 legend.position = "right",
                 legend.title = element_text(size = 14),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm'))
       , units = 'cm', height = 25, width = 30, dpi = 300)

# Kruskal Wallis test

river_dun_WQI <- river_WQI_DG
river_dun_WQI$Fluxes <- factor(river_dun_WQI$Fluxes, labels = c("CO2", "CH4", "N2O"))

# dun_f <- function(x, column, column2){
#     # x[,column] <- as.factor(x[,column])
#     # x[,column2] <- as.factor(x[,column2])
#     z <- as.data.frame(dunn.test(x[,column], x[,column2], method=c('bonferroni'))[4:5])
#     return(z)
# } 
# dun_f(river_dun_WQI %>% filter(WQI == "Prati Index"), "Concentration", "Water Quality")

river_dun_CO2 <- as.data.frame(dunn.test(river_dun_WQI$Concentration[river_dun_WQI$Fluxes == "CO2" & river_dun_WQI$WQI == "Prati Index"], river_dun_WQI$`Water Quality`[river_dun_WQI$Fluxes == "CO2" & river_dun_WQI$WQI == "Prati Index"], method=c('bonferroni'))[4:5])
river_dun_CH4 <- as.data.frame(dunn.test(river_dun_WQI$Concentration[river_dun_WQI$Fluxes == "CH4" & river_dun_WQI$WQI == "Prati Index"], river_dun_WQI$`Water Quality`[river_dun_WQI$Fluxes == "CH4" & river_dun_WQI$WQI == "Prati Index"], method=c('bonferroni'))[4:5])
river_dun_N2O <- as.data.frame(dunn.test(river_dun_WQI$Concentration[river_dun_WQI$Fluxes == "N2O" & river_dun_WQI$WQI == "Prati Index"], river_dun_WQI$`Water Quality`[river_dun_WQI$Fluxes == "N2O" & river_dun_WQI$WQI == "Prati Index"], method=c('bonferroni'))[4:5])
river_dun_CO2_2 <- as.data.frame(dunn.test(river_dun_WQI$Concentration[river_dun_WQI$Fluxes == "CO2" & river_dun_WQI$WQI == "Oregon Index"], river_dun_WQI$`Water Quality`[river_dun_WQI$Fluxes == "CO2" & river_dun_WQI$WQI == "Oregon Index"], method=c('bonferroni'))[4:5])
river_dun_CH4_2 <- as.data.frame(dunn.test(river_dun_WQI$Concentration[river_dun_WQI$Fluxes == "CH4" & river_dun_WQI$WQI == "Oregon Index"], river_dun_WQI$`Water Quality`[river_dun_WQI$Fluxes == "CH4" & river_dun_WQI$WQI == "Oregon Index"], method=c('bonferroni'))[4:5])
river_dun_N2O_2 <- as.data.frame(dunn.test(river_dun_WQI$Concentration[river_dun_WQI$Fluxes == "N2O" & river_dun_WQI$WQI == "Oregon Index"], river_dun_WQI$`Water Quality`[river_dun_WQI$Fluxes == "N2O" & river_dun_WQI$WQI == "Oregon Index"], method=c('bonferroni'))[4:5])

river_dun_D <- bind_rows(river_dun_CO2, river_dun_CH4, river_dun_N2O, .id = "Dissolved gases")
river_dun_D$`Dissolved gases` <- factor(river_dun_D$`Dissolved gases`)
levels(river_dun_D$`Dissolved gases`) <- c("Dissolved CO2", "Dissolved CH4", "Dissolved N2O")

river_dun_D_2 <- bind_rows(river_dun_CO2_2, river_dun_CH4_2, river_dun_N2O_2, .id = "Dissolved gases")
river_dun_D_2$`Dissolved gases` <- factor(river_dun_D_2$`Dissolved gases`)
levels(river_dun_D_2$`Dissolved gases`) <- c("Dissolved CO2", "Dissolved CH4", "Dissolved N2O")

river_dun <-bind_rows(river_dun_D, river_dun_D_2, .id = "WQ Index")

write_csv(river_dun, "river_Krukal Wallis.csv")

#only the significant ones

river_dun_sig <- river_dun %>% filter(P.adjusted <= 0.05)
write_csv(river_dun_sig, "river_Krukal Wallis_sig.csv")

# sum up WQI

# no co2 equivalent

river_WQI_DG_noCO2 <- river_WQI_DG
river_WQI_DG_noCO2_sum_Prati <- river_WQI_DG_noCO2 %>% filter(WQI == "Prati Index")
river_WQI_DG_noCO2_sum_Prati <- merge(aggregate(data = river_WQI_DG_noCO2_sum_Prati, Concentration ~ `Water Quality` + Fluxes, FUN =mean), 
                              aggregate(data = river_WQI_DG_noCO2_sum_Prati, Concentration ~ `Water Quality` + Fluxes, FUN =std.error), 
                              by = c("Water Quality", "Fluxes"))

river_WQI_DG_noCO2_sum_Oregon <- river_WQI_DG_noCO2 %>% filter(WQI == "Oregon Index")
river_WQI_DG_noCO2_sum_Oregon <- merge(aggregate(data = river_WQI_DG_noCO2_sum_Oregon, Concentration ~ `Water Quality` + Fluxes, FUN =mean), 
                                      aggregate(data = river_WQI_DG_noCO2_sum_Oregon, Concentration ~ `Water Quality` + Fluxes, FUN =std.error), 
                                      by = c("Water Quality", "Fluxes"))

river_WQI_DG_Prati <- river_WQI_DG %>% filter(WQI == "Prati Index")
river_WQI_DG_Oregon  <- river_WQI_DG %>% filter(WQI == "Oregon Index")
summary(as.factor(river_WQI_DG_Prati$`Water Quality`))/3
summary(as.factor(river_WQI_DG_Oregon$`Water Quality`))/3

# co2 equivalent


river_WQI_DG[river_WQI_DG$Fluxes == '"CH"["4"]',] <- river_WQI_DG %>% filter(Fluxes == '"CH"["4"]') %>% mutate(Concentration = Concentration*28)
river_WQI_DG <- river_WQI_DG %>% arrange(Fluxes)
river_WQI_DG$Concentration[145:216] <- river_WQI_DG$Concentration[145:216]*265

river_WQI_DG[river_WQI_DG$Fluxes != 'N"["2"] \\* "O"',] <- river_WQI_DG %>% filter(Fluxes == 'N"["2"] \\* "O"') %>% mutate(Concentration = Concentration*28)
river2_WQI_sum_Prati <- river_WQI_DG %>% filter(WQI == "Prati Index")
river2_WQI_sum_Prati <- merge(aggregate(data = river2_WQI_sum_Prati, Concentration ~ `Water Quality` + Fluxes, FUN =mean), 
                             aggregate(data = river2_WQI_sum_Prati, Concentration ~ `Water Quality` + Fluxes, FUN =std.error), 
                             by = c("Water Quality", "Fluxes"))

river2_WQI_sum_Prati_CO2 <- river_WQI_DG %>% filter(WQI == "Prati Index")
river2_WQI_sum_Prati_CO2 <- merge(aggregate(data = river2_WQI_sum_Prati_CO2, Concentration ~ `Water Quality`, FUN =mean), 
                              aggregate(data = river2_WQI_sum_Prati_CO2, Concentration ~ `Water Quality`, FUN =std.error), 
                              by = c("Water Quality"))

river2_WQI_sum_Oregon <- river_WQI_DG %>% filter(WQI == "Oregon Index")
river2_WQI_sum_Oregon$`Water Quality` <- as.character(river2_WQI_sum_Oregon$`Water Quality`)
for (i in 1:nrow(river2_WQI_sum_Oregon)){
    for (j in 1:length(new_Ore)){
        river2_WQI_sum_Oregon$`Water Quality`[i] <- str_replace_all(river2_WQI_sum_Oregon$`Water Quality`[i], new_Ore[j], old_Ore[j])
    }
}
river2_WQI_sum_Oregon$`Water Quality` <- as.factor(river2_WQI_sum_Oregon$`Water Quality`)
river2_WQI_sum_Oregon <- merge(aggregate(data = river2_WQI_sum_Oregon, Concentration ~ `Water Quality` + Fluxes, FUN =mean), 
                              aggregate(data = river2_WQI_sum_Oregon, Concentration ~ `Water Quality` + Fluxes, FUN =std.error), 
                              by = c("Water Quality", "Fluxes")) # Fair and Good of Oregon contain only one variable --> No SEM


# sum up WQI 11.06.2020 CO2 equivalent

river2_WQI_sum_Prati_CO2 <- river_WQI_DG %>% filter(WQI == "Prati Index")
river2_WQI_sum_Prati_CO2 <- merge(aggregate(data = river2_WQI_sum_Prati_CO2, Concentration ~ `Water Quality` + Fluxes, FUN =mean), 
                                 aggregate(data = river2_WQI_sum_Prati_CO2, Concentration ~ `Water Quality` + Fluxes, FUN =std.error), 
                                 by = c("Water Quality", "Fluxes"))
river2_WQI_sum_Prati_CO2 <- merge(aggregate(data = river2_WQI_sum_Prati_CO2, Concentration.x ~ `Water Quality`, FUN = sum),
                                 aggregate(data = river2_WQI_sum_Prati_CO2, Concentration.y ~ `Water Quality`, FUN = sum),
                                 by = c("Water Quality"))
river2_WQI_sum_Prati_CO2[,2:3] <- river2_WQI_sum_Prati_CO2[,2:3]/1000

river2_WQI_sum_Oregon_CO2 <- river_WQI_DG %>% filter(WQI == "Oregon Index")
river2_WQI_sum_Oregon_CO2$`Water Quality` <- as.character(river2_WQI_sum_Oregon_CO2$`Water Quality`)
for (i in 1:nrow(river2_WQI_sum_Oregon_CO2)){
    for (j in 1:length(new_Ore)){
        river2_WQI_sum_Oregon_CO2$`Water Quality`[i] <- str_replace_all(river2_WQI_sum_Oregon_CO2$`Water Quality`[i], new_Ore[j], old_Ore[j])
    }
}
river2_WQI_sum_Oregon_CO2$`Water Quality` <- as.factor(river2_WQI_sum_Oregon_CO2$`Water Quality`)
river2_WQI_sum_Oregon_CO2 <- merge(aggregate(data = river2_WQI_sum_Oregon_CO2, Concentration ~ `Water Quality` + Fluxes, FUN =mean), 
                                  aggregate(data = river2_WQI_sum_Oregon_CO2, Concentration ~ `Water Quality` + Fluxes, FUN =std.error), 
                                  by = c("Water Quality", "Fluxes")) # Fair and Good of Oregon_CO2 contain only one variable --> No SEM

river2_WQI_sum_Oregon_CO2 <- merge(aggregate(data = river2_WQI_sum_Oregon_CO2, Concentration.x ~ `Water Quality`, FUN =sum), 
                                  aggregate(data = river2_WQI_sum_Oregon_CO2, Concentration.y ~ `Water Quality`, FUN =sum, na.rm=TRUE, na.action=NULL), 
                                  by = c("Water Quality"))
river2_WQI_sum_Oregon_CO2[,2:3] <- river2_WQI_sum_Oregon_CO2[,2:3]/1000

# make new graph with WQ category in the axis label


river_WQI_DG <- river_WQI[, c(5, 44, 47)] 

river_WQI_DG <- bind_cols(river_WQI_DG, river[,c(46:48)])
river_WQI_DG$Dis_CO2_cor <- river_WQI_DG$Dis_CO2_cor/1000

river_WQI_DG <- river_WQI_DG %>% pivot_longer(c(-River,-Prati_WQI_1, -`OWQI-2`), names_to = "GHGs", values_to = "Concentration")


river_WQI_DG$GHGs <- as_factor(river_WQI_DG$GHGs)
river_WQI_DG <- river_WQI_DG[,-1]

colnames(river_WQI_DG) <- c("Prati Index", "Oregon Index", "Fluxes", "Concentration")

river_WQI_DG <- river_WQI_DG %>%
    gather(key = "WQI", value = "Water Quality", - `Fluxes`, - Concentration)

river_WQI_DG$Fluxes <- factor(river_WQI_DG$Fluxes, levels = c("Dis_CO2_cor", "Dis_CH4_cor", "Dis_N2O_cor"), 
                              labels = c(expression("CO"["2"]*" (matm)"), expression("CH"["4"]*" ("*mu*"g "*L^-1*")"), 
                                         expression("N"["2"]*"O"*" ("*mu*"g "*L^-1*")")))
river_WQI_DG$`Water Quality` <- as.factor(river_WQI_DG$`Water Quality`)
river_WQI_DG$`Water Quality` <- factor(river_WQI_DG$`Water Quality`, 
                                       levels = c("Good Quality", "Acceptable Quality",
                                                  "Polluted", "Heavily Polluted", "Very Heavily Polluted", "Good", "Fair", "Poor", "Very Poor"),
                                       labels = c(" Good", "Acceptable",
                                                  "Polluted", "Heavily Polluted", "Very Heavily Polluted", "Good", "Fair", "Poor", "Very Poor"))

ggsave("Dissolved GHG conc_WQI_2.jpeg", 
       river_WQI_DG %>% 
           ggplot(aes(y=Concentration, x=`Water Quality`,
                      fill = factor(`Water Quality`
                                    # , 
                                    # levels=  c("Good Quality", "Acceptable Quality",
                                    #            "Polluted", "Heavily Polluted", "Very Heavily Polluted")
                      ))) +
           geom_boxplot() +
           stat_summary(fun =mean, geom="point", shape=15, size=3, color="black", fill="black") +
           theme_bw() +
           labs(fill = "Prati Index") +
           ylab(bquote("Dissolved concentration")) +
           facet_wrap(WQI~Fluxes, scales = "free"
                      , labeller = labeller(Fluxes = label_parsed)) +
           scale_fill_manual(values = c("blue","green","yellow","orange","red"
                                        , "green", "yellow", "orange", "red"
                                       ), name = "Prati/Oregon Index", 
                                       labels = c("Good Quality/Excellent", "Acceptable Quality/Good",
                                                  "Polluted/Fair", "Heavily Polluted/Poor", "Very Heavily Polluted/Very Poor")) +
           # scale_x_discrete(labels =c(bquote("CO"[2]), bquote("CH"[4]), bquote("N"[2]*"O")))+
           theme(text=element_text(size=14),
                 strip.text.x =element_text(size=14),
                 axis.text.x = element_text(size=13, angle = 45, hjust = 1),
                 # axis.ticks.x = element_blank(),
                 axis.title.x = element_blank(),
                 legend.position = "none",
                 legend.title = element_text(size = 14),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm'))
       , units = 'cm', height = 25, width = 25, dpi = 300)


#### Correct_dissolved gas concentrations per land use ####

river_LS_DG <- river_WQI[, c(15, 16)] 
river_LS_DG <- bind_cols(river_LS_DG, river[,c(46:48)])

river_LS_DG$Dis_CO2_cor <- river_LS_DG$Dis_CO2_cor/1000
river_LS_DG <- river_LS_DG %>% pivot_longer(c(-LB, -RB), names_to = "Fluxes", values_to = "Concentration") %>% 
    pivot_longer(c(- `Fluxes`, - Concentration), names_to = "Bank", values_to = "Land use")
river_LS_DG$Bank <- str_replace_all(river_LS_DG$Bank, "LB", "Left Bank")
river_LS_DG$Bank <- str_replace_all(river_LS_DG$Bank, "RB", "Right Bank")
old_lu <- as.character(levels(as.factor(river_LS_DG$`Land use`)))
new_lu <- c("Agriculture", "Urban", "Industry", "Nature", "Industry", "Agriculture", "Urban",
            "Nature", "Road", "Urban")
for (i in 1:nrow(river_LS_DG)){
    for (j in 1:length(new_lu)){
        river_LS_DG$`Land use`[i] <- str_replace_all(river_LS_DG$`Land use`[i], old_lu[j], new_lu[j])
    }
}

river_LS_DG$Fluxes <- as.factor(river_LS_DG$Fluxes)
river_LS_DG$Fluxes <- relevel(river_LS_DG$Fluxes,"Dis_CO2_cor")

river_LS_DG$Fluxes <- factor(river_LS_DG$Fluxes, 
                             labels = c(expression("CO"["2"]*" (matm)"), expression("CH"["4"]*" ("*mu*"g "*L^-1*")"), 
                                        expression("N"["2"]*"O"*" ("*mu*"g "*L^-1*")")))
river_LS_DG$`Land use` <- as.factor(river_LS_DG$`Land use`)

river_LS_DG$`Land use` <- factor(river_LS$`Land use`, 
                              levels = c("Nature", "Industry", "Agriculture", "Road", "Urban"))
cc_model <- seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=30))

ggsave("Dissolved GHG conc_LS.jpeg",
       river_LS_DG %>% 
           ggplot(aes(x = `Land use`, y = Concentration, fill = `Land use`)) +
           geom_boxplot() +
           stat_summary(fun=mean, geom="point", shape=15, size=3, color="black", fill="black") +
           theme_bw() +
           ylab(bquote("Dissolved concentration")) +
           facet_wrap(.~Fluxes, scales = "free"
                      , labeller = labeller(Fluxes = label_parsed)) +
           scale_fill_manual(values=cc_model[rev(c(30,23,18,13,8,3,1))], name = "Land use category") + 
           # scale_fill_brewer(palette = "Paired", name = "Tributaries")+
           # scale_fill_manual(
           #     values = c("blue","green","yellow", "orange","red"), name = "Land use category")+
           theme(text=element_text(size=14),
                 strip.text.x = element_text(size=14),
                 # axis.text.x = element_blank(),
                 # axis.ticks.x = element_blank(),
                 # axis.title.x = element_blank(),
                 legend.position="none",
                 legend.title = element_text(size = 14),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm'))
       , units = 'cm', height = 15, width = 30, dpi = 300)

# Kruskal Wallis test

river_dun_LS <- river_LS_DG
river_dun_LS$Fluxes <- factor(river_dun_LS$Fluxes, labels = c("CO2", "CH4", "N2O"))

river_dun_CO2_LS <- as.data.frame(dunn.test(river_dun_LS$Concentration[river_dun_LS$Fluxes == "CO2"
                                                                       # & river_dun_LS$Bank == "Right Bank"
                                                                       ], 
                                            river_dun_LS$`Land use`[river_dun_LS$Fluxes == "CO2" 
                                                                    # & river_dun_LS$Bank == "Right Bank"
                                                                    ], method=c('bonferroni'))[4:5])
river_dun_CH4_LS <- as.data.frame(dunn.test(river_dun_LS$Concentration[river_dun_LS$Fluxes == "CH4"
                                                                       # & river_dun_LS$Bank == "Right Bank"
                                                                       ], 
                                            river_dun_LS$`Land use`[river_dun_LS$Fluxes == "CH4" 
                                                                    # & river_dun_LS$Bank == "Right Bank"
                                                                    ], method=c('bonferroni'))[4:5])
river_dun_N2O_LS <- as.data.frame(dunn.test(river_dun_LS$Concentration[river_dun_LS$Fluxes == "N2O"
                                                                       # & river_dun_LS$Bank == "Right Bank"
                                                                       ], 
                                            river_dun_LS$`Land use`[river_dun_LS$Fluxes == "N2O" 
                                                                    # & river_dun_LS$Bank == "Right Bank"
                                                                    ], method=c('bonferroni'))[4:5])



# no CO2 equivalent

river_LS_DG_no_CO2 <- river_LS_DG


river_LS_DG_no_CO2 <- merge(aggregate(data = river_LS_DG_no_CO2, Concentration ~ `Land use` + Fluxes, FUN =mean), 
                            aggregate(data = river_LS_DG_no_CO2, Concentration ~ `Land use` + Fluxes, FUN =std.error), 
                            by = c("Land use", "Fluxes"))

# sum up LS CO2 equivalent


river_LS_DG[river_LS_DG$Fluxes == '"CH"["4"]',] <- river_LS_DG %>% filter(Fluxes == '"CH"["4"]') %>% mutate(Concentration = Concentration*28)
river_LS_DG <- river_LS_DG %>% arrange(Fluxes)
river_LS_DG$Concentration[145:216] <- river_LS_DG$Concentration[145:216]*265

river_LS_DG_v2_sum <- merge(aggregate(data = river_LS_DG, Concentration ~ `Land use` + Fluxes, FUN =mean), 
                         aggregate(data = river_LS_DG, Concentration ~ `Land use` + Fluxes, FUN =std.error), 
                         by = c("Land use", "Fluxes"))

river_LS_DG_v2_sum <- merge(aggregate(data = river_LS_DG_v2_sum, Concentration.x ~ `Land use`, FUN =sum), 
                         aggregate(data = river_LS_DG_v2_sum, Concentration.y ~ `Land use`, FUN =sum), 
                         by = c("Land use"))

river_LS_DG_v2_sum[,2:3] <- river_LS_DG_v2_sum[,2:3]/1000

#### Extra_DO per WQI ####


river_DO_DG <- river_WQI[, c(5, 8, 44, 47)] 

river_DO_DG <- river_DO_DG[,-1]



colnames(river_DO_DG)[2:3] <- c("Prati Index", "Oregon Index")
river_DO_DG <- river_DO_DG %>% pivot_longer(cols = -DO, names_to = "WQI", values_to = "Water Quality")

river_DO_DG$WQI <- as.factor(river_DO_DG$WQI)
river_DO_DG$WQI <- factor(river_DO_DG$WQI, levels = c("Prati Index", "Oregon Index"))
river_DO_DG$`Water Quality` <- as.factor(river_DO_DG$`Water Quality`)
river_DO_DG$`Water Quality` <- factor(river_DO_DG$`Water Quality`, 
                                      levels = c("Good Quality", "Acceptable Quality",
                                                 "Polluted", "Heavily Polluted", "Very Heavily Polluted", "Good", "Fair", "Poor", "Very Poor"),
                                      labels = c(" Good", "Acceptable",
                                                 "Polluted", "Heavily Polluted", "Very Heavily Polluted", "Good", "Fair", "Poor", "Very Poor"))
ggsave("Dissolved GHG conc_DO.jpeg", 
       river_DO_DG %>% 
           ggplot(aes(y=DO, x=`Water Quality`,
                      fill = factor(`Water Quality`
                                    , 
                                    # levels=  c("Good Quality", "Acceptable Quality",
                                    #            "Polluted", "Heavily Polluted", "Very Heavily Polluted")
                                    ))) +
           geom_boxplot() +
           stat_summary(fun =mean, geom="point", shape=15, size=4, color="black", fill="black") +
           theme_bw() +
           labs(fill = "Prati Index") +
           scale_y_reverse() +
           ylab(bquote("Dissolved oxygen concentrations (mg "*L^-1*")")) +
           facet_wrap(.~WQI, scales = "free_x") +
           scale_fill_manual(values = c("blue","green","yellow","orange","red"
                                        , "green", "yellow", "orange", "red"
                                       ), name = "Prati/Oregon Index", 
                                       labels = c("Good Quality/Excellent", "Acceptable Quality/Good",
                                                  "Polluted/Fair", "Heavily Polluted/Poor", "Very Heavily Polluted/Very Poor")) +
           # scale_x_discrete(labels =c(bquote("CO"[2]), bquote("CH"[4]), bquote("N"[2]*"O")))+
           theme(text=element_text(size=14),
                 strip.text.x =element_text(size=14),
                 axis.text.x = element_text(size=13, angle = 45, hjust = 1),
                 # axis.ticks.x = element_blank(),
                 axis.title.x = element_blank(),
                 legend.position = "none",
                 legend.title = element_text(size = 14),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm'))
       , units = 'cm', height = 20, width = 30, dpi = 300)

#### FUZZY MODEL #######
#### save files ####

# CO2
river <- river %>% arrange(No)
river_WQI_fuzzy <- river_WQI[, c(1:34, 44)] 
river_WQI_fuzzy <- bind_cols(river_WQI_fuzzy, river[,c(46:48)])


wqi_std <- tribble(
    ~Prati_WQI_1, ~WQI,
    "Good Quality", '1',
    "Acceptable Quality", '2',
    "Polluted", '3',
    "Heavily Polluted", '4',
    "Very Heavily Polluted", '5'
)

river_WQI_fuzzy <- river_WQI_fuzzy %>% right_join(wqi_std) %>% select(-Prati_WQI_1)

river_v2_CO2 <- river_WQI_fuzzy %>% select("DO", "NH4", "NO2", "Velocity", "Depth", "T_w", "LB", "RB", "WQI", "Dis_CO2_cor") %>% 
    pivot_longer(cols = -c("DO", "NH4", "NO2", "Velocity", "Depth", "T_w", "WQI", "Dis_CO2_cor"), names_to = "Bank", values_to = "Land use") %>% 
    select(-"Bank")

land_std <- tribble(
    ~`Land use`,~Land,
    "Forest", "1",
    "River", "1",
    "Factory", "2",
    "Mining", "2",
    "Arable", "3",
    "Orchard", "3",
    "Road", "4",
    "Construction", "5",
    "Resident", "5",
    "Urban", "5"
)

river_v2_CO2 <- river_v2_CO2 %>% right_join(land_std) %>% select(-`Land use`)
colnames(river_v2_CO2)[c(2,3, 4, 6,8)] <- c("NH", "NO", "Vel", "Tw", "CO2")
river_v2_CO2 <- river_v2_CO2[,c(1:7,9,8)]
write.csv(river_v2_CO2, "river_fuzzy_CO2.csv")

# CH4

river_v2_CH4 <- river_WQI_fuzzy %>% select("DO", "COD", "Velocity", "Depth", "T_w", "LB", "RB", "WQI", "Dis_CH4_cor") %>% 
    pivot_longer(cols = -c("DO", "COD", "Velocity", "Depth", "T_w", "WQI", "Dis_CH4_cor"), names_to = "Bank", values_to = "Land use") %>% 
    select(-"Bank")
river_v2_CH4 <- river_v2_CH4 %>% right_join(land_std) %>% select(-`Land use`)

colnames(river_v2_CH4)[c(3,5, 7)] <- c("Vel", "Tw", "CH4")
river_v2_CH4 <- river_v2_CH4[,c(1:6, 8,7)]
write.csv(river_v2_CH4, "river_fuzzy_CH4.csv")

# N2O

river_v2_N2O <- river_WQI_fuzzy %>% select("DO", "NH4", "NO2", "COD", "Depth", "T_w", "LB", "RB", "WQI", "Dis_N2O_cor") %>% 
    pivot_longer(cols = -c("DO", "NH4", "NO2", "COD", "Depth", "T_w", "WQI", "Dis_N2O_cor"), names_to = "Bank", values_to = "Land use") %>% 
    select(-"Bank")
river_v2_N2O <- river_v2_N2O %>% right_join(land_std) %>% select(-`Land use`)

colnames(river_v2_N2O)[c(2,3,6,8)] <- c("NH", "NO", "Tw", "N2O")
river_v2_N2O <- river_v2_N2O[,c(1:7,9,8)]

write.csv(river_v2_N2O, "river_fuzzy_N2O.csv")
