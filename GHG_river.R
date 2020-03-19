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

co2.aq   = (Ca.co2 + Cah.co2)  # mg co2/L H2O
co2.aq

river$Dis_CO2_cor <- co2.aq

river_mis <- river[0,]

for (i in 1:ncol(river_mis)){
    if (is.factor(river_mis[,i])){
        river_mis[,i] <- as.numeric(as.character(river_mis[,i]))
    }
    river_mis[1,i] <- sum(is.na(river[,i]))
}

river <- river %>% select(-c(38:43))

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
ggsave("Mosaic_river_FV.tiff",ggplot(river)+
           geom_mosaic(aes(x= product(River), fill = Flow_variation))+
           labs(x ="", y = "")+
           theme_bw()+
           theme(axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 text=element_text(size=13),
                 strip.text.x = element_text(size=13)),
       units = 'cm', height = 15, width = 20, dpi = 300)
ggsave("Mosaic_river_shading.tiff",ggplot(river)+
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
           ylab(bquote("GWP (mg"~CO[2]*"-equivalent."*m^-2*"."*d^-1*")")) +
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

ggsave("Fluxes_GWP_no_Co2_equi.tiff", river_flux_GWP3 %>% ggplot(aes(x = River, y = Concentration, fill = River)) +
           geom_boxplot() +
           stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
           theme_bw()+
           ylab(bquote("Fluxes (mg."*m^-2*"."*d^-1*")")) +
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
           ylab("Mean of the fluxes (mg"~CO[2]*"-equivalent. "*m^-2*"."*d^-1*")") +
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

ggsave("Total_fluxes_no_Co2_equi.tiff", river_sem %>% 
           ggplot(aes(x = Tributaries, y = Mean, fill = Tributaries)) +
           geom_bar(stat = 'identity') +
           geom_errorbar(aes(ymin= Mean - SEM, ymax=Mean+SEM), width=.2,
                         position=position_dodge(.9)) +
           theme_bw() +
           ylab(bquote("Mean of the fluxes (mg."*m^-2*"."*d^-1*")")) +
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
           ylab("Mean of the total emission per year (Gg"~CO[2]*"-equivalent. "*yr^-1*")") +
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
           ylab(bquote("Mean of the total emission per year (Gg."*yr^-1*")")) +
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
river_WQI$F_CH4_mg <- river_WQI$F_CH4*(12+4)/1000
river_WQI$F_N2O_mg <- river_WQI$F_N2O*(14*2+16)/1000

river_WQI_v2 <- river_WQI[, c(5, 44, 47, 51:53)] %>% pivot_longer(c(-River,-Prati_WQI_1, -`OWQI-2`), names_to = "Fluxes", values_to = "Concentration")
river_WQI_v2$Fluxes <- as_factor(river_WQI_v2$Fluxes)
river_WQI_v2 <- river_WQI_v2[,-1]
old_Ore <- c("Good","Fair", "Very Poor", "Poor")
new_Ore <- c("Good Quality", "Acceptable Quality", "Heavily Polluted", "Polluted")
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

ggsave("WQI_final_CO2_equi.tiff", river_WQI_v2 %>% 
           ggplot(aes(y=Concentration, x=`Water Quality`,
                      fill = `Water Quality`)) +
           geom_boxplot() +
           stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="blue", fill="blue") +
           theme_bw() +
           labs(fill = "Prati Index") +
           ylab("GWP (mg"~CO[2]*"-equivalent. "*m^-2*"."*d^-1*")") +
           facet_wrap(WQI~Fluxes, scales = "free"
                      , labeller = labeller(Fluxes = label_parsed)) +
           
           scale_fill_manual(
               values = c("blue","green","yellow", "orange","red"),
               name = "Prati/Oregon Index",
                             labels = c("Good Quality/Good", "Acceptable Quality/Fair",
                                        "Polluted/Poor", "Heavily Polluted/Very Poor",  "Very Heavily Polluted/NA")) + 
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
           stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="blue", fill="blue") +
           theme_bw() +
           labs(fill = "Prati Index") +
           ylab(bquote("Fluxes (mg."*m^-2*"."*d^-1*")")) +
           facet_wrap(WQI~Fluxes, scales = "free"
                      , labeller = labeller(Fluxes = label_parsed)) +
           scale_fill_manual(values = c("blue","green","yellow","orange","red"), name = "Prati/Oregon Index", 
                             labels = c("Good Quality/Good", "Acceptable Quality/Fair",
                                        "Polluted/Poor", "Heavily Polluted/Very Poor", "Very Heavily Polluted/NA")) +
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
river_WQI_sum_Prati <- merge(aggregate(data = river_WQI_sum_Prati, Concentration ~ `Water Quality` + Fluxes, FUN =mean), 
                             aggregate(data = river_WQI_sum_Prati, Concentration ~ `Water Quality` + Fluxes, FUN =std.error), 
                             by = c("Water Quality", "Fluxes"))

river_WQI_sum_Oregon <- river_WQI_v2 %>% filter(WQI == "Oregon Index")
river_WQI_sum_Oregon$`Water Quality` <- as.character(river_WQI_sum_Oregon$`Water Quality`)
for (i in 1:nrow(river_WQI_sum_Oregon)){
    for (j in 1:length(new_Ore)){
        river_WQI_sum_Oregon$`Water Quality`[i] <- str_replace_all(river_WQI_sum_Oregon$`Water Quality`[i], new_Ore[j], old_Ore[j])
    }
}
river_WQI_sum_Oregon$`Water Quality` <- as.factor(river_WQI_sum_Oregon$`Water Quality`)
river_WQI_sum_Oregon <- merge(aggregate(data = river_WQI_sum_Oregon, Concentration ~ `Water Quality` + Fluxes, FUN =mean), 
                             aggregate(data = river_WQI_sum_Oregon, Concentration ~ `Water Quality` + Fluxes, FUN =std.error), 
                             by = c("Water Quality", "Fluxes")) # Fair and Good of Oregon contain only one variable --> No SEM




    
#### Correct_box plot Land-use #####

river_LS <- river_WQI[, c(15, 16, 51:53)] %>% pivot_longer(c(-LB, -RB), names_to = "Fluxes", values_to = "Concentration") %>% 
    pivot_longer(c(- `Fluxes`, - Concentration), names_to = "Bank", values_to = "Land use")
river_LS$Bank <- str_replace_all(river_LS$Bank, "LB", "Left Bank")
river_LS$Bank <- str_replace_all(river_LS$Bank, "RB", "Right Bank")

for (i in 1:nrow(river_LS)){
    for (j in 1:length(new_lu)){
        river_LS$`Land use`[i] <- str_replace_all(river_LS$`Land use`[i], old_lu[j], new_lu[j])
    }
}
river_LS$Fluxes <- as.factor(river_LS$Fluxes)
river_LS$Fluxes <- relevel(river_LS$Fluxes,"F_CO2_mg")

river_LS$Fluxes <- factor(river_LS$Fluxes, 
                              labels = c(expression("CO"["2"]), expression("CH"["4"]), 
                                         expression("N"["2"]*"O")))

# CO2 equivalent

ggsave("LS_final_CO2_equi.tiff", river_LS %>% 
           ggplot(aes(x = `Land use`, y = Concentration, fill = `Land use`)) +
           geom_boxplot() +
           stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="blue", fill="blue") +
           theme_bw() +
           ylab("GWP (mg"~CO[2]*"-equivalent. "*m^-2*"."*d^-1*")") +
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

# no CO2 equivalent

river_LS$Concentration2[river_LS$Fluxes == '"CO"["2"]'] <- river_LS$Concentration[river_LS$Fluxes == '"CO"["2"]']/1
river_LS$Concentration2[river_LS$Fluxes == '"CH"["4"]'] <- river_LS$Concentration[river_LS$Fluxes == '"CH"["4"]']/28
river_LS$Concentration2[river_LS$Fluxes == '"N"["2"] * "O"'] <- river_LS$Concentration[river_LS$Fluxes == '"N"["2"] * "O"']/265

ggsave("LS_final_no_CO2_equi.tiff", river_LS %>% 
           ggplot(aes(x = `Land use`, y = Concentration2, fill = `Land use`)) +
           geom_boxplot() +
           stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="blue", fill="blue") +
           theme_bw() +
           ylab(bquote("Fluxes (mg."*m^-2*"."*d^-1*")")) +
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
# sum up



#### Correct_summarise flux in each river ####

river_flux_each <- river_flux_GWP %>% # mg.m-2.d-1 not CO2 equivalent
    select(River, `Flux CO2`, `Flux CH4`, `Flux N2O`) %>%
    group_by(River) %>% summarise_each(funs = sum) %>%
    mutate_at(vars(`Flux CO2`, `Flux CH4`, `Flux N2O`), funs("percent" = ./sum(.)))
river_flux_each$Area <- c(95.92, 111.19, 138.98, 113.03, 113.81) # km2
river_flux_each <- river_flux_each %>% mutate_at(vars(`Flux CO2`, `Flux CH4`, `Flux N2O`), funs("area" = .*365/Area)) # g.year-1
river_flux_each <- river_flux_each %>% mutate_at(vars(`Flux CO2_area`, `Flux CH4_area`, `Flux N2O_area`), funs("percent" = .*100/ sum(.))) # percentage of area

river_flux_stacked <- river_flux_each[,c(1, 12:14)] %>% pivot_longer(cols = -River, names_to = "GHG", values_to = "Flux_area")
river_flux_stacked$GHG <- as.factor(river_flux_stacked$GHG)
river_flux_stacked$GHG <- relevel(river_flux_stacked$GHG,"Flux CO2_area_percent")
river_flux_stacked$GHG <- factor(river_flux_stacked$GHG, 
                                 labels = c(expression("Flux CO"["2"]), expression("Flux CH"["4"]), expression("Flux N"["2"]*"O")))

ggsave("Per_river_total_emissions.tiff", river_flux_stacked %>% ggplot() +
           geom_bar(aes(y=Flux_area, x=GHG,fill = `River`), stat = 'identity')+
           theme_bw() +
           # xlab("Year") +
           ylab("Fraction of the total emission per year (%)") +
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

#############################################WRONG################################
#### Wrong_Boxplot of not-corrected DG regarding rivers ####
# 
# river2 <- cbind(river[,3:5],river[,40:42])
# 
# ggsave("Boxplot_river_CO2.tiff", river2 %>% 
#            ggplot() +
#            geom_boxplot(aes(x = River, y = Dis_CO2_1)) +
#            xlab("River") +
#            ylab("CO2"))
# 
# ggsave("Boxplot_river_N2O.tiff", river2 %>% 
#            ggplot() +
#            geom_boxplot(aes(x = River, y = Dis_N2O_1)) +
#            xlab("River") +
#            ylab("N2O"))
# 
# ggsave("Boxplot_river_CH4.tiff", river2 %>% 
#            ggplot() +
#            geom_boxplot(aes(x = River, y = Dis_CH4_1)) +
#            xlab("River") +
#            ylab("CH4"))
# 
# ggsave("Boxplot_river_DG.tiff", river2 %>% gather(key = "Dissolved_gases", value = "Concentration", - Time, - Location, -River) %>% 
#            ggplot() +
#            geom_boxplot(aes(x = River, y = Concentration)) +
#            xlab("River") +
#            ylab("Dissolved gases (ppm)")+
#            facet_wrap(.~ as.factor(Dissolved_gases), scales = "free"),
#        units = 'cm', height = 20, width = 40, dpi = 300
#        )
#### Wrong_Boxplot of not-corrected DG in different positions in the river ####
# 
# river3 <- cbind(river_dis[,3:5],river_dis[,40:48])
# river3 <- river3 %>%  gather(key = "Dissolved_gases", value = "Concentration", - Time, - Location, -River)
# 
# # Boxplot in different positions 
# 
# ggsave("Boxplot_river_position_CO2.tiff", river3 %>% filter(str_detect(Dissolved_gases, "Dis_CO2_1|Dis_CO2_2|Dis_CO2_3")) %>% 
#            ggplot(aes(x = as.factor(Dissolved_gases), y = Concentration)) +
#            geom_bar(stat = "identity") +
#            xlab("Dissolved_gas") +
#            ylab("CO2 (ppm)") +
#            facet_wrap(.~Location),
#        units = 'cm', height = 20, width = 40, dpi = 300)
# 
# ggsave("Boxplot_river_position_CH4.tiff", river3 %>% filter(str_detect(Dissolved_gases, "Dis_CH4_1|Dis_CH4_2|Dis_CH4_3")) %>% 
#            ggplot(aes(x = as.factor(Dissolved_gases), y = Concentration)) +
#            geom_bar(stat = "identity") +
#            xlab("Dissolved_gas") +
#            ylab("CH4 (ppm)") +
#            facet_wrap(.~Location, scales = "free"),
#        units = 'cm', height = 20, width = 40, dpi = 300)
# 
# ggsave("Boxplot_river_position_N2O.tiff", river3 %>% filter(str_detect(Dissolved_gases, "Dis_N2O_1|Dis_N2O_2|Dis_N2O_3")) %>% 
#            ggplot(aes(x = as.factor(Dissolved_gases), y = Concentration)) +
#            geom_bar(stat = "identity") +
#            xlab("Dissolved_gas") +
#            ylab("N2O (ppm)") +
#            facet_wrap(.~Location, scales = "free"),
#        units = 'cm', height = 20, width = 40, dpi = 300)

#### Wrong_Boxplot of corrected DG regarding rivers ####

river2 <- cbind(river[,3:5],river[,40:42])

colnames(river2)[4:6] <- c("Dissolved N2O", "Dissolved CH4","Dissolved CO2")



ggsave("Boxplot_river_CO2_cor.tiff", river2 %>% 
           ggplot() +
           geom_boxplot(aes(x = River, y = Dis_CO2_cor)) +
           xlab("River") +
           ylab("CO2"))

ggsave("Boxplot_river_N2O_cor.tiff", river2 %>% 
           ggplot() +
           geom_boxplot(aes(x = River, y = Dis_N2O_cor)) +
           xlab("River") +
           ylab("N2O"))

ggsave("Boxplot_river_CH4_cor.tiff", river2 %>% 
           ggplot() +
           geom_boxplot(aes(x = River, y = Dis_CH4_cor)) +
           xlab("River") +
           ylab("CH4"))

ggsave("Boxplot_river_DG_cor.tiff", river2 %>% gather(key = "Dissolved_gases", value = "Concentration", - Time, - Location, -River) %>% 
           ggplot() +
           geom_boxplot(aes(x = River, y = Concentration)) +
           xlab("River") +
           ylab(bquote("Dissolved gases ("*mu~'g.'~L^-1*")"))+
           theme_bw()+
           facet_wrap(.~ as.factor(Dissolved_gases), scales = "free"),
       units = 'cm', height = 20, width = 40, dpi = 300
)


#### Wrong_Box plot of GWP regarding rivers ####

river$N2O_GWP <- river$`Dissovled N2O`*265
river$CH4_GWP <- river$`Dissovled CH4`*28
river$CO2_GWP <- river$`Dissolved CO2`*1

river5 <- cbind(river[,3:5],river[,43:45])
river5 <- river5 %>% gather(key = "Dissolved_gases", value = "GWP", - Time, - Location, -River)
river5$Dissolved_gases <- factor(river5$Dissolved_gases, levels = c('CO2_GWP','CH4_GWP','N2O_GWP'))

ggsave("Boxplot_GWP.tiff", river5 %>% 
           ggplot() +
           geom_boxplot(aes(x = River, y = GWP)) +
           xlab("River") +
           ylab("GWP (CO2-equivalent)")+
           facet_wrap(.~ as.factor(Dissolved_gases), scales = "free"),
       units = 'cm', height = 20, width = 40, dpi = 300
)

#### Wrong_Stacked plot of GWP regarding rivers #### 
river6 <- cbind(river[,3:5],river[,43:45]) %>% filter(River == "Cuenca") %>% select(c("N2O_GWP","CH4_GWP","CO2_GWP")) %>% colSums()
river7 <- transpose(as.data.frame(river6))

for (i in 2:(nlevels(river$River))){
    river6 <- cbind(river[,3:5],river[,43:45]) %>% filter(River == levels(river$River)[i]) %>% select(c("N2O_GWP","CH4_GWP","CO2_GWP")) %>% colSums()
    river7 <- rbind(river7,transpose(as.data.frame(river6))) 
}
colnames(river7) <- c("N2O_GWP","CH4_GWP","CO2_GWP")
river7$River <- levels(river$River)
river7 <- river7 %>% gather(key = "Dissolved_gases", value = "GWP", -River)
river7$Dissolved_gases <- as.factor(river7$Dissolved_gases)
ggsave("Stacked_Bar_GWP.tiff", river7 %>% ggplot(aes(x=River, y = GWP, fill = Dissolved_gases)) +
           geom_bar(stat= 'identity') +
           xlab("River")+
           ylab("GWP (CO2-equivalent)") + 
           guides(fill = guide_legend (title = "Dissolved gases")) + 
           theme_bw() +
           scale_fill_manual(values = colors[1:3],
                             labels= c("CH4", "CO2", "N2O")),
       units = 'cm', height = 20, width = 20, dpi = 300
)

#### Wrong_Permutation testing  ####
# # lack of data --> non parameteric analysis (not sure about the distribution of the data)
# # --> using Permanova for multivariate comparison for testing the simultaneous response of one or more variables to one or more 
# # factors in an ANOVA experimental design on the basis of any distance measure, using permutation methods
# # to accommodate random effects, hierarchical models, mixed models, quantitative covariates, 
# # repeated measures, unbalanced and/or asymmetrical designs, and, most recently, heterogeneous dispersions among groups.
# # or Fried.mann for univariate comparison repeated measures with block effects to avoid dependent samples.
# # Test the multivariate homogeneity of groups dispersions
# 
# mod <- betadisper(daisy(GHGes, metric = "euclidean", stand = TRUE), group = river$River) # using betadisper is a multivariate analogue of Levene's test for homogeneity of variances.
# permutest(mod)
# anova(mod)
# plot(mod, hull=FALSE, ellipse=TRUE)
# boxplot(mod)
# 
# # p value > 0.05 --> homogeneity of multivariate dispersions. 
# # PERMANOVA (likeANOVA) is very robust to heterogeneity for balanced designs but not unbalanced designs. 
# # Fortunately, in this case, it is homoegenous 
# 
# # using Permanova anyway
# 
# pairwise.adonis <- function(x,factors, sim.function = 'vegdist', sim.method = 'euclidean', p.adjust.m ='bonferroni'){
#     library(vegan)
#     
#     co = combn(unique(as.character(factors)),2)
#     pairs = c()
#     F.Model =c()
#     R2 = c()
#     p.value = c()
#     
#     for(elem in 1:ncol(co)){
#         if(sim.function == 'daisy'){
#             library(cluster)
#             x1 = daisy(x[factors %in% c(co[1,elem],co[2,elem]),],metric=sim.method)
#         } else {
#             x1 = vegdist(x[factors %in% c(co[1,elem],co[2,elem]),],method=sim.method)
#         }
#         
#         ad = adonis(x1 ~ factors[factors %in% c(co[1,elem],co[2,elem])] );
#         pairs = c(pairs,paste(co[1,elem],'vs',co[2,elem]));
#         F.Model =c(F.Model,ad$aov.tab[1,4]);
#         R2 = c(R2,ad$aov.tab[1,5]);
#         p.value = c(p.value,ad$aov.tab[1,6])
#     }
#     
#     p.adjusted = p.adjust(p.value,method=p.adjust.m)
#     sig = c(rep('',length(p.adjusted)))
#     sig[p.adjusted <= 0.05] <-'.'
#     sig[p.adjusted <= 0.01] <-'*'
#     sig[p.adjusted <= 0.001] <-'**'
#     sig[p.adjusted <= 0.0001] <-'***'
#     
#     pairw.res = data.frame(pairs,F.Model,R2,p.value,p.adjusted,sig)
#     print("Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1")
#     return(pairw.res)
#     
# } 
# 
# GHGes <- river[,46:48]
# GHGes_dis_matrix <- daisy(GHGes, metric = "euclidean", stand = TRUE)
# 
# set.seed(2805)
# 
# permanova_river_phys <- adonis(GHGes_dis_matrix ~ T_w + DO + pH + EC+ Sal + Turb + Chlr, 
#                                data = river, permutations = 999, 
#                                method = "euclidean", strata = river$River)
# summary(permanova_river_phys)
# permanova_river_phys
# permanova_river_phys$aov.tab[,6] 
# 
# pairwise.adonis(river[,6:13], river$River) #  physical
# 
# 
# # more about the 
# 

#### Wrong_WQI per river ####

river_WQI <- read_csv("River_WQI.csv")
river_WQI$Prati_WQI_1 <- as.factor(river_WQI$Prati_WQI_1)
river_WQI$Prati_WQI_2 <- as.factor(river_WQI$Prati_WQI_2)
river_WQI$`OWQI-2`<-as.factor(river_WQI$`OWQI-2`)

# Concentration (13/03)

river4 <- cbind(river_WQI[,5],river_WQI[,37:39], river_WQI[,43:47])
colnames(river4)[2:4] <- c("Dissolved N2O", "Dissolved CH4","Dissolved CO2")
river4 <- river4 %>% gather(key = "Dissolved_gases", value = "Concentration", 
                            -River, -Prati_INDEX_1,- Prati_WQI_1,- Prati_INDEX_2, -Prati_WQI_2, -`OWQI-2`)

river_GWP2_CO2 <- river4 %>% filter(Dissolved_gases == "Dissolved CO2") %>% select(Concentration)*1
river_GWP2_CH4 <- river4 %>% filter(Dissolved_gases == "Dissolved CH4") %>% select(Concentration)*28
river_GWP2_N2O <- river4 %>% filter(Dissolved_gases == "Dissolved N2O") %>% select(Concentration)*265

river_GWP2 <- bind_rows(river_GWP2_N2O, river_GWP2_CH4, river_GWP2_CO2)

river4$GWP <- as.numeric(unlist(river_GWP2))
river4$Dissolved_gases <- as.factor(river4$Dissolved_gases)


river4$Dissolved_gases <- relevel(river4$Dissolved_gases,"Dissolved CO2")
river4$Dissolved_gases <- factor(river4$Dissolved_gases, 
                                 labels = c(expression("Dissolved CO"["2"]), expression("Dissolved CH"["4"]), expression("Dissolved N"["2"]*"O")))
for(i in c(1,3,5,6)){
    river4[,i] <- as.factor(river4[,i])
}

river4$Prati_WQI_1 <- ordered(river4$Prati_WQI_1, labels= c("Good Quality", "Acceptable Quality","Polluted",
                                                            "Heavily Polluted", "Very Heavily Polluted"))
river4$Prati_WQI_2 <- ordered(river4$Prati_WQI_2, labels= c("Good Quality", "Acceptable Quality","Polluted",
                                                            "Heavily Polluted", "Very Heavily Polluted"))
river4$`OWQI-2` <- relevel(river4$`OWQI-2`, "Good")

ggsave("Boxplot_river_PWQI_GWP.tiff", river4 %>% ggplot() +
           geom_boxplot(aes(x = Prati_WQI_1, y = GWP, fill = Prati_WQI_1)) +
           # xlab("River") +
           ylab(bquote("GWP ("~CO[2]~"-equivalent )"))+
           theme_bw()+
           scale_fill_manual(values = c("blue","green","yellow","orange","red"))+
           theme(text=element_text(size=14),
                 strip.text.x = element_text(size=14),
                 axis.text.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.title.x = element_blank(),
                 legend.position="bottom",
                 legend.title = element_blank(),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm'))+
           facet_wrap(.~ Dissolved_gases, scales = "free", labeller = label_parsed), 
       units = 'cm', height = 20/1.1, width = 40/1.1, dpi = 300
)

ggsave("Boxplot_river_WQI_GWP_2.tiff", river4 %>% ggplot() +
           geom_boxplot(aes(x = Prati_WQI_2, y = GWP, fill = Prati_WQI_2)) +
           # xlab("River") +
           ylab(bquote("GWP ("~CO[2]~"-equivalent)"))+
           theme_bw()+
           scale_fill_manual(values = c("blue","green","yellow","orange","red"))+
           theme(text=element_text(size=14),
                 strip.text.x = element_text(size=14),
                 axis.text.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.title.x = element_blank(),
                 legend.position="bottom",
                 legend.title = element_blank(),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm'))+
           facet_wrap(.~ Dissolved_gases, scales = "free", labeller = label_parsed), 
       units = 'cm', height = 20, width = 40, dpi = 300
)

ggsave("Boxplot_river_OWQI_GWP.tiff", river4 %>% ggplot() +
           geom_boxplot(aes(x = river4$`OWQI-2`, y = GWP, fill = river4$`OWQI-2`),
                        # outlier.shape = NA
           ) +
           # xlab("River") +
           ylab(bquote("GWP ("~CO[2]~"-equivalent )"))+
           theme_bw()+
           scale_fill_manual(values = c("blue","green","yellow","orange","red"))+
           theme(text=element_text(size=14),
                 strip.text.x = element_text(size=14),
                 axis.text.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.title.x = element_blank(),
                 legend.position="bottom",
                 legend.title = element_blank(),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm'))+
           # scale_y_continuous(limits = quantile(river4$Concentration, c(0.1, 0.9)))+
           facet_wrap(.~ Dissolved_gases, scales = "free", labeller = label_parsed), 
       units = 'cm', height = 20/1.1, width = 40/1.1, dpi = 300
)

#### Wrong_stacked bar WQ ####

river4_per_Prati <- river4 %>% select(Prati_WQI_1, `OWQI-2`, `Dissolved_gases`, GWP)

river4_per_Prati$Prati_WQI_1 <- as.character(river4_per_Prati$Prati_WQI_1)
river4_per_Prati$Prati_WQI_1 <- str_replace_all(river4_per_Prati$Prati_WQI_1, "Very Heavily Polluted", "Heavily Polluted")
# river4_per_Prati$Prati_WQI_1 <- ordered(river4_per_Prati$Prati_WQI_1, 
#                                         labels= c("Good Quality", "Acceptable Quality",
#                                                   "Polluted", "Heavily Polluted"))
river4_per_Prati$`OWQI-2` <- as.character(river4_per_Prati$`OWQI-2`)
old_Ore <- c("Good","Fair", "Very Poor", "Poor")
new_Ore <- c("Good Quality", "Acceptable Quality", "Heavily Polluted", "Polluted")

for (i in 1:nrow(river4_per_Prati)){
    for (j in 1:length(new_Ore)){
        river4_per_Prati$`OWQI-2`[i] <- str_replace_all(river4_per_Prati$`OWQI-2`[i], old_Ore[j], new_Ore[j])
    }
}


colnames(river4_per_Prati) <- c("Prati Index", "Oregon Index", "Dissolved Gases", "Concentration")

river4_per_Prati <- river4_per_Prati %>% gather(key = "WQI", value = "Water Quality", - `Dissolved Gases`, - Concentration)


river4_per_Prati <- river4_per_Prati %>% group_by(`Dissolved Gases`, WQI, `Water Quality`) %>% 
    summarise(Concentration2 = sum(Concentration)) %>% 
    mutate(Percentage = Concentration2*100/sum(Concentration2))

river4_per_Prati$`Dissolved Gases` <- as.factor(river4_per_Prati$`Dissolved Gases`)
river4_per_Prati$WQI <- as.factor(river4_per_Prati$WQI)
river4_per_Prati$`Water Quality` <- as.factor(river4_per_Prati$`Water Quality`)
# river4_per_Prati$WQI <-ordered(river4_per_Prati$WQI,labels= c("Prati Index", "Oregon Index"))
river4_per_Prati$`Water Quality` <-ordered(river4_per_Prati$`Water Quality`,
                                           labels= c("Good Quality", "Acceptable Quality",
                                                     "Polluted", "Heavily Polluted", "Very Heavily Polluted"))

river4_2 <- river_WQI[,c(5, 44, 47:50)] 
river4_2$F_CH4 <- river4_2$F_CH4*28
river4_2$F_N2O <- river4_2$F_N2O*265
river4_2 <- river4_2 %>% pivot_longer(c(-River,-Prati_WQI_1, -`OWQI-2`), names_to = "Fluxes", values_to = "Concentration")
river4_2$Fluxes <- as_factor(river4_2$Fluxes)
river4_2 <- river4_2[,-1]
river4_2$`OWQI-2` <- as.character(river4_2$`OWQI-2`)

for (i in 1:nrow(river4_2)){
    for (j in 1:length(new_Ore)){
        river4_2$`OWQI-2`[i] <- str_replace_all(river4_2$`OWQI-2`[i], old_Ore[j], new_Ore[j])
    }
}


colnames(river4_2) <- c("Prati Index", "Oregon Index", "Fluxes", "Concentration")

river4_2 <- river4_2 %>% gather(key = "WQI", value = "Water Quality", - `Fluxes`, - Concentration)


river4_3 <- river4_2 %>% group_by(WQI, `Water Quality`, `Fluxes`) %>% 
    summarise(Concentration2 = sum(Concentration))
river4_3 <- river4_3 %>% group_by(WQI, `Fluxes`) %>% 
    mutate(Percentage = Concentration2*100/sum(Concentration2))

river4_3$`Fluxes` <- as.factor(river4_3$`Fluxes`)
river4_3$WQI <- as.factor(river4_3$WQI)
river4_3$`Water Quality` <- as.factor(river4_3$`Water Quality`)
river4_3$`Water Quality` <-ordered(river4_3$`Water Quality`,
                                   labels= c("Good Quality", "Acceptable Quality",
                                             "Polluted", "Heavily Polluted", "Very Heavily Polluted"))
river4_4 <- river4_2 %>% group_by( WQI, `Water Quality`, `Fluxes`) %>% 
    summarise(Concentration2 = mean(Concentration)) 
river4_4 <- river4_4 %>% group_by(WQI, `Fluxes`) %>% 
    mutate(Percentage = Concentration2*100/sum(Concentration2))

river4_4$`Fluxes` <- as.factor(river4_4$`Fluxes`)
river4_4$WQI <- as.factor(river4_4$WQI)
river4_4$`Water Quality` <- as.factor(river4_4$`Water Quality`)
river4_4$`Water Quality` <-ordered(river4_4$`Water Quality`,
                                   labels= c("Good Quality", "Acceptable Quality",
                                             "Polluted", "Heavily Polluted", "Very Heavily Polluted"))

river4_4$Fluxes <- factor(river4_4$Fluxes, 
                          labels = c(expression("CO"["2"]), expression("CH"["4"]), 
                                     expression("N"["2"]*"O")))

ggsave("Per_river_WQI.tiff", river4_per_Prati %>% 
           ggplot(aes(y=Percentage, x=`Dissolved Gases`,
                      fill = factor(`Water Quality`, 
                                    levels=  c("Good Quality", "Acceptable Quality",
                                               "Polluted", "Heavily Polluted", "Very Heavily Polluted")))) +
           geom_bar(stat = 'identity') +
           theme_bw() +
           labs(fill = "Prati Index") +
           ylab("Percentage of the GHG concentration (%)") +
           facet_grid(.~WQI) +
           scale_fill_manual(values = c("blue","green","yellow","orange","red"))+
           scale_x_discrete(labels =c(bquote("CO"[2]), bquote("CH"[4]), bquote("N"[2]*"O")))+
           theme(text=element_text(size=14),
                 strip.text.x = element_text(size=14),
                 axis.text.x = element_text(size = 14),
                 axis.ticks.x = element_blank(),
                 axis.title.x = element_blank(),
                 legend.position="bottom",
                 legend.title = element_text(size=14),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm')),
       units = 'cm', height = 20, width = 30, dpi = 300
)

river4_per_Oreg <- river4_per_Prati
river4_per_Oreg$`Water Quality` <-ordered(river4_per_Oreg$`Water Quality`,
                                          labels= c("Good ", "Fair",
                                                    "Poor", "Very Poor", "Very Heavily Polluted"))


ggsave("Oregon_river_WQI.tiff", river4_per_Oreg %>% 
           ggplot(aes(y=Percentage, x=`Dissolved Gases`,
                      fill = factor(`Water Quality`, 
                                    levels=  c("Good ", "Fair",
                                               "Poor", "Very Poor", "Very Heavily Polluted")))) +
           geom_bar(stat = 'identity') +
           theme_bw() +
           labs(fill = "Oregon Index") +
           ylab("Percentage of the GHG concentration (%)") +
           facet_grid(.~WQI) +
           scale_fill_manual(values = c("blue","green","yellow","orange","red"))+
           scale_x_discrete(labels =c(bquote("CO"[2]), bquote("CH"[4]), bquote("N"[2]~"O")))+
           theme(text=element_text(size=14),
                 strip.text.x = element_text(size=14),
                 axis.text.x = element_text(size = 14),
                 axis.ticks.x = element_blank(),
                 axis.title.x = element_blank(),
                 legend.position="bottom",
                 legend.title = element_text(size=14),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm')),
       units = 'cm', height = 20, width = 30, dpi = 300
)    

# Flux (13/03)


ggsave("Per_river_WQI_v2.tiff", river4_3 %>% 
           ggplot(aes(y=Percentage, x=`Fluxes`,
                      fill = factor(`Water Quality`, 
                                    levels=  c("Good Quality", "Acceptable Quality",
                                               "Polluted", "Heavily Polluted", "Very Heavily Polluted")))) +
           geom_bar(stat = 'identity') +
           theme_bw() +
           labs(fill = "Prati Index") +
           ylab("Fraction of the fluxes (%)") +
           facet_grid(.~WQI) +
           scale_fill_manual(values = c("blue","green","yellow","orange","red"))+
           scale_x_discrete(labels =c(bquote("CO"[2]), bquote("CH"[4]), bquote("N"[2]*"O")))+
           theme(text=element_text(size=14),
                 strip.text.x = element_text(size=14),
                 axis.text.x = element_text(size = 14),
                 axis.ticks.x = element_blank(),
                 axis.title.x = element_blank(),
                 legend.position="bottom",
                 legend.title = element_text(size=14),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm')),
       units = 'cm', height = 20, width = 30, dpi = 300
)

# Rate 15/03 - The rate should not be in percentage 



ggsave("Per_river_Prati_v2.tiff", river4_4 %>% filter(WQI == "Prati Index") %>% 
           ggplot(aes(y=Concentration2, x=`Water Quality`,
                      fill = factor(`Water Quality`, 
                                    levels=  c("Good Quality", "Acceptable Quality",
                                               "Polluted", "Heavily Polluted", "Very Heavily Polluted")))) +
           geom_bar(position="dodge", stat = 'identity') +
           theme_bw() +
           labs(fill = "Prati Index") +
           ylab("Mean of the fluxes (mg"~CO[2]*"-equivalent. "*m^-2*"."*d^-1*")") +
           facet_wrap(.~Fluxes, scales = "free", labeller = label_parsed) +
           scale_fill_manual(values = c("blue","green","yellow","orange","red"), name = "Prati/Oregon Index", 
                             labels = c("Good Quality/Good", "Acceptable Quality/Fair",
                                        "Polluted/Poor", "Heavily Polluted/Very Poor", "Very Heavily Polluted/NA") )+
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
       units = 'cm', height = 10, width = 30, dpi = 300
)

ggsave("Per_river_Oregon_v2.tiff", river4_4 %>% filter(WQI == "Oregon Index") %>% 
           ggplot(aes(y=Concentration2, x=`Water Quality`,
                      fill = factor(`Water Quality`, 
                                    levels=  c("Good Quality", "Acceptable Quality",
                                               "Polluted", "Heavily Polluted", "Very Heavily Polluted")))) +
           geom_bar(position="dodge", stat = 'identity') +
           theme_bw() +
           labs(fill = "Oregon Index") +
           ylab("Mean of the fluxes (mg"~CO[2]*"-equivalent. "*m^-2*"."*d^-1*")") +
           facet_wrap(.~Fluxes, scales = "free", labeller = label_parsed) +
           scale_fill_manual(values = c("blue","green","yellow","orange","red"), name = "Prati/Oregon Index", 
                             labels = c("Good Quality/Good", "Acceptable Quality/Fair",
                                        "Polluted/Poor", "Heavily Polluted/Very Poor", "Very Heavily Polluted/NA")) +
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
       units = 'cm', height = 10, width = 30, dpi = 300
)

river_flux_land_4 %>% ggplot(aes(x = `Land use`, y = Concentration2, fill = `Land use`)) +
    geom_bar(position="dodge", stat = 'identity') +
    ylab(bquote("Mean of the fluxes per land use category (mg"~CO[2]*"-equivalent. "*m^-2*"."*d^-1*")")) +
    facet_wrap(Bank ~ Fluxes, scales = "free"
               , labeller = labeller(Fluxes = label_parsed)
    )+
#### Wrong_Land-use per river ####
river_flux <- read_csv("River.csv")

river_flux$Date <- as.factor(river_flux$Date)
river_flux$Location <- as.factor(river_flux$Location)
river_flux$River <- as.factor(river_flux$River)
river_flux$LB <- as.factor(river_flux$LB)
river_flux$RB <- as.factor(river_flux$RB)
river_flux$Shading <- as.factor(river_flux$Shading)
river_flux$Erosion <- as.factor(river_flux$Erosion)
river_flux$Flow_variation <- as.factor(river_flux$Flow_variation)

# change the colnames

colnames(river_flux)[6:41] <- c("Water temperature", "pH", "DO", "EC", "TDS", "Salinity", "Turbility", "Chlorophyll", "Left Bank", 
                                "Right Bank", "Shading", "Erosion", "Flow variability", "Average depth", "Average velocity",
                                "Pool Class", "BOD", "COD", "TN" ,"NH4", "NO2", "NO3", "TP", "PO4", "Air temperature", "Wind velocity",
                                "Rain", "Solar radiation", "Latitude", "Longitude", "Dissolved N2O", "Dissolved CH4", "Dissolved CO2",
                                "Flux CO2", "Flux CH4", "Flux N2O")
# boxplot flux vs land use

river_flux_land <- cbind(river_flux[,5], river_flux[,14:15], river_flux[,39:41])
river_flux_land <- river_flux_land %>% gather(key = "Fluxes", value = "Concentration", 
                                              -River, -`Left Bank`,-`Right Bank`)
river_flux_land$Fluxes <- as.factor(river_flux_land$Fluxes)
river_flux_land$Fluxes <- relevel(river_flux_land$Fluxes,"Flux CO2")
river_flux_land$Fluxes <- factor(river_flux_land$Fluxes, 
                                 labels = c(expression("CO"["2"]), expression("CH"["4"]), expression("N"["2"]*"O")))

ggsave("Boxplot_river_Flux_LB.tiff", river_flux_land %>% ggplot() +
           geom_boxplot(aes(x = `Left Bank`, y = Concentration, fill = `Left Bank`)) +
           ylab(bquote("Fluxes ("*~mu~"mol."~m^{-2}~"."~d^{-1}~")"))+
           facet_wrap(.~ Fluxes, scales = "free", labeller = label_parsed)+
           theme_bw()+
           scale_fill_brewer(palette = "Paired")+
           theme(text=element_text(size=14),
                 strip.text.x = element_text(size=14),
                 axis.text.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.title.x = element_blank(),
                 legend.position="bottom",
                 legend.title = element_blank(),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm')), 
       units = 'cm', height = 20/1.1, width = 40/1.1, dpi = 300
)


ggsave("Boxplot_river_Flux_RB.tiff", river_flux_land %>% ggplot() +
           geom_boxplot(aes(x = `Right Bank`, y = Concentration, fill = `Right Bank`)) +
           ylab(bquote("Fluxes ("*~mu~"mol."~m^{-2}~"."~d^{-1}~")"))+
           facet_wrap(.~ Fluxes, scales = "free", labeller = label_parsed)+
           theme_bw()+
           scale_fill_brewer(palette = "Paired")+
           theme(text=element_text(size=14),
                 strip.text.x = element_text(size=14),
                 axis.text.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.title.x = element_blank(),
                 legend.position="bottom",
                 legend.title = element_blank(),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm')), 
       units = 'cm', height = 20/1.1, width = 40/1.1, dpi = 300
)

river_flux_land_2 <- river_flux_land[,-1] %>% gather(key = "Bank", value = "Land use", - Fluxes, - Concentration)
river_flux_land_2$Bank <- as.factor(river_flux_land_2$Bank)


old_lu <- as.character(levels(as.factor(river_flux_land_2$`Land use`)))
new_lu <- c("Agriculture", "Construction", "Industry", "Nature", "Industry", "Agriculture", "Urban",
            "Nature", "Road", "Urban")
for (i in 1:nrow(river_flux_land_2)){
    for (j in 1:length(new_lu)){
        river_flux_land_2$`Land use`[i] <- str_replace_all(river_flux_land_2$`Land use`[i], old_lu[j], new_lu[j])
    }
}
river_flux_land_2$`Land use` <- as.factor(river_flux_land_2$`Land use`)

ggsave("Boxplot_river_Flux_BB.tiff", river_flux_land_2 %>% ggplot() +
           geom_boxplot(aes(x = `Land use`, y = Concentration, fill = `Land use`)) +
           ylab(bquote("Fluxes ("*~mu~"mol."~m^{-2}~"."~d^{-1}~")"))+
           facet_wrap(Bank ~ Fluxes, scales = "free"
                      , labeller = labeller(Fluxes = label_parsed)
           )+
           theme_bw()+
           scale_fill_brewer(palette = "Paired")+
           theme(text=element_text(size=14),
                 strip.text.x = element_text(size=14),
                 axis.text.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.title.x = element_blank(),
                 legend.position="bottom",
                 legend.title = element_blank(),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm')), 
       units = 'cm', height = 20/1.1, width = 30/1.1, dpi = 300
)

# bar chart showing percentage

river_flux_land_3 <- river_flux_land_2 %>% group_by(Bank, `Land use`, Fluxes) %>% 
    summarise(Concentration2 = sum(Concentration))
river_flux_land_3 <- river_flux_land_3 %>% group_by(Bank, Fluxes) %>%     
    mutate(Percentage = Concentration2*100/sum(Concentration2))


ggsave("Per_river_Flux_BB_v2.tiff", river_flux_land_3 %>% ggplot() +
           geom_bar(aes(y=Percentage, x=Fluxes,fill = `Land use`), stat = 'identity')+
           theme_bw() +
           # xlab("Year") +
           ylab("Fraction of the fluxes (%)") +
           facet_grid(.~Bank) +
           scale_fill_brewer(palette = "Paired") +
           scale_x_discrete(labels =c(bquote("CO"[2]), bquote("CH"[4]), bquote("N"[2]*"O")))+
           theme(text=element_text(size=14),
                 strip.text.x = element_text(size=14),
                 axis.text.x = element_text(size = 14),
                 axis.ticks.x = element_blank(),
                 axis.title.x = element_blank(),
                 legend.position="bottom",
                 legend.title = element_text(size=14),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm')),
       units = 'cm', height = 20, width = 30, dpi = 300
)


#### Wrong_stacked bar land-use ####


river_conc_land <- cbind(river_flux[,5], river_flux[,14:15], river_flux[,36:38])
river_conc_land <- river_conc_land %>% gather(key = "DG", value = "Concentration", 
                                              -River, -`Left Bank`,-`Right Bank`)
river_conc_land$DG <- as.factor(river_conc_land$DG)
river_conc_land$DG <- relevel(river_conc_land$DG,"Dissolved CO2")
river_conc_land$DG <- factor(river_conc_land$DG, 
                             labels = c(expression("Dissolved CO"["2"]), expression("Dissolved CH"["4"]), 
                                        expression("Dissolved N"["2"]*"O")))

river_conc_land_2 <- river_conc_land[,-1] %>% gather(key = "Bank", value = "Land use", - DG, - Concentration)
river_conc_land_2$Bank <- as.factor(river_conc_land_2$Bank)


old_lu <- as.character(levels(as.factor(river_conc_land_2$`Land use`)))
new_lu <- c("Agriculture", "Construction", "Industry", "Nature", "Industry", "Agriculture", "Urban",
            "Nature", "Road", "Urban")
for (i in 1:nrow(river_conc_land_2)){
    for (j in 1:length(new_lu)){
        river_conc_land_2$`Land use`[i] <- str_replace_all(river_conc_land_2$`Land use`[i], old_lu[j], new_lu[j])
    }
}
river_conc_land_2$`Land use` <- as.factor(river_conc_land_2$`Land use`)

river_conc_land_3 <- river_conc_land_2 %>% group_by(DG, Bank, `Land use`) %>% 
    summarise(Concentration2 = sum(Concentration)) %>% 
    mutate(Percentage = Concentration2*100/sum(Concentration2))


ggsave("Per_river_Conc_BB.tiff", river_conc_land_3 %>% ggplot() +
           geom_bar(aes(y=Percentage, x=DG, fill = `Land use`), stat = 'identity')+
           theme_bw() +
           # xlab("Year") +
           ylab("Percentage of the GHG concentration (%)") +
           facet_grid(.~Bank) +
           scale_fill_brewer(palette = "Paired") +
           scale_x_discrete(labels =c(bquote("CO"[2]), bquote("CH"[4]), bquote("N"[2]*"O")))+
           theme(text=element_text(size=14),
                 strip.text.x = element_text(size=14),
                 axis.text.x = element_text(size = 14),
                 axis.ticks.x = element_blank(),
                 axis.title.x = element_blank(),
                 legend.position="bottom",
                 legend.title = element_text(size=14),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm')),
       units = 'cm', height = 20, width = 30, dpi = 300
)

summary(river_flux_land$`Right Bank`)/sum(summary(river_flux_land$`Right Bank`))
summary(river_flux_land$`Left Bank`)/sum(summary(river_flux_land$`Left Bank`))

#### Wrong_Area plot from source to Mouth ####

river_SM <- read_csv("River.csv")

river_SM$Date <- as.factor(river_SM$Date)
river_SM$Location <- as.factor(river_SM$Location)
river_SM$River <- as.factor(river_SM$River)
river_SM$LB <- as.factor(river_SM$LB)
river_SM$RB <- as.factor(river_SM$RB)
river_SM$Shading <- as.factor(river_SM$Shading)
river_SM$Erosion <- as.factor(river_SM$Erosion)
river_SM$Flow_variation <- as.factor(river_SM$Flow_variation)

# change the colnames

colnames(river_SM)[6:41] <- c("Water temperature", "pH", "DO", "EC", "TDS", "Salinity", "Turbility", "Chlorophyll", "Left Bank", 
                              "Right Bank", "Shading", "Erosion", "Flow variability", "Average depth", "Average velocity",
                              "Pool Class", "BOD", "COD", "TN" ,"NH4", "NO2", "NO3", "TP", "PO4", "Air temperature", "Wind velocity",
                              "Rain", "Solar radiation", "Latitude", "Longitude", "Dissolved N2O", "Dissolved CH4", "Dissolved CO2",
                              "Flux CO2", "Flux CH4", "Flux N2O")

# Using fluxes
river_SM2 <- river_SM[,c(5, 39:42)] 

# lump all rivers

river_SM2 <- river_SM2 %>% gather(key = "Fluxes", value = "Concentration", -River, -Distance)
# river_SM$Distance <- as.factor(river_SM$Distance)
river_SM2$Fluxes <- as.factor(river_SM2$Fluxes)
river_SM2 <- river_SM2 %>% 
    group_by(River, Fluxes, Distance) %>% 
    summarise(Concentration2 = sum(Concentration)) 
# river_SM2$Distance <- as.numeric(river_SM2$Distance)

ggsave("River_Flux_SM.tiff", river_SM2 %>% ggplot() +
           geom_area(aes(y=Concentration2, x=Distance,fill = `Fluxes`),
                     colour="black", size=.2, alpha=.8)+
           theme_bw() +
           xlab("Distance from the source (km)") +
           ylab(bquote("Fluxes ("*~mu~"mol."~m^{-2}~"."~d^{-1}~")"))+
           facet_grid(.~River, scales = "free") +
           scale_fill_manual(labels =c(bquote("CH"[4]), bquote("CO"[2]), bquote("N"[2]*"O")),
                             values=c("#1B9E77", "#D95F02", "#7570B3"))+
           theme(text=element_text(size=14),
                 strip.text.x = element_text(size=14),
                 axis.text.x = element_text(size = 11),
                 # axis.ticks.x = element_blank(),
                 # axis.title.x = element_blank(),
                 # legend.position="bottom",
                 legend.title = element_text(size=14),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm')),
       units = 'cm', height = 20/1.2, width = 40/1.2, dpi = 300
)

# Using fluxes in mg.m-2.d-1

river_SM3 <- river_flux_GWP[,c(5, 42:45)]

river_SM3$Distance[river_SM3$River == "Cuenca"] <- river_SM3$Distance[river_SM3$River == "Cuenca"] - 17.33

# lump all rivers

river_SM3 <- river_SM3 %>% gather(key = "Greenhouse gases", value = "Concentration", -River, -Distance)
river_SM3$Fluxes <- as.factor(river_SM3$`Greenhouse gases`)
river_SM3 <- river_SM3 %>% 
    group_by(River, `Greenhouse gases`, Distance) %>% 
    summarise(Concentration2 = sum(Concentration)) 

ggsave("River_Flux_SM.tiff", river_SM3 %>% ggplot() +
           geom_area(aes(y=Concentration2, x=Distance,fill = `Greenhouse gases`),
                     colour="black", size=.2, alpha=.8)+
           theme_bw() +
           xlab("Distance from the source (km)") +
           ylab(bquote("Fluxes (mg."~m^{-2}*"."~d^{-1}*")"))+
           facet_grid(.~River, scales = "free") +
           scale_fill_manual(labels =c(bquote("CH"[4]), bquote("CO"[2]), bquote("N"[2]*"O")),
                             values=c("#1B9E77", "#D95F02", "#7570B3"))+
           theme(text=element_text(size=14),
                 strip.text.x = element_text(size=14),
                 axis.text.x = element_text(size = 11),
                 # axis.ticks.x = element_blank(),
                 # axis.title.x = element_blank(),
                 # legend.position="bottom",
                 legend.title = element_blank(),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm')),
       units = 'cm', height = 20/1.2, width = 40/1.2, dpi = 300
)

# using DG with GWP

river_SM_DG <- river_SM[,c(5, 36:38, 42)] 
river_SM_DG$`Dissolved CH4` <- river_SM_DG$`Dissolved CH4`*28
river_SM_DG$`Dissolved N2O` <- river_SM_DG$`Dissolved N2O`*265


river_SM_DG <- river_SM_DG %>% gather(key = "Dissolved Gases", value = "Concentration", -River, -Distance)
# river_SM$Distance <- as.factor(river_SM$Distance)
river_SM_DG$`Dissolved Gases` <- as.factor(river_SM_DG$`Dissolved Gases`)
river_SM_DG <- river_SM_DG %>% 
    group_by(River, `Dissolved Gases`, Distance) %>% 
    summarise(Concentration2 = sum(Concentration)) 
# river_SM_DG$Distance <- as.numeric(river_SM_DG$Distance)

ggsave("River_DG_SM.tiff", river_SM_DG %>% ggplot() +
           geom_area(aes(y=Concentration2, x=Distance, fill = `Dissolved Gases`),
                     colour="black", size=.2, alpha=.8)+
           theme_bw() +
           xlab("Distance from the source (km)") +
           ylab(bquote("GWP ("~CO[2]~"-equivalent )"))+
           facet_grid(.~River, scales = "free") +
           scale_fill_manual(labels =c(bquote("CH"[4]), bquote("CO"[2]), bquote("N"[2]*"O")),
                             values=c("#1B9E77", "#D95F02", "#7570B3"))+
           theme(text=element_text(size=14),
                 strip.text.x = element_text(size=14),
                 axis.text.x = element_text(size = 11),
                 # axis.ticks.x = element_blank(),
                 # axis.title.x = element_blank(),
                 # legend.position="bottom",
                 legend.title = element_text(size=14),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm')),
       units = 'cm', height = 20/1.2, width = 40/1.2, dpi = 300
)

# using Nutrient 

river_SM_NU <- river_SM[,c(5, 24:29, 42)] 
river_SM_NU <- river_SM_NU %>% gather(key = "Nutrients", value = "Concentration", -River, -Distance)
# river_SM$Distance <- as.factor(river_SM$Distance)
river_SM_NU$`Nutrients` <- as.factor(river_SM_NU$`Nutrients`)
river_SM_NU <- river_SM_NU %>% 
    group_by(River, `Nutrients`, Distance) %>% 
    summarise(Concentration2 = sum(Concentration)) 
# river_SM_NU$Distance <- as.numeric(river_SM_NU$Distance)

ggsave("River_NU_SM.tiff", river_SM_NU %>% ggplot() +
           geom_area(aes(y=Concentration2, x=Distance, fill = `Nutrients`),
                     colour="black", size=.2, alpha=.8)+
           theme_bw() +
           xlab("Distance from the source (km)") +
           ylab(bquote("Concentration ("~mg.L^{-1}~")"))+
           facet_grid(.~River, scales = "free") +
           scale_fill_manual(labels =c(bquote("NH"[4]), bquote("NO"[2]), bquote("NO"[2]), 
                                       bquote("PO"[4]), "TN", "TP"),
                             values=c("#1B9E77", "#D95F02", "#7570B3","#E7298A", "#66A61E", "#E6AB02"))+
           theme(text=element_text(size=14),
                 strip.text.x = element_text(size=14),
                 axis.text.x = element_text(size = 11),
                 # axis.ticks.x = element_blank(),
                 # axis.title.x = element_blank(),
                 # legend.position="bottom",
                 legend.title = element_text(size=14),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm')),
       units = 'cm', height = 20/1.2, width = 40/1.2, dpi = 300
)

# using Nutrient and oxygen

river_SM_NU_2 <- river_SM[,c(5, 8, 24:29, 42)] 

river_SM_NU_2$Distance[river_SM_NU_2$River == "Cuenca"] <- river_SM_NU_2$Distance[river_SM_NU_2$River == "Cuenca"] -17.33

river_SM_NU_2 <- river_SM_NU_2[-c(1:4,10,18),]
river_SM_NU_2 <- river_SM_NU_2 %>% gather(key = "Nutrients", value = "Concentration", -River, -Distance)
# river_SM$Distance <- as.factor(river_SM$Distance)
river_SM_NU_2$`Nutrients` <- as.factor(river_SM_NU_2$`Nutrients`)
river_SM_NU_2 <- river_SM_NU_2 %>% 
    group_by(River, `Nutrients`, Distance) %>% 
    summarise(Concentration2 = sum(Concentration)) 
# river_SM_NU_2$Distance <- as.numeric(river_SM_NU_2$Distance)

ggsave("River_NU_SM_2.tiff", river_SM_NU_2 %>% ggplot() +
           geom_area(aes(y=Concentration2, x=Distance, fill = `Nutrients`),
                     colour="black", size=.2, alpha=.8)+
           theme_bw() +
           xlab("Distance from the source (km)") +
           ylab(bquote("Concentration ("*mg.L^{-1}*")"))+
           facet_grid(.~River, scales = "free") +
           scale_fill_manual(labels =c(bquote("O"[2]) , bquote("NH"[4]), bquote("NO"[2]), bquote("NO"[2]), 
                                       bquote("PO"[4]), "TN", "TP"),
                             values=c("#666666", "#1B9E77", "#D95F02", "#7570B3","#E7298A", "#66A61E", "#E6AB02"))+
           theme(text=element_text(size=14),
                 strip.text.x = element_text(size=14),
                 axis.text.x = element_text(size = 11),
                 # axis.ticks.x = element_blank(),
                 # axis.title.x = element_blank(),
                 # legend.position="bottom",
                 legend.title = element_blank(),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm')),
       units = 'cm', height = 20/1.2, width = 40/1.2, dpi = 300
)

# using Nutrient without TN and TPand oxygen

river_SM_NU_3 <- river_SM[,c(5, 8, 25:27,29, 42)] 
river_SM_NU_3$Distance[river_SM_NU_3$River == "Cuenca"] <- river_SM_NU_3$Distance[river_SM_NU_3$River == "Cuenca"] -17.33
river_SM_NU_3 <- river_SM_NU_3[-c(1:4,10,18),]
river_SM_NU_3 <- river_SM_NU_3 %>% gather(key = "Nutrients", value = "Concentration", -River, -Distance)
# river_SM$Distance <- as.factor(river_SM$Distance)
river_SM_NU_3$`Nutrients` <- as.factor(river_SM_NU_3$`Nutrients`)
river_SM_NU_3 <- river_SM_NU_3 %>% 
    group_by(River, `Nutrients`, Distance) %>% 
    summarise(Concentration2 = sum(Concentration)) 
# river_SM_NU_3$Distance <- as.numeric(river_SM_NU_3$Distance)

ggsave("River_NU_SM_3.tiff", river_SM_NU_3 %>% ggplot() +
           geom_area(aes(y=Concentration2, x=Distance, fill = `Nutrients`),
                     colour="black", size=.2, alpha=.8)+
           theme_bw() +
           xlab("Distance from the source (km)") +
           ylab(bquote("Concentration ("*mg.L^{-1}*")"))+
           facet_grid(.~River, scales = "free") +
           scale_fill_manual(labels =c(bquote("DO") , bquote("NH"[4]), bquote("NO"[2]), bquote("NO"[3]), 
                                       bquote("PO"[4])),
                             values=c("#666666", "#E7298A", "#66A61E", "#E6AB02", "#A6761D"))+
           theme(text=element_text(size=14),
                 strip.text.x = element_text(size=14),
                 axis.text.x = element_text(size = 11),
                 # axis.ticks.x = element_blank(),
                 # axis.title.x = element_blank(),
                 # legend.position="bottom",
                 legend.title = element_blank(),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm')),
       units = 'cm', height = 20/1.2, width = 40/1.2, dpi = 300
)

# using OM 

river_SM_OM <- river_SM[,c(5, 22,23, 42)] 
river_SM_OM <- river_SM_OM %>% gather(key = "Organic Matter", value = "Concentration", -River, -Distance)
# river_SM$Distance <- as.factor(river_SM$Distance)
river_SM_OM$`Organic Matter` <- as.factor(river_SM_OM$`Organic Matter`)
river_SM_OM <- river_SM_OM %>% 
    group_by(River, `Organic Matter`, Distance) %>% 
    summarise(Concentration2 = sum(Concentration)) 
# river_SM_OM$Distance <- as.numeric(river_SM_OM$Distance)

ggsave("River_OM_SM.tiff", river_SM_OM %>% ggplot() +
           geom_area(aes(y=Concentration2, x=Distance, fill = `Organic Matter`),
                     colour="black", size=.2, alpha=.8)+
           theme_bw() +
           xlab("Distance from the source (km)") +
           ylab(bquote("Concentration ("~mg.L^{-1}~")"))+
           facet_grid(.~River, scales = "free") +
           scale_fill_manual(labels =c("BOD", "COD"),
                             values=c("#1B9E77", "#D95F02"))+
           theme(text=element_text(size=14),
                 strip.text.x = element_text(size=14),
                 axis.text.x = element_text(size = 11),
                 # axis.ticks.x = element_blank(),
                 # axis.title.x = element_blank(),
                 # legend.position="bottom",
                 legend.title = element_text(size=14),
                 legend.text = element_text(size = 12),
                 legend.spacing.x = unit(0.5, 'cm')),
       units = 'cm', height = 20/1.2, width = 40/1.2, dpi = 300
)

