###############################
# Author: Tommaso Cancellario #
# Reviewer:                   #
# Creation: 2022 - 05 - 05    #
# Last update: 2022 - 05 - 30 #
###############################

# Libraries
library(ggplot2)
library(readxl)
library(vegan)

setwd("~/Desktop/")

# Load data
chl.uv_vis <- read.csv("./SIMILE/Data/UV-VIS.csv")
chl.hplc <- read.csv("./SIMILE/Data/HPLC.csv")
chl.lm1 <- read.csv("./SIMILE/Data/BUOY.csv")
chl.fprobe <- read.csv("./SIMILE/Data/FLUOROPROBE.csv")
chl.biovol <- read.csv("./SIMILE/Data/BIOVOL.csv")

head(chl.uv_vis)
# head(chl.hplc)
# head(chl.lm1)
# head(chl.fprobe)
# head(chl.biovol)


# Convert data in timeseries format
chl.uv_vis$data <- as.POSIXct(chl.uv_vis$data, tryFormats = "%m/%d/%Y"); str(chl.uv_vis) 
chl.hplc$data <- as.POSIXct(chl.hplc$data, tryFormats = "%m/%d/%Y"); str(chl.hplc) 
chl.lm1$data <- as.POSIXct(chl.lm1$data, tryFormats = "%m/%d/%Y"); str(chl.lm1) 
chl.fprobe$data <- as.POSIXct(chl.fprobe$data, tryFormats = "%m/%d/%Y"); str(chl.fprobe) 
chl.biovol$data <- as.POSIXct(chl.biovol$data, tryFormats = "%m/%d/%Y"); str(chl.biovol) 

##########
# UV-VIS #
##########
# Rows 1 and 3 are NA
chl.uv_vis <- chl.uv_vis[complete.cases(chl.uv_vis), ] # Remove NA

# Plot chl by deep "#00AFBB", "#E7B800", "#FC4E07"
# chl a
ggplot(chl.uv_vis, aes(x=data, y=Chl_a, color=prof)) +
  geom_point(alpha = .8) +
  geom_line(alpha = .4) +
  scale_color_manual(values = c("#E7B800", "#8A2BE2")) +
  scale_fill_manual(values = c("#E7B800", "#8A2BE2")) +
  geom_smooth(method = lm, se = FALSE) +
  xlab("") +  
  ggtitle("Chlorophyll a") +
  theme_bw()
# chl b
ggplot(chl.uv_vis, aes(x=data, y=Chl_b, color=prof)) +
  geom_point(alpha = .8) +
  geom_line(alpha = .4) +
  scale_color_manual(values = c("#E7B800", "#8A2BE2")) +
  scale_fill_manual(values = c("#E7B800", "#8A2BE2")) +
  geom_smooth(method = lm, se = FALSE) +
  xlab("") +  
  ggtitle("Chlorophyll b") +
  theme_bw()
# chl c
ggplot(chl.uv_vis, aes(x=data, y=Chl_c, color=prof)) +
  geom_point(alpha = .8) +
  geom_line(alpha = .4) +
  scale_color_manual(values = c("#E7B800", "#8A2BE2")) +
  scale_fill_manual(values = c("#E7B800", "#8A2BE2")) +
  geom_smooth(method = lm, se = FALSE) +
  xlab("") +  
  ggtitle("Chlorophyll c") +
  theme_bw()
# chl tot
ggplot(chl.uv_vis, aes(x=data, y=Chl_tot, color=prof)) +
  geom_point(alpha = .8) +
  geom_line(alpha = .4) +
  scale_color_manual(values = c("#E7B800", "#8A2BE2")) +
  scale_fill_manual(values = c("#E7B800", "#8A2BE2")) +
  geom_smooth(method = lm, se = FALSE) +
  xlab("") +  
  ggtitle("Chlorophyll tot") +
  theme_bw()

# Plot chl per type  
chl.type <- data.frame(data = rep(chl.uv_vis$data, times = 3),
                       deep = rep(chl.uv_vis$prof, times = 3),
                       chlType = rep(c("a", "b", "c"), each = nrow(chl.uv_vis)),
                       chlValue = c(chl.uv_vis$Chl_a, chl.uv_vis$Chl_b, chl.uv_vis$Chl_c))
head(chl.type)

ggplot(chl.type, aes(x=data, y=chlValue, color=chlType)) +
  geom_point(alpha = .8) +
  geom_line(alpha = .4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#8A2BE2")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#8A2BE2")) +
  geom_smooth(method = lm, se = FALSE) +
  xlab("") +  
  theme_bw() +
  ggtitle("Chlorophyll a-b-c") +
facet_wrap(~deep)


########
# HPLC #
########
# Rows 1 and 3 are NA
chl.hplc <- chl.hplc[complete.cases(chl.hplc), ] # Remove NA

# Check if dates are equal to UV-VIS
unique(chl.hplc$data == chl.uv_vis$data)

# Plot chl by deep
# chl tot
ggplot(chl.hplc, aes(x=data, y=Chl_tot, color=prof)) +
  geom_point(alpha = .8) +
  geom_line(alpha = .4) +
  scale_color_manual(values = c("#E7B800", "#8A2BE2")) +
  scale_fill_manual(values = c("#E7B800", "#8A2BE2")) +
  geom_smooth(method = lm, se = FALSE) +
  xlab("") +  
  ggtitle("Chlorophyll tot") +
  theme_bw()

# Plot chla - chla0 - chla1 - chladv - chlide_a - grazing
chl.type <- data.frame(date = rep(chl.hplc$data, times = 6),
                       deep = rep(chl.hplc$prof, times = 6),
                       chlType = rep(c("Chla", "Chla0", "Chla1", "ChlaDv", "Chlide_a", "Grazing"),
                                     each = nrow(chl.hplc)),
                       chlValue = c(chl.hplc$Chla, chl.hplc$Chla0, chl.hplc$Chla1,
                                    chl.hplc$ChlaDv, chl.hplc$Chlide_a, chl.hplc$Grazing))

chl.type <- chl.type[complete.cases(chl.type), ]
head(chl.type)

# ggplot(chl.type, aes(x=date, y=chlValue, color=chlType)) +
#   geom_point() +
#   geom_line() +
#   geom_smooth(method = lm, se = FALSE) +
#   xlab("") +
#   facet_wrap(~deep)

p <- ggplot(chl.type, aes(x = date, y = chlValue, group = chlType)) +
  theme_bw()
p + facet_grid(chlType ~ deep, scales="free") +
  geom_point(data = subset(chl.type, chlType == "Chla"), alpha = .8) +
  geom_point(data = subset(chl.type, chlType == "Chla0"), alpha = .8) +
  geom_point(data = subset(chl.type, chlType == "Chla1"), alpha = .8) +
  geom_point(data = subset(chl.type, chlType == "ChlaDv"), alpha = .8) +
  geom_point(data = subset(chl.type, chlType == "Chlide_a"), alpha = .8) +
  geom_point(data = subset(chl.type, chlType == "Grazing"), alpha = .8) +
  geom_line(data = subset(chl.type, chlType == "Chla"), alpha = .4) +
  geom_line(data = subset(chl.type, chlType == "Chla0"), alpha = .4) +
  geom_line(data = subset(chl.type, chlType == "Chla1"), alpha = .4) +
  geom_line(data = subset(chl.type, chlType == "ChlaDv"), alpha = .4) +
  geom_line(data = subset(chl.type, chlType == "Chlide_a"), alpha = .4) +
  geom_line(data = subset(chl.type, chlType == "Grazing"), alpha = .4) +
  ggtitle("Chlorophyll pigments") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = .5))
  

#######
# LM1 #
#######

# Rows 1 and 3 are NA
chl.lm1 <- chl.lm1[-c(1,3), ]

# Check if dates are equal to UV-VIS
unique(chl.lm1$data == chl.uv_vis$data)

# Convert Volt to chlorophyll
chl.lm1$chl <- chl.lm1$LM1buoy*9.02

# Plot chl by deep
# chl ?
ggplot(chl.lm1, aes(x=data, y=chl, color=prof)) +
  geom_point(alpha = .8) +
  geom_line(alpha = .4) +
  scale_color_manual(values = c("#E7B800", "#8A2BE2")) +
  scale_fill_manual(values = c("#E7B800", "#8A2BE2")) +
  geom_smooth(method = lm, se = FALSE) +
  xlab("") +  ylab("Chlorophyll") + 
  ggtitle("Chlorophyll") +
  theme_bw()


###############
# Fluoroprobe #
###############
# we can remove rows 1 and 3 because we removed them from the other files
chl.fprobe <- chl.fprobe[-c(1,3), ]

# Check if dates are equal to UV-VIS
unique(chl.fprobe$data == chl.uv_vis$data)
  
# Plot chl by deep
# chl green (chl a)
ggplot(chl.fprobe, aes(x = data, y = Green, color = prof)) +
    geom_point(alpha = .8) +
    geom_line(alpha = .4) +
    scale_color_manual(values = c("#E7B800", "#8A2BE2")) +
    scale_fill_manual(values = c("#E7B800", "#8A2BE2")) +
    geom_smooth(method = lm, se = FALSE) +
    xlab("") +
  ggtitle("Chlorophyll Green") +
    theme_bw()

# chl Bluegreen (chl b)
ggplot(chl.fprobe, aes(x = data, y = Bluegreen, color = prof)) +
  geom_point(alpha = .8) +
  geom_line(alpha = .4) +
  scale_color_manual(values = c("#E7B800", "#8A2BE2")) +
  scale_fill_manual(values = c("#E7B800", "#8A2BE2")) +
  geom_smooth(method = lm, se = FALSE) +
  xlab("") +
  ggtitle("Chlorophyll Bluegreen") +
  theme_bw()

# chl Diatoms (chl c)
ggplot(chl.fprobe, aes(x = data, y = Diatoms, color = prof)) +
  geom_point(alpha = .8) +
  geom_line(alpha = .4) +
  scale_color_manual(values = c("#E7B800", "#8A2BE2")) +
  scale_fill_manual(values = c("#E7B800", "#8A2BE2")) +
  geom_smooth(method = lm, se = FALSE) +
  xlab("") +
  ggtitle("Chlorophyll Diatoms") +
  theme_bw()

# chl Diatoms (chl Tot)
ggplot(chl.fprobe, aes(x = data, y = Chl_tot, color = prof)) +
  geom_point(alpha = .8) +
  geom_line(alpha = .4) +
  scale_color_manual(values = c("#E7B800", "#8A2BE2")) +
  scale_fill_manual(values = c("#E7B800", "#8A2BE2")) +
  geom_smooth(method = lm, se = FALSE) +
  xlab("") +
  ggtitle("Chlorophyll Tot") +
  theme_bw()

#############
# Biovolume #
#############
# we can remove rows 1 and 3 because we removed them from the other files
chl.biovol <- chl.biovol[-c(1,3), ]

# Check if dates are equal to UV-VIS
unique(chl.biovol$data == chl.uv_vis$data)

ggplot(chl.biovol, aes(x = data, y = chl.biovol$biov_tot, color = prof, fill = prof)) +
  geom_bar(stat = "identity") 

df.biovol <- chl.biovol[ ,c("data", "Cyanobacteria", "Bacillariophyceae", "Cryptophyceae", "Crysophyceae","Chlorophyta", "Dinophyceae")]

df.biovol <- tidyr::gather(df.biovol, chlGroup, chl, Cyanobacteria:Dinophyceae)
ggplot(df.biovol, aes(fill=chlGroup, y=chl, x=data)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Biovolumes %") +
  theme_bw()


# Biovolume vs fluoroprobe 
# Aggiungere HPLC nuovi valori Andrea

# Green (Fluoroprobe) = Chlorphyta (biovolume)    
bio.fluoro <- data.frame(data = chl.biovol$data,
                         method = rep("FLUOROPROBE", nrow(chl.biovol)),
                         chlType = rep("Chlorphyta", nrow(chl.biovol)),
                         chlValue = chl.fprobe$Green)
bio.fluoro <- rbind(bio.fluoro, data.frame(data = chl.biovol$data, 
                                method = rep("BIVOLUME", nrow(chl.biovol)),
                                chlType = rep("Chlorphyta", nrow(chl.biovol)),
                                chlValue = chl.biovol$Chlorophyta))
bio.fluoro <- rbind(bio.fluoro, data.frame(data = chl.biovol$data, 
                                           method = rep("HPLC", nrow(chl.biovol)),
                                           chlType = rep("Chlorphyta", nrow(chl.biovol)),
                                           chlValue = chl.hplc$Chloro_2))
  
# Bluegreen + Plantotrix (Fluoroprobe) = Cyanobacteria (biovolume)              
bio.fluoro <- rbind(bio.fluoro, data.frame(data = chl.biovol$data, 
                                           method = rep("FLUOROPROBE", nrow(chl.biovol)),
                                           chlType = rep("Cyanobacteria", nrow(chl.biovol)),
                                           chlValue = c(chl.fprobe$Bluegreen +
                                                        chl.fprobe$Planktothrix)))
bio.fluoro <- rbind(bio.fluoro, data.frame(data = chl.biovol$data, 
                                           method = rep("BIVOLUME", nrow(chl.biovol)),
                                           chlType = rep("Cyanobacteria", nrow(chl.biovol)),
                                           chlValue = chl.biovol$Cyanobacteria))
bio.fluoro <- rbind(bio.fluoro, data.frame(data = chl.biovol$data, 
                                           method = rep("HPLC", nrow(chl.biovol)),
                                           chlType = rep("Cyanobacteria", nrow(chl.biovol)),
                                           chlValue = chl.hplc$Ciano_2))

# Diatomee (Fluoroprobe) = Bacillariophicee + Crysophycea (biovolume)
bio.fluoro <- rbind(bio.fluoro, data.frame(data = chl.biovol$data, 
                                           method = rep("FLUOROPROBE", nrow(chl.biovol)),
                                           chlType = rep("Bacillariophicee + Crysophycea", nrow(chl.biovol)),
                                           chlValue = chl.fprobe$Diatoms))
bio.fluoro <- rbind(bio.fluoro, data.frame(data = chl.biovol$data, 
                                           method = rep("BIVOLUME", nrow(chl.biovol)),
                                           chlType = rep("Bacillariophicee + Crysophycea", nrow(chl.biovol)),
                                           chlValue = c(chl.biovol$Bacillariophyceae +
                                                        chl.biovol$Crysophyceae)))
bio.fluoro <- rbind(bio.fluoro, data.frame(data = chl.biovol$data, 
                                           method = rep("HPLC", nrow(chl.biovol)),
                                           chlType = rep("Bacillariophicee + Crysophycea", nrow(chl.biovol)),
                                           chlValue = chl.hplc$Diato_2))

# Cryptophyta (Fluoroprobe) = Cryptophyta + Dinophycea (biovolume)
bio.fluoro <- rbind(bio.fluoro, data.frame(data = chl.biovol$data, 
                                           method = rep("FLUOROPROBE", nrow(chl.biovol)),
                                           chlType = rep("Cryptophyta + Dinophycea", nrow(chl.biovol)),
                                           chlValue = chl.fprobe$Cryptophyta))
bio.fluoro <- rbind(bio.fluoro, data.frame(data = chl.biovol$data, 
                                           method = rep("BIVOLUME", nrow(chl.biovol)),
                                           chlType = rep("Cryptophyta + Dinophycea", nrow(chl.biovol)),
                                           chlValue = c(chl.biovol$Crysophyceae +
                                                        chl.biovol$Dinophyceae)))
bio.fluoro <- rbind(bio.fluoro, data.frame(data = chl.biovol$data, 
                                           method = rep("HPLC", nrow(chl.biovol)),
                                           chlType = rep("Cryptophyta + Dinophycea", nrow(chl.biovol)),
                                           chlValue = chl.hplc$Cripto_2))


head(bio.fluoro)

# Note we convert the cyl variable to a factor here in order to fill by cylinder
p <- ggplot(bio.fluoro, aes(x=data, y=chlValue, group=method)) +
  theme_bw()
p + facet_grid(method ~ chlType, scales="free") +
  geom_bar(data = subset(bio.fluoro, method == "FLUOROPROBE"), stat = "identity") +
  geom_bar(data = subset(bio.fluoro, method == "HPLC"), stat = "identity") +
  geom_bar(data = subset(bio.fluoro, method == "BIVOLUME"), stat = "identity") +
  #geom_bar(df.biovol, mapping=aes(fill=chlGroup, y=chl, x=data), position="stack", stat="identity")
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = .5))


#################################
# Plot with different equipment #
#################################

chl.eq <- data.frame(date = chl.uv_vis$data,
                     deep = chl.uv_vis$prof,
                     method = rep("UV-VIS", nrow(chl.uv_vis)),
                     chlTot = chl.uv_vis$Chl_tot)
df <- data.frame(date = chl.hplc$data,
                 deep = chl.hplc$prof,
                 method = rep("HPLC", nrow(chl.hplc)),
                 chlTot = chl.hplc$Chl_tot)
chl.eq <- rbind(chl.eq, df)
df <- data.frame(date = chl.fprobe$data,
                 deep = chl.fprobe$prof,
                 method = rep("FLUOROPROBE", nrow(chl.fprobe)),
                 chlTot = chl.fprobe$Chl_tot)
chl.eq <- rbind(chl.eq, df)
df <- data.frame(date = chl.biovol$data,
                 deep = chl.biovol$prof,
                 method = rep("BIOVOLUME", nrow(chl.biovol)),
                 chlTot = chl.biovol$biov_tot)
chl.eq <- rbind(chl.eq, df)
df <- data.frame(date = chl.lm1$data,
                 deep = chl.lm1$prof,
                 method = rep("BUOY", nrow(chl.lm1)),
                 chlTot = chl.lm1$chl)
chl.eq <- rbind(chl.eq, df); rm (df)


p <- ggplot(chl.eq, aes(x=date,y=chlTot, group=method)) +
     theme_bw()
p + facet_grid(method ~ deep, scales="free") +
  geom_line(data = subset(chl.eq, method == "UV-VIS")) +
  geom_line(data = subset(chl.eq, method == "HPLC")) +
  geom_line(data = subset(chl.eq, method == "FLUOROPROBE")) +
  geom_line(data = subset(chl.eq, method == "BUOY")) +
  geom_bar(data = subset(chl.eq, method == "BIOVOLUME"), stat = "identity") +
  #geom_bar(df.biovol, mapping=aes(fill=chlGroup, y=chl, x=data), position="stack", stat="identity")
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = .5))


#---------------#
# Linear models #
#---------------#
# UV-VIS vs Fluoroprobe (chl_tot)
# chl.eq.sup <- chl.eq[chl.eq$deep == "s",]
df.lm <- data.frame(uvVis = chl.eq$chlTot[chl.eq$method == "UV-VIS"],
                    hplc = chl.eq$chlTot[chl.eq$method == "HPLC"],
                    fluoroprobe = chl.eq$chlTot[chl.eq$method == "FLUOROPROBE"],
                    buoy = chl.eq$chlTot[chl.eq$method == "BUOY"]) 
head(df.lm)

# Histograms to check for the normal distribution of our variables
hist(df.lm$uvVis)
hist(df.lm$hplc)
hist(df.lm$fluoroprobe)
hist(df.lm$buoy)

# Rimuoviamo gli NA. Dobbiamo avere lo stesso numero di valori per tutti i metodo di misura
#df.ggplot <- df.ggplot[complete.cases(df.ggplot), ]


# UV-VIS vs HPLC (chl_tot)
model.1 = lm(uvVis ~ hplc, data = df.lm) 
summary(model.1)
par(mfrow=c(2,2))
plot(model.1)
dev.off()

plot(cooks.distance(model.1), pch = 16, col = "blue") # Check possible outliers. PRESENT (47)
performance::check_heteroscedasticity(model.1) # Detected
performance::check_model(model.1)

# To resolve heteroscedasticity we can use a weighted approach
# Tutorial: https://www.statology.org/weighted-least-squares-in-r/
plot(fitted(model.1), resid(model.1), xlab='Fitted Values', ylab='Residuals')
abline(0,0, col = "red") # “cone” shape


#perform weighted least squares regression
wt <- 1 / lm(abs(model.1$residuals) ~ model.1$fitted.values)$fitted.values^2
wls_model.1 <- lm(uvVis ~ hplc, data = df.lm, weights=wt)
summary(wls_model.1)
performance::check_heteroscedasticity(wls_model.1) # Detected
performance::check_model(wls_model.1) # BETTER!!!

Pvalue = pf(summary(model.1)$fstatistic[1],
            summary(model.1)$fstatistic[2],
            summary(model.1)$fstatistic[3],
            lower.tail = FALSE)

R2 = summary(model.1)$r.squared

t1 = paste0("p-value: ", signif(Pvalue, digits=3))
t2 = paste0("R-squared: ", signif(R2, digits=3))
t3 = paste0("Intercept: ", signif(coef(model.1)[1], digits=3))
t4 = paste0("Slope: ", signif(coef(model.1)[2], digits=3))

ggplot(df.lm, aes(x = uvVis, y = hplc)) +
  geom_smooth(method = lm, se = TRUE, colour="#E7B800", fill  = "#E7B800") +
  geom_point() +
  ggtitle("uvVis vs hplc") +
  theme_bw()


# HPLC vs Fluoroprobe (chl_tot)
model.2 = lm(fluoroprobe ~ hplc, data = df.lm) 
summary(model.2)
par(mfrow=c(2,2))
plot(model)
dev.off()

plot(cooks.distance(model.2), pch = 16, col = "blue") # Check possible outliers
performance::check_heteroscedasticity(model.2)
performance::check_model(model.2)

# Resolve heteroscedasticity
plot(fitted(model.2), resid(model.2), xlab='Fitted Values', ylab='Residuals')
abline(0,0, col = "red") # “cone” shape

#perform weighted least squares regression
wt <- 1 / lm(abs(model.2$residuals) ~ model.2$fitted.values)$fitted.values^2
wls_model.2 <- lm(fluoroprobe ~ hplc, data = df.lm, weights=wt)
summary(wls_model.2)
performance::check_heteroscedasticity(wls_model.2) # Detected
performance::check_model(wls_model.2) # BETTER!!!

Pvalue = pf(summary(model.2)$fstatistic[1],
            summary(model.2)$fstatistic[2],
            summary(model.2)$fstatistic[3],
            lower.tail = FALSE)

R2 = summary(model.2)$r.squared

t1 = paste0("p-value: ", signif(Pvalue, digits=3))
t2 = paste0("R-squared: ", signif(R2, digits=3))
t3 = paste0("Intercept: ", signif(coef(model.2)[1], digits=3))
t4 = paste0("Slope: ", signif(coef(model.2)[2], digits=3))

ggplot(df.lm, aes(x = fluoroprobe, y = hplc)) +
  geom_point() +
  geom_smooth(method = lm, se = TRUE, colour="#E7B800", fill  = "#E7B800") +
  ggtitle("fluoroprobe vs hplc") +
  theme_bw()

# UV-VIS vs Fluoroprobe (chl_tot)
model.3 = lm(uvVis ~ fluoroprobe, data = df.lm) 
summary(model.3)
par(mfrow=c(2,2))
plot(model.3)
dev.off()

plot(cooks.distance(model.3), pch = 16, col = "blue") # Check possible outliers
performance::check_heteroscedasticity(model.3)
performance::check_model(model.3)

Pvalue = pf(summary(model.3)$fstatistic[1],
            summary(model.3)$fstatistic[2],
            summary(model.3)$fstatistic[3],
            lower.tail = FALSE)

R2 = summary(model.3)$r.squared

t1 = paste0("p-value: ", signif(Pvalue, digits=3))
t2 = paste0("R-squared: ", signif(R2, digits=3))
t3 = paste0("Intercept: ", signif(coef(model.3)[1], digits=3))
t4 = paste0("Slope: ", signif(coef(model.3)[2], digits=3))

ggplot(df.lm, aes(x = uvVis, y = fluoroprobe)) +
  geom_point() +
  geom_smooth(method = lm, se = TRUE, colour="#E7B800", fill  = "#E7B800") +
  ggtitle("uvVis vs fluoroprobe") +
  theme_bw()

# If we use the data of the buoy, we have different NA compared to UV-VIS, HPLC, and FLUOROPROBE. So we need to have the same data.

# UV-VIS vs buoy (chl_tot)
df.lm.filtered <- df.lm[complete.cases(df.lm), ]

model.4 = lm(uvVis ~ buoy, data = df.lm.filtered) 
summary(model.4)
par(mfrow=c(2,2))
plot(model.4)
dev.off()

plot(cooks.distance(model.4), pch = 16, col = "blue") # Check possible outliers
performance::check_heteroscedasticity(model.4)
performance::check_model(model.4)

Pvalue = pf(summary(model.4)$fstatistic[1],
            summary(model.4)$fstatistic[2],
            summary(model.4)$fstatistic[3],
            lower.tail = FALSE)

R2 = summary(model.4)$r.squared

t1 = paste0("p-value: ", signif(Pvalue, digits=3))
t2 = paste0("R-squared: ", signif(R2, digits=3))
t3 = paste0("Intercept: ", signif(coef(model.4)[1], digits=3))
t4 = paste0("Slope: ", signif(coef(model.4)[2], digits=3))

ggplot(df.lm.filtered, aes(x = uvVis, y = buoy)) +
  geom_smooth(method = lm, se = TRUE, colour="#E7B800", fill  = "#E7B800") +
  geom_point() +
  ggtitle("uvVis vs buoy") +
  theme_bw()

# HPLC vs buoy (chl_tot)
model.5 = lm(hplc ~ buoy, data = df.lm.filtered) 
summary(model.5)
par(mfrow=c(2,2))
plot(model.5)
dev.off()

plot(cooks.distance(model.5), pch = 16, col = "blue") # Check possible outliers
performance::check_heteroscedasticity(model.5)
performance::check_model(model.5)


Pvalue = pf(summary(model.5)$fstatistic[1],
            summary(model.5)$fstatistic[2],
            summary(model.5)$fstatistic[3],
            lower.tail = FALSE)

R2 = summary(model.5)$r.squared

t1 = paste0("p-value: ", signif(Pvalue, digits=3))
t2 = paste0("R-squared: ", signif(R2, digits=3))
t3 = paste0("Intercept: ", signif(coef(model.5)[1], digits=3))
t4 = paste0("Slope: ", signif(coef(model.5)[2], digits=3))

ggplot(df.lm.filtered, aes(x = hplc, y = buoy)) +
  geom_smooth(method = lm, se = TRUE, colour="#E7B800", fill  = "#E7B800") +
  geom_point() +
  ggtitle("hplc vs buoy") +
  theme_bw()

# fluoroprobe vs buoy (chl_tot)
model.6 = lm(fluoroprobe ~ buoy, data = df.lm.filtered) 
summary(model.6)
par(mfrow=c(2,2))
plot(model.6)
dev.off()

plot(cooks.distance(model.6), pch = 16, col = "blue") # Check possible outliers
performance::check_heteroscedasticity(model.6)
performance::check_model(model.6)


# Resolve heteroscedasticity
plot(fitted(model.6), resid(model.6), xlab='Fitted Values', ylab='Residuals')
abline(0,0, col = "red") # “cone” shape

#perform weighted least squares regression
wt <- 1 / lm(abs(model.6$residuals) ~ model.6$fitted.values)$fitted.values^2
wls_model.6 <- lm(fluoroprobe ~ buoy, data = df.lm.filtered, weights=wt)
summary(wls_model.2)
performance::check_heteroscedasticity(wls_model.2) # Detected
performance::check_model(wls_model.2) # BETTER!!!

Pvalue = pf(summary(model.6)$fstatistic[1],
            summary(model.6)$fstatistic[2],
            summary(model.6)$fstatistic[3],
            lower.tail = FALSE)

R2 = summary(model.6)$r.squared

t1 = paste0("p-value: ", signif(Pvalue, digits=3))
t2 = paste0("R-squared: ", signif(R2, digits=3))
t3 = paste0("Intercept: ", signif(coef(model.6)[1], digits=3))
t4 = paste0("Slope: ", signif(coef(model.6)[2], digits=3))

ggplot(df.lm.filtered, aes(x = fluoroprobe, y = buoy)) +
  geom_smooth(method = lm, se = TRUE, colour="#E7B800", fill  = "#E7B800") +
  geom_point() +
  ggtitle("fluoroprobe vs buoy") +
  theme_bw()

########################
# Correlation analysis #
########################
cor(df.lm, use = "complete.obs", method = "spearman")
pairs(df.lm)

corrplot::corrplot(cor(df.lm, use = "complete.obs", method = "spearman"), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)




