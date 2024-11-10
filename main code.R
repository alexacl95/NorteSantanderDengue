library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra) 
library(lubridate)
library(ggtext)
library(tsibble)
source("./auxiliar.R")

###################################################################
################# D. de S. general Information ####################
###################################################################

### Temperature range by subregion
map %>% 
  filter(temperatur > 0 ) %>% 
  group_by(subregion) %>% 
  summarise(min(temperatur), max(temperatur))

### Altura range by subregion
map %>% 
  filter(temperatur > 0 ) %>% 
  group_by(subregion) %>% 
  summarise(min(altura), max(altura))

### Polygons by subregion 
map_subregions <- map %>% 
  group_by(subregion) %>% 
  summarize(geometry = st_union(geometry), temperatur = mean(temperatur))

############## N. de S. in Colombia
ggplot(Colombia) + 
  geom_sf(color = "#b0b0b0", aes(fill = as.factor(id))) + 
  scale_fill_manual(values = c('#b0b0b0','#686868')) +
  theme_void() +
  theme(legend.position = "none")  +
  geom_text(data = data.frame("temp" = c(1)),
            aes(x = c(1109248), y = c(961222.6), label = c("COLOMBIA"))) +
  geom_text(data = data.frame("temp" = c(1)),
            aes(x = c(1309905), y = c(1413365), label = c("Norte de \n Santander")), size = 3)


############ Polygons by subregion of N. de S.
ggplot(
  map %>% 
    group_by(subregion) %>% 
    summarize(geometry = st_union(geometry), temperatur = mean(temperatur))
  ) +
  geom_sf(fill = "white", color = "#686868") + 
  geom_text(data = map_subregions %>%
              dplyr::group_by(data.frame(st_coordinates(st_centroid(map_subregions$geometry)))) %>%
              dplyr::summarise(x = mean(X), y = mean(Y), subregion = subregion),
            aes(x =x, y = y, label = subregion), size = 2.5, fontface = "bold") +
  theme_void()


################################################################################
################### SIVIGILA processed data ####################################
################################################################################

data <- read.csv('processed_data.csv') %>%
  mutate( consulting_time = as.numeric(ymd(fec_con_)-ymd(ini_sin_) ))

data[data$ajuste_ == unique(data$ajuste_)[2], "ajuste_"] = "Descartado"
data[data$ajuste_ == unique(data$ajuste_)[3], "ajuste_"] = "Descarte por error de digitacion"
data[data$ajuste_ == unique(data$ajuste_)[4], "ajuste_"] = "Confirmador por Nexo Epidemiol?gico"
data[data$ajuste_ == unique(data$ajuste_)[7], "ajuste_"] = "Confirmado por Cl?nica"

###Filtering data to extract only confirmed cases 

data <- data %>%
  filter(nom_eve == "DENGUE" | nom_eve == "DENGUE GRAVE")

df_confirm <- data %>%
  mutate(
    week = week(ymd(fec_con_)),
    year = year(ymd(ini_sin_))
         ) %>% 
  filter(
    ajuste_ == "Confirmado por Laboratorio" | 
    ajuste_ == "Confirmador por Nexo Epidemiol?gico" | 
    ajuste_ == "Confirmado por Cl?nica" 
    ) %>%
  filter(year >= 2015)

df_confirm <- df_confirm %>%
  filter(ndep_resi == "NORTE SANTANDER")

df_confirm[df_confirm$nmun_resi == "OCA?'A", "nmun_resi"] = "OCA?A"

#### To filter out columns with no data
for(name in names(df_confirm)){
  vac <- sum(df_confirm[name] == "")
  if(!is.na(vac) & vac == length(df_confirm$X)){
    df_confirm[name] <- NULL
  }else if(sum(is.na(df_confirm[name])== length(df_confirm$X))){
    df_confirm[name] <- NULL
  }
}

syntoms2correct <- c("cefalea", "dolrretroo", "mialgias", "artralgias", "erupcionr",
                     "dolor_abdo", "vomito", "diarrea", "somnolenci", "hipotensio",
                     "hepatomeg", "hem_mucosa", "hipotermia", "aum_hemato", "caida_plaq", 
                     "acum_liqui")

for (syntom in syntoms2correct){
  df_confirm[df_confirm[, syntom] == "", syntom] <- "No"
}

plot_list <- lapply(X = c(2015:2019, 99), FUN = graph)
# Using the cowplot package
plot_list[[6]] <- cowplot::get_legend(plot_list[[6]])
cowplot::plot_grid(plotlist = plot_list, ncol = 3)

################################################################################
################################# Cases time series ############################
################################################################################

df <- data.frame(table(df_confirm %>%
                         mutate(date_onset = yearweek(df_confirm$fec_con_)) %>%
                         mutate(year = year(df_confirm$fec_con_)) %>%
                         select(date_onset)))

xlabels <- sort(as.character(unique(df$date_onset)))
min <- min(xlabels)
max <- max(xlabels)
xlabels[-seq(25, length(xlabels), 45)] <- ""

ggplot(data = df) + 
  geom_line(aes(x = date_onset, y = Freq, group = 1)) +
  scale_x_discrete(labels = xlabels) +
  geom_line(aes(x = date_onset, y  = 0, group = 1), size = 0.1)#+
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_textbox(hjust = 0.5,vjust = 3, face="bold", color = "white",
                                     fill = "#000000", box.color = "#191B4F",
                                     padding = margin(1, 1, 1, 1)),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

################################################################################
#### Counting cases by subregion
### To map municipalities to subregions
df_mun2subreg <- data.frame(
  "nmun_resi" = map$municipio,
  "subregion" = map$subregion
)
df_mun2subreg$nmun_resi[39] <- "VILLA CARO"
df_mun2subreg[41,] <- c("", "ORIENTAL")

df_confirm <- dplyr::left_join(df_confirm, df_mun2subreg, by = "nmun_resi")
df_confirm$diff = as.integer(df_confirm$deterioration_time) - as.integer(df_confirm$consulting_time)

plot_list <- lapply(X = unique(df_confirm$nom_eve), FUN = graph2)
# Using the cowplot package
cowplot::plot_grid(plotlist = plot_list, ncol = 2)


################################################################################
### Filtering only the hospitalized cases
hosp_df <- df_confirm[!is.na(df_confirm$deterioration_time), ]
no_hosp_df <- df_confirm[is.na(df_confirm$deterioration_time), ]

####
table(df_confirm$sexo_, df_confirm$nom_eve)
#### Confirmed cases by event by subregion
table(df_confirm$subregion)
table(df_confirm$nom_eve)
table(hosp_df$subregion)
table(hosp_df$nom_eve)
table(hosp_df$subregion)/table(df_confirm$subregion)

#### Confirmed cases by event by subregion
table(df_confirm$nom_eve, df_confirm$subregion)

table(df_confirm$subregion)
#### Confirmed cases by year by subregion
table(df_confirm$subregion, df_confirm$year)
table(df_confirm$subregion, df_confirm$year)[4,]*100000/1032024

#### Confirmed cases by event by subregion Chikungunya
table(df_confirm$nom_eve, df_confirm$subregion)[1,]/table(df_confirm$subregion)
#### Confirmed cases by event by subregion Dengue
table(df_confirm$nom_eve, df_confirm$subregion)[2,]/table(df_confirm$subregion)
sum(table(hosp_df$nom_eve, hosp_df$subregion)[2,])/sum(table(df_confirm$nom_eve, df_confirm$subregion)[2,])
#### Confirmed cases by event by subregion Dengue Grave
table(df_confirm$nom_eve, df_confirm$subregion)[3,]/table(df_confirm$subregion)
sum(table(hosp_df$nom_eve, hosp_df$subregion)[3,])/sum(table(df_confirm$nom_eve, df_confirm$subregion)[3,])


df_confirm$gp_gestan[hosp_df$sexo_ == "M" & hosp_df$gp_gestan == "" ] <- "No"
table(df_confirm$gp_gestan, df_confirm$nom_eve)

#### Confirmed cases by event by subregion Zika
table(df_confirm$nom_eve, df_confirm$subregion)[4,]/table(df_confirm$subregion)

#### Hospitilized cases by event by subregion
table(hosp_df$nom_eve, hosp_df$subregion)
table(hosp_df$nom_eve)


table(hosp_df$estrato_)

#### Confirmed cases by event by edad
table(df_confirm$nom_eve)
table(df_confirm$Grupos.edad, df_confirm$nom_eve)
100 * table(df_confirm$Grupos.edad, df_confirm$nom_eve)/sum(table(df_confirm$Grupos.edad, df_confirm$nom_eve))
      
df_confirm[is.na(df_confirm$area_), "ocupacion_"] <- ""
table(df_confirm$ocupacion_, df_confirm$nom_eve)

df_confirm[is.na(df_confirm$ocupacion_), "ocupacion_"] <- ""
data.frame(table(df_confirm$ocupacion_, df_confirm$nom_eve)) %>%
  arrange(Var2, -Freq, Var1)

df_confirm[is.na(df_confirm$estrato_), "estrato_"] <- ""
table(df_confirm$estrato_, df_confirm$nom_eve)

table(df_confirm$gp_gestan, df_confirm$nom_eve)

table(df_confirm$desplazami, df_confirm$nom_eve)


df_confirm[is.na(df_confirm$per_etn_), "per_etn_"] <- ""
table(df_confirm$per_etn_, df_confirm$nom_eve)

#### Historicos
table(df_confirm$subregion, df_confirm$year)

hosp_df %>%
  dplyr::select(Grupos.edad, deterioration_time) %>%
  group_by(Grupos.edad) %>%
  summarise(
    mean = mean(deterioration_time),
    median = median(deterioration_time),
    perc25 = quantile(deterioration_time, 0.25),
    perc75 = quantile(deterioration_time, 0.75),
    n = n(),
    sd = sd(deterioration_time)
    )

hosp_df %>%
  dplyr::select(sexo_, deterioration_time) %>%
  group_by(sexo_) %>%
  summarise(
    mean = mean(deterioration_time),
    median = median(deterioration_time),
    perc25 = quantile(deterioration_time, 0.25),
    perc75 = quantile(deterioration_time, 0.75),
    n = n(),
    sd = sd(deterioration_time)
  )

#### Confirmed cases by edad by subregion
table(df_confirm$Grupos.edad, df_confirm$subregion)


################################################################################
####################### Extracting delay times #################################
################################################################################

####### Truncating delay times to maximum of 15 days
df_confirm <- df_confirm %>% 
  filter(consulting_time <= 15) %>% 
  filter(is.na(deterioration_time) | deterioration_time <= 15)

hosp_df <- hosp_df %>% 
  filter(consulting_time <= 15) %>% 
  filter(is.na(deterioration_time) | deterioration_time <= 15)

no_hosp_df <- no_hosp_df %>% 
  filter(consulting_time <= 15)

for(event in c("DENGUE", "DENGUE GRAVE")){#, "ZIKA", "CHIKUNGUNYA")){
  print(event)
  
  print( 
    hosp_df %>%
      filter(nom_eve==event & consulting_time <= 15) %>%
      summarise(mean(consulting_time), sd(consulting_time))
  )
  
  print( 
    hosp_df %>%
      filter(nom_eve==event & deterioration_time <= 15) %>%
      summarise(mean(deterioration_time), sd(deterioration_time))
  )
  
  print( 
    hosp_df %>%
      filter(nom_eve==event & deterioration_time <= 15) %>%
      summarise(mean(diff, na.rm = TRUE), sd(diff, na.rm = TRUE))
  )
  
  print("############################################################")
}



aggregate(consulting_time ~ nom_eve, data = df_confirm, FUN = mean)
aggregate(consulting_time ~ nom_eve, data = df_confirm, FUN = sd)

aggregate(deterioration_time ~ nom_eve, data = hosp_df, FUN = mean)
aggregate(deterioration_time ~ nom_eve, data = hosp_df, FUN = sd)


hist(as.numeric((hosp_df %>% filter((nom_eve=="DENGUE" | nom_eve=="DENGUE GRAVE") & as.integer(deterioration_time) <= 15))$deterioration_time))
#hist(as.numeric((hosp_df %>% filter(nom_eve=="ZIKA" & as.integer(consulting_time) <= 15))$consulting_time))
#hist(as.numeric((hosp_df %>% filter(nom_eve=="CHIKUNGUNYA"))$consulting_time))
#hist(as.numeric((hosp_df %>% filter((nom_eve=="ZIKA" | nom_eve=="CHIKUNGUNYA") & as.integer(consulting_time) <= 15))$consulting_time))

################################## Wilcoxon test #############################

####### DENV vs SEVERE DENV
wilcox.test(as.numeric((hosp_df %>% filter(nom_eve=="DENGUE"))$consulting_time),
            as.numeric((hosp_df %>% filter(nom_eve=="DENGUE GRAVE"))$consulting_time))

wilcox.test((hosp_df %>% filter(nom_eve=="DENGUE"))$deterioration_time,
            (hosp_df %>% filter(nom_eve=="DENGUE GRAVE"))$deterioration_time)

wilcox.test((hosp_df %>% filter(nom_eve=="DENGUE"))$diff,
            (hosp_df %>% filter(nom_eve=="DENGUE GRAVE"))$diff)
####### DENV vs ZIKV
wilcox.test(as.numeric((hosp_df %>% filter(nom_eve=="DENGUE"))$consulting_time),
            as.numeric((hosp_df %>% filter(nom_eve=="ZIKA"))$consulting_time))

wilcox.test((hosp_df %>% filter(nom_eve=="DENGUE"))$deterioration_time,
       (hosp_df %>% filter(nom_eve=="ZIKA"))$deterioration_time)

####### DENV vs CHIKV
wilcox.test(as.numeric((hosp_df %>% filter(nom_eve=="DENGUE"))$consulting_time),
            as.numeric((hosp_df %>% filter(nom_eve=="CHIKUNGUNYA"))$consulting_time))

wilcox.test((hosp_df %>% filter(nom_eve=="DENGUE"))$deterioration_time,
            (hosp_df %>% filter(nom_eve=="CHIKUNGUNYA"))$deterioration_time)

####### ZIKV vs CHIKV

wilcox.test(as.numeric((hosp_df %>% filter(nom_eve=="ZIKA"))$consulting_time),
            as.numeric((hosp_df %>% filter(nom_eve=="CHIKUNGUNYA"))$consulting_time))

wilcox.test((hosp_df %>% filter(nom_eve=="ZIKA"))$deterioration_time,
       (hosp_df %>% filter(nom_eve=="CHIKUNGUNYA"))$deterioration_time)

###### consulting time for hospitalized vs not hospitalized
wilcox.test(as.integer(hosp_df$consulting_time), 
            as.integer(no_hosp_df$consulting_time))

################################################################################
################################## For dengue ##################################
################################################################################
hosp_dengue_df <- df_confirm[!is.na(df_confirm$deterioration_time), ] %>%
  filter((nom_eve=="DENGUE" | nom_eve=="DENGUE GRAVE") & (consulting_time <= 15 | deterioration_time <= 15))

temporal <- is.na(df_confirm$deterioration_time)
no_hosp_dengue_df <- df_confirm %>%
  filter(is.na(deterioration_time))

wilcox.test(as.integer(no_hosp_dengue_df$consulting_time), as.integer(hosp_dengue_df$consulting_time), alternative = "two.sided")
wilcox.test(as.integer(hosp_dengue_df$deterioration_time), as.integer(hosp_dengue_df$consulting_time), alternative = "two.sided")

######## estimation for NOT hospitalized patients: consulting_time
hist(as.integer(no_hosp_dengue_df[, "consulting_time"]))
r1 <- delay_estimation(no_hosp_dengue_df[, "consulting_time"], 15, "Medical consultation time for not hospitalized patients")
r1$graph; r1$Weib_est$MAPE; r1$Lognorm_est$MAPE; r1$poiss_est$MAPE

######## estimation for hospitalized patients: consulting_time
hist(as.integer(hosp_dengue_df[, "consulting_time"]))
r2 <- delay_estimation(hosp_dengue_df[, "consulting_time"], 15, "Medical consultation time for hospitalized patients")
r2$graph; r2$Weib_est$MAPE; r2$Lognorm_est$MAPE; r2$poiss_est$MAPE

################ Deterioration time: symptoms onset to hospitalization ###################
hist(as.integer(hosp_dengue_df[, "deterioration_time"]))
r3 <- delay_estimation(hosp_dengue_df[, "deterioration_time"], 15, "Symptoms onset to hospitalization")
r3$graph; r3$Weib_est$MAPE; r3$Lognorm_est$MAPE; r3$poiss_est$MAPE

################ Lack time: consultation to hospitalization ###################
barplot(table(hosp_dengue_df$diff)/sum(table(hosp_dengue_df$diff)))

r4 <- delay_estimation(hosp_dengue_df[, "diff"], 8, "Medical consultation to hospitalization", "exp")
r4$graph; r4$Weib_est$MAPE; r4$Exp_est$MAPE; r4$poiss_est$MAPE

######################### dengue continuos estimation ###############################
x <- seq(0, 15, 0.1)
continuos_dengue <- data.frame("x" = c(x, x, x, x[-1:-4]), 
                  "fx" = c(dweibull(x, r1$Weib_est$shape, r1$Weib_est$scale), dweibull(x, r2$Weib_est$shape, r2$Weib_est$scale), dweibull(x, r3$Weib_est$shape, r3$Weib_est$scale), dweibull(x[-1:-4], r4$Weib_est$shape, r4$Weib_est$scale)),
                  "delayTime" = factor(c(rep("Consultation (Not hosp.)", length(x)), rep("Consultation (Hosp.)", length(x)), rep("Hospitalization", length(x)), rep("Cons. to hosp.", length(x[-1:-4]))),
                                       levels = c("Consultation (Not hosp.)", "Consultation (Hosp.)", "Hospitalization", "Cons. to hosp."))
  )

ggplot()  +
  geom_line(data = continuos_dengue, 
            aes(x = x,y = fx, linetype = delayTime, color = delayTime),  size = 1) +
  labs(title = "Weibull estimation for Dengue delay times",
       color = "delayTime", size = 0.7) +
  guides(color=guide_legend(nrow = 1,byrow=TRUE)) +
  theme(legend.title = element_blank(), legend.position = "top", 
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) + 
  xlab("Time (days)") +
  scale_colour_grey() +
  ylab(NULL) 

######################### dengue discrete estimation ###############################
x1 <- seq(0, 15)
discrete_dengue <- data.frame("x" = c(x1, x1, x1, x1[1:9]), 
                              "fx" = c(dpois(x1, r1$poiss_est$lambda), dpois(x1, r2$poiss_est$lambda), dpois(x1, r3$poiss_est$lambda), dpois(x1[1:9], r4$poiss_est$lambda)),
                              "Legend" = factor(c(rep("Consultation (Not hosp.)", length(x1)), rep("Consultation (Hosp.)", length(x1)), rep("Hospitalization", length(x1)), rep("Cons. to hosp.", length(x1[1:9]))),
                                                levels = c("Consultation (Not hosp.)", "Consultation (Hosp.)", "Hospitalization", "Cons. to hosp."))
)

ggplot()  +
  geom_point(data = discrete_dengue, 
             aes(x = x, y = fx, shape = Legend, color = Legend)) +
  labs(title = "Poisson estimation Dengue delay times",
       shape = "Legend") +
  guides(shape=guide_legend(nrow=2,byrow=TRUE)) +
  theme(legend.title = element_blank(), 
        legend.position = "top", 
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) + 
  scale_color_grey() +
  xlab("Time (days)") +
  ylab("Probability density")



################################################################################
################################## For ZIKV and CHIKV  ##################################
################################################################################

hosp_ZC_df <- df_confirm[!is.na(df_confirm$deterioration_time), ] %>%
  filter((nom_eve=="ZIKA" | nom_eve=="CHIKUNGUNYA") & consulting_time <= 15)
hist(as.integer(hosp_ZC_df$consulting_time))
hist(as.integer(hosp_ZC_df$deterioration_time))

no_hosp_ZC_df <- df_confirm[is.na(df_confirm$deterioration_time), ] %>%
  filter((nom_eve=="ZIKA" | nom_eve=="CHIKUNGUNYA") & consulting_time <= 15)
hist(as.integer(no_hosp_ZC_df$consulting_time))

wilcox.test(as.integer(no_hosp_ZC_df$consulting_time), as.integer(hosp_ZC_df$consulting_time), alternative = "two.sided")
wilcox.test(as.integer(hosp_ZC_df$deterioration_time), as.integer(hosp_ZC_df$consulting_time), alternative = "two.sided")

######## estimation 
r5 <- delay_estimation(no_hosp_ZC_df[, "consulting_time"], 15, "Symptoms onset to medical consultation \n not hospitalized patients", "exp")
r5$graph; r5$Weib_est$MAPE; r5$Exp_est$MAPE; r5$poiss_est$MAPE

r6 <- delay_estimation(hosp_ZC_df[, "consulting_time"], 14, "Symptoms onset to medical consultation \n hospitalized patients", "exp")
r6$graph; r6$Weib_est$MAPE; r6$Exp_est$MAPE; r6$poiss_est$MAPE

######################### all in one for ZIKV and CHIKV ###############################
x <- seq(0.1, 15, 0.1)
continuos_ZC <- data.frame(
  "x" = c(x, x),
  "fx" = c(dweibull(x, r5$Weib_est$shape, r5$Weib_est$scale), dweibull(x, r6$Weib_est$shape, r6$Weib_est$scale)),
  "Legend" = c(rep("Onset to cons. (Not hosp.)", length(x)), rep("Onset to cons. (Hosp.)", length(x)))
  )

ggplot()  +
  geom_line(
    data = continuos_ZC,
    aes(x = x, y = fx, color = Legend)
  )+
  labs(title = "Zika and Chikungunya delay times distribution \n Weibull estimation",
       shape = "Legend") +
  theme(
    legend.position = "top", 
    panel.background = element_blank(),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5)) + 
  xlab("Time (days)") +
  ylab("Probability density")
  
x1 <- seq(0, 15)
discrete_ZC <- data.frame(
  "x" = c(x1, x1),
  "fx" = c(dpois(x1, r5$poiss_est$lambda), dpois(x1, r6$poiss_est$lambda)),
  "Legend" = c(rep("Onset to cons. (Not hosp.)", length(x1)), rep("Onset to cons. (Hosp.)", length(x1)))
)

ggplot()  +
  geom_point(
    data = discrete_ZC,
    aes(x = x, y = fx, shape = Legend, color = Legend)
  )+
  labs(title = "Zika and Chikungunya delay times distribution \n Poisson estimation",
       shape = "Legend") +
  theme(
    legend.position = "top", 
    panel.background = element_blank(),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5)) + 
  xlab("Time (days)") +
  ylab("Probability density")

############
library(randomForest)
library(e1071)
library(caret)
library(nnet)

df_confirm$hosp <- 1
df_confirm[!is.na(df_confirm$deterioration_time), "hosp"] <- 0
df_confirm[is.na(df_confirm$estrato_), "estrato_"] <- 0

table(df_confirm$nom_eve)

df_confirm <- df_confirm %>%
#  filter(nom_eve != "ZIKA" & nom_eve != "CHIKUNGUNYA") %>%
  mutate(hosp = as.factor(hosp))



x_columns <- c("area_", "ocupacion_", "sexo_", "subregion", "Grupos.edad", "cefalea", "dolrretroo", "mialgias", "artralgias", "erupcionr", 
               "dolor_abdo", "vomito", "diarrea", "somnolenci", "hipotensio", 
               "hepatomeg", "hem_mucosa", "hipotermia", "aum_hemato", 
               "caida_plaq", "acum_liqui")

for (column in x_columns){
  print(unique(df_confirm[, column]))
  df_confirm[, column] <- as.factor(df_confirm[, column])
}

df_confirm$ocupacion_ <- as.character(df_confirm$ocupacion_)

df_confirm$ocupacion_[df_confirm$ocupacion_ == "Directores y gerentes"] <- "Other"
df_confirm$ocupacion_[df_confirm$ocupacion_ == "Oficiales, operarios, artesanos y oficios relacionados"] <- "Other"
df_confirm$ocupacion_[df_confirm$ocupacion_ == "Operadores de instalaciones y m?quinas y ensambladores"] <- "Other"
df_confirm$ocupacion_[df_confirm$ocupacion_ == "Profesionales, cient?ficos e intelectuales"] <- "Other"
df_confirm$ocupacion_[df_confirm$ocupacion_ == "T?cnicos y profesionales de nivel medio"] <- "Other"
df_confirm$ocupacion_[df_confirm$ocupacion_ == "Agricultores y trabajadores calificados agropecuarios, forestales y pesqueros"] <- "Other"
df_confirm$ocupacion_[df_confirm$ocupacion_ == "Personal de apoyo administrativo"] <- "Other"
df_confirm$ocupacion_[df_confirm$ocupacion_ == "Ocupaciones elementales"] <- "Other"

df_confirm$subregion <- as.character(df_confirm$subregion)

df_confirm$subregion[df_confirm$subregion == "NORTE"] <- "Other"
df_confirm$subregion[df_confirm$subregion == "SUR-OCCIDENTAL"] <- "Other"
df_confirm$subregion[df_confirm$subregion == "CENTRO"] <- "Other"
df_confirm$subregion[df_confirm$subregion == "SUR-ORIENTAL"] <- "Other"

df_confirm$Grupos.edad <- as.character(df_confirm$Grupos.edad )

df_confirm$Grupos.edad[df_confirm$Grupos.edad  == "Vejez"] <- "Other"
df_confirm$Grupos.edad[df_confirm$Grupos.edad  == "Juventud"] <- "Other"
df_confirm$Grupos.edad[df_confirm$Grupos.edad  == "Infancia"] <- "Other"
df_confirm$Grupos.edad[df_confirm$Grupos.edad  == "Adolescencia"] <- "Other"

error <- NULL; errorRF <- NULL; errorSVM <- NULL
MODELS <- list(); MODELSRF <- list(); MODELSSVM <- list()
MC <- list(); MCRF <- list(); MCSVM <- list()
N <- NULL; NRF <- NULL; NSVM <- NULL

f <- function(i, df_confirm, x_columns){
  smp_size <- floor(runif(1, 0.7, 0.8) * nrow(df_confirm))
  train_ind <- sample(seq_len(nrow(df_confirm)), size = smp_size, replace = FALSE)
  train <- df_confirm[train_ind, ]; test <- df_confirm[-train_ind, ]
  
  model <- step(glm("hosp ~ area_ + sexo_ + subregion + Grupos.edad + 
                     cefalea + dolrretroo + mialgias + artralgias + erupcionr +
                     dolor_abdo + vomito + diarrea + somnolenci + hipotensio + hepatomeg + 
                     hem_mucosa + hipotermia + aum_hemato + caida_plaq + acum_liqui",
                    data = train,
                    family = binomial(link = "logit")))
  summary(model)
  y_fit <- rep(0, length(test$X))
  y_fit[predict(model, test, type = "response")>=0.5] <- 1
  
  cM <- confusionMatrix(as.factor(test$hosp), as.factor(y_fit))
  error <- cM$table[1, 2] + cM$table[2, 1]
  MC <- cM
  MODELS <- model
  N <- smp_size
  
  model_RF <- randomForest(x = train[x_columns],
                          y = as.factor(train$hosp),
                          ntree = sample(50:250, 1))
  
  # Predicting the Test set results
  y_pred <- predict(model_RF, newdata = test[x_columns])
  
  # Confusion Matrix
  cM <- confusionMatrix(as.factor(test$hosp), y_pred)
  errorRF <- cM$table[1, 2] + cM$table[2, 1]
  MCRF <- cM
  MODELSRF <- model_RF
  NRF<- smp_size
  
  model_svm <- svm(hosp ~ area_ + ocupacion_ + sexo_ + subregion + Grupos.edad + cefalea + dolrretroo + mialgias + artralgias + erupcionr +
                     dolor_abdo + vomito + diarrea + somnolenci + hipotensio + hepatomeg + 
                     hem_mucosa + hipotermia + aum_hemato + caida_plaq + acum_liqui,
                   data = train)
  y_pred <- predict(model_svm, test[, x_columns])
  cM <- confusionMatrix(as.factor(test$hosp), y_pred)
  errorSVM <- cM$table[1, 2] + cM$table[2, 1]
  MCSVM <- cM
  MODELSSVM <- model_svm
  NSVM <- smp_size
  
  return(list("error" = error, "MC" = MC, "MODELS" = MODELS, "N" = N,
           "errorRF" = errorRF, "MCRF" = MCRF, "MODELSRF" = MODELSRF, "NRF" = NRF,
           "errorSVM" = errorSVM, "MCSVM" = MCSVM, "MODELSSVM" = MODELSSVM, "NSVM" = NSVM))
}

library(parallel)

cl <- makeCluster(detectCores() - 1)

clusterEvalQ(cl, {
  library(dplyr)
  library(ggplot2)
  library(grid)
  library(gridExtra) 
  library(lubridate)
  library(ggtext)
  library(tsibble)
  source("./auxiliar.R")
})

R <- NULL
for (i in 3:10){
  system.time(results <- parLapply(cl, 1:100, f, df_confirm, x_columns))
  R <- c(R, results)
}


stopCluster(cl)


for(i in 1:12){

  smp_size <- floor(runif(1, 0.7, 0.8) * nrow(df_confirm))
  train_ind <- sample(seq_len(nrow(df_confirm)), size = smp_size, replace = FALSE)
  train <- df_confirm[train_ind, ]; test <- df_confirm[-train_ind, ]
  
  model <- step(glm("hosp ~ area_ + sexo_ + subregion + Grupos.edad + 
                     cefalea + dolrretroo + mialgias + artralgias + erupcionr +
                     dolor_abdo + vomito + diarrea + somnolenci + hipotensio + hepatomeg + 
                     hem_mucosa + hipotermia + aum_hemato + caida_plaq + acum_liqui",
               data = train,
               family = binomial(link = "logit")))
  summary(model)
  y_fit <- rep(0, length(test$X))
  y_fit[predict(model, test, type = "response")>=0.5] <- 1
  
  cM <- confusionMatrix(as.factor(test$hosp), as.factor(y_fit))
  error[i] <- cM$table[1, 2] + cM$table[2, 1]
  MC[[i]] <- cM
  MODELS[[i]] <- model
  N[i]<- smp_size
  
  model_RF = randomForest(x = train[x_columns],
                               y = as.factor(train$hosp),
                               ntree = sample(50:250, 1))

  # Predicting the Test set results
  y_pred = predict(model_RF, newdata = test[x_columns])
  
  # Confusion Matrix
  cM = confusionMatrix(as.factor(test$hosp), y_pred)
  errorRF[i] <- cM$table[1, 2] + cM$table[2, 1]
  MCRF[[i]] <- cM
  MODELSRF[[i]] <- model_RF
  NRF[i]<- smp_size
  
  model_svm <- svm(hosp ~ area_ + ocupacion_ + sexo_ + subregion + Grupos.edad + cefalea + dolrretroo + mialgias + artralgias + erupcionr +
                     dolor_abdo + vomito + diarrea + somnolenci + hipotensio + hepatomeg + 
                     hem_mucosa + hipotermia + aum_hemato + caida_plaq + acum_liqui,
               data = train)
  y_pred <- predict(model_svm, test[, x_columns])
  cM = confusionMatrix(as.factor(test$hosp), y_pred)
  errorSVM[i] <- cM$table[1, 2] + cM$table[2, 1]
  MCSVM[[i]] <- cM
  MODELSSVM[[i]] <- model_svm
  NSVM[i]<- smp_size

  print(i)
}


lr_model = MODELS[seq(0,1000)[acc==max(acc[1:1000])]][[1]]
summary(lr_model)

columns_inte <- x_columns

library(class)
for(col in columns_inte){
  train[, col] <- as.factor(train[, col])
}
knn(train[, columns_inte], test[, columns_inte], cl = as.factor(train$hosp), k = 3, prob=TRUE)

worst <- seq(1, length(error))[error == max(error)]
a = MC[[worst]]$overall

i <- 1
acc <- NULL; Sensit <- NULL; Specif <- NULL; precision <- NULL; pos <- NULL; neg <- NULL; recall <- NULL; f1 <- NULL
accRF <- NULL; SensitRF <- NULL; SpecifRF <- NULL; precisionRF <- NULL; posRF <- NULL; negRF <- NULL; recallRF <- NULL; f1RF <- NULL
accSVM <- NULL; SensitSVM <- NULL; SpecifSVM <- NULL; precisionSVM <- NULL; posSVM <- NULL; negSVM <- NULL; recallSVM <- NULL; f1SVM <- NULL


for(x in MC){
  acc[i] <- x$overall[1]; accRF[i] <- MCRF[[i]]$overall[1]; accSVM[i] <- MCSVM[[i]]$overall[1]
  Sensit[i] <- x$byClass[1]; SensitRF[i] <- MCRF[[i]]$byClass[1]; SensitSVM[i] <- MCSVM[[i]]$byClass[1]
  Specif[i] <- x$byClass[2]; SpecifRF[i] <- MCRF[[i]]$byClass[2]; SpecifSVM[i] <- MCSVM[[i]]$byClass[2]
  pos[i] <- x$byClass[3]; posRF[i] <- MCRF[[i]]$byClass[3]; posSVM[i] <- MCSVM[[i]]$byClass[3]
  neg[i] <- x$byClass[4]; negRF[i] <- MCRF[[i]]$byClass[4]; negSVM[i] <- MCSVM[[i]]$byClass[4]
  precision[i] <- x$byClass[5]; precisionRF[i] <- MCRF[[i]]$byClass[5]; precisionSVM[i] <- MCSVM[[i]]$byClass[5]
  f1[i] <- x$byClass[7]; f1RF[i] <- MCRF[[i]]$byClass[7]; f1SVM[i] <- MCSVM[[i]]$byClass[7]
  i <- i + 1
}

df <- data.frame(
  "acc" = acc, "Sensit" = Sensit, "Specif" = Specif, "pos" = pos, "neg" = neg, "f1" = f1,
  "accRF" = accRF, "SensitRF" = SensitRF, "SpecifRF" = SpecifRF, "posRF" = posRF, "negRF" = negRF, "f1RF" = f1RF,
  "accSVM" = accSVM, "SensitSVM" = SensitSVM, "SpecifSVM" = SpecifSVM, "posSVM" = posSVM, "negSVM" = negSVM, "f1SVM" = f1SVM
)

lst <- list(
  "Accuracy" = df[, c("acc", "accRF", "accSVM")], 
  "Sensitivity" = df[, c("Sensit", "SensitRF", "SensitSVM")],
  "Specificity" = df[, c("Specif", "SpecifRF", "SpecifSVM")],
  "Positivity" = df[, c("pos", "posRF", "posSVM")],
  "Negativity" = df[, c("neg", "negRF", "negSVM")],
  #"Precision" = df[, c("precision", "precisionRF")],
  "F1" = df[, c("f1", "f1RF", "f1SVM")]
          )

library(ggplot2)
library(tidyr)
temp <- NULL
for (metric in names(lst)){
  
  results <- lst[[metric]] %>% 
    pivot_longer(
      everything(),
      names_to = c("type"),
      values_to = c("value")
    )
  results[["Model"]] <- "Logistic regression"
  results[results$type == names(lst[[metric]])[2], "Model"] <- "Random forest"
  results[results$type == names(lst[[metric]])[3], "Model"] <- "Support Vector \n Machine"
  results$tipo2 <- metric
  temp <- rbind(temp, results)
}

results$Model <- as.factor(results$Model)

ggplot(data = temp) +
  geom_boxplot(aes(y = value, x= tipo2, fill = Model)) +
  xlab("Score") + ylab("Metric") +
  theme(
    panel.background = element_blank(),
    legend.title = element_blank(),
    legend.position = "top",
  )


summary(df)

### Here we are interested in the positivity of the confution matrix because it 
### estimates from data the probability that a person who was detected as hospitalized case
### by the model in fact has to need hospitalized


shapiro.test(acc)
t.test(acc, accRF)

library(rstatix)
data.frame(acc = acc, accRF = accRF) %>% wilcox_test(acc ~ accRF)


lmo <- step(glm(
  "hosp ~ subregion + Grupos.edad + cefalea + dolor_abdo  + artralgias + 
    erupcionr + dolrretroo  + vomito + diarrea + somnolenci + 
    hem_mucosa + aum_hemato + cefalea * dolor_abdo  + artralgias * 
    erupcionr + dolrretroo  * vomito + diarrea * somnolenci + 
    hem_mucosa * aum_hemato + caida_plaq * hem_mucosa + dolor_abdo * diarrea", 
  data = train,
  family = binomial(link = "logit")
))
summary(lmo)
model <- step(lmo)
summary(model)

#############################################################################
################################################################################

library(caTools)
library(randomForest)

split <- sample.split(df_confirm, SplitRatio = 0.7)

train <- subset(df_confirm, split == "TRUE")
test <- subset(df_confirm, split == "FALSE")

# Fitting Random Forest to the train dataset
set.seed(120)  # Setting seed
x_columns <- c("sexo_", "ocupacion_", "area_", "Grupos.edad", "gp_gestan", 
               "cefalea", "dolrretroo", "mialgias", "artralgias", "erupcionr", 
               "dolor_abdo", "vomito", "diarrea", "somnolenci", "hipotensio", 
               "hepatomeg", "hem_mucosa", "hipotermia", "aum_hemato", 
               "caida_plaq", "acum_liqui", "extravasac", "hemorr_hem", "nom_eve")
classifier_RF = randomForest(x = train[x_columns],
                             y = as.factor(train$hosp),
                             ntree = 500)

classifier_RF

# Predicting the Test set results
y_pred = predict(classifier_RF, newdata = test[x_columns])

# Confusion Matrix
confusion_mtx = confusionMatrix(as.factor(test$hosp), y_pred)
confusion_mtx

# Plotting model
plot(classifier_RF)

# Importance plot
importance(classifier_RF)

# Variable importance plot
varImpPlot(classifier_RF)


################################################################################
library("survival")
library("survminer")

hosp_df$estrato_ <- as.factor(hosp_df$estrato_)
hosp_df$area_ <- as.factor(hosp_df$area_)
hosp_df$Grupos.edad <- as.factor(hosp_df$Grupos.edad)

hosp_dengue_df$Grupos.edad <- if_else(hosp_dengue_df$Grupos.edad == "Adultez", "Adultez", "noAdultez")
hosp_dengue_df$area_ <- if_else(hosp_dengue_df$area_ == "Rural disperso", "Rural disperso", "no Rural disperso")
hosp_dengue_df$subregion <- if_else(hosp_dengue_df$subregion == "OCCIDENTAL", "OCCIDENTAL", "OTHER")

res.cox <- coxph(Surv(deterioration_time) ~  area_ + ocupacion_ + sexo_ + subregion + Grupos.edad + cefalea + dolrretroo + mialgias + artralgias + erupcionr +
                   dolor_abdo + vomito + diarrea + somnolenci + hipotensio + hepatomeg + 
                   hem_mucosa + hipotermia + aum_hemato + caida_plaq + acum_liqui, data = hosp_dengue_df)
step(res.cox)

res.cox <- coxph(Surv(deterioration_time) ~  subregion + Grupos.edad + 
                   dolrretroo + artralgias + erupcionr + dolor_abdo + 
                   vomito + hem_mucosa + caida_plaq, 
                 data = hosp_dengue_df)
summary(res.cox)

df_confirm$Grupos.edad <- as.character(df_confirm$Grupos.edad)

df_confirm$Grupos.edad[df_confirm$Grupos.edad == "Infancia"] <- "Other"
df_confirm$Grupos.edad[df_confirm$Grupos.edad == "Juventud"] <- "Other"
df_confirm$Grupos.edad[df_confirm$Grupos.edad == "Adolescencia"] <- "Other"
df_confirm$Grupos.edad[df_confirm$Grupos.edad == "Vejez"] <- "Other"
df_confirm$Grupos.edad[df_confirm$Grupos.edad == "Primera infancia"] <- "Other"

res.cox2 <- coxph(Surv(consulting_time) ~  Grupos.edad + cefalea + dolrretroo + 
                    erupcionr + dolor_abdo + diarrea + somnolenci + hem_mucosa +
                    caida_plaq, 
                  data = df_confirm )

summary(res.cox2)

library(coxrobust)
res.coxr <- coxr(Surv(consulting_time) ~  subregion + Grupos.edad + 
                   cefalea + dolrretroo + erupcionr + dolor_abdo + vomito + 
                   diarrea + somnolenci + hepatomeg + hem_mucosa + caida_plaq, 
                 data = df_confirm )


summary(res.coxr)
res.coxr$wald.test
res.coxr$var.ple
res.coxr$coefficients


pwr.f2.test(u = 2, f2 = 0.26/(1 - 0.26), sig.level = 0.05, power = 0.8)
