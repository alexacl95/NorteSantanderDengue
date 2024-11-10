library(randomForest)
library(coxrobust)
library(survminer)
library(gridExtra) 
library(lubridate)
library(survival)
library(rstatix)
library(ggplot2)
library(caTools)
library(tsibble)
library(ggtext)
library(dplyr)
library(tidyr)
library(e1071)
library(caret)
library(class)
library(nnet)
library(sf)

map <- st_read("mapa/geo_export_eea622b4-b79d-468a-a021-d74de8a5f0e7.shp")
map$municipio[1] = "ABREGO"; map$municipio[5] = "CACHIRA"; map$municipio[6] = "CACOTA"
map$municipio[7] = "CHINACOTA"; map$municipio[8] = "CONVENCION"; map$municipio[9] = "CUCUTA"
map$municipio[17] = "HERRAN"; map$municipio[24] = "OCAÑA"; map$municipio[36] = "TIBU"; map$municipio[40] = "CHITAGA"

Colombia <- st_read("mapa/colombia/depto.shp")
Colombia$id <- 0
Colombia[Colombia$NOMBRE_DPT == "NORTE DE SANTANDER", "id"] <- 1

graph <- function(año){  
  ### This function take a year a plot the # of cases by subregion in N. de S.
  position <- "none"
  
  if(año == 2019){
    año_title <- "2019-1Q"
  }else if(año == 99){
    año <- 2019
    año_title <- año # esta variable no importa pero si no esta no funciona la lógica
    position <- "bottom"
  }else{
    año_title <- año
  }
  
  df <- df_confirm %>%
    filter(year == año)
  
  data_by_mun <- table(df$nmun_resi)
  
  #### setting the number of cases to each municipality
  n_mun <- length(map$municipio)
  map$cases <- rep(NA, n_mun)
  for(i in 1:n_mun){
    map$cases[i] <- data_by_mun[map$municipio[i]]
  }
  
  ####################### Extracting centroids of municipalities to agglomerate into sub-regions
  centroids_df <- data.frame(st_coordinates(st_centroid(map$geometry))) ### extract coordinates
  centroids_df$mun <- map$municipio  ### Adding municipio
  centroids_df$subregion <- as.factor(map$subregion)  ### Adding subregion
  centroids_df$cases <- map$cases
  
  temp <- centroids_df[,c('mun', 'subregion')]
  names(temp) <- c("nmun_resi", "subregion")
  
  centroids_df <- centroids_df %>%
    dplyr::group_by(subregion) %>%
    dplyr::summarise(median(X), median(Y), sum(cases, na.rm = TRUE))
  
  names(centroids_df) <- c("subregion", "Longitud", "Latitud", "cases")
  centroids_df[centroids_df$subregion == "NORTE", "Longitud"] = centroids_df[centroids_df$subregion == "NORTE", "Longitud"] +0.05
  centroids_df$porc_cases <- paste(round(100 * centroids_df$cases/sum(centroids_df$cases), 1), "%")
  
  #### Ploting N. de S. with counted cases and the same as percentage
  ggplot() + 
    geom_sf(data = map, aes(fill = subregion), alpha = 0.3, linetype = 0) + 
    xlim(c(-73.63755, -72.01669))+
    ylim(c(6.874562, 9.293719)) +
    geom_text(data = centroids_df, aes(x = Longitud, y = Latitud + 0.08, label = cases), size = 3.2, col = "blue") +
    geom_text(data = centroids_df, aes(x = Longitud, y = Latitud - 0.08, label = porc_cases), size = 2.5) +
    labs(title = paste("N. de S. ", año_title) ) +
    theme_void() +
    theme(plot.caption = element_text(size = 3, face = "italic"),
          legend.position = position, legend.direction = "vertical")
}

graph2 <- function(nombre_eve){  
  df <- data.frame(table(df_confirm %>%
                           mutate(date_onset = yearweek(df_confirm$fec_con_)) %>%
                           mutate(year = year(df_confirm$fec_con_)) %>%
                           filter(nom_eve == nombre_eve) %>%
                           select(date_onset)))
  
  xlabels <- sort(as.character(unique(df$date_onset)))
  min <- min(xlabels)
  max <- max(xlabels)
  xlabels[-seq(25, length(xlabels), 45)] <- ""
  
  ggplot(data = df) + 
    geom_line(aes(x = date_onset, y = Freq, group = 1)) +
    ggtitle(nombre_eve) +
    scale_x_discrete(labels = xlabels) +
    geom_line(aes(x = date_onset, y  = 0, group = 1), size = 0.1)+
    theme(panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x=element_blank(),
          plot.title = element_textbox(hjust = 0.5,vjust = 3, face="bold", color = "white",
                                       fill = "#000000", box.color = "#191B4F"),
          legend.position = "none",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
}

delay_estimation <- function(delay_times, max_t, title, second = "lognormal"){
  
  results <- list()
  delay_times <- as.integer(delay_times)
  Fx <- ecdf(delay_times)
  
  results[["ECDF"]] <- Fx
  t <- seq(0.1, max_t-0.1, 0.1); p <- Fx(t)
  x <- log(t); y <- log(-log(1-p)) #
  weibull_model <- lm(y~x) ###
  t <- seq(0, max_t + 1, 0.1)
  beta0 <- weibull_model$coefficients[1]
  beta <- weibull_model$coefficients[2] #shape = beta
  nu <- exp(-beta0/beta) #scale = nu
  
  ts <- 0:max_t
  F_hat <- pweibull(ts, shape = beta, scale = nu)
  results[["Weib_est"]] <- list("t" = ts, "F_hat" = F_hat, "shape" = beta, "scale" = nu, "MAPE" = MAPE(Fx(seq(0, 15)), F_hat))
  
  #### Ajustando una distribución poisson con linealización de la distribución
  freqs <- table(delay_times)
  lambda <- sum(as.integer(names(freqs)) * freqs/sum(freqs))
  F_hat3 <- ppois(0:max_t, lambda)
  results[["poiss_est"]] <- list("t" = 0:max_t, "F_hat" = F_hat3, "lambda" = lambda, "MAPE" = MAPE(Fx(seq(0, 15)), F_hat3))
  
  g <- ggplot() +
    stat_ecdf(
      data = data.frame(
        "consulting_time" = delay_times
      ),
      aes(consulting_time,  color = "ECDF")
      ) + #linetype = "ECDF",
    geom_line(
      data = data.frame(
        "t_hat" = t,
        "F_hat" = ppois(t, lambda)), 
      aes(x = t_hat, y = F_hat,  color ="Poisson"), alpha = 0.5
    ) + #linetype = "Poisson",
    geom_point(
      data = data.frame(
        "t_hat" = ts,
        "F_hat" = ppois(ts, lambda)), 
      aes(x = t_hat, y = F_hat), col = "#999999"
    ) + 
    geom_line(
      data = data.frame(
        "t_hat" = t,
        "F_hat" = pweibull(t, shape = beta, scale = nu)), 
      aes(x = t_hat, y = F_hat,  color = "Weibull"), alpha = 0.5
    ) # linetype = "Weibull",
  
  if(second == "lognormal"){
    t <- seq(0.1, max_t-0.1, 0.1)
    ######## Ajustando una distribución lognormal con linealización de la distribución
    y <- qnorm(p)
    lognormal_model <- lm(y~t)
    
    coeff0 <- lognormal_model$coefficients[1]
    sigma <- 1/lognormal_model$coefficients[2]
    mu <- -sigma*coeff0
    
    F_hat2 <- plnorm(ts, meanlog = log(mu), sdlog = log(sigma))
    results[["Lognorm_est"]] <- list("t" = ts, "F_hat" = F_hat2, "meanlog" = log(mu), "sdlog" = log(sigma), "MAPE" = MAPE(Fx(seq(0, 15)), F_hat2))
    t <- seq(0, max_t + 1, 0.1)
    g <- g +
      geom_line(
        data = data.frame(
          "t_hat" = t,
          "F_hat" = plnorm(t, meanlog = log(mu), sdlog = log(sigma))), 
        aes(x = t_hat, y = F_hat,  color = "Lognormal"), alpha = 0.5
      ) + #linetype = "Lognormal",
      scale_colour_manual("", 
                          breaks = c("ECDF", "Poisson", "Weibull", "Lognormal"),
                          values = c("black", "#999999","#5F8D4E", "#2146C7"))
    
  }else if(second == "exp" | second == "exponential"){
    t <- seq(0.1, max_t-0.1, 0.1)
    ######## Ajustando una exponencial con linealización de la distribución
    y <- -log(1-p)
    exp_model <- lm(y~t)
    
    rate <- exp_model$coefficients[2]
    
    F_hat2 <- pexp(ts, rate = rate)
    results[["Exp_est"]] <- list("t" = ts, "F_hat" = F_hat2, "rate" = rate, "MAPE" = MAPE(Fx(seq(0, 15)), F_hat2))
    t <- seq(0, max_t + 1, 0.1)
    g <- g +
      geom_line(
        data = data.frame(
          "t_hat" = t,
          "F_hat" = pexp(t, rate = rate)
          ),
        aes(x = t_hat, y = F_hat,   color = "Exponential"), alpha = 0.5
      ) + #linetype = "Exponential",
      scale_colour_manual("", 
                          breaks = c("ECDF", "Poisson", "Weibull", "Exponential"),
                          values = c("black", "#999999","#5F8D4E", "#2146C7"))
    
  }else{
    print("Not second estimated distribution")
  }
  
  g <- g +
    theme(panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5)) + 
    labs(title = title,
         color = "Legend") +
    xlab("Time (days)") +
    ylab("Cumulative probability") +
    xlim(c(0, max_t + 0.2))
  
  results[["graph"]] <- g
  
  return(results)
}

MAPE <- function(actual, forecast){
  return(mean(abs((actual-forecast)/actual)) * 100)
}
