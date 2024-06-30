rm(list = ls())

library(tidyverse) #Para manejar bases de datos
library(tsibble) #Para manejar bases para series de tiempo
library(ggplot2) #Para graficar
library(knitr) #Para tablas
library(fpp3) #No recuerdo para que era
library(vars)
library(tseries)
library(urca) #para el adf.test
library(forecast)#para el auto.arima
library(aTSA)
library(fable) #Para correr modelos de series de tiempo
library(strucchange) #Para el test de Chow

df <- readRDS("bases/base_tp2.RDS")
dfh2003 <- readRDS("bases/base_tp2_h2003.RDS")
dfd2004 <- readRDS("bases/base_tp2_d2004.RDS")

# Test de Chow ####
######################
resultados_M <- data.frame(Point = integer(), P_Value = numeric())

for (i in 1:nrow(df)) {
  test_result <- tryCatch({
    sctest(log_M ~ log_PBI_Arg + log_TCRM, type = "Chow", point = i, data = df)
  }, error = function(e) {
    return(list(p.value = NA))  # Return a list with NA p.value if there's an error
  })
  
  # Check if the p.value is NA or not
  if (!is.na(test_result$p.value)) {
    resultados_M <- rbind(resultados_M, data.frame(Point = i, P_Value = test_result$p.value))
  }
}

resultados_M_kable <- kable(resultados_M, caption = "P-Valores del Test Chow - Importaciones")

ggplot(resultados_M, aes(x = Point, y = P_Value)) +
  geom_point() +  
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  # scale_y_log10() +
  labs(title = "P-Valores del Test Chow - Importaciones",
       x = "",
       y = "P-Valor") +
  theme_minimal() 
######################
results_X <- data.frame(Point = integer(), P_Value = numeric())

for (i in 1:nrow(df)) {
  test_result <- tryCatch({
    sctest(log_X ~ log_PBI_Socios + log_TCRM, type = "Chow", point = i, data = df)
  }, error = function(e) {
    return(list(p.value = NA))  # Return a list with NA p.value if there's an error
  })
  
  # Check if the p.value is NA or not
  if (!is.na(test_result$p.value)) {
    results_X <- rbind(results_X, data.frame(Point = i, P_Value = test_result$p.value))
  }
}

kable(results_X, caption = "P-Values from Structural Change Test")

ggplot(results_X, aes(x = Point, y = P_Value)) +
  geom_point() +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  scale_y_log10() +
  labs(title = "P-Valores del Test Chow - Exportaciones",
       x = "Punto",
       y = "P-Valor") +
  theme_minimal() 

######################