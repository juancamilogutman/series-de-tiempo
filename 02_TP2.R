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

#Hacemos los test de Dickey-Fuller y reportamos los resultados
#en una tabla
#reporta los p.values del test aumentado de dickey-fuller
#para las series originales y sus primeras diferencias.
#encontramos que todas tienen el mismo orden de integración.

#################
# BASE COMPLETA #
#################

adf_log_X <- tseries::adf.test(df$log_X)
adf_log_M <- tseries::adf.test(df$log_M)
adf_log_PBI_Arg <- tseries::adf.test(df$log_PBI_Arg)
adf_log_PBI_Socios <- tseries::adf.test(df$log_PBI_Socios)
adf_log_TCRM <- tseries::adf.test(df$log_TCRM)
adf_log_demandaGlobal <- tseries::adf.test(df$log_demandaGlobal)

p.values <- c(adf_log_X[["p.value"]],
              adf_log_M[["p.value"]],
              adf_log_PBI_Arg[["p.value"]],
              adf_log_PBI_Socios[["p.value"]],
              adf_log_TCRM[["p.value"]],
              adf_log_demandaGlobal[["p.value"]]
              ) 

variables_log <- c("log_X",
                   "log_M",
                   "log_PBI_Arg",
                   "log_PBI_Socios",
                   "log_TCRM",
                   "log_demandaGlobal"
                   )

tabla_ADF <- data.frame(variables_log, p.values)
tabla_ADF

adf_dlog_X <- tseries::adf.test(diff(df$log_X))
adf_dlog_M <- tseries::adf.test(diff(df$log_M))
adf_dlog_PBI_Arg <- tseries::adf.test(diff(df$log_PBI_Arg))
adf_dlog_PBI_Socios <- tseries::adf.test(diff(df$log_PBI_Socios))
adf_dlog_TCRM <- tseries::adf.test(diff(df$log_TCRM))
adf_dlog_demandaGlobal <- tseries::adf.test(diff(df$log_demandaGlobal))

p_values_diff <- c(adf_dlog_X[["p.value"]],
                   adf_dlog_M[["p.value"]],
                   adf_dlog_PBI_Arg[["p.value"]],
                   adf_dlog_PBI_Socios[["p.value"]],
                   adf_dlog_TCRM[["p.value"]],
                   adf_dlog_demandaGlobal[["p.value"]]) 

variables_dlog <- c("dlog_X",
                    "dlog_M",
                    "dlog_PBI_Arg",
                    "dlog_PBI_Socios",
                    "dlog_TCRM",
                    "dlog_TCRM")

tabla_dADF <- data.frame(variables_dlog, p_values_diff)

tabla_ADF
tabla_dADF

rm(adf_log_X,
   adf_log_M,
   adf_log_PBI_Arg,
   adf_log_PBI_Socios,
   adf_log_TCRM,
   adf_dlog_X,
   adf_dlog_M,
   adf_dlog_PBI_Arg,
   adf_dlog_PBI_Socios,
   adf_dlog_TCRM
   )

# Chequeamos la estabilidad estructural de la función de importaciones
# TEST DE CHOW IMPO
sctest(log_M ~ log_PBI_Arg + log_TCRM, type="Chow", point=24, data=df)




# ECM IMPORTACIONES (bivariado, uniecuacional)
reg_coint_impo <- lm(log_M ~ log_PBI_Arg,data=df) #Relación de largo plazo
residuos_impo <- reg_coint_impo$residuals # Capturamos los residuos
tseries::adf.test(residuos_impo) # Vemos si el residuo es estacionario (OK)

residuos_impo_lag <- -lag(residuos_impo)[-1]
ecm_impo_2 <- lm(diff(log_M) ~ residuos_impo_lag + diff(log_PBI_Arg), data = df)
summary(ecm_impo_2)

# ECM IMPORTACIONES (trivariado, uniecuacional)
reg_coint_impo_3 <- lm(log_M ~ log_PBI_Arg + log_TCRM,data=df)
residuos_impo_3 <- reg_coint_impo_3$residuals
tseries::adf.test(residuos_impo_3)

residuos_impo_3_lag <- -lag(residuos_impo_3)[-1]
ecm_impo_3 <- lm(diff(log_M) ~ residuos_impo_3_lag + diff(log_PBI_Arg) + diff(log_TCRM), data = df)
summary(reg_coint_impo_3)
summary(ecm_impo_3)

# Importaciones con Wicken y Breusch, 2 variables
log_M_lag <- lag(df$log_M)[-1]
log_PBI_Arg_lag <- lag(df$log_PBI_Arg)[-1]
wb_impo_2 <- lm(diff(log_M) ~ diff(log_PBI_Arg) + log_M_lag + log_PBI_Arg_lag, data = df)
summary(wb_impo_2)

# Importaciones con Wicken y Breusch, 3 variables
log_M_lag <- lag(df$log_M)[-1]
log_PBI_Arg_lag <- lag(df$log_PBI_Arg)[-1]
wb_impo_3 <- lm(diff(log_M) ~ diff(log_PBI_Arg) + log_M_lag + log_PBI_Arg_lag, data = df)
summary(wb_impo_3)

# Faltaría chequear con el test de johansen:
# data_vecm <- df[, c("log_M", "log_TCRM", "log_PBI_Arg")]
# vecm <- ca.jo(data_vecm, type="trace", ecdet="const", K=2)
# summary(vecm)

# Para las exportaciones
# reg_coint_expo <- lm(log_X ~ log_PBI_Socios + log_TCRM,data=df)
# residuos_expo <- reg_coint_expo$residuals
# tseries::adf.test(residuos_expo)

# X_expo <- cbind(df$log_PBI_Socios, df$log_TCRM)
# coint.test(df$log_X, X_expo, d = 1, nlag = NULL, output = TRUE)
# coint.test(df$log_X, X_expo, d = 0, nlag = NULL, output = TRUE)