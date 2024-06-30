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

#################
# BASE COMPLETA #
#################
adf_log_M <- tseries::adf.test(df$log_M)
adf_log_demandaGlobal <- tseries::adf.test(df$log_demandaGlobal)
adf_log_TCRM <- tseries::adf.test(df$log_TCRM)

adf_log_M_h2003 <- tseries::adf.test(dfh2003$log_M)
adf_log_demandaGlobal_h2003 <- tseries::adf.test(dfh2003$log_demandaGlobal)
adf_log_TCRM_h2003 <- tseries::adf.test(dfh2003$log_TCRM)

adf_log_M_d2004 <- tseries::adf.test(dfd2004$log_M)
adf_log_demandaGlobal_d2004 <- tseries::adf.test(dfd2004$log_demandaGlobal)
adf_log_TCRM_d2004 <- tseries::adf.test(dfd2004$log_TCRM)

p.values <- c(adf_log_M[["p.value"]],
              adf_log_demandaGlobal[["p.value"]],
              adf_log_TCRM[["p.value"]]
              ) 

p.values_h2003 <- c(adf_log_M_h2003[["p.value"]],
              adf_log_demandaGlobal_h2003[["p.value"]],
              adf_log_TCRM_h2003[["p.value"]]
)

p.values_d2004 <- c(adf_log_M_d2004[["p.value"]],
              adf_log_demandaGlobal_d2004[["p.value"]],
              adf_log_TCRM_d2004[["p.value"]]
)

variables_log <- c("log_M",
                   "log_demandaGlobal",
                   "log_TCRM"
                   )

tabla_ADF <- data.frame(variables_log, p.values)
tabla_ADF_h2003 <- data.frame(variables_log, p.values_h2003)
tabla_ADF_d2004 <- data.frame(variables_log, p.values_d2004)

tabla_ADF
tabla_ADF_h2003
tabla_ADF_d2004

#### PARA LAS DIFERENCIAS ####

adf_dlog_M <- tseries::adf.test(diff(df$log_M))
adf_dlog_demandaGlobal <- tseries::adf.test(diff(df$log_demandaGlobal))
adf_dlog_TCRM <- tseries::adf.test(diff(df$log_TCRM))

adf_dlog_M_h2003 <- tseries::adf.test(diff(dfh2003$log_M))
adf_dlog_demandaGlobal_h2003 <- tseries::adf.test(diff(dfh2003$log_demandaGlobal))
adf_dlog_TCRM_h2003 <- tseries::adf.test(diff(dfh2003$log_TCRM))

adf_dlog_M_d2004 <- tseries::adf.test(diff(dfd2004$log_M))
adf_dlog_demandaGlobal_d2004 <- tseries::adf.test(diff(dfd2004$log_demandaGlobal))
adf_dlog_TCRM_d2004 <- tseries::adf.test(diff(dfd2004$log_TCRM))

p.values_d <- c(adf_dlog_M[["p.value"]],
              adf_dlog_demandaGlobal[["p.value"]],
              adf_dlog_TCRM[["p.value"]]
) 

p.values_d_h2003 <- c(adf_dlog_M_h2003[["p.value"]],
                    adf_dlog_demandaGlobal_h2003[["p.value"]],
                    adf_dlog_TCRM_h2003[["p.value"]]
)

p.values_d_d2004 <- c(adf_dlog_M_d2004[["p.value"]],
                    adf_dlog_demandaGlobal_d2004[["p.value"]],
                    adf_dlog_TCRM_d2004[["p.value"]]
)

variables_log <- c("log_M",
                   "log_demandaGlobal",
                   "log_TCRM"
)

tabla_ADF_d <- data.frame(variables_log, p.values_d)
tabla_ADF_d_h2003 <- data.frame(variables_log, p.values_d_h2003)
tabla_ADF_d_d2004 <- data.frame(variables_log, p.values_d_d2004)

tabla_ADF_d
tabla_ADF_d_h2003
tabla_ADF_d_d2004

# rm(adf_log_X,
#    adf_log_M,
#    adf_log_PBI_Arg,
#    adf_log_PBI_Socios,
#    adf_log_TCRM,
#    adf_dlog_X,
#    adf_dlog_M,
#    adf_dlog_PBI_Arg,
#    adf_dlog_PBI_Socios,
#    adf_dlog_TCRM
# )

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