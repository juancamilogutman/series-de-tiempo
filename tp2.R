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

# LIMPIEZA ####

# library(readxl)
# base_excel <- read_excel("TP 2 - Datos.xlsx")
# saveRDS(excel_data, "base_tp2.rds")

df <- readRDS("base_tp2.RDS")

df <- df %>%
  rename(
    Q = ...1,
    PBI_ARG = PIBARG,
    PBI_SOCIOS = PIBSOCIOS
  )

# Pequeña exploración
kable(head(df, 15))
kable(tail(df, 15))
names(df)

# Formato fechas:
df <- df %>%
  mutate(Q = yearquarter(Q))

df <- df %>% as_tsibble(index = Q)

#Las variables en logaritmo
df <- df %>% mutate(log_X = log(df$EXPO))
df <- df %>% mutate(log_M = log(df$IMPO))
df <- df %>% mutate(log_PBI_Arg = log(df$PBI_ARG))
df <- df %>% mutate(log_PBI_Socios = log(df$PBI_SOCIOS))
df <- df %>% mutate(log_TCRM = log(df$TCRM))

# combined_plot <- autoplot(df, log_X, color="Blue") +
#   labs(title = "Exportaciones e Importaciones Argentinas",
#        subtitle = "1996-2019",
#        y = "Logaritmo de Exportaciones", x= "Año y trimestre") + 
#   theme_minimal()
# 
# combined_plot <- combined_plot + 
#   autolayer(df, log_M, series = "Importaciones", colour="Red") +
#   labs(y = "Logaritmo de Exportaciones e Importaciones") +
#   theme_minimal()
# 
# print(combined_plot)
# 
# otro_autoplot <- autoplot(df, log_PBI_Socios, color="Blue") +
#   labs(title = "Exportaciones e Importaciones Argentinas",
#        subtitle = "1996-2019",
#        y = "Logaritmo de Exportaciones",
#        x= "Año y trimestre") + 
#   theme_minimal() 
# 
# otro_autoplot + autolayer(df, log_PBI_Arg, series = "Importaciones", colour="Red") +
#   labs(y = "Logaritmo de Exportaciones e Importaciones") +
#   theme_minimal()
# 
# df_long <- df %>%
#   pivot_longer(cols = c(log_X, log_PBI_Socios, log_TCRM), 
#                names_to = "series", 
#                values_to = "value")
# 
# ggplot(df_long, aes(x = Q, y = value, color = series)) +
#   geom_line() +
#   theme_minimal() +
#   labs(title = "Time Series Plot", x = "Quarter", y = "Log Value")
# 
# names(df)
# 
# 
# para_plot <- df[, c("Q", "log_M", "log_PBI_Arg", "log_TCRM"), drop = FALSE]
# 
# 
# autoplot(para_plot) +
#   facet_wrap(~ para_plot$Q) + # Replace key_variable with your actual key column
#   labs(x = "Año", y = "Variables", 
#        title = "Argentina: PIB, importaciones y tipo de cambio real") +
#   guides(colour=guide_legend(title="Variables"))
# 
# ggplot(df_post_2001_long, aes(x = Q, y = value, color = series)) +
#   geom_line() +
#   scale_x_yearquarter(date_breaks = "1 quarter") + 
#   theme_minimal() +
#   labs(title = "Time Series Plot", x = "Quarter", y = "Log Value")

# ARRANCA LO IMPORTANTE ####

#Hacemos los test de Dickey-Fuller y reportamos los resultados
#en una tabla
#reporta los p.values del test aumentado de dickey-fuller
#para las series originales y sus primeras diferencias.
#encontramos que todas tienen el mismo orden de integración.

adf_log_X <- tseries::adf.test(df$log_X)
adf_log_M <- tseries::adf.test(df$log_M)
adf_log_PBI_Arg <- tseries::adf.test(df$log_PBI_Arg)
adf_log_PBI_Socios <- tseries::adf.test(df$log_PBI_Socios)
adf_log_TCRM <- tseries::adf.test(df$log_TCRM)

p.values <- c(adf_log_X[["p.value"]],
              adf_log_M[["p.value"]],
              adf_log_PBI_Arg[["p.value"]],
              adf_log_PBI_Socios[["p.value"]],
              adf_log_TCRM[["p.value"]]
              ) 

variables_log <- c("log_X",
                   "log_M",
                   "log_PBI_Arg",
                   "log_PBI_Socios",
                   "log_TCRM"
                   ) 

tabla_ADF <- data.frame(variables_log, p.values)
tabla_ADF

adf_dlog_X <- tseries::adf.test(diff(df$log_X))
adf_dlog_M <- tseries::adf.test(diff(df$log_M))
adf_dlog_PBI_Arg <- tseries::adf.test(diff(df$log_PBI_Arg))
adf_dlog_PBI_Socios <- tseries::adf.test(diff(df$log_PBI_Socios))
adf_dlog_TCRM <- tseries::adf.test(diff(df$log_TCRM))

p_values_diff <- c(adf_dlog_X[["p.value"]],
                   adf_dlog_M[["p.value"]],
                   adf_dlog_PBI_Arg[["p.value"]],
                   adf_dlog_PBI_Socios[["p.value"]],
                   adf_dlog_TCRM[["p.value"]]) 

variables_dlog <- c("dlog_X",
                    "dlog_M",
                    "dlog_PBI_Arg",
                    "dlog_PBI_Socios",
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

X_impo <- cbind(df$log_PBI_Arg)
# X_impo <- cbind(df$log_PBI_Arg, df$log_TCRM)
coint.test(df$log_M, X_impo, d = 1, nlag = NULL, output = TRUE)
coint.test(df$log_M, X_impo, d = 0, nlag = NULL, output = TRUE)

# ECM IMPORTACIONES (bivariado, uniecuacional)
reg_coint_impo <- lm(log_M ~ log_PBI_Arg,data=df) #Relación de largo plazo
residuos_impo <- reg_coint_impo$residuals # Capturamos los residuos
tseries::adf.test(residuos_impo) # Vemos si el residuo es estacionario (OK)

residuos_impo_lag <- -lag(residuos_impo)[-1]
ecm_impo <- lm(diff(log_M) ~ residuos_impo_lag + diff(log_PBI_Arg), data = df)
summary(ecm_impo)

# ECM IMPORTACIONES (trivariado, uniecuacional)
reg_coint_impo_3 <- lm(log_M ~ log_PBI_Arg + log_TCRM,data=df)
residuos_impo_3 <- reg_coint_impo_3$residuals
tseries::adf.test(residuos_impo_3)

residuos_impo_3_lag <- -lag(residuos_impo_3)[-1]
ecm_impo <- lm(diff(log_M) ~ residuos_impo_3_lag + diff(log_PBI_Arg) + diff(log_TCRM), data = df)
summary(ecm_impo)

# Importaciones con Wicken y Breusch, 2 variables
log_M_lag <- lag(df$log_M)[-1]
log_PBI_Arg_lag <- lag(df$log_PBI_Arg)[-1]
wb_impo_2 <- lm(diff(log_M) ~ diff(log_PBI_Arg) + log_M_lag + log_PBI_Arg_lag, data = df)

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