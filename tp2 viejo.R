#alt + guión
#control shift M es el pipe
#ctrl + alt + arriba crea cursores
#ctrl shift C comenta
#ctrl shift 1 y 2 

library(tidyverse) #Para manejar bases de datos
library(tsibble) #Para manejar bases para series de tiempo
library(fable) #Para correr modelos de series de tiempo
library(ggplot2) #Para graficar
library(knitr) #Para tablas
library(fpp3) #No recuerdo para que era
library(vars)
library(urca) #para el adf.test
library(tseries)
library(ggplot2)
library(forecast)#para el auto.arima 


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

names(df)


combined_plot <- autoplot(df, log_X, color="Blue") +
  labs(title = "Exportaciones e Importaciones Argentinas",
       subtitle = "1996-2019",
       y = "Logaritmo de Exportaciones", x= "Año y trimestre") + 
  theme_minimal()

combined_plot <- combined_plot + 
  autolayer(df, log_M, series = "Importaciones", colour="Red") +
  labs(y = "Logaritmo de Exportaciones e Importaciones") +
  theme_minimal()

print(combined_plot)


# ARRANCA LO IMPORTANTE ####

#Hacemos los test de Dickey-Fuller y reportamos los resultados
#en una tabla
#reporta los p.values del test aumentado de dickey-fuller
#para las series originales y sus primeras diferencias.
#encontramos que todas tienen el mismo orden de integración.

adf_log_X <- adf.test(df$log_X)
adf_log_M <- adf.test(df$log_M)
adf_log_PBI_Arg <- adf.test(df$log_PBI_Arg)
adf_log_PBI_Socios <- adf.test(df$log_PBI_Socios)
adf_log_TCRM <- adf.test(df$log_TCRM)

adf_log_X[["p.value"]]
adf_log_M[["p.value"]]
adf_log_PBI_Arg[["p.value"]]
adf_log_PBI_Socios[["p.value"]]
adf_log_TCRM[["p.value"]]

p.values <- c(adf_log_X[["p.value"]],
              adf_log_M[["p.value"]],
              adf_log_PBI_Arg[["p.value"]],
              adf_log_PBI_Socios[["p.value"]],
              adf_log_TCRM[["p.value"]]) 

variables_log <- c("log_X",
              "log_M",
              "log_PBI_Arg",
              "log_PBI_Socios",
              "log_TCRM") 

tabla_ADF <- data.frame(variables_log, p.values)
tabla_ADF

adf_dlog_X <- adf.test(diff(df$log_X))
adf_dlog_M <- adf.test(diff(df$log_M))
adf_dlog_PBI_Arg <- adf.test(diff(df$log_PBI_Arg))
adf_dlog_PBI_Socios <- adf.test(diff(df$log_PBI_Socios))
adf_dlog_TCRM <- adf.test(diff(df$log_TCRM))

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

#Ahora hay que hacer el test de cointegración
#Y chequear la estacionalidad
