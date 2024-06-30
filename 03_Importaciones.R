# Inicialización ####
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


# Adfs ####

#---------------#
# BASE COMPLETA #
#---------------#

adf_log_M <- ur.df(df$log_M,
                   type = c("none"),
                   lags = 4,
                   selectlags = "AIC")
summary(adf_log_M) 

adf_log_demandaGlobal <- ur.df(df$log_demandaGlobal,
                   type = c("none"),
                   lags = 4,
                   selectlags = "AIC")

summary(adf_log_demandaGlobal)

adf_log_TCRM <- ur.df(df$log_TCRM,
                               type = c("none"),
                               lags = 4,
                               selectlags = "AIC")

summary(adf_log_TCRM)

#------------------------------#
# BASE COMPLETA EN DIFERENCIAS #
#------------------------------#

adf_dlog_M <- ur.df(diff(df$log_M),
                   type = c("none"),
                   lags = 4,
                   selectlags = "AIC")
summary(adf_dlog_M)

adf_dlog_demandaGlobal <- ur.df(diff(df$log_demandaGlobal),
                               type = c("none"),
                               lags = 4,
                               selectlags = "AIC")

summary(adf_dlog_demandaGlobal)

adf_dlog_TCRM <- ur.df(diff(df$log_TCRM),
                      type = c("none"),
                      lags = 4,
                      selectlags = "AIC")

summary(adf_dlog_TCRM)

#-----------------#
# BASE HASTA 2003 #
#-----------------#

adf_log_M_h2003 <- ur.df(dfh2003$log_M,
                   type = c("none"),
                   lags = 4,
                   selectlags = "AIC")
summary(adf_log_M_h2003)

adf_log_demandaGlobal_h2003 <- ur.df(dfh2003$log_demandaGlobal,
                               type = c("none"),
                               lags = 4,
                               selectlags = "AIC")

summary(adf_log_demandaGlobal_h2003)

adf_log_TCRM_h2003 <- ur.df(dfh2003$log_TCRM,
                      type = c("none"),
                      lags = 4,
                      selectlags = "AIC")

summary(adf_log_TCRM_h2003)

#------------------------------#
# BASE HASTA 2003 DIFERENCIADA #
#------------------------------#

adf_dlog_M_h2003 <- ur.df(diff(dfh2003$log_M),
                    type = c("none"),
                    lags = 4,
                    selectlags = "AIC"
                    )
summary(adf_dlog_M_h2003)

adf_dlog_demandaGlobal_h2003 <- ur.df(diff(dfh2003$log_demandaGlobal),
                                type = c("none"),
                                lags = 4,
                                selectlags = "AIC")

summary(adf_dlog_demandaGlobal_h2003)

adf_dlog_TCRM_h2003 <- ur.df(diff(dfh2003$log_TCRM),
                       type = c("none"),
                       lags = 4,
                       selectlags = "AIC")

summary(adf_dlog_TCRM_h2003)

#-----------------#
# BASE DESDE 2004 #
#-----------------#

adf_log_M_d2004 <- ur.df(dfd2004$log_M,
                   type = c("none"),
                   lags = 4,
                   selectlags = "AIC"
                   )
summary(adf_log_M_d2004)

adf_log_demandaGlobal_d2004 <- ur.df(dfd2004$log_demandaGlobal,
                               type = c("none"),
                               lags = 4,
                               selectlags = "AIC")

summary(adf_log_demandaGlobal_d2004)

adf_log_TCRM_d2004 <- ur.df(dfd2004$log_TCRM,
                      type = c("none"),
                      lags = 4,
                      selectlags = "AIC")

summary(adf_log_TCRM_d2004)

#------------------------------#
# BASE DESDE 2004 DIFERENCIADA #
#------------------------------#

adf_dlog_M_d2004 <- ur.df(diff(dfd2004$log_M),
                    type = c("none"),
                    lags = 4,
                    selectlags = "AIC"
                    )
summary(adf_dlog_M_d2004)

adf_dlog_demandaGlobal_d2004 <- ur.df(diff(dfd2004$log_demandaGlobal),
                                type = c("none"),
                                lags = 4,
                                selectlags = "AIC"
                                )

summary(adf_dlog_demandaGlobal_d2004)

adf_dlog_TCRM_d2004 <- ur.df(diff(dfd2004$log_TCRM),
                       type = c("none"),
                       lags = 4,
                       selectlags = "AIC")

summary(adf_dlog_TCRM_d2004)

rm(list = ls(pattern = "^adf"))

# Estacionalidad ####
## Estacionalidad en toda la base ####
### Estacionalidad importaciones

x11_log_M <- df |>
  model(x11 = X_13ARIMA_SEATS(log_M ~ x11())) |>
  components()

x11_log_M |>
  ggplot(aes(x = Q)) +
  geom_line(aes(y = log_M, colour = "Data")) +
  geom_line(aes(y = season_adjust,
                colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "Persons (thousands)",
       title = "Ajuste Estacional - Importaciones") +
  scale_colour_manual(
    values = c("gray", "#0072B2", "#D55E00"),
    breaks = c("Data", "Seasonally Adjusted", "Trend")
  )

### Estacionalidad demanda global
x11_log_demandaGlobal <- df |>
  model(x11 = X_13ARIMA_SEATS(log_demandaGlobal ~ x11())) |>
  components()

### Estacionalidad TCRM
x11_log_TCRM <- df |>
  model(x11 = X_13ARIMA_SEATS(log_TCRM ~ x11())) |>
  components()

## Estacionalidad hasta 2003 ####
### Estacionalidad importaciones
x11_log_M_h2003 <- dfh2003 |>
  model(x11 = X_13ARIMA_SEATS(log_M ~ x11())) |>
  components()

x11_log_M_h2003 |>
  ggplot(aes(x = Q)) +
  geom_line(aes(y = log_M, colour = "Data")) +
  geom_line(aes(y = season_adjust,
                colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "Persons (thousands)",
       title = "Ajuste Estacional - Importaciones hasta 2003") +
  scale_colour_manual(
    values = c("gray", "#0072B2", "#D55E00"),
    breaks = c("Data", "Seasonally Adjusted", "Trend")
  )

### Estacionalidad demanda global
x11_log_demandaGlobal_h2003 <- dfh2003 |>
  model(x11 = X_13ARIMA_SEATS(log_demandaGlobal ~ x11())) |>
  components()

### Estacionalidad TCRM
x11_log_TCRM_h2003 <- dfh2003 |>
  model(x11 = X_13ARIMA_SEATS(log_TCRM ~ x11())) |>
  components()
## Estacionalidad desde 2004 ####
x11_log_M_d2004 <- dfd2004 |>
  model(x11 = X_13ARIMA_SEATS(log_M ~ x11())) |>
  components()

x11_log_M_d2004 |>
  ggplot(aes(x = Q)) +
  geom_line(aes(y = log_M, colour = "Data")) +
  geom_line(aes(y = season_adjust,
                colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "Persons (thousands)",
       title = "Ajuste Estacional - Importaciones hasta 2003") +
  scale_colour_manual(
    values = c("gray", "#0072B2", "#D55E00"),
    breaks = c("Data", "Seasonally Adjusted", "Trend")
  )

### Estacionalidad demanda global
x11_log_demandaGlobal_d2004 <- dfd2004 |>
  model(x11 = X_13ARIMA_SEATS(log_demandaGlobal ~ x11())) |>
  components()

### Estacionalidad TCRM
x11_log_TCRM_d2004 <- dfd2004 |>
  model(x11 = X_13ARIMA_SEATS(log_TCRM ~ x11())) |>
  components()

# Desestacionalizando ####
df <- df %>% mutate(log_M = x11_log_M$season_adjust) 
df <- df %>% mutate(log_demandaGlobal = x11_log_demandaGlobal$season_adjust) 
df <- df %>% mutate(log_TCRM = x11_log_TCRM$season_adjust) 

dfh2003 <- dfh2003 %>% mutate(log_M = x11_log_M_h2003$season_adjust) 
dfh2003 <- dfh2003 %>% mutate(log_demandaGlobal = x11_log_demandaGlobal_h2003$season_adjust)
dfh2003 <- dfh2003 %>% mutate(log_TCRM = x11_log_TCRM_h2003$season_adjust)

dfd2004 <- dfd2004 %>% mutate(log_M = x11_log_M_d2004$season_adjust) 
dfd2004 <- dfd2004 %>% mutate(log_demandaGlobal = x11_log_demandaGlobal_d2004$season_adjust)
dfd2004 <- dfd2004 %>% mutate(log_TCRM = x11_log_TCRM_d2004$season_adjust)

# Tests de Engle y Granger ####

## Base completa
reg_coint <- lm(log_M ~ log_demandaGlobal + log_TCRM, data = df)
residuos_coint <- reg_coint$residuals
test_eg <- ur.df(residuos_coint,
                   type = c("none"),
                   lags = 4,
                   selectlags = "AIC"
                   )
summary(test_eg)

## Base hasta 2003
reg_coint_h2003 <- lm(log_M ~ log_demandaGlobal + log_TCRM, data = dfh2003)
residuos_coint_h2003 <- reg_coint_h2003$residuals
test_eg_h2003 <- ur.df(residuos_coint_h2003,
                 type = c("none"),
                 lags = 4,
                 selectlags = "AIC"
                 )
summary(test_eg_h2003)

## Base desde 2004
reg_coint_d2004 <- lm(log_M ~ log_demandaGlobal + log_TCRM, data = dfd2004)
residuos_coint_d2004 <- reg_coint_d2004$residuals
test_eg_d2004 <- ur.df(residuos_coint_d2004,
                       type = c("none"),
                       lags = 4,
                       selectlags = "AIC"
)
summary(test_eg_d2004)

# ECM (trivariado, uniecuacional) ####
## Base Completa
residuos_coint_lag <- lag(residuos_coint)[-1]
ecm <- lm(diff(log_M) ~ residuos_coint_lag + diff(log_demandaGlobal) + diff(log_TCRM), data = df)
summary(reg_coint)
summary(ecm)

## Base hasta 2003
residuos_coint_h2003_lag <- lag(residuos_coint_h2003)[-1]
ecm_h2003 <- lm(diff(log_M) ~ residuos_coint_h2003_lag + diff(log_demandaGlobal) + diff(log_TCRM), data = dfh2003)
summary(reg_coint_h2003)
summary(ecm_h2003)

## Base desde 2004
residuos_coint_d2004_lag <- lag(residuos_coint_d2004)[-1]
ecm_d2004 <- lm(diff(log_M) ~ residuos_coint_d2004_lag + diff(log_demandaGlobal) + diff(log_TCRM), data = dfd2004)
summary(reg_coint_d2004)
summary(ecm_d2004)


# Identificación siguiendo a Wicken y Breusch ####
## WB con base completa
log_M_lag <- lag(df$log_M)[-1]
log_demandaGlobal_lag <- lag(df$log_demandaGlobal)[-1]
log_TCRM_lag <- lag(df$log_TCRM)[-1]

wb <- lm(diff(log_M) ~ diff(log_demandaGlobal)+ diff(log_TCRM) + log_M_lag + log_demandaGlobal_lag + log_TCRM_lag, data = df)
summary(wb)

## WB con base hasta 2003
log_M_lag_h2003 <- lag(dfh2003$log_M)[-1]
log_demandaGlobal_lag_h2003 <- lag(dfh2003$log_demandaGlobal)[-1]
log_TCRM_lag_h2003 <- lag(dfh2003$log_TCRM)[-1]

wb_h2003 <- lm(diff(log_M) ~ diff(log_demandaGlobal)+ diff(log_TCRM) + log_M_lag_h2003 + log_demandaGlobal_lag_h2003 + log_TCRM_lag_h2003, data = dfh2003)
summary(wb_h2003)

## WB con base desde 2004
log_M_lag_d2004 <- lag(dfd2004$log_M)[-1]
log_demandaGlobal_lag_d2004 <- lag(dfd2004$log_demandaGlobal)[-1]
log_TCRM_lag_d2004 <- lag(dfd2004$log_TCRM)[-1]

wb_d2004 <- lm(diff(log_M) ~ diff(log_demandaGlobal)+ diff(log_TCRM) + log_M_lag_d2004 + log_demandaGlobal_lag_d2004 + log_TCRM_lag_d2004, data = dfd2004)
summary(wb_d2004)

# VEC ####
# Faltaría chequear con el test de johansen:
data_vecm <- df[, c("log_M", "log_TCRM", "log_demandaGlobal")]
vecm <- ca.jo(data_vecm, type="trace", ecdet="const", K=4)
summary(vecm)

# # PRENESTOR:
# data_vecm <- df[, c("log_M", "log_TCRM", "log_demandaGlobal")]
# vecm <- ca.jo(data_vecm, type="trace", ecdet="const", K=4)
# summary(vecm)


## POST NESTOR:
data_vecm_d2004 <- dfd2004[, c("log_M", "log_TCRM", "log_demandaGlobal")]
vecm_d2004 <- ca.jo(data_vecm_d2004, type="trace", ecdet="const", K=4)
summary(vecm_d2004)

