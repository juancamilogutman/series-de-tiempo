library(tidyverse) #Para manejar bases de datos
library(tsibble) #Para manejar bases para series de tiempo
library(fable) #Para correr modelos de series de tiempo
library(ggplot2) #Para graficar
library(knitr) #Para tablas
library(fpp3) #No recuerdo para que era
library(vars)
library(urca)
library(ggplot2)
library(forecast) # Assuming autoplot comes from the forecast package

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
  mutate(t = yearquarter(Q))

df <- df %>% as_tsibble(index = Q)

#Las variables en logaritmo
df <- df %>% mutate(log_X = log(df$EXPO))
df <- df %>% mutate(log_M = log(df$IMPO))
df <- df %>% mutate(log_PBI_Arg = log(df$PBI_ARG))
df <- df %>% mutate(log_PBI_Socios = log(df$PBI_SOCIOS))
df <- df %>% mutate(log_TCRM = log(df$TCRM))

names(df)

base <- df %>% select(Q,
                      log_X)
                      log_M,
                      log_PBI_Arg,
                      log_PBI_Socios,
                      log_TCRM)

# Plot PBI ARG
autoplot(df, PBI_ARG) +
  labs(title = "PBI Argentino",
       subtitle = "1996-2019",
       y = "PBI")

# Plot EXPO ARG
autoplot(df, log_EXPO) +
  labs(title = "Exportaciones Argentinas",
       subtitle = "1996-2019",
       y = "Exportaciones")

# Plot IMPO ARG
autoplot(df, log_IMPO) +
  labs(title = "Importaciones Argentinas",
       subtitle = "1996-2019",
       y = "Importaciones")

combined_plot <- autoplot(df, log_EXPO) +
  labs(title = "Exportaciones e Importaciones Argentinas",
       subtitle = "1996-2019",
       y = "Logaritmo de Exportaciones") +
  theme_minimal()

combined_plot <- combined_plot + 
  autolayer(df, log_IMPO, series = "Importaciones") +
  labs(y = "Logaritmo de Exportaciones e Importaciones") +
  theme_minimal()

print(combined_plot)

# ARRANCA LO IMPORTANTE ####


