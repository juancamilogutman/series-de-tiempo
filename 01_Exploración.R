rm(list = ls())

library(tidyverse) #Para manejar bases de datos
library(tsibble) #Para manejar bases para series de tiempo
library(ggplot2) #Para graficar
library(knitr) #Para tablas
library(fpp3) #No recuerdo para que era
# library(vars)
# library(tseries)
# library(urca) #para el adf.test
# library(forecast)#para el auto.arima
# library(aTSA)
# library(fable) #Para correr modelos de series de tiempo

df <- readRDS("bases/base_tp2.RDS")

# Pequeña exploración
names(df)
str(df)
head(df, 15)
tail(df, 15)

######################
# Plot Impo vs. Expo #
######################
plot_impo_expo <- autoplot(df, log_X, color="Blue") +
  labs(title = "Exportaciones e Importaciones Argentinas",
       subtitle = "1996-2019",
       y = "Logaritmo de Exportaciones", x= "Año y trimestre") +
  theme_minimal()

plot_impo_expo <- plot_impo_expo +
  autolayer(df, log_M, series = "Importaciones", colour="Red") +
  labs(y = "Logaritmo de Exportaciones e Importaciones") +
  theme_minimal()

print(plot_impo_expo)

#######################
# Plot Variables Expo #
#######################
df_X_long <- df %>%
  pivot_longer(cols = c(log_X, log_PBI_Socios, log_TCRM),
               names_to = "series",
               values_to = "value")

df_X_long <- df_X_long[, c("Q", "series", "value"), drop = FALSE]

autoplot(df_X_long) +
  labs(x = "Año", y = "Variables",
       title = "Argentina: Exportaciones, PBI de Socios Comerciales y Tipo de Cambio Real Multilateral") +
  guides(colour=guide_legend(title="Variables")) +
  facet_wrap(~series, scales = "free")

#######################
# Plot Variables Impo #
#######################
df_M_long <- df %>%
  pivot_longer(cols = c(log_M, log_demandaGlobal, log_TCRM),
               names_to = "series",
               values_to = "value")

df_M_long <- df_M_long[, c("Q", "series", "value"), drop = FALSE]

autoplot(df_M_long) +
  labs(x = "Año", y = "Variables",
       title = "Argentina: Importaciones, Demanda Global y Tipo de Cambio Real Multilateral") +
  guides(colour=guide_legend(title="Variables")) +
  facet_wrap(~series, scales = "free")

#######################
# Plot PBI vs dGlobal #
#######################
df_dg_long <- df %>%
  pivot_longer(cols = c(log_PBI_Arg, log_demandaGlobal),
               names_to = "series",
               values_to = "value")

df_dg_long <- df_dg_long[, c("Q", "series", "value"), drop = FALSE]

autoplot(df_dg_long) +
  labs(x = "Año", y = "Variables",
       title = "Argentina: Producto Bruto Interno y Demanda Global") +
  guides(colour=guide_legend(title="Variables")) +
  facet_wrap(~series, scales = "free", nrow = 2)

