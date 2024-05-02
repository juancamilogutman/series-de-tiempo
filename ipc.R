library(tidyverse)
library(tsibble)
library(fable)
library(fpp3)

# ipc_ceped <- 
#   read.xlsx("ipc_ceped_data.xlsx",
#             sheet = "BP_mill U$ const",
#             startRow = 2,rows = c(2,5:73),
#             skipEmptyRows = T)

# Parseo de base ####
ipc <- readRDS("base_ipc.RDS")

colnames(ipc)
unique(ipc$cod.variable)

# Para trabajar con valores mensuales, desde 2003:
ipc <- ipc %>%
  filter(cod.variable == "IPC_Mensual_2006") %>%
  filter(ANO4 >= 2003)

# Formato fechas:
ipc <- ipc %>%
  mutate(periodo = yearmonth(make_date(ANO4, sub)))

# Generamos variable de inflación como:
# inflación = ln(ipc_t) - ln (ipc_t-1)

ipc <- ipc %>% 
  mutate(ln_ipc = log(valor))

ipc <- ipc %>% mutate(inflacion = ln_ipc - lag(ln_ipc))

# infla <- ipc %>% 
#   mutate(var_porc = tryCatch((valor / lag(valor) - 1) * 100, error = function(e) NaN))
# eso reconstruye la variable var tal como viene de ceped.data

ipc <- ipc %>% select(c("periodo", "inflacion"))

ipc <- ipc %>% as_tsibble(index = periodo)
               
# Simple gráfico de la evolución en el tiempo de la inflación
autoplot(ipc, inflacion) +
  labs(title = "Inflación mensual ",
       subtitle = "2003-2023",
       y = "Inflación")

# Gráfico de dispersión entre la inflación corriente y la del periodo
# inmediatamente anterior (esto es, si no me equivoco, un gráfico que 
# ilustra la autocorrelación con un solo rezago:

ipc %>% gg_lag(
  inflacion, geom = "point", lags = 1) +
  labs(x = "lag(inflacion, k)")

# Lo mismo con 9 rezagos
ipc %>% gg_lag(
  inflacion, geom = "point", lags = 1:9) +
  labs(x = "lag(inflacion, k)")

# ACF (autocorrelation function):
ipc %>% ACF(inflacion, lag_max = 9)
# se observa fácil como decae la autocorrelación con cada mes
# que nos alejamos

# Correlograma:
ipc |>
  ACF(inflacion) |>
  autoplot() + labs(title= "Inflacion argentina 2003-2023",
                    subtitle= "Correlograma")

# A priori no se ve estacionalidad, si se ve clara tendencia, por eso decae


