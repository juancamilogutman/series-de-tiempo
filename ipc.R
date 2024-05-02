library(tidyverse)
library(tsibble)
library(fable)

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
               
autoplot(ipc, inflacion) +
  labs(title = "Inflación mensual ",
       subtitle = "2003-2023",
       y = "Inflación")

