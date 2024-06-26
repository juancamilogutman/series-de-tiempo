rm(list = ls())

library(readxl)

base_tp <- read_excel("bases/TP 2 - Datos.xlsx") %>% 
  rename(
    Q = ...1,
    PBI_Arg = PIBARG,
    PBI_Socios = PIBSOCIOS
  )

empalme_dg <- read_excel("bases/empalme_demanda_global.xlsx", range = "A2:C98") %>% 
  rename(pbiArg = PBI,
    pbiArg = PBI,
    demandaGlobal = Demanda_Global
    )

df <- merge(base_tp, empalme_dg, by = "Q")

# Formato fechas:
df <- df %>%
  mutate(Q = yearquarter(Q))

df <- df %>% as_tsibble(index = Q)

#Las variables en logaritmo
df <- df %>% mutate(log_X = log(df$EXPO))
df <- df %>% mutate(log_M = log(df$IMPO))
df <- df %>% mutate(log_PBI_Arg = log(df$PBI_Arg))
df <- df %>% mutate(log_PBI_Socios = log(df$PBI_Socios))
df <- df %>% mutate(log_TCRM = log(df$TCRM))
df <- df %>% mutate(log_demandaGlobal = log(df$demandaGlobal))

#Slicing según Chow
start_2003_Q4 <- which(df$Q == yearquarter("2003 Q4"))
df_hasta_2003 <- df %>% slice(1:(start_2003_Q4 - 1))
df_desde_2004 <- df %>% slice(start_2003_Q4:n())


saveRDS(df, "bases/base_tp2.rds")
saveRDS(df_hasta_2003, "bases/base_tp2_h2003.rds")
saveRDS(df_desde_2004, "bases/base_tp2_d2004.rds")
