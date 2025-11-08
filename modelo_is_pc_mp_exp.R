###########################################################################
############### Coleta de dados modelo IS-PC-MP-Exp #######################


# Carregar pacotes --------------------------------------------------------

library(GetBCBData)
library(rbcb)
library(meedr)
library(tidyverse)
library(sidrar)
library(bimets)
library(plyr)
library(gmm)

# Coleta de dados ---------------------------------------------------------
#> CPI qq sa, CPI, EXP

dados_ipca_raw = sidrar::get_sidra(
  api = "/t/1737/n1/all/v/2266/p/all/d/v2266%2013"
) |> dplyr::mutate(date = lubridate::ymd(paste0(`Mês (Código)`, "01"))) |> 
  dplyr::select(date, Valor)

dados_ipca = dados_ipca_raw |> dplyr::mutate(
  quarter = tsibble::yearquarter(date)
) |> 
  dplyr::filter(as.Date(date) >= as.Date("1998-01-01")) |> 
  dplyr::group_by(quarter) |> 
  dplyr::summarise(cpi = mean(Valor)) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(marginal = ((cpi/lag(cpi,1))-1)*100)

# Dados dessazonalizados e trimestralizados

dados_qq_sa_raw = sidrar::get_sidra(
  api = "/t/118/n1/all/v/all/p/all/d/v306%202"
) |> 
  dplyr::mutate(date = ymd(paste0(`Mês (Código)`, "01"))) |> 
  dplyr::select(date, Valor) |> 
  dplyr::mutate(quarter = tsibble::yearquarter(date)) |> 
  dplyr::group_by(quarter) |> 
  dplyr::summarise(cpi_sa = sum(Valor))

# Expectativas de inflação

exp = rbcb::get_market_expectations(
  type = "inflation-12-months",
  indic = "IPCA",
  start_date = "1998-01-01",
  end_date = Sys.Date()
) |> dplyr::filter(
  Suavizada == "S", baseCalculo == 0
) |> dplyr::select(Data, Mediana)

exp = exp |> 
  dplyr::mutate(quarter = tsibble::yearquarter(Data)) |> 
  dplyr::group_by(quarter) |> 
  dplyr::summarise(exp = mean(Mediana))

# Dados de PIB dessazonalizado

pib_sa = sidrar::get_sidra(
  api = "/t/1621/n1/all/v/all/p/all/c11255/90707/d/v584%202"
) |>
  dplyr::mutate(date = lubridate::yq(`Trimestre (Código)`)) |> 
  dplyr::select(date, Valor) |> 
  dplyr::mutate(quarter = tsibble::yearquarter(date)) |> 
  dplyr::select(quarter, pib_sa = Valor)
  #dplyr::mutate(variacao = ((Valor/lag(Valor, 1))-1)*100)


# Carregar Filtro de Kalman no GDP -----------------------------------------------
  
pib_filtro = readxl::read_excel(
  path = 'kalman.xlsx', # O filtro de Kalman
  col_names = TRUE
) |> 
  dplyr::mutate(date = as.Date(data)) |> 
  dplyr::mutate(quarter = tsibble::yearquarter(date)) |> 
  dplyr::select(quarter, kalman)

# Juntar dados
pib = dplyr::left_join(
  x = pib_sa,
  y = pib_filtro,
  by = "quarter"
)

pib = pib |> 
  dplyr::mutate(
    ygap = ((pib_sa / kalman) - 1 )*100
  )

# Visualizar 

ggplot(pib, aes(x=as.Date(quarter), y=ygap))+
  geom_line()+
  geom_point()

# Meta --------------------------------------------------------------------

meta_inflacao = GetBCBData::gbcbd_get_series(
  id = 13521,
  last.date = Sys.Date(),
  first.date = "1998-01-01",
  use.memoise = FALSE
)

meta_inflacao = meta_inflacao |> dplyr::select(
  ref.date, value
)

# criar série trimestral
meta <- meta_inflacao |>
  mutate(year = year(ref.date)) |>
  select(year, value) |>
  distinct() |>
  # expandir para os 4 trimestres de cada ano
  mutate(quarter = list(1:4)) |>
  unnest(quarter) |>
  mutate(ref.date = yq(paste(year, quarter))) |>
  arrange(ref.date) |>
  select(ref.date, value) |> 
  dplyr::mutate(quarter = tsibble::yearquarter(ref.date)) |> 
  dplyr::select(quarter, pi_star= value)


#meta = readxl::read_excel(
#  path = 'target.xlsx', # O filtro de Kalman
#  col_names = T
#) |> 
#  dplyr::mutate(quarter = tsibble::yearquarter(quarter),
#                pi_star = pi_star*100) |> 
#  dplyr::select(quarter, pi_star)

# Meta trimestral


# Câmbio ------------------------------------------------------------------
init_date = init_date <- lubridate::as_date("2005-01-01")

brlusd_raw <- GetBCBData::gbcbd_get_series(
  id = c(
    "fx" = 10813),
  #first.date = init_date,
  use.memoise = FALSE,
  format.data = "wide") |> 
  dplyr::rename("date" = "ref.date") |> 
  dplyr::mutate(date = as.Date(date))

brlusd = brlusd_raw |> 
  dplyr::mutate(quarter = tsibble::yearquarter(.data$date)) |> 
  dplyr::select(quarter, fx) |> 
  dplyr::group_by(quarter) |> 
  dplyr::summarise(fx = mean(fx)) |> 
  ungroup() 


# Selic -------------------------------------------------------------------

selic = GetBCBData::gbcbd_get_series(
  id = 432,
  #first.date = "1997-01-01",
  last.date = Sys.Date(),
  use.memoise = FALSE
) |> 
  dplyr::mutate(quarter = tsibble::yearquarter(ref.date)) |> 
  dplyr::group_by(quarter) |> 
  dplyr::summarise(
    mpr = mean(value)
  ) |> dplyr::ungroup()


# Juros neutro ------------------------------------------------------------

juros_5y = readxl::read_excel(
  path = "juros_5y.xlsx", # Peguei do euromonitor
  col_names = T
)
  
juros_5y_ts = ts(
  data = juros_5y$y5yield,
  start = c(2002, 1),
  end = c(2024,2),
  frequency = 4
)
# Passar FIltro HP --------------------------------------------------------

hp = mFilter::hpfilter(juros_5y_ts, freq = 1600)
trend = hp$trend |> as_tibble()
trend = trend |> 
  dplyr::mutate(
    quarter = meta$quarter,
    y5yield_hp = trend$`Series 1`
  ) |> dplyr::select(quarter, y5yield_hp) |> 
  as_tibble(
    
  )

trend = data |> 
  dplyr::mutate(quarter = tsibble::yearquarter(date)) |> 
  dplyr::group_by(quarter) |> 
  dplyr::summarise(
    neutro = mean(neutro)
  ) |> dplyr::ungroup()
  
# Juntar dados ------------------------------------------------------------

dados = purrr::reduce(
  .x = list(dados_ipca, dados_qq_sa_raw, exp, brlusd, selic, pib, meta, trend),
  .f = dplyr::left_join,
  by = "quarter"
) |> dplyr::filter(
  as.Date(quarter) >= "2002-01-01"
) |> drop_na()



# Criar variáveis  --------------------------------------------------------

dados = dados |> 
  dplyr::mutate(
    pi = ((((cpi_sa/100)+1)^4)-1)*100,
    pigap = pi-pi_star,
    #quarter = lubridate::yq(quarter),
    ygap_lag1 = lag(ygap,1),
    ygap_lag2 = lag(ygap,2),
    mpr_lag1 = lag(mpr, 1)
  ) |> drop_na()

# Exportar dados ----------------------------------------------------------
write_csv2(x = dados, file = "dados.csv")
#write_csv2(x = pib_sa, file = "gdp_total.csv")

# Modelo de Equações Simultâneas ------------------------------------------

model_spec <- "
MODEL

COMMENT> IS CURVE
BEHAVIORAL> ygap
TSRANGE 2005 2 2024 2
EQ> ygap = b1*TSLAG(ygap,1) + b2*(mpr - exp - y5yield_hp + pi_star)
COEFF> b1 b2
b1+b2 = 1

COMMENT> Phillips Curve
BEHAVIORAL> pi
TSRANGE 2005 2 2024 2
EQ> pi = b3*TSLAG(pi,1) + b4*ygap
COEFF> b3 b4

COMMENT> Taylor Curve
BEHAVIORAL> mpr
TSRANGE 2005 2 2024 2
EQ> mpr = b5*TSLAG(mpr,1) + b6*exp
COEFF> b5 b6

END
"

macro_model <- LOAD_MODEL(modelText = model_spec)

dados_ts <- dados %>%
  pivot_longer(-quarter, names_to = 'var', values_to = 'value') %>%
  dlply(
    .variables = 'var',
    .fun = function(x) {
      TIMESERIES(x$value, START = c(2002,3), FREQ = 4)
    }
  )

macro_model <- LOAD_MODEL_DATA(
  macro_model,
  dados_ts
)

model_fit <- ESTIMATE(
  macro_model,
  eqList = c('ygap'),
  estTech = "IV",
  IV = c(
    "TSLAG(ygap,2)",
    "TSLAG(mpr,1)",
    "pi"
  )
)

