library(pxweb)
library(dplyr)
library(readxl)


# A module that collects data from scb.se about privately owned cars and median income and makes
# that data available
module <- local({
  # Define URLs and queries
  personbilar_antal_riket_url='https://api.scb.se/OV0104/v1/doris/sv/ssd/TK/TK1001/TK1001A/PersBilarA'
  personbilar_antal_riket_query <- list(
    Region = c('00'),
    Agarkategori = c('000'),
    ContentsCode = c('TK1001AB'),
    Tid = as.character(2011:2023)
  )
  
  disp_medianink_tkr_riket_url = 'https://api.scb.se/OV0104/v1/doris/sv/ssd/HE/HE0110/HE0110F/TabVX1DispInkN'
  disp_medianink_tkr_riket_query <- list(
    Region = c('00'),
    InkomstTyp = c('DispInkInkl'),
    ContentsCode = c('000006R7'),
    Tid = as.character(2011:2023)
  )
  
  # Generic function for fetching data
  get_untidy_data <- function(url,query) {
    data = pxweb_get(
      url,
      query = query
    )
    data_tibble = tibble(
      as.data.frame(
        data,
        column.name.type = 'text',
        variable.value.type = 'text'
      )
    )
    return(data_tibble)
  }
  
  # Function for getting the personbilar_riket_antal_data
  get_personbilar_riket_antal <- function(){
    untidy <- get_untidy_data(
      personbilar_antal_riket_url,
      personbilar_antal_riket_query
    )
    tidy <- untidy |>
      mutate(
        year = as.integer(år),
        num_private_cars = Antal
      ) |>
      select(year,num_private_cars)
    return(tidy)
  }
  
  # Function for getting the disp_medianink_riket_tkr data
  get_disp_medianink_riket_tkr <- function(){
    untidy <- get_untidy_data(
      disp_medianink_tkr_riket_url,
      disp_medianink_tkr_riket_query
    )
    tidy <- untidy |>
      mutate(
        year = as.integer(år),
        median_inc_tkr = `Medianvärde, tkr`
      ) |>
      select(
        year,
        median_inc_tkr
      )
    
    return(tidy)
    
  }
  list(
   private_cars_total = get_personbilar_riket_antal(),
   disp_medianink = get_disp_medianink_riket_tkr()
  )
})


time_series <- module$private_cars_total |>
inner_join(module$disp_medianink,by = 'year')
car_data = read_excel('data/datainsamling_komplett.xlsx') |>
  select(!c('Datum_i_trafik','Märke'))


