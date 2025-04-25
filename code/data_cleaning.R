library(stringr)
library(tidyr)
source('code/data_fetch.R')
source('code/prel_checks.R')
library(lubridate)

# Do the following:
#
# 1. Correct typos in all columns
# 2. Remove spaces from numeric columns
# 3. Remove units from Hästkrafter column
# 4. Remove the decimal part from some Försäljningspris
#    and miltal
# 5. Remove rows where a color was entered in the Modell
#    column
# 6. Drop rows that contain missing values
clean_car_data <- car_data[!duplicated(car_data), ] |>
  mutate(
    Bränsle = case_when(
      Bränsle == 'Miljöbränsle/hybrid' ~ 'Miljöbränsle/Hybrid',
      Bränsle == 'Miljöbränsle' ~ 'Miljöbränsle/Hybrid',
      Bränsle == 'Disel' ~ 'Diesel',
      Bränsle == 'diesel' ~ 'Diesel',
      Bränsle == 'bensin' ~ 'Bensin', 
      TRUE ~ Bränsle
    ),
    Växellåda = case_when(
      Växellåda == 'automat' ~ 'Automat',
      Växellåda == 'manuell' ~ 'Manuell',
      TRUE ~ Växellåda
    ),
    Biltyp = case_when(
      Biltyp == 'familjebuss' ~ 'Familjebuss',
      Biltyp == 'halvkombi' ~ 'Halvkombi',
      Biltyp == 'Halvkomni' ~ 'Halvkombi',
      TRUE ~ Biltyp
    ),
    Färg = case_when(
      Färg == 'grå' ~ 'Grå',
      Färg == 'Siver' ~ 'Silver',
      Färg == 'svart' ~ 'Svart',
      Färg == 'vit' ~ 'Vit',
      Färg == 'Vít' ~ 'Vit',
      Färg == 'Mörbllå' ~ 'Mörkblå',
      TRUE ~ Färg
    ),
    Modell = case_when(
      Modell == 'california' ~ 'California',
      Modell == 'UP!' ~ 'UP',
      Modell == 'T-CROSS' ~ 'T-Cross',
      TRUE ~ Modell
    ),
    Försäljningspris = str_replace_all(Försäljningspris,'\\s+',''),
    Miltal = str_replace_all(Miltal,'\\s+',''),
    Modellår = str_replace_all(Modellår,'\\s+',''),
  ) |>
  separate(
    `Hästkrafter (HK)`,
    into = c('Hästkrafter (HK)','unit'),
    sep = ' ',
    fill = 'right'
  ) |>  

  separate(
    Försäljningspris,
    into = c('Försäljningspris','comma'),
    sep = ',',
    fill = 'right'
  ) |>
  separate(
    Miltal,
    into = c('Miltal','comma2'),
    sep = ',',
    fill = 'right'
  ) |>
  
  select(!c('unit','comma','comma2')) |>
  filter(!(Modell %in% c('Silver','Svart','Vit'))) |>
  drop_na()
  
# Perform checks using the prel_checks script
result <- do_checks(clean_car_data)
print_result(result)

# Convert to numerics and factors
clean_car_data <- clean_car_data |>
  mutate(
    Försäljningspris = as.numeric(Försäljningspris),
    Säljare = as.factor(Säljare),
    Bränsle = as.factor(Bränsle),
    Växellåda = as.factor(Växellåda),
    Miltal = as.numeric(Miltal),
    Modellår = as.numeric(Modellår),
    Biltyp = as.factor(Biltyp),
    Drivning = as.factor(Drivning),
    `Hästkrafter (HK)` = as.numeric(`Hästkrafter (HK)`),
    Färg = as.factor(Färg),
    Modell = as.factor(Modell),
    Region = as.factor(Region)
    
  )
# Print the cleaned data
print(clean_car_data)