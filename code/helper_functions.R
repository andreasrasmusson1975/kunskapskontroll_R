library(tidyverse)

# A function for viewing a specific observation from the transformed
# data in the original df.
view_original_observation <- function(n,orginal_df,transformed_df) {
  region <- transformed_df$Region[n]
  försäljningspris <- exp(transformed_df$Försäljningspris[n])
  return(
    df |>
      filter(
        Försäljningspris < försäljningspris + 1000 & 
        Försäljningspris > försäljningspris -1000 &
        Region == region
      )
  )
}