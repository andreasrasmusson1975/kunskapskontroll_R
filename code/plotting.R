library(ggplot2)
library(forcats)
library(patchwork)
library(effectsize)
library(rstatix)
library(tidyverse)
library(progress)
library(dplyr)

# ---------------------------- Plotting for introduction ----------------------------

# Create a smooth line plot from a column in a tibble
plot_line <- function(df,col_name,ylabel,title) {
  p <- ggplot(df,aes(x=year,y=.data[[col_name]]))+
    geom_smooth(color='blue',size=1.5,se = FALSE) +
    scale_x_continuous(breaks = unique(time_series$year)) +
    scale_y_continuous(labels = label_comma(big.mark = ' ')) +
    labs(
      x = 'Year',
      y = ylabel,
      title = title
    )
  return(p)
}

# ---------------------------- Plotting for univariate analysis ----------------------------

# A function for plotting distribution plots for a single numerical column
plot_numerical_column <- function(df,col_name,n_bins) {
  p1 <- ggplot(df,aes(x = .data[[col_name]]))+
    geom_histogram(
      aes(y = after_stat(density)),
      bins = n_bins,
      fill = 'lightblue',
      color = 'black',
      alpha = 0.6,
    ) +
    geom_density(
      color='darkblue',
      linewidth = 1
    ) +
    labs(
      x = col_name,
      y='Denisty'
    ) +
    theme_minimal()+
    theme(
      panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.5),
      plot.title = element_text(hjust = 0.5)
    )
  
  p2 <- ggplot(df, aes(y = .data[[col_name]])) +
    geom_boxplot(
      fill = 'lightgreen', 
      color = 'black', 
      alpha = 0.6
    ) +
    labs(
      x = col_name, 
      y = NULL
    ) +
    theme_minimal() +
    theme(
      panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.5),
      axis.title.y = element_blank()
    )
  
  combined <- p1 + p2 + plot_layout(widths = c(1, 1))
  
  return(combined)
}

# A function for plotting a distribution plot for a single cate-
# gorical column
plot_categorical_column <- function(df,col_name,rare_thresh = 0.1) {
  col_sym <- rlang::sym(col_name)
  
  summary_table <- df |>
    count(
      level=!!col_sym,
      name = 'count'
    ) |>
    mutate(
      percent = count / sum(count),
      label = scales::percent(percent,accuracy = 0.1),
      rare = percent < rare_thresh
    ) |>
    mutate(
      fill_color = ifelse(rare,'red','lightgreen')
    ) |>
    arrange(desc(count))
  
  n_levels <- nrow(summary_table)
  
  p <- ggplot(summary_table,aes(x=fct_reorder(level,count),y=count)) +
    geom_col(
      aes(fill = fill_color),
      color = 'black',
      width = 0.6
    ) +
    geom_text(
      aes(label = label),
      hjust = -0.1,
      size = 3.2
    ) +
    scale_fill_identity() +
    coord_flip() +
    labs(
      title = paste('Distribution of ',col_name),
      x = col_name,
      y = 'Count'
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  if(n_levels < 10){
    message('Number of levels is ok.')
  }
  else if(n_levels < 20){
    message('10 <= Number of levels <20: Ok if balanced')
  }
  else{
    message('Too many levels.')
  
  }
  return(p)
}

# A function for plotting the interaction of a single categorical column
# to Försäljningspris
plot_categorical_target_interaction <- function(df,col_name) {
  p <- ggplot(df,aes(x=fct_reorder(.data[[col_name]],.data[['Försäljningspris']]),y=.data[['Försäljningspris']])) +
    geom_boxplot(
      fill = 'lightgreen', 
      color = 'black', 
      alpha = 0.6
    )
  return(p)
}

# A function for plotting the interaction of a single numerical column
# to a target column
plot_numerical_target_interaction <- function(df,col_name,target_name) {
  p <- ggplot(df,aes(x=.data[[col_name]],y=.data[[target_name]])) +
    geom_point(color = 'blue', alpha = 0.3, size = 2) +
    geom_smooth(color='red', method = 'loess', se = FALSE) +
    theme_light() +
    labs(
      title = glue('{col_name} vs {target_name}'),
      x = col_name,
      y = target_name,
    )
  return(p)
}
# ---------------------------- Plotting for multivariate analysis ----------------------------

# A function for plotting an association matrix. The first step is to create
# the matrix as follows:
#
# 1. Iterate over the column name pairs for each pair(x,y), do the following:
#    1.1 if both columns are numeric, record the correleation
#    1.2 if exactly one of the columns is numeric record the eta-squared
#    1.3 if none of the columns are numeric record the Cramér's V
#
# 2. Create the association matrix using this data
#
# The final step is to plot the association matrix using a tile plot
plot_association_matrix <- function(df) {
  
  vars <- names(df)
  results <- list()
  
  # Iterate over the rows and collect the associations
  for (i in 1:(length(vars) - 1)) {
    for (j in (i + 1):length(vars)) {
      var1 <- vars[i]
      var2 <- vars[j]
      x <- df[[var1]]
      y <- df[[var2]]
      
      if (is.numeric(x) && is.numeric(y)) {
        result <- tibble(
          Var1 = var1,
          Var2 = var2,
          Method = 'Pearson',
          r = suppressWarnings(cor(x, y, use = 'complete.obs'))
        )
        results[[length(results) + 1]] <- result
        
      } else if (is.numeric(x) && is.factor(y)) {
         test <- aov(x ~ y)
         eta <- effectsize::eta_squared(test, partial = FALSE)
         result <- tibble(
           Var1 = var1,
           Var2 = var2,
           Method = 'eta2',
           r = eta$Eta2[1]
         )
         results[[length(results) + 1]] <- result
         
      } else if (is.factor(x) && is.numeric(y)) {
         test <- aov(y ~ x)
         eta <- effectsize::eta_squared(test, partial = FALSE)
         result <- tibble(
           Var1 = var1,
           Var2 = var2,
           Method = 'eta2',
           r = eta$Eta2[1]
         )
         results[[length(results) + 1]] <- result
         
      } else if (is.factor(x) && is.factor(y)) {
          tbl <- table(x, y)
          result <- tibble(
            Var1 = var1,
            Var2 = var2,
            Method = 'cramers_v',
            r = cramer_v(tbl)
          )
          results[[length(results) + 1]] <- result
      }
      else {
        print('Strange')
      }
    }
  }
  #collect the results into a tibble
  assoc_df <- bind_rows(results)
  
  # Mirror the matrix
  assoc_df_mirrored <- bind_rows(
    assoc_df,
    assoc_df %>% rename(Var1 = Var2, Var2 = Var1)
  )
  
  # Add diagonal for plotting
  diag_df <- tibble(
    Var1 = vars,
    Var2 = vars,
    Method = 'self',
    r = 1
  )
  
  plot_df <- bind_rows(assoc_df_mirrored, diag_df) 
  
  # Plot
  p <- ggplot(plot_df, aes(x = Var1, y = Var2, fill = r, label = ifelse(abs(r) < 0.001, '', sprintf('%.2f', r)))) +
    geom_tile(color = 'white') +
    geom_text(color = 'black', size = 3) +
    scale_fill_gradient2(
      low = 'blue', mid = 'white', high = 'red',
      midpoint = 0, limits = c(-1, 1), name = 'Association'
    ) +
    coord_fixed() +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_blank()
    ) +
    labs(title = 'Correlation / Association Matrix')
  
  return(list(data = assoc_df, plot = p))
}


# ---------------------------- Plotting for predictions ----------------------------

# A function for plotting prediction intervals using test data by bootstrapping train data
# The following actions are performed
#
# 1. Do the following B number of times
#    1.1 sample the training data with replacement
#    1.2 train a linear model using the sample
#    1.3 Record the predictions and predictions+residuals
#
# 2. Calculate the mean prediction that corresponds to each
#    observation of the response
# 3. Calculate the lower and upper bound of a prediction interval
#    for each of the observations of the response
# 4. Collect the results into a tibble
# 5. plot the 
#
#    5.1 prediction intervals as a ribbon
#    5.2 prediction means as points
#    5.3 theoretical perfect predictions as dashed line
plot_prediction_intervals <- function(train_data, test_data, formula, B = 1000, ci_level = 0.95) {
  alpha <- 1 - ci_level
  n <- nrow(test_data)
  predictions <- list()
  predictions_plus_residuals <- list()
  
  for (b in 1:B) {
    boot_idx <- sample(nrow(train_data), replace = TRUE)
    boot_sample <- train_data[boot_idx, ]
    
    model <- lm(formula, data = boot_sample)
    preds <- predict(model, newdata = test_data)
    
    residual_sample <- sample(residuals(model), size = n)
    predictions <- append(predictions, list(as_tibble_row(exp(preds))))
    predictions_plus_residuals <- append(predictions_plus_residuals, list(as_tibble_row(exp(preds + residual_sample))))
  }
  
  predictions <- bind_rows(predictions)
  predictions_plus_residuals <- bind_rows(predictions_plus_residuals)
  
  actual_y <- tibble(actual_y = exp(test_data$Försäljningspris))
  
  mean_predictions <- predictions |>
    summarise(across(everything(), mean, na.rm = TRUE)) |>
    pivot_longer(cols = everything(), names_to = NULL, values_to = 'mean_prediction')
  
  lower_pi_bounds <- predictions_plus_residuals |>
    summarise(across(everything(), ~ quantile(.x, probs = alpha/2, na.rm = TRUE))) |>
    pivot_longer(cols = everything(), names_to = NULL, values_to = 'lower_pi_bound')
  upper_pi_bounds <- predictions_plus_residuals |>
    summarise(across(everything(), ~ quantile(.x, probs = 1-alpha/2, na.rm = TRUE))) |>
    pivot_longer(cols = everything(), names_to = NULL, values_to = 'upper_pi_bound')
  
  result_df <- bind_cols(actual_y,mean_predictions,lower_pi_bounds,upper_pi_bounds)
  result_df$relative_interval_width = (result_df$upper_pi_bound - result_df$lower_pi_bound)/result_df$mean_prediction
  
  
  p1 <- ggplot(result_df, aes(x = actual_y, y = mean_prediction)) +
    geom_ribbon(aes(ymin = lower_pi_bound, ymax = upper_pi_bound), fill = 'lightblue', alpha = 0.4)

  p1 <- p1 +
    geom_point(alpha = 0.3, size = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed', color = 'gray40', linewidth = 1) +
    labs(
      title = 'Bootstrapped Predictions vs Actual Response',
      x = 'True Response (y)',
      y = 'Predicted Mean (ŷ)',
      subtitle = '95% Prediction Intervals (blue)'
    ) +
    scale_x_continuous(labels = label_comma()) +
    scale_y_continuous(labels = label_comma()) +
    theme_minimal()

  p2 <- ggplot(result_df, aes(x = actual_y, y = relative_interval_width)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = 'loess', se = FALSE, color = 'darkblue') +
    labs(
      title = 'Relative Prediction Interval Width vs Actual Response',
      x = 'Actual y',
      y = 'Relative Prediction Interval Width'
    ) +
    scale_x_continuous(labels = label_comma()) +
    scale_y_continuous(labels = label_comma()) +
    theme_minimal()
  return(list(data = result_df, mean_pi_width = mean(result_df$relative_interval_width), pi_plot = p1,pi_width_plot = p2))
}
