library(tidyverse)
library(rpart)

# A function for transforming a high cardinality categorical column to a
# low cardinality one. This is done by fitting a decision tree with an
# appropriate number of leaves to the (column,target) pairs. The intuition
# here is that a decision tree makes splits in a way that reduces uncertainty
# about the target so we should get good groupings of the categories in the leaves.
high_cardinality_to_low <- function(df,col_name,target_name,desired_cardinality=10) {
  formula_str <- paste(target_name, '~', col_name)
  formula <- as.formula(formula_str)
  tree_model <- rpart(
    formula,
    data = df,
    method = 'anova',
    control = rpart.control(cp = 0, minsplit = 10)
  )
  n_splits <- desired_cardinality - 1
  cptable <- tree_model$cptable
  target_cp <- cptable[which(cptable[, 'nsplit'] == n_splits), 'CP']
  pruned_tree <- rpart::prune(tree_model, cp = target_cp)
  df$group <- as.factor(predict(pruned_tree, df, type = 'vector'))

  group_labels <- df %>%
    distinct(group, .data[[col_name]]) %>%
    group_by(group) %>%
    summarise(group_label = paste(sort(unique(.data[[col_name]])), collapse = ','), .groups = 'drop')
  df <- df |>
    left_join(group_labels, by = 'group') |>
    select(!c(col_name)) |>
    rename(!!col_name := all_of('group_label')) |>
    select(!group) |>
    mutate(
      !!rlang::sym(col_name) := as.factor(.data[[col_name]])
    )
  return(df)
} 