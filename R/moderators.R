## shared plumbing for the plot functions: moderator handling and the
## canonical MetaLab multilevel model

solarized_palette <- c("#268bd2", "#cb4b16", "#859900", "#6c71c4", "#2aa198",
                       "#b58900", "#d33682", "#dc322f")

## a moderator is categorical if its data are non-numeric (numeric-typed spec
## fields are numeric columns after tidy coercion)
categorical_mods <- function(metalab_data, moderators) {
  purrr::keep(moderators, function(mod) {
    !is.numeric(metalab_data[[mod]])
  })
}

mod_group <- function(metalab_data, moderators) {
  cats <- categorical_mods(metalab_data, moderators)
  if (length(cats)) paste(cats, collapse = "_") else "all_mod"
}

## drop rows missing any selected moderator; add the combined grouping column
mod_data <- function(metalab_data, moderators) {
  for (mod in moderators) {
    metalab_data <- metalab_data %>% filter(!is.na(.data[[mod]]))
  }
  cats <- categorical_mods(metalab_data, moderators)
  group_col <- mod_group(metalab_data, moderators)
  if (!group_col %in% names(metalab_data)) {
    metalab_data[[group_col]] <-
      do.call(paste, c(purrr::map(cats, ~ metalab_data[[.x]]), sep = "\n"))
  }
  metalab_data
}

no_mod_model <- function(metalab_data, es_col, es_var_col) {
  metafor::rma.mv(yi = metalab_data[[es_col]], V = metalab_data[[es_var_col]],
                  random = ~ 1 | metalab_data[["short_cite"]] /
                    metalab_data[["same_infant_calc"]] /
                    metalab_data[["unique_row"]],
                  slab = make.unique(metalab_data[["short_cite"]]),
                  method = "REML")
}

## the canonical MetaLab model: multilevel random-effects REML with effect
## sizes nested in infant groups nested in papers; moderators enter as
## additive fixed effects
metalab_model <- function(metalab_data, moderators, es_col, es_var_col) {
  if (length(moderators) == 0) {
    return(no_mod_model(metalab_data, es_col, es_var_col))
  }
  dat <- mod_data(metalab_data, moderators)
  rma_formula <- stats::as.formula(
    sprintf("%s ~ %s", es_col, paste(moderators, collapse = "+")))
  metafor::rma.mv(rma_formula, V = dat[[es_var_col]],
                  random = ~ 1 | short_cite / same_infant_calc / unique_row,
                  slab = make.unique(short_cite), data = dat,
                  method = "REML")
}
