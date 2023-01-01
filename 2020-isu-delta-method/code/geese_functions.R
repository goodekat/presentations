
library(dplyr)

# ---------------------------------------------------------------------------
# Function for extracting betas associated with an age, a site, and a time
# ---------------------------------------------------------------------------

# get_betas_and_vc extracts the coefficients and covariances 
# from the data frames associated with an age, a site, and a time. 
# The output from these functions will be used in the `deltamethod` function.

### Inputs

# - age = character specifying an age group ('Juvenile', 'Sub Adult', or 'Adult')
# - site = character specifying a site type ('Rural' or 'Urban')
# - time = integer specifying a year (1999 to 2019)
# - betas_full = data frame with all beta values from the rmark model (beta_df)
# - vc_full = data frame with full covariance matrix from the rmark model (vc_df)

### Output

# List with three objects:
# - case: Vector with age, site, and time values
# - betas: Data frame with two columns:
#     - term = model terms for r and S
#     - beta = corresponding coefficient
# - vc: Matrix with covariance values corresponding to coefficients in betas

get_betas_and_vc <- function(age, site, time, betas_full, vc_full) {
  
  # Convert age to an interval and stop if entered incorrectly
  if (age == "Juvenile") {
    age_int = "(0,0.5]"
  } else if (age == "Sub Adult") {
    age_int = "(0.5,2.5]"
  } else if (age == "Adult") {
    age_int = "(2.5,23]"
  } else {
    stop("Age entered incorrectly. Must be 'Juvenile', 'Sub Adult', or 'Adult'.")
  }
  
  # Stop if site entered incorrectly
  if (!(site %in% c("Rural", "Urban"))) {
    stop("Site entered incorrectly. Must be 'Rural' or 'Urban'.")
  }
  
  # Stop if year entered incorrectly
  if (!(time %in% 1999:2019)) {
    stop("Time entered incorrectly. Must be a year between 1999 and 2019.")
  }
  
  # Based on input age, site, and time, create a vector of terms in the 
  # model (intercept, main effect, two-way interaction, and three-way 
  # interaction)
  terms = c("(Intercept)", 
            paste0("age", age_int), 
            paste0("site", site), 
            paste0("time", time),
            paste0("age", age_int, ":site", site),
            paste0("age", age_int, ":time", time), 
            paste0("site", site, ":time", time), 
            paste0("age", age_int, ":site", site, ":time", time))
  
  # Select the betas specified in the model terms, join the param and 
  # coef columns into one column, and arrange the values by term
  betas_selected <-
    betas_full %>%
    dplyr::filter(coef %in% terms) %>%
    tidyr::unite(col = "term", param:coef, sep = ":") %>%
    dplyr::arrange(term)
  
  # Select the covariances assocaited with the specified in the model
  # terms, join the param1 and coef1 columns and param2 and coef2 into 
  # one column, arrange the values by term1 and term2, and convert the
  # data frame back to a matrix
  vc_selected <-
    vc_full %>% 
    dplyr::filter(coef1 %in% terms, coef2 %in% terms) %>%
    tidyr::unite(col = "term1", param1:coef1, sep = ":") %>%
    tidyr::unite(col = "term2", param2:coef2, sep = ":") %>%
    arrange(term1, term2) %>%
    tidyr::pivot_wider(names_from = term2, values_from = cov)
  vc_selected_matrix = as.matrix(vc_selected %>% select(-term1))
  rownames(vc_selected_matrix) = vc_selected$term1
  
  # Return the betas and vc in a list
  list(case = c(age, site, time), betas = betas_selected, vc = vc_selected_matrix)
  
}

# ---------------------------------------------------------------------------
#  Function for applying msm::deltamethod to a specified age, site, and time
# ---------------------------------------------------------------------------

# `compute_dmse` computes the delta method standard error for f 
# associated with an age, a site, and a time. This can be used in 
# conjunction with the map functions from `purrr` to quickly and easily 
# compute the delta method standard errors for multiple sets of age, site, 
# and time values. Note that `compute_dmse` requires the same inputs as 
# the `get_betas_and_vc` function and depends on the `get_betas_and_vc` 
# function.

### Inputs

# - `age` = character specifying an age group (`'Juvenile'`, `'Sub Adult'`, or `'Adult'`)
# - `site` = character specifying a site type (`'Rural'`, `'Urban'`)
# - `time` = integer specifying a year (1999 to 2019)
# - `betas_full` = data frame with all beta values from the rmark model (`beta_df`)
# - `vc_full` = data frame with full covariance matrix from the rmark model (`vc_df`)

### Output

# Data frame with 4 columns:  
# - `age`: input age
# - `site`: input site
# - `time`: input year
# - `se`: standard error for f using the delta method

compute_dmse <- function(age, site, time, betas_full, vc_full){
  
  # Extract the betas and vc
  beta_vc = 
    get_betas_and_vc(age = age,
                     site = site, 
                     time = time, 
                     betas_full = betas_full, 
                     vc_full = vc_full)
  
  # Extract terms from beta_vc
  case = beta_vc$case
  betas = beta_vc$betas
  vc = beta_vc$vc
  
  # Check that the betas and vc are ordered in the same way
  if(!identical(betas$term, rownames(vc)) | !(identical(betas$term, colnames(vc)))) {
    stop("Terms in beta and vc not ordered the same.")
  }
  
  # Determine the length of the beta vector divided by 2 (number of betas per r and S)
  p = length(betas$beta) / 2
  
  # Double check that the r parameters are before the S parameters
  if(!sum(stringr::str_detect(betas$term[1:p], "r:")) == p |
     !sum(stringr::str_detect(betas$term[(p+1):(p+p)], "S:")) == p) {
    stop("r parameters are not before S parameters")
  }
  
  # Prepare the f formula expression for the delta method function based on the 
  # number of coefficients in the beta vector
  top = paste0("(exp(", paste0(rep("x", p), 1:p, collapse = ' + '), "))")
  bottom_left = paste0("(1 + exp(", paste0(rep("x", p), 1:p, collapse = ' + '), "))")
  bottom_right = paste0("(1 + exp(", paste0(rep("x", p), (p+1):(p+p), collapse = ' + '), "))")
  bottom = paste0("(", bottom_left, " * ", bottom_right, ")")
  formula = paste("~", top, "/", bottom)
  
  # Apply the deltamethod function
  se = msm::deltamethod(as.formula(formula), mean = betas$beta, cov = vc)
  
  # Return the se in a dataframe with the age, site, and time
  return(data.frame(age, site, time, se))
  
}