# OR Calculation Functions for HTNPoRT Algorithm Viewer
# Functions to calculate odds ratios across age for predictors with RCS transformations

#' Calculate Restricted Cubic Spline (RCS) Terms
#'
#' @param x Numeric vector of input values
#' @param knots Numeric vector of knot positions
#' @return Matrix with RCS terms (columns = number of terms)
calculate_rcs <- function(x, knots) {
  # Number of knots
  nk <- length(knots)

  # Number of RCS terms is nk - 1 (linear term + nk-2 restricted terms)
  if (nk < 3) {
    stop("At least 3 knots required for RCS")
  }

  # Initialize matrix to store RCS terms
  # For k knots, we get k-1 terms total (1 linear + (k-2) restricted)
  n_terms <- nk - 1
  rcs_matrix <- matrix(NA, nrow = length(x), ncol = n_terms)

  # First term is just x
  rcs_matrix[, 1] <- x

  # Calculate additional restricted terms
  if (n_terms > 1) {
    # Knot positions
    t1 <- knots[1]
    tk_minus_1 <- knots[nk - 1]
    tk <- knots[nk]

    # Calculate terms 2 through (nk-2)
    for (j in 2:n_terms) {
      tj <- knots[j]

      # Calculate the three cubic terms with truncation at zero
      term1 <- pmax(x - tj, 0)^3
      term2 <- pmax(x - tk_minus_1, 0)^3 * (tk - tj) / (tk - t1)
      term3 <- pmax(x - tk, 0)^3 * (tk_minus_1 - tj) / (tk - t1)

      rcs_matrix[, j] <- term1 - term2 + term3
    }
  }

  return(rcs_matrix)
}

#' Apply Centering to Values
#'
#' @param values Numeric vector or matrix to center
#' @param center_vals Centering values (single value or vector matching dimensions)
#' @return Centered values
apply_centering <- function(values, center_vals) {
  if (is.matrix(values)) {
    # Center each column
    if (length(center_vals) != ncol(values)) {
      stop(paste("Center values length", length(center_vals),
                 "doesn't match matrix columns", ncol(values)))
    }
    sweep(values, 2, center_vals, "-")
  } else if (length(values) > 1) {
    # Vector centering
    if (length(center_vals) != length(values)) {
      stop(paste("Center values length", length(center_vals),
                 "doesn't match values length", length(values)))
    }
    values - center_vals
  } else {
    # Single value
    values - center_vals
  }
}

#' Calculate Log-Odds for Given Predictor Values
#'
#' @param coefficients Named vector of coefficients
#' @param predictor_values Named list of predictor values (after transformations and centering)
#' @return Numeric value of log-odds
calculate_log_odds <- function(coefficients, predictor_values) {
  # Start with intercept
  log_odds <- coefficients["Intercept"]

  # Add contribution from each predictor
  for (var_name in names(predictor_values)) {
    if (var_name %in% names(coefficients)) {
      log_odds <- log_odds + coefficients[var_name] * predictor_values[[var_name]]
    }
  }

  return(log_odds)
}

#' Calculate Odds Ratio Curve for a Predictor Across Age Range
#'
#' @param predictor Variable name (e.g., "diabx")
#' @param predictor_value Value or category (e.g., 1 for diabx_1, or numeric value for continuous)
#' @param model_data List containing all model data (logistic, rcs, center, interactions, etc.)
#' @param age_range Vector of ages to calculate OR for
#' @return Data frame with age and OR columns
calculate_or_curve <- function(predictor, predictor_value, model_data, age_range = 20:79) {

  # Get model components
  coefficients <- setNames(model_data$logistic$coefficient, model_data$logistic$variable)
  variables <- model_data$variables
  rcs_info <- model_data$rcs
  center_info <- model_data$center
  interactions <- model_data$interactions

  # Get predictor information
  pred_info <- variables[variables$variable == predictor, ]
  pred_type <- pred_info$variableType

  # Initialize results
  results <- data.frame(
    age = age_range,
    OR = NA_real_
  )

  # Get age RCS information
  age_rcs_info <- rcs_info[rcs_info$variable == "clc_age", ]
  age_knots <- as.numeric(strsplit(age_rcs_info$knots, ";")[[1]])
  age_rcs_vars <- strsplit(age_rcs_info$rcsVariables, ";")[[1]]

  # Get centering values for age RCS terms
  age_center_vals <- center_info[center_info$origVariable %in% age_rcs_vars, ]
  age_center_vals <- setNames(age_center_vals$centerValue, age_center_vals$origVariable)

  # Loop through each age in the range
  for (i in seq_along(age_range)) {
    age <- age_range[i]

    # Calculate age RCS terms
    age_rcs <- calculate_rcs(age, age_knots)[1, ]  # Extract as vector

    # Center age RCS terms
    age_rcs_list <- list()
    for (j in seq_along(age_rcs_vars)) {
      centered_value <- age_rcs[j] - age_center_vals[[age_rcs_vars[j]]]
      age_rcs_list[[paste0(age_rcs_vars[j], "_C")]] <- centered_value
    }

    # Calculate log-odds for exposed vs unexposed
    if (pred_type == "Categorical") {
      # For categorical: Compare category vs reference (which has OR = 1)
      # Exposed: predictor = 1 (or specified value)
      # Unexposed: predictor = 0 (reference category)

      # Build variable name (e.g., "diabx_1_C")
      pred_var_centered <- paste0(predictor, "_", predictor_value, "_C")

      # Get centering value
      pred_center <- center_info[center_info$centeredVariable == pred_var_centered, "centerValue"]

      # Exposed (category present)
      exposed_value <- 1 - pred_center
      pred_list_exposed <- c(age_rcs_list, setNames(list(exposed_value), pred_var_centered))

      # Unexposed: reference category (value = 0)
      pred_list_unexposed <- age_rcs_list

      # Check for interactions with age
      interaction_terms <- interactions[grepl(paste0(predictor, "_", predictor_value),
                                              interactions$interactingVariables), ]

      if (nrow(interaction_terms) > 0) {
        for (k in 1:nrow(interaction_terms)) {
          int_var <- interaction_terms$interactionVariable[k]
          int_var_centered <- paste0(int_var, "_C")

          # Parse which age RCS term this interacts with
          int_parts <- strsplit(interaction_terms$interactingVariables[k], ";")[[1]]
          age_rcs_term <- int_parts[grepl("clc_age_rcs", int_parts)]

          # Get interaction centering value
          int_center <- center_info[center_info$centeredVariable == int_var_centered, "centerValue"]

          # Interaction value for exposed (age_rcs * predictor_value, then center)
          int_value_exposed <- age_rcs_list[[paste0(age_rcs_term, "_C")]] * exposed_value - int_center
          pred_list_exposed[[int_var_centered]] <- int_value_exposed

          # Interaction value for unexposed (age_rcs * 0, then center)
          int_value_unexposed <- age_rcs_list[[paste0(age_rcs_term, "_C")]] * (0 - pred_center) - int_center
          pred_list_unexposed[[int_var_centered]] <- int_value_unexposed
        }
      }

      # Calculate log-odds for exposed and unexposed
      log_odds_exposed <- calculate_log_odds(coefficients, pred_list_exposed)
      log_odds_unexposed <- calculate_log_odds(coefficients, pred_list_unexposed)

    } else {
      # For continuous predictors: Compare one-unit increase
      # Get RCS info if continuous predictor has RCS transformation
      pred_rcs_info <- rcs_info[rcs_info$variable == predictor, ]

      if (nrow(pred_rcs_info) > 0) {
        # Predictor has RCS transformation
        pred_knots <- as.numeric(strsplit(pred_rcs_info$knots, ";")[[1]])
        pred_rcs_vars <- strsplit(pred_rcs_info$rcsVariables, ";")[[1]]

        # Get centering values
        pred_center_vals <- center_info[center_info$origVariable %in% pred_rcs_vars, ]
        pred_center_vals <- setNames(pred_center_vals$centerValue, pred_center_vals$origVariable)

        # Calculate RCS for reference value (median/mean) and reference + predictor_value
        reference_value <- pred_center_vals[[pred_rcs_vars[1]]] # Use first term's center as reference

        # Exposed: reference + predictor_value
        pred_rcs_exposed <- calculate_rcs(reference_value + predictor_value, pred_knots)[1, ]
        # Unexposed: reference
        pred_rcs_unexposed <- calculate_rcs(reference_value, pred_knots)[1, ]

        # Build predictor lists with centered values
        pred_list_exposed <- age_rcs_list
        pred_list_unexposed <- age_rcs_list

        for (j in seq_along(pred_rcs_vars)) {
          pred_list_exposed[[paste0(pred_rcs_vars[j], "_C")]] <-
            pred_rcs_exposed[j] - pred_center_vals[[pred_rcs_vars[j]]]
          pred_list_unexposed[[paste0(pred_rcs_vars[j], "_C")]] <-
            pred_rcs_unexposed[j] - pred_center_vals[[pred_rcs_vars[j]]]
        }

        # Check for interactions with age
        interaction_terms <- interactions[grepl(predictor, interactions$interactingVariables), ]

        if (nrow(interaction_terms) > 0) {
          for (k in 1:nrow(interaction_terms)) {
            int_var <- interaction_terms$interactionVariable[k]
            int_var_centered <- paste0(int_var, "_C")

            # Parse interaction components
            int_parts <- strsplit(interaction_terms$interactingVariables[k], ";")[[1]]
            age_rcs_term <- int_parts[grepl("clc_age_rcs", int_parts)]
            pred_rcs_term <- int_parts[grepl(predictor, int_parts)]

            # Get centered predictor RCS values
            pred_exposed_centered <- pred_list_exposed[[paste0(pred_rcs_term, "_C")]]
            pred_unexposed_centered <- pred_list_unexposed[[paste0(pred_rcs_term, "_C")]]

            # Interaction values (product of centered age RCS and centered predictor RCS)
            int_value_exposed <- age_rcs_list[[paste0(age_rcs_term, "_C")]] * pred_exposed_centered
            int_value_unexposed <- age_rcs_list[[paste0(age_rcs_term, "_C")]] * pred_unexposed_centered

            # Get interaction centering value
            int_center <- center_info[center_info$centeredVariable == int_var_centered, "centerValue"]

            pred_list_exposed[[int_var_centered]] <- int_value_exposed - int_center
            pred_list_unexposed[[int_var_centered]] <- int_value_unexposed - int_center
          }
        }

        # Calculate log-odds
        log_odds_exposed <- calculate_log_odds(coefficients, pred_list_exposed)
        log_odds_unexposed <- calculate_log_odds(coefficients, pred_list_unexposed)

      } else {
        stop("Continuous predictor without RCS not yet implemented")
      }
    }

    # Calculate OR
    results$OR[i] <- exp(log_odds_exposed - log_odds_unexposed)
  }

  return(results)
}

#' Get OR Curves for All Categories of a Categorical Predictor
#'
#' @param predictor Variable name (e.g., "diabx")
#' @param model_data List containing all model data
#' @param age_range Vector of ages to calculate OR for
#' @return Data frame with age, category, and OR columns
get_categorical_or_curves <- function(predictor, model_data, age_range = 20:79) {

  # Get coefficients to find which categories are in the model
  coefficients <- model_data$logistic
  pred_coeffs <- coefficients[grepl(paste0("^", predictor, "_[0-9]+_C$"), coefficients$variable), ]

  if (nrow(pred_coeffs) == 0) {
    stop(paste("No coefficients found for predictor:", predictor))
  }

  # Get variable details for all categories
  var_details <- model_data$variable_details[
    model_data$variable_details$variable == predictor &
    model_data$variable_details$typeEnd == "cat" &
    !grepl("NA::b", model_data$variable_details$dummyVariable),
  ]

  # Determine reference category (the one NOT in the coefficients)
  all_dummies <- var_details$dummyVariable[var_details$dummyVariable != "N/A"]
  coeff_dummies <- gsub("_C$", "", pred_coeffs$variable)
  ref_dummy <- setdiff(all_dummies, coeff_dummies)

  if (length(ref_dummy) > 0) {
    ref_label <- var_details$catLabel[var_details$dummyVariable == ref_dummy[1]]
  } else {
    ref_label <- "Reference"
  }

  # Initialize results list
  all_curves <- list()

  # Calculate OR curve for each non-reference category
  for (i in 1:nrow(pred_coeffs)) {
    # Extract category number from coefficient name (e.g., "diabx_1_C" -> "1")
    coeff_name <- pred_coeffs$variable[i]
    category_num <- gsub(paste0(predictor, "_|_C"), "", coeff_name)

    # Get category label from variable details
    category_dummy <- paste0(predictor, "_", category_num)
    category_label <- var_details$catLabel[var_details$dummyVariable == category_dummy]

    if (length(category_label) == 0) {
      category_label <- paste("Category", category_num)
    }

    # Calculate OR curve
    or_curve <- calculate_or_curve(predictor, category_num, model_data, age_range)
    or_curve$category <- category_label
    or_curve$category_label <- paste0(category_label, " vs ", ref_label)

    all_curves[[i]] <- or_curve
  }

  # Combine all curves
  result <- do.call(rbind, all_curves)
  rownames(result) <- NULL

  return(result)
}
