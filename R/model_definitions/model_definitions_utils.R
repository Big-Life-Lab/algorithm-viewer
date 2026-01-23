library(shiny)
library(tools)
library(dplyr)
library(rlang)

#' Check if Data Value is Missing
#'
#' Tests whether a value is NULL, empty, or represents missing data ("n/a").
#'
#' @param val Value to check.
#'
#' @return TRUE if the value is missing, FALSE otherwise.
#'
#' @export
is_data_missing <- function(val) {
  is.null(val) ||
    length(val) == 0 ||
    (is.character(val) && stringr::str_to_lower(val) == "n/a")
}

#' Get Variable Metadata
#'
#' Retrieves a specific metadata field for a variable from the model's
#' variables table.
#'
#' @param model_data List containing model data with variables table.
#' @param variable Character string specifying the variable name.
#' @param heading Character string specifying the column name to retrieve.
#'
#' @return The requested metadata value, or NULL if missing.
#'
#' @export
get_variable_info <- function(model_data, variable, heading) {
  val <- model_data$variables |>
    filter(variable == !!variable) |>
    pull(!!heading)
  if (is_data_missing(val)) {
    NULL
  } else {
    val
  }
}

#' Get Predictor Range
#'
#' Retrieves the range of valid values for a predictor variable.
#'
#' @param model_data List containing model data with predictor_ranges.
#' @param variable Character string specifying the variable name.
#'
#' @return Numeric vector of valid predictor values.
#'
#' @export
get_predictor_range <- function(model_data, variable) {
  model_data$predictor_ranges[[variable]]
}

#' Map Variable Values to Labels
#'
#' Creates a named list mapping internal variable values to display labels
#' for categorical variables.
#'
#' @param model_data List containing model data with variable_details.
#' @param variable Character string specifying the variable name.
#'
#' @return A named list mapping values to their display labels.
#'
#' @export
get_variable_values_to_labels <- function(model_data, variable) {
  info <- get_variable_labels_to_values(model_data, variable)
  as.list(setNames(names(info), info))
}

#' Map Variable Labels to Values
#'
#' Creates a named list mapping display labels to internal variable values
#' for categorical variables.
#'
#' @param model_data List containing model data with variable_details.
#' @param variable Character string specifying the variable name.
#'
#' @return A named list mapping labels to their internal values.
#'
#' @keywords internal
get_variable_labels_to_values <- function(model_data, variable) {
  info <- model_data$variable_details |>
    filter(variable == !!variable) |>
    filter(recEnd != "NA::b") |>
    pull(recEnd, catLabel) |>
    as.list()
  info
}

#' Check Variable Type
#'
#' Tests whether a variable is of a specific type.
#'
#' @param model_data List containing model data with variables table.
#' @param variable Character string specifying the variable name.
#' @param type Character string specifying the type to check (e.g.,
#'   "Categorical").
#'
#' @return TRUE if the variable is of the specified type, FALSE otherwise.
#'
#' @export
is_variable_of_type <- function(model_data, variable, type) {
  info <- model_data$variables |>
    filter(variable == !!variable) |>
    filter(variableType == !!type)
  nrow(info) > 0
}

#' Check if Variable is Categorical
#'
#' Tests whether a variable is of categorical type.
#'
#' @param model_data List containing model data with variables table.
#' @param variable Character string specifying the variable name.
#'
#' @return TRUE if the variable is categorical, FALSE otherwise.
#'
#' @export
is_variable_categorical <- function(model_data, variable) {
  is_variable_of_type(model_data, variable, "Categorical")
}

#' Check if Variable is Continuous
#'
#' Tests whether a variable is of continuous type.
#'
#' @param model_data List containing model data with variables table.
#' @param variable Character string specifying the variable name.
#'
#' @return TRUE if the variable is continuous, FALSE otherwise.
#'
#' @export
is_variable_continuous <- function(model_data, variable) {
  is_variable_of_type(model_data, variable, "Continuous")
}

#' Get Variable Value from Label
#'
#' Converts a display label to its internal variable value for categorical
#' variables.
#'
#' @param model_data List containing model data with variable_details.
#' @param variable Character string specifying the variable name.
#' @param label Character string specifying the display label, or a vector
#'  of character strings specifying multiple display labels.
#'
#' @return The internal value corresponding to the label, or a vector of
#'  values corresponding to the multiple labels.
#'
#' @export
get_variable_value_from_label <- function(model_data, variable, label) {
  labels_to_values <- get_variable_labels_to_values(model_data, variable)
  unname(unlist(labels_to_values)[label])
}

#' Get Variable Label from Value
#'
#' Converts an internal variable value to its display label for categorical
#' variables.
#'
#' @param model_data List containing model data with variable_details.
#' @param variable Character string specifying the variable name.
#' @param value The internal value to convert, or a vector of values to
#'  convert.
#'
#' @return The display label corresponding to the value, or a vector of
#'  display labels corresponding to the multiple values.
#'
#' @export
get_variable_label_from_value <- function(model_data, variable, value) {
  values_to_labels <- get_variable_values_to_labels(model_data, variable)
  unname(unlist(values_to_labels)[value])
}

#' Get Variable Label with Units
#'
#' Retrieves the display label for a variable, appending units if available.
#'
#' @param model_data List containing model data with variables table.
#' @param variable Character string specifying the variable name.
#'
#' @return Character string with the label and optional units in parentheses.
#'
#' @keywords internal
get_variable_label_and_units <- function(model_data, variable) {
  label <- get_variable_info(model_data, variable, "label")
  units <- get_variable_info(model_data, variable, "units")

  res <- label
  if (!is.null(units)) {
    res <- glue::glue("{res} ({units})")
  }
  res
}

#' Get Variable Label
#'
#' Retrieves the display label for a variable.
#'
#' @param model_data List containing model data with variables table.
#' @param variable Character string specifying the variable name.
#'
#' @return Character string with the variable's display label.
#'
#' @keywords internal
get_variable_label <- function(model_data, variable) {
  get_variable_info(model_data, variable, "label")
}

#' Convert Data Frame Variable Values to Labels
#'
#' Replaces internal variable values with display labels in a data frame column
#' for categorical variables.
#'
#' @param df Data frame containing the column to convert.
#' @param model_data List containing model data with variable_details.
#' @param variable Character string specifying the variable name.
#' @param df_column Character string specifying the column name to convert.
#'
#' @return The data frame with converted labels in the specified column.
#'
#' @keywords internal
convert_df_variable_to_label <- function(df, model_data, variable, df_column) {
  if (is_variable_categorical(model_data, variable)) {
    allowable_values <- get_predictor_range(model_data, variable)
    labels <- get_variable_label_from_value(model_data, variable,
                                            allowable_values)
    # Get the index into allowable_values for all values in df_column
    indices <- unlist(df[[df_column]]) |>
      lapply(function(x) which(x == allowable_values)) |>
      unlist()
    df[[df_column]] <- labels[indices]
  }
  df
}

#' Extract Field from All Models
#'
#' Retrieves a specific field value from each model in a list of models.
#'
#' @param models List of model data objects.
#' @param field Character string specifying the field name to extract.
#'
#' @return Vector of field values, with NULL for models missing the field.
#'
#' @keywords internal
get_all_models_field <- function(models, field) {
  fields <- c()
  for (model_data in models) {
    if (field %in% names(model_data)) {
      fields <- append(fields, model_data[[field]])
    } else {
      fields <- append(fields, NULL)
    }
  }
  fields
}

#' Get Model Colors
#'
#' Retrieves the color assigned to each model for plotting.
#'
#' @param models List of model data objects.
#' @param names_field Character string specifying which field to use for naming
#'   the result. Default is "title". Set to NULL to return unnamed vector.
#'
#' @return Named character vector of colors, keyed by the names_field values.
#'
#' @keywords internal
get_model_colors <- function(models, names_field = "title") {
  model_colors <- get_all_models_field(models, "model_color")
  if (!is.null(names_field)) {
    names(model_colors) <- get_all_models_field(models, names_field)
  }

  model_colors
}

#' Get Model Choices for UI Selection
#'
#' Builds a named list of available models for use in Shiny selectInput widgets.
#' The names are the human-readable model titles visible to users, and the
#' values are the model IDs as found in the loaded configuration file.
#'
#' @param models List of model data objects.
#' @param names_field Character string specifying which field to use for display
#'   names. Default is "title". Set to NULL to return unnamed list.
#'
#' @return Named list where names are model titles and values are model IDs.
#'
#' @export
get_model_choices <- function(models, names_field = "title") {
  choices <- get_model_ids(models)
  if (!is.null(names_field)) {
    names(choices) <- get_all_models_field(models, names_field)
  }

  choices
}

#' Get Model IDs
#'
#' Retrieves the unique identifiers for all models.
#'
#' @param models List of model data objects.
#'
#' @return Character vector of model IDs.
#'
#' @keywords internal
get_model_ids <- function(models) {
  names(models)
}

#' Get Model Titles
#'
#' Retrieves display titles for all models, optionally with color indicators.
#'
#' @param models List of model data objects.
#' @param include_model_colors Logical indicating whether to include colored
#'   HTML spans alongside titles. Default is FALSE.
#'
#' @return List of model titles, as HTML if include_model_colors is TRUE.
#'
#' @keywords internal
get_model_titles <- function(models, include_model_colors = FALSE) {
  model_values <- list()

  for (model_data in models) {
    title <- model_data$title
    if (include_model_colors) {
      title <- shiny::HTML(add_model_color(model_data, title, "12px", "12px"))
    }
    model_values[[length(model_values) + 1]] <- title
  }

  model_values
}

#' Add Color Indicator to Label
#'
#' Appends or prepends an HTML color box to a label string.
#'
#' @param model_data List containing model data with model_color field.
#' @param label Character string to add the color indicator to.
#' @param width Character string specifying the CSS width of the color box.
#' @param height Character string specifying the CSS height of the color box.
#' @param after Logical indicating whether to place the color box after (TRUE)
#'   or before (FALSE) the label. Default is TRUE.
#'
#' @return Character string with HTML color box added to the label.
#'
#' @keywords internal
add_model_color <- function(model_data, label, width, height, after = TRUE) {
  color <- model_data$model_color
  box <- glue::glue(
    "<span style='background-color: {color}; display: inline-block; ",
    "width: {width}; height: {height}; vertical-align: middle; ",
    "margin-bottom: 4px'></span>"
  )
  label <- ifelse(
    after,
    glue::glue("{label} {box}"),
    glue::glue("{box} {label}")
  )
  label
}

#' Get Last Reference Group Values
#'
#' Retrieves the current reference group values, merging any user-modified
#' values from last_reference_group with the original defaults.
#'
#' @param model_data List containing model data with reference_group and
#'   optionally last_reference_group fields.
#'
#' @return Named list of reference group values for all predictor variables.
#'
#' @keywords internal
get_last_refgroup_values <- function(model_data) {
  reference_group <- model_data$reference_group

  if (!is.null(model_data$last_reference_group)) {
    last_ref <- model_data$last_reference_group

    for (variable in names(last_ref)) {
      val <- last_ref[[variable]]
      reference_group[[variable]] <- val
    }
  }

  reference_group
}
