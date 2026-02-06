#' @title Model Definitions
#'
#' @description
#' Model definitions reads a YAML configuration file containing information
#' about various models (eg. model titles, predictor ranges, reference
#' groups, model export files, etc.). It parses all information about
#' the models and allows the user to extract information from them.
#'
#' @examples
#' \dontrun{
#' source("R/model_definitions/model_definitions.R")
#'
#' # The following model_definitions will have meta information at $meta
#' # and model information at $models. See the function read_model_definitions
#' # for further details.
#' model_definitions <- read_model_definitions(
#'   file.path("data", "models", "htnport-mpp", "htnport-reduced.yaml")
#' )
#' }
#' @name model_definitions
NULL

library(yaml)
library(viridis)
library(model.parameters.pipeline)

source("R/model_definitions/model_definitions_utils.R")
source("R/utils/function_parser.R")

all_tag <- "_all_"

#' Read Model Definitions from YAML File
#'
#' Loads and processes a YAML model definitions file, including loading
#' referenced data files, parsing predictor ranges, and applying shared
#' configuration. The returned named list has a $meta field containing
#' meta information about the algorithm that all models represent (eg.
#' the algorithm name and version), and has a $models field containing
#' all models that can be used.
#'
#' The following is an example of a full model definitions list returned
#' by this function:
#'
#'   meta = list(
#'     algorithm = "HTNPoRT",
#'     version = "1.0.0"
#'   ),
#'   models = list(
#'     female = list(
#'        title = "Female",
#'        model_export = <data.frame>,
#'        reference_group = list(
#'           clc_age = 20,
#'           fmh_15 = 2,
#'           hwmdbmi = 14.9,
#'           diabx = 2
#'        ),
#'        predictor_ranges = list(
#'           hwmdbmi = <value>,
#'           clc_age = <value>,
#'           fmh_15 = <value>,
#'           diabx = <value>
#'        ),
#'        root_dir = "data/models/htnport-mpp",
#'        variables = <data.frame>,
#'        variable_details = <data.frame>,
#'        model_steps = <data.frame>,
#'        model_index = 1,
#'        model_id = "female",
#'        model_color = "#440154FF",
#'        pipelines = list()
#'     ),
#'     male = ...
#'   )
#'
#' @param file Character string specifying the path to the YAML definitions
#'   file.
#'
#' @return A named list containing processed model definitions with loaded data.
#'   The main top-level names are $meta and $models.
#'
#' @export
read_model_definitions <- function(file) {
  root_dir <- dirname(file)
  info <- read_yaml(file)

  info <- .copy_from_all_model(info)
  info <- .assign_root_dir(info, root_dir)
  info <- .load_model_pipelines(info, root_dir)
  info <- .load_model_export_files(info, root_dir)
  info <- .parse_predictor_ranges(info)
  info <- .add_model_indices_and_ids(info)
  info <- .add_model_colors(info)
  info <- .add_empty_pipelines(info)

  info
}

#' Initialize Empty Pipeline Lists
#'
#' Adds an empty pipelines list to each model for storing cached
#' model pipeline objects during runtime.
#'
#' @param info Model definitions list containing models.
#'
#' @return Updated model definitions with empty pipelines lists.
#'
#' @keywords internal
.add_empty_pipelines <- function(info) {
  for (model_id in names(info$models)) {
    info$models[[model_id]]$pipelines <- list()
  }

  info
}

#' Create Predictor Range from Various Formats
#'
#' Converts range specifications (string expressions, vectors, or list
#' definitions) into numeric vectors.
#'
#' @param range_info Range specification as string (e.g., "1:10",
#'   "seq(1,10,2)"), vector, or list with seq parameters.
#'
#' @return Numeric vector representing the range, or NULL if parsing fails.
#'
#' @keywords internal
.create_predictor_range <- function(range_info) {
  if (is.character(range_info) && length(range_info) == 1) {
    func_info <- get_function_and_params(range_info)
    if (!is.null(func_info)) {
      if (func_info$func == "seq" && length(func_info$params) >= 2) {
        return(do.call(seq, func_info$params))
      }
    } else if (stringr::str_count(range_info, ":") == 1) {
      lower_upper <- as.integer(unlist(strsplit(range_info, ":")))
      return(lower_upper[[1]]:lower_upper[[2]])
    }
  } else if (is.vector(range_info) && is.null(names(range_info))) {
    return(range_info)
  }

  NULL
}

#' Copy Missing Values from Template
#'
#' Recursively copies values from a template object to the target object
#' for any keys not already present in the target.
#'
#' @param info List to receive copied values.
#' @param all_info Template list to copy values from.
#'
#' @return The info list with missing values filled from all_info.
#'
#' @keywords internal
.copy_from_all <- function(info, all_info) {
  for (key in names(all_info)) {
    if (!(key %in% names(info))) {
      if (is.list(info[[key]])) {
        info[[key]] <- .copy_from_all(info[[key]], all_info[[key]])
      } else {
        info[[key]] <- all_info[[key]]
      }
    }
  }

  info
}

#' Get Range from Variable Details
#'
#' Extracts the valid range of values for a variable from the variable_details
#' table in model data.
#'
#' @param model_data List containing model data with variable_details.
#' @param variable Character string specifying the variable name.
#'
#' @return Numeric vector of valid values for the variable.
#'
#' @keywords internal
.get_range_from_variable_details <- function(model_data, variable) {
  info <- model_data$variable_details |>
    filter(variable == !!variable) |>
    select(recStart, recEnd)

  full_range <- c()

  for (idx in seq_len(nrow(info))) {
    rec_start <- info$recStart[[idx]]
    rec_end <- info$recEnd[[idx]]
    rng <- rec_end
    if (rng == "copy") {
      rng <- rec_start
    }
    if (rng == "else" || is_data_missing(rng)) {
      next
    }
    rng_yaml <- read_yaml(text = rng)
    if (is.null(rng_yaml)) {
      next
    }
    if (is.vector(rng_yaml) && length(rng_yaml) == 2) {
      full_range <- append(full_range, rng_yaml[1]:rng_yaml[2])
    } else {
      # Note that read_yaml above will do some automatic conversion, such as
      # converting the string "Y" to the boolean TRUE. We do not want this,
      # so we stick with adding the range rng before calling read_yaml,
      # provided that it was not loaded as another valid format (eg. an array
      # of size 2, which represents a range from rng[1] to rng[2])
      full_range <- append(full_range, rng)
    }
  }

  full_range
}

#' Get Model Predictors
#'
#' Retrieves all predictor variable names from a model's variables table.
#'
#' @param model_data List containing model data with variables table.
#'
#' @return Character vector of predictor variable names.
#'
#' @keywords internal
.get_model_predictors <- function(model_data) {
  model_data$variables |>
    filter(role == "Predictor") |>
    pull(variable)
}

#' Parse Predictor Ranges
#'
#' Converts predictor range specifications in model definitions to numeric
#' vectors and fills in missing ranges from variable details.
#'
#' @param info Model definitions list containing models with predictor_ranges.
#'
#' @return Updated model definitions with parsed predictor ranges.
#'
#' @keywords internal
.parse_predictor_ranges <- function(info) {
  for (model_id in names(info$models)) {
    if ("predictor_ranges" %in% names(info$models[[model_id]])) {
      for (variable in names(info$models[[model_id]][["predictor_ranges"]])) {
        info$models[[model_id]]$predictor_ranges[[variable]] <-
          .create_predictor_range(
            info$models[[model_id]]$predictor_ranges[[variable]]
          )
      }
    }
  }

  for (model_id in names(info$models)) {
    if (model_id == all_tag) {
      next
    }
    predictors <- .get_model_predictors(info$models[[model_id]])
    for (predictor in predictors) {
      if (!(predictor %in% names(info$models[[model_id]]$predictor_ranges))) {
        info$models[[model_id]]$predictor_ranges[[predictor]] <-
          .get_range_from_variable_details(info$models[[model_id]], predictor)
      }
    }
  }

  info
}

#' Load Model Files
#'
#' Loads CSV files referenced in model definitions and model export files.
#'
#' @param info Model definitions list containing file references.
#' @param root_dir Character string specifying the root directory for file
#'   paths.
#'
#' @return Updated model definitions with loaded data frames.
#'
#' @keywords internal
.load_model_export_files <- function(info, root_dir) {
  for (model_id in names(info$models)) {
    model_export_file <- info$models[[model_id]]$model_export
    if (!is.null(model_export_file)) {
      model_export_file <- file.path(root_dir, model_export_file)
      model_export <- read.csv(model_export_file)
      info$models[[model_id]]$model_export <- model_export

      model_export_root <- dirname(model_export_file)

      # Load variables file
      variables_path <- model_export |>
        filter(fileType == "variables") |>
        pull(filePath)
      variables_path <- file.path(model_export_root, variables_path)
      info$models[[model_id]]$variables <- read.csv(variables_path)

      # Load variable details file
      var_details_path <- model_export |>
        filter(fileType == "variable-details") |>
        pull(filePath)
      var_details_path <- file.path(model_export_root, var_details_path)
      info$models[[model_id]]$variable_details <- read.csv(var_details_path)

      # Load model steps file
      model_steps_path <- model_export |>
        filter(fileType == "model-steps") |>
        pull(filePath)
      model_steps_path <- file.path(model_export_root, model_steps_path)
      info$models[[model_id]]$model_steps <- read.csv(model_steps_path)
    }
  }
  info
}

#' Load Model Pipelines
#'
#' Loads the model pipeline for each model in the definitions by reading
#' the model export file and preparing it with \code{prepare_model_pipeline}.
#'
#' @param info Model definitions list.
#' @param root_dir Character string specifying the root directory containing
#'   model export files.
#'
#' @return Updated model definitions with model_pipeline loaded for each model.
#'
#' @keywords internal
.load_model_pipelines <- function(info, root_dir) {
  for (model_id in names(info$models)) {
    model_export <- info$models[[model_id]]$model_export
    model_export_file <- file.path(root_dir, model_export)
    info$models[[model_id]]$model_pipeline <-
      prepare_model_pipeline(model_export_file)
  }

  info
}

#' Assign Root Directory to Models
#'
#' Sets the root_dir field for each model in the definitions.
#'
#' @param info Model definitions list.
#' @param root_dir Character string specifying the root directory.
#'
#' @return Updated model definitions with root_dir assigned.
#'
#' @keywords internal
.assign_root_dir <- function(info, root_dir) {
  for (model_id in names(info$models)) {
    info$models[[model_id]]$root_dir <- root_dir
  }
  info
}

#' Copy Shared Configuration to All Models
#'
#' Copies configuration from the "_all_" template model to all other models,
#' without overwriting existing values.
#'
#' @param info Model definitions list potentially containing an "_all_" model.
#'
#' @return Updated model definitions with shared configuration applied.
#'
#' @keywords internal
.copy_from_all_model <- function(info) {
  if (all_tag %in% names(info$models)) {
    for (model_id in names(info$models)) {
      if (model_id == all_tag) {
        next
      }
      info$models[[model_id]] <- .copy_from_all(
        info$models[[model_id]],
        info$models[[all_tag]]
      )
    }

    info$models[[all_tag]] <- NULL
  }

  info
}

#' Add Sequential Indices to Models
#'
#' Assigns a sequential model_index to each model in the definitions.
#'
#' @param info Model definitions list.
#'
#' @return Updated model definitions with model_index assigned.
#'
#' @keywords internal
.add_model_indices_and_ids <- function(info) {
  idx <- 1
  for (model_id in names(info$models)) {
    info$models[[model_id]]$model_index <- idx
    info$models[[model_id]]$model_id <- model_id
    idx <- idx + 1
  }

  info
}

#' Assign Colors to Models
#'
#' Assigns distinct colors from the viridis palette to each model. If the
#' model already has the $model_colors field specified (from the loaded
#' config), then the existing model color is used.
#'
#' @param info Model definitions list.
#'
#' @return Updated model definitions with model_color assigned to each model.
#'
#' @keywords internal
.add_model_colors <- function(info) {
  model_colors <- viridis(length(info$models))
  for (idx in seq_along(names(info$models))) {
    model_id <- names(info$models)[[idx]]
    if (is.null(info$models[[model_id]]$model_color)) {
      info$models[[model_id]]$model_color <- model_colors[[idx]]
    }
  }

  info
}
