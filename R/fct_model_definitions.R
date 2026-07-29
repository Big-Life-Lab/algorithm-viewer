#' Model Definitions
#'
#' Model definitions reads a YAML configuration file containing information
#' about various models (eg. model titles, predictor allowable values, reference
#' groups, model export files, etc.). It parses all information about
#' the models and allows the user to extract information from them.
#'
#' @examples
#' \dontrun{
#' # The following model_definitions will have meta information at $meta
#' # and model information at $models. See the function read_model_definitions
#' # for further details.
#' model_definitions <- read_model_definitions(
#'   file.path("inst/extdata/models/htnport-mpp/htnport-reduced.yaml")
#' )
#' }
#' @name fct_model_definitions
#' @noRd
#' @keywords internal
NULL

# The key under the models key in an algorithm config file that contains
# values that should be applied to all models
.all_tag <- "_all_"

# For validating color strings with grepl (#RGB, #RGGBA, #RRGGBB, #RRGGBBAA)
.valid_model_color_pattern <-
  "^(#([0-9A-Fa-f]{8}|[0-9A-Fa-f]{6}|[0-9A-Fa-f]{4}|[0-9A-Fa-f]{3})|[A-Za-z]+)$"

#' Read Model Definitions from YAML File
#'
#' Loads and processes a YAML model definitions file, including loading
#' referenced data files, parsing predictor allowable values, and applying
#' shared configuration. The returned named list has a $meta field
#' containing meta information about the algorithm that all models represent
#' (eg. the algorithm name and version), and has a $models field containing
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
#'        predictor_allowable_values = list(
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
#' @noRd
#' @keywords internal
read_model_definitions <- function(file) {
  # Normalize the file path
  tryCatch(
    {
      file <- normalizePath(file, mustWork = TRUE)
    },
    error = function(e) {
      stop(htmltools::htmlEscape(paste0(
        "Could not load model definitions file: ",
        basename(file)
      )))
    }
  )

  # Get the JSON schema file, so we can validate the model definitions file.
  schema_file <- system.file(
    "extdata/schema/algorithm.schema.json",
    package = utils::packageName()
  )
  if (!nzchar(schema_file)) {
    stop("Could not find algorithm JSON schema file in package.")
  }

  # Load and validate the model definitions file
  tryCatch(
    {
      # Load and validate the file
      info <- read_and_validate_yaml(file, schema_file)
    },
    error = function(e) {
      stop(paste0(
        "Error in model definitions file ",
        htmltools::htmlEscape(basename(file)),
        ":\n\n",
        conditionMessage(e)
      ))
    }
  )

  root_dir <- dirname(file)

  info$source_file <- file

  info <- .copy_from_all_model(info)
  info <- .make_model_titles_unique(info)
  info <- .assign_root_dir(info, root_dir)
  info <- .load_model_pipelines(info, root_dir)
  info <- .load_model_export_files(info, root_dir)
  info <- .parse_predictor_allowable_values(info)
  info <- .cleanup_reference_groups(info)
  info <- .add_model_indices_and_ids(info)
  info <- .add_model_colors(info)
  .warn_nonuniform_continuous_predictors(info)
  .validate_reference_groups(info)
  .validate_predictor_consistency(info)

  info
}

#' Make model titles unique
#'
#' Ensures all model titles within the model definitions are unique by
#' appending a numeric suffix (e.g., \code{"Title (2)"}) to any duplicate
#' titles. Duplicates are resolved in order, so the first occurrence keeps
#' its original title and subsequent duplicates receive incrementing suffixes.
#'
#' @param info Model definitions list containing at minimum a \code{$models}
#'   element where each entry has a \code{title} field.
#'
#' @return The \code{info} list with model titles updated so that all titles
#'   are unique.
#'
#' @noRd
#' @keywords internal
.make_model_titles_unique <- function(info) {
  # Get all the titles
  titles <- info$models |>
    sapply(`[[`, "title") |>
    unlist() |>
    unname()

  # Modify titles so that all titles are unique
  titles <- make_string_values_unique(titles)

  # Assign all the titles to all the models
  for (idx in seq_along(info$models)) {
    model_id <- names(info$models)[[idx]]
    info$models[[model_id]]$title <- titles[[idx]]
  }

  info
}

#' Create Predictor Allowable Values from Various Formats
#'
#' Converts allowable value specifications (vectors or named list definitions)
#' into numeric vectors.
#'
#' @param info Allowable values specification as an unnamed vector, or a named
#'   list with a top-level "seq" key whose value is a named list of seq()
#'   parameters (e.g., list(seq = list(from = 1, to = 10, by = 0.1))).
#'
#' @return Numeric vector representing the allowable values, or NULL if parsing
#'   fails.
#'
#' @noRd
#' @keywords internal
.create_predictor_allowable_values <- function(info) {
  if (!is.null(names(info)) && names(info)[[1]] == "seq") {
    # Instead of setting the params with params = list(from = ..., to = ...)
    # we set each name separately (params[["from"]] <- ...). This ensures
    # that if any of the provided params are missing, that the name for
    # that param will not appear in params (ie. params[["var"]] <- NULL
    # results in a list where the "var" name does NOT exist).
    params <- list()
    seq_info <- info[["seq"]]
    params[["from"]] <- seq_info[["from"]]
    params[["to"]] <- seq_info[["to"]]
    params[["by"]] <- seq_info[["by"]]
    params[["length.out"]] <- seq_info[["length.out"]]
    return(do.call(seq, params))
  } else if (is.vector(info) && is.null(names(info))) {
    return(info)
  }

  NULL
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
#' @noRd
#' @keywords internal
.copy_from_all_model <- function(info) {
  if (.all_tag %in% names(info$models)) {
    for (model_id in names(info$models)) {
      if (model_id == .all_tag) {
        next
      }
      info$models[[model_id]] <- .copy_from_all(
        info$models[[model_id]],
        info$models[[.all_tag]]
      )
    }

    info$models[[.all_tag]] <- NULL
  }

  info
}

#' Copy Missing Values from Template
#'
#' Recursively copies values from a template object to the target object
#' for any keys not already present in the target. This function
#' is used by .copy_from_all_model (which should usually be called
#' instead of .copy_from_all)
#'
#' @param info List to receive copied values.
#' @param all_info Template list to copy values from.
#'
#' @return The info list with missing values filled from all_info.
#'
#' @noRd
#' @keywords internal
.copy_from_all <- function(info, all_info) {
  for (key in names(all_info)) {
    if (is.list(info[[key]])) {
      info[[key]] <- .copy_from_all(info[[key]], all_info[[key]])
    } else if (!(key %in% names(info))) {
      info[[key]] <- all_info[[key]]
    }
  }

  info
}

#' Get Allowable Values from Variable Details
#'
#' Extracts the allowable values for a variable from the variable_details
#' table in model data.
#'
#' @param model_data List containing model data with variable_details.
#' @param variable Character string specifying the variable name.
#'
#' @return Numeric vector of allowable values for the variable.
#'
#' @noRd
#' @keywords internal
.get_allowable_values_from_variable_details <- function(model_data, variable) {
  info <- model_data$variable_details |>
    dplyr::filter(variable == !!variable) |>
    dplyr::select(recStart, recEnd)

  allowable_values <- c()

  for (idx in seq_len(nrow(info))) {
    rec_start <- info$recStart[[idx]]
    rec_end <- info$recEnd[[idx]]
    val <- rec_end
    if (val == "copy") {
      val <- rec_start
    }
    if (val == "else" || is_data_missing(val)) {
      next
    }
    val_yaml <- yaml::read_yaml(text = val)
    if (is.null(val_yaml)) {
      next
    }
    if (is.vector(val_yaml) && length(val_yaml) == 2) {
      allowable_values <- append(
        allowable_values,
        seq(val_yaml[1], val_yaml[2])
      )
    } else {
      # Note that read_yaml above will do some automatic conversion, such as
      # converting the string "Y" to the boolean TRUE. We do not want this,
      # so we stick with adding the allowable value val before calling
      # read_yaml, provided that it was not loaded as another valid format
      # (eg. an array of size 2, which represents a range from val[1] to
      # val[2])
      allowable_values <- append(allowable_values, val)
    }
  }

  allowable_values
}

#' Get Model Predictors
#'
#' Retrieves all predictor variable names from a model's variables table.
#'
#' @param model_data List containing model data with variables table.
#'
#' @return Character vector of predictor variable names.
#'
#' @noRd
#' @keywords internal
.get_model_predictors <- function(model_data) {
  model_data$variables |>
    dplyr::filter(role == "Predictor") |>
    dplyr::pull(variable)
}

#' Parse Predictor Allowable Values
#'
#' Converts predictor allowable value specifications in model definitions to
#' numeric vectors and fills in missing allowable values from variable details.
#'
#' @param info Model definitions list containing models with
#'   predictor_allowable_values.
#'
#' @return Updated model definitions with parsed predictor allowable values.
#'
#' @noRd
#' @keywords internal
.parse_predictor_allowable_values <- function(info) {
  for (model_id in names(info$models)) {
    if ("predictor_allowable_values" %in% names(info$models[[model_id]])) {
      for (variable in names(
        info$models[[model_id]][["predictor_allowable_values"]]
      )) {
        info$models[[model_id]]$predictor_allowable_values[[variable]] <-
          .create_predictor_allowable_values(
            info$models[[model_id]]$predictor_allowable_values[[variable]]
          )
      }
    }
  }

  for (model_id in names(info$models)) {
    if (model_id == .all_tag) {
      next
    }
    predictors <- .get_model_predictors(info$models[[model_id]])
    for (predictor in predictors) {
      if (!(predictor %in% names(
        info$models[[model_id]]$predictor_allowable_values
      ))) {
        info$models[[model_id]]$predictor_allowable_values[[predictor]] <-
          .get_allowable_values_from_variable_details(
            info$models[[model_id]], predictor
          )
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
#' @noRd
#' @keywords internal
.load_model_export_files <- function(info, root_dir) {
  for (model_id in names(info$models)) {
    model_export_file <- info$models[[model_id]]$model_export
    if (!is.null(model_export_file)) {
      # Load model export file and assign to the model's $model_export
      model_export_file <- file.path(root_dir, model_export_file)
      model_export <- utils::read.csv(model_export_file)
      info$models[[model_id]]$model_export <- model_export

      # Load variables, variable-details, and model-steps files,
      # assign them to the model's $variables, $variable_details, $model_steps
      model_export_root <- dirname(model_export_file)
      for (file_type in c("variables", "variable-details", "model-steps")) {
        file_key <- gsub("[^A-Za-z0-9]", "_", file_type)

        if (file_key %in% names(info$models[[model_id]]$model_pipeline)) {
          # The file has already been loaded by the model_pipeline, so use
          # it instead of reloading it from disk
          info$models[[model_id]][[file_key]] <-
            info$models[[model_id]]$model_pipeline[[file_key]]
        } else {
          # The file has not been loaded by the model_pipeline, so load the
          # file from disk
          orig_file_path <-
            model_export[model_export$fileType == file_type, ]$filePath
          # With zero matching rows, file.path() and is_file_descendant_of()
          # would return zero-length values and the if() below would fail with
          # an opaque "argument is of length zero" error; with multiple rows
          # the file path would be ambiguous. Report both cases clearly.
          if (length(orig_file_path) != 1) {
            stop(htmltools::htmlEscape(paste0(
              "Expected exactly one row with fileType '",
              file_type,
              "' in the model export file for model '",
              model_id,
              "', but found ",
              length(orig_file_path),
              "."
            )))
          }
          file_path <- file.path(model_export_root, orig_file_path)
          if (!is_file_descendant_of(file_path, root_dir)) {
            stop(htmltools::htmlEscape(paste0(
              "The model export file of type '",
              file_type,
              "' does not exist or points to a file outside of the allowed ",
              "directory structure: ",
              orig_file_path
            )))
          }
          info$models[[model_id]][[file_key]] <- utils::read.csv(file_path)
        }
      }
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
#' @noRd
#' @keywords internal
.load_model_pipelines <- function(info, root_dir) {
  for (model_id in names(info$models)) {
    model_export <- info$models[[model_id]]$model_export
    model_export_file <- file.path(root_dir, model_export)

    if (!is_file_descendant_of(model_export_file, root_dir)) {
      stop(htmltools::htmlEscape(paste(
        "A model export file specified in the algorithm definition file does",
        "not exist or points to a file outside of the allowed directory",
        "structure:",
        model_export
      )))
    }

    info$models[[model_id]]$model_pipeline <-
      model.parameters.pipeline::prepare_model_pipeline(
        model_export_file,
        sandbox_path = root_dir
      )
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
#' @noRd
#' @keywords internal
.assign_root_dir <- function(info, root_dir) {
  for (model_id in names(info$models)) {
    info$models[[model_id]]$root_dir <- root_dir
  }
  info
}

#' Add Model IDs and Sequential Indices to Models
#'
#' Assigns a model_id and a sequential model_index to each model in the
#' definitions.
#'
#' @param info Model definitions list.
#'
#' @return Updated model definitions with model_index assigned.
#'
#' @noRd
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

#' Warn About Non-Uniform Continuous Predictor Allowable Values
#'
#' Checks every continuous predictor's allowable values for non-uniform spacing.
#' A uniform sequence (e.g. from \code{seq:} or a consecutive integer range) is
#' always reachable on a slider. A non-uniform explicit array (e.g.
#' \code{[1, 2, 10]}) causes the slider to allow intermediate positions that are
#' not valid inputs (3–9 in that example); the slider uses the minimum gap as
#' its step, but the mismatch should be fixed by using a \code{seq:}
#' specification instead.
#'
#' @param info Model definitions list (must have \code{$variables} and
#'   \code{$predictor_allowable_values} loaded).
#'
#' @return Called for side-effects only; returns NULL invisibly.
#'
#' @noRd
#' @keywords internal
.warn_nonuniform_continuous_predictors <- function(info) {
  for (model_id in names(info$models)) {
    model_data <- info$models[[model_id]]
    if (
      is.null(model_data$variables) ||
      is.null(model_data$predictor_allowable_values)
    ) {
      next
    }

    # Gather all continuous variables
    continuous_vars <- model_data$variables$variable[
      model_data$variables$variableType == "Continuous" &
        model_data$variables$role == "Predictor"
    ]

    for (variable in continuous_vars) {
      vals <- model_data$predictor_allowable_values[[variable]]
      if (is.null(vals) || length(vals) < 3) {
        next
      }

      gaps <- diff(sort(vals))
      if (!isTRUE(all.equal(min(gaps), max(gaps)))) {
        warning(paste0(
          "Continuous predictor '", variable, "' in model '", model_id,
          "' has non-uniform allowable values. The slider step will be set to ",
          "the minimum gap (", signif(min(gaps), 5), "), which allows the ",
          "slider to reach positions that are not in the defined value set. ",
          "Use a seq: specification to ensure uniform spacing."
        ))
      }
    }
  }

  invisible(NULL)
}

#' Validate Reference Group Variable Types
#'
#' Checks that every variable named in each model's reference_group exists in
#' the model's variables table and has a recognised variableType
#' ("Categorical" or "Continuous"). Stops with a descriptive error if not,
#' so that a misspelled variableType in the CSV is caught at load time rather
#' than silently rendering the wrong UI widget.
#'
#' @param info Model definitions list containing models with reference_group
#'   and variable metadata (must have $variables loaded).
#'
#' @return Called for side-effects only; returns NULL invisibly.
#'
#' @noRd
#' @keywords internal
.validate_reference_groups <- function(info) {
  recognised_types <- c("Categorical", "Continuous")

  # Iterate over each model and validate the reference_group
  for (model_id in names(info$models)) {
    model_data <- info$models[[model_id]]
    reference_group <- model_data$reference_group
    if (is.null(reference_group) || is.null(model_data$variables)) next
    
    # Iterate over each variable in reference_group, make sure it exists
    # in the $variables table
    for (variable in names(reference_group)) {
      rows <- model_data$variables[model_data$variables$variable == variable, ]
      if (nrow(rows) == 0) {
        stop(htmltools::htmlEscape(paste0(
          "Variable '",
          variable,
          "' in reference_group for model '",
          model_id,
          "' was not found in the variables file."
        )))
      }

      # Make sure the variableType is recognized
      vtype <- rows$variableType[[1]]
      if (!vtype %in% recognised_types) {
        stop(htmltools::htmlEscape(paste0(
          "Variable '",
          variable,
          "' in reference_group for model '",
          model_id,
          "' has unrecognised variableType '",
          vtype,
          "' ",
          "(expected one of: ",
          paste(recognised_types, collapse = ", "),
          ")."
        )))
      }
    }
  }
  invisible(NULL)
}

#' Validate Predictor Consistency Across Models
#'
#' Checks that every predictor variable shared by two or more models has the
#' same \code{variableType} and \code{label} in all of those models.
#'
#' A \code{variableType} conflict (e.g. "Categorical" in one model but
#' "Continuous" in another) is a hard error because it would cause the wrong
#' UI widget to be rendered for some models. A label conflict is a warning
#' because it is a display inconsistency only; \code{gather_predictor_choices}
#' will use the label from whichever model appears first.
#'
#' @param info Model definitions list (must have \code{$variables} loaded).
#'
#' @return Called for side-effects only; returns NULL invisibly.
#'
#' @noRd
#' @keywords internal
.validate_predictor_consistency <- function(info) {
  seen <- list()

  for (model_id in names(info$models)) {
    model_data <- info$models[[model_id]]
    if (is.null(model_data$variables)) {
      stop(htmltools::htmlEscape(paste0(
        "Model '",
        model_data$model_id,
        "' has no variables loaded. Please make sure that the model export ",
        "file has a 'variable' fileType and that the file exists."
      )))
    }

    # Gather Predictor rows from the variables
    predictors <- model_data$variables[
      model_data$variables$role == "Predictor",
    ]

    for (i in seq_len(nrow(predictors))) {
      variable <- predictors$variable[[i]]
      vtype    <- predictors$variableType[[i]]
      label    <- predictors$label[[i]]

      if (!is.null(seen[[variable]])) {
        prev <- seen[[variable]]

        # Make sure the type is the same, stop if it isn't
        if (!identical(prev$type, vtype)) {
          stop(htmltools::htmlEscape(paste0(
            "Variable '", variable, "' has a conflicting variableType across ",
            "models: '", prev$type, "' (model '", prev$model_id, "') vs '",
            vtype, "' (model '", model_id, "'). ",
            "All models must agree on variableType for shared predictor ",
            "variables."
          )))
        }

        # Make sure the label is the same, emit a warning if it isn't.
        if (!identical(prev$label, label)) {
          warning(paste0(
            "Variable '", variable, "' has different labels across models: '",
            prev$label, "' (model '", prev$model_id, "') vs '",
            label, "' (model '", model_id, "'). ",
            "The label from model '", prev$model_id, "' will be used."
          ))
        }
      } else {
        # This is the first time we've seen this variable, so store its
        # information in the seen list.
        seen[[variable]] <- list(
          type = vtype, label = label, model_id = model_id
        )
      }
    }
  }

  invisible(NULL)
}

#' Convert Categorical Reference Group Values to Strings
#'
#' Ensures all categorical predictor values in each model's reference group
#' are stored as character strings. This prevents type mismatches when
#' comparing reference group values against the character values used by
#' the UI controls (radio buttons). Also coerces continuous values to numeric.
#'
#' @param info Model definitions list containing models with reference_group
#'   and variable metadata.
#'
#' @return Updated model definitions with categorical reference group values
#'   converted to character strings.
#'
#' @noRd
#' @keywords internal
.cleanup_reference_groups <- function(info) {
  for (model_id in names(info$models)) {
    reference_group <- info$models[[model_id]]$reference_group
    for (variable in names(reference_group)) {
      if (is_variable_categorical(info$models[[model_id]], variable)) {
        reference_group[[variable]] <- as.character(reference_group[[variable]])
      } else if (is_variable_continuous(info$models[[model_id]], variable)) {
        reference_group[[variable]] <- as.numeric(reference_group[[variable]])
      }
    }
    info$models[[model_id]]$reference_group <- reference_group
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
#' @noRd
#' @keywords internal
.add_model_colors <- function(info) {
  # First pass: validate all user-specified colors up front.
  for (model_id in names(info$models)) {
    color <- info$models[[model_id]]$model_color
    if (
      !is.null(color) &&
      !grepl(.valid_model_color_pattern, color, perl = TRUE)
    ) {
      stop(htmltools::htmlEscape(paste0(
        "Invalid model_color '", color, "' for model '", model_id, "'. ",
        "Use a CSS hex color (e.g. '#FF0000', '#440154FF') ",
        "or a CSS named color (e.g. 'red')."
      )))
    }
  }

  # Second pass: assign auto-colors only to models that need one. Size the
  # viridis palette to the number of unspecified models so the auto-colors
  # always span the full palette range, regardless of how many models have
  # user-specified colors.
  unspecified <- Filter(
    function(id) is.null(info$models[[id]]$model_color),
    names(info$models)
  )

  if (length(unspecified) > 0) {
    auto_colors <- viridis::viridis(length(unspecified))
    for (idx in seq_along(unspecified)) {
      info$models[[unspecified[[idx]]]]$model_color <- auto_colors[[idx]]
    }
  }

  info
}
