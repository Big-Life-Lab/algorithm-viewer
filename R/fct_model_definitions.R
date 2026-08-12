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

# For validating color strings with grepl (#RGB, #RGBA, #RRGGBB, #RRGGBBAA)
.valid_model_color_pattern <-
  "^(#([0-9A-Fa-f]{8}|[0-9A-Fa-f]{6}|[0-9A-Fa-f]{4}|[0-9A-Fa-f]{3})|[A-Za-z]+)$"

# The keys used in an algorithm definition file to attach free-text notes to a
# value: "_notes_" holds the notes themselves and "_value_" the value they
# describe. See .add_notes_and_values for how they are processed.
.notes_key <- "_notes_"
.value_key <- "_value_"


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
#'   ),
#'   source_file = "data/models/htnport-mpp/htnport-mpp.yaml",
#'   notes = list(...)
#'
#' Any value in the definitions file may carry free-text notes. Those notes are
#' not returned alongside the values they describe, but in a separate $notes
#' tree that mirrors the structure of the definitions (see .split_off_notes).
#'
#' @param file Character string specifying the path to the YAML definitions
#'   file.
#'
#' @return A named list containing processed model definitions with loaded data.
#'   The main top-level names are $meta and $models, plus $source_file (the
#'   normalized path this definition was read from) and $notes (the free-text
#'   notes attached to the values in the definitions file).
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

  # All files referenced by the definitions file are relative to the directory
  # containing it
  root_dir <- dirname(file)

  info$source_file <- file

  # Arrange the info list so that every node has both a _notes_ field and a
  # _value_ field. We do this so that the list structure of all the models
  # (including the _all_ model if there is one) is the same. It then allows us
  # to more easily copy values from the _all_ model to the other models, since
  # the structure of all models are now in a similar pattern.
  # An example new object returned by .add_notes_and_values(info) would be:
  #   models:
  #     _notes_: "Model note"
  #     _value_:
  #       female:
  #         _notes_: "Female note"
  #         _value_:
  #           title:
  #             _notes_: "Note about the title"
  #             _value_: Female
  info <- .add_notes_and_values(info)

  # Copy all the values and notes from the _all_ model to every other model
  # (whenever the other model does not explicitly specify the values or notes)
  info <- .copy_from_all_model_with_notes(info)

  # Split the _notes_ values to their own list at info$notes, and remove
  # all the _notes_ and _value_ keys from info. An example of the
  # output of .split_off_notes(info) is:
  #   models:
  #     female:
  #       title: Female
  #
  #   notes:
  #     models:
  #       _notes_: "Model note"
  #       _value_:
  #         female:
  #           _notes_: "Female note"
  #           _value_:
  #             title:
  #               _notes_: "Note about the title"
  info <- .split_off_notes(info)

  # Load the files each model refers to and fill in everything the definitions
  # file leaves out (allowable values taken from the variable details, colors,
  # indices and IDs). Each step takes the definitions and returns them updated.
  info <- .make_model_titles_unique(info)
  info <- .assign_root_dir(info, root_dir)
  info <- .load_model_pipelines(info, root_dir)
  info <- .load_model_export_files(info, root_dir)
  info <- .parse_predictor_allowable_values(info)
  info <- .cleanup_reference_groups(info)
  info <- .add_model_indices_and_ids(info)
  info <- .add_model_colors(info)
  info <- .sort_model_variables(info)

  # Check the fully assembled definitions. These are called for their
  # side-effects only (an error for anything that would break the UI, a warning
  # for anything merely inconsistent), so they do not return the definitions.
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
#' Converts allowable value specifications (vectors, unnamed lists, or named
#' list definitions) into a vector of the allowable values.
#'
#' @param info Allowable values specification as an unnamed vector or list, or a
#'   named list with a top-level "seq" key whose value is a named list of seq()
#'   parameters (e.g., list(seq = list(from = 1, to = 10, by = 0.1))).
#'
#' @return Vector representing the allowable values (numeric where they are all
#'   numbers, character where any of them is a string), or NULL if parsing
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
    # An explicit list of values is read as a list, rather than the vector it
    # usually is, whenever its values are not all of one type: a mix of whole
    # and decimal numbers ([18, 20.5, 23]), or of numbers and strings. The
    # values are used together from here on (as one column of the pipeline's
    # input, and as one axis of a plot), so collapse them to the vector of
    # whichever single type holds them all.
    return(unlist(info))
  }

  NULL
}

#' Copy Shared Configuration to All Models
#'
#' Copies configuration from the "_all_" template model to all other models,
#' without overwriting existing values, then removes the template. Runs on the
#' tree of \code{_notes_}/\code{_value_} nodes produced by
#' \code{.add_notes_and_values}, so a shared value copied into a model brings
#' the notes attached to it along with it.
#'
#' @param info Model definitions list, as a tree of
#'   \code{_notes_}/\code{_value_} nodes, potentially containing an "_all_"
#'   model.
#'
#' @return Updated model definitions with shared configuration applied and the
#'   "_all_" model removed.
#'
#' @noRd
#' @keywords internal
.copy_from_all_model_with_notes <- function(info) {
  if (.all_tag %in% names(info$models[[.value_key]])) {
    for (model_id in names(info$models[[.value_key]])) {
      if (model_id == .all_tag) {
        next
      }
      info$models[[.value_key]][[model_id]] <-
        .recurse_copy_from_all_model_with_notes(
          info$models[[.value_key]][[model_id]],
          info$models[[.value_key]][[.all_tag]]
        )
    }

    info$models[[.value_key]][[.all_tag]] <- NULL
  }

  info
}

#' Copy Missing Values from Template
#'
#' Recursive worker for \code{.copy_from_all_model_with_notes}, which should
#' usually be called instead. Copies values from a template object to the target
#' object for any keys not already present in the target, recursing into keys
#' the two have in common so that values are merged at every level rather than
#' the target's whole subtree being kept or replaced.
#'
#' @param info List to receive copied values, as a tree of
#'   \code{_notes_}/\code{_value_} nodes.
#' @param all_info Template list to copy values from, in the same form.
#'
#' @return The info list with missing values filled from all_info.
#'
#' @noRd
#' @keywords internal
.recurse_copy_from_all_model_with_notes <- function(info, all_info) {
  for (key in names(all_info)) {
    if (is.list(info[[key]])) {
      # Present in both, so merge what is below it rather than keeping the
      # target's subtree as-is
      info[[key]] <- .recurse_copy_from_all_model_with_notes(
        info[[key]],
        all_info[[key]]
      )
    } else if (!(key %in% names(info)) || is.na(info[[key]])) {
      # Missing from the target, or an empty value such as the NA notes of an
      # unannotated value, so take the template's
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

#' Rewrite an Algorithm Definition as a Tree of Notes and Values
#'
#' Rewrites a freshly loaded algorithm definition so that every value in it
#' becomes a node of the form \code{list(`_notes_` = ..., `_value_` = ...)},
#' holding the free-text notes for that value (\code{NA} when it has none) and
#' the value itself. Values that are lists become nodes whose \code{_value_} is
#' a list of further nodes, so the whole definition becomes a uniform tree.
#'
#' Notes may be attached to any value in a definitions file, in either of the
#' two forms described in \code{.extract_notes_and_value}. Normalizing them into
#' one shape here means the rest of the loading process does not have to know
#' where notes may appear: they stay attached to their values while the
#' \code{_all_} block is merged in (\code{.copy_from_all_model_with_notes}) and
#' are separated out again at the end (\code{.split_off_notes}).
#'
#' For example, these definitions
#'
#'   models:
#'     female:
#'       title: Female
#'
#' are rewritten as
#'
#'   models:
#'     _notes_: NA
#'     _value_:
#'       female:
#'         _notes_: NA
#'         _value_:
#'           title:
#'             _notes_: NA
#'             _value_: Female
#'
#' @param info Algorithm definition list, as returned by
#'   \code{read_and_validate_yaml}.
#'
#' @return The definition rewritten as a tree of \code{_notes_}/\code{_value_}
#'   nodes. The root node is unwrapped, so the return value is keyed the same
#'   way as \code{info} (\code{$meta}, \code{$models}, ...) and any notes
#'   attached to the definitions file as a whole are dropped.
#'
#' @noRd
#' @keywords internal
.add_notes_and_values <- function(info) {
  info <- .recurse_add_notes_and_values(info)

  # Return the root node's value rather than the node itself, so that callers
  # can keep addressing the definitions as info$meta, info$models, ...
  info[[.value_key]]
}

#' Separate a Node's Own Notes from the Value it Holds
#'
#' Splits one value of an algorithm definition into the free-text notes
#' attached to it and the value itself. Notes may be written either beside the
#' value (a \code{_notes_} key next to a \code{_value_} key) or, where the value
#' is a list, as a \code{_notes_} key within the value itself. Both forms are
#' equivalent:
#'
#'   male:                            male:
#'     _notes_: Male notes              _notes_: Male notes
#'     _value_:               ==        title: Male
#'       title: Male                    model_export: ./male.csv
#'       model_export: ./male.csv
#'
#' Either way the notes annotate the value they appear in, and are never one of
#' the entries that value holds.
#'
#' @param info One value of an algorithm definition, with or without notes.
#'
#' @return A list with two elements: \code{notes}, the notes attached to
#'   \code{info} (\code{NA} when it has none), and \code{value}, \code{info}
#'   with those notes removed. If notes are given in both forms at once, the
#'   ones written beside the value win.
#'
#' @noRd
#' @keywords internal
.extract_notes_and_value <- function(info) {
  notes <- if (.notes_key %in% names(info)) info[[.notes_key]] else NA
  value <- if (.value_key %in% names(info)) info[[.value_key]] else info

  # Notes written within the value belong to this node too, so lift them out of
  # the value. Notes written beside the value take precedence over them.
  if (is.list(value) && .notes_key %in% names(value)) {
    if (isTRUE(is.na(notes))) {
      notes <- value[[.notes_key]]
    }
    value[[.notes_key]] <- NULL
  }

  list(notes = notes, value = value)
}

#' Rewrite One Value as a Notes and Value Node
#'
#' Recursive worker for \code{.add_notes_and_values}, which should usually be
#' called instead. Rewrites a single value of an algorithm definition as a
#' \code{_notes_}/\code{_value_} node, recursing into the value so that
#' everything it contains is rewritten as well.
#'
#' @param info One value of an algorithm definition.
#'
#' @return A list with a \code{_notes_} element holding the notes attached to
#'   \code{info} (\code{NA} when it has none) and a \code{_value_} element
#'   holding either the value itself or, when the value is a list, a list of the
#'   nodes its entries were rewritten as.
#'
#' @noRd
#' @keywords internal
.recurse_add_notes_and_values <- function(info) {
  new_info <- list()

  split <- .extract_notes_and_value(info)
  new_info[[.notes_key]] <- split$notes

  info <- split$value
  if (is.list(info)) {
    # Rewrite every entry the value holds. Entries of an unnamed list (such as
    # an explicit list of allowable values) are addressed by position instead.
    for (i in seq_along(info)) {
      key = names(info)[[i]]
      if (is.null(key))
        key <- i
      sub <- .recurse_add_notes_and_values(info[[key]])
      if (!.value_key %in% names(new_info))
        new_info[[.value_key]] <- list()
      new_info[[.value_key]][[key]] <- sub
    }
  } else {
    # A value that is not a list has nothing below it to rewrite
    new_info[[.value_key]] <- info
  }

  new_info
}

#' Sort Model Variables into Variables File Order
#'
#' Reorders each model's \code{reference_group} and
#' \code{predictor_allowable_values} to follow the order the variables appear in
#' that model's variables file. The definitions file may list predictors in any
#' order, so this keeps the order the UI displays its controls in tied to the
#' variables file rather than to the definitions file.
#'
#' @param info Model definitions list (must have \code{$variables} loaded).
#'
#' @return Updated model definitions with each model's \code{reference_group}
#'   and \code{predictor_allowable_values} sorted.
#'
#' @noRd
#' @keywords internal
.sort_model_variables <- function(info) {
  for (model_id in names(info$models)) {
    variable_order <- info$models[[model_id]]$variables$variable

    info$models[[model_id]]$reference_group <- .sort_keys(
      info$models[[model_id]]$reference_group,
      order = variable_order
    )
    info$models[[model_id]]$predictor_allowable_values <- .sort_keys(
      info$models[[model_id]]$predictor_allowable_values,
      order = variable_order
    )
  }

  info
}

#' Sort the Entries of a Named List
#'
#' Reorders a named list so its entries follow the given order of names. Names
#' in \code{order} that the list does not have are ignored, and entries whose
#' names are not in \code{order} are kept, in their existing order, at the end.
#'
#' @param obj Named list to sort.
#' @param order Character vector of names, in the order they should appear.
#'
#' @return \code{obj} with its entries reordered.
#'
#' @noRd
#' @keywords internal
.sort_keys <- function(obj, order) {
  # Drop names obj does not have, then append the names order does not cover
  order <- intersect(order, names(obj))
  order <- union(order, names(obj))

  obj[order]
}

#' Split Notes Off an Algorithm Definition
#'
#' From \code{info}, remove all the \code{_notes_} fields and move them
#' to the root at \code{info$notes}. Also collapse all the \code{_value_} keys.
#'
#' For example, if \code{info} is equal to:
#'
#'   models:
#'     _notes_: "Model note"
#'     _value_:
#'       female:
#'         _notes_: "Female note"
#'         _value_:
#'           title:
#'             _notes_: "Note about the title"
#'             _value_: Female
#'
#' Then this function will return:
#'
#'   models:
#'     female:
#'       title: Female
#'
#'   notes:
#'     models:
#'       _notes_: "Model note"
#'       _value_:
#'         female:
#'           _notes_: "Female note"
#'           _value_:
#'             title:
#'               _notes_: "Note about the title"
#'
#' @param info Algorithm definition as a tree of \code{_notes_}/\code{_value_}
#'   nodes, as returned by \code{.add_notes_and_values}.
#'
#' @return The plain values of the definition, with the notes for those values
#'   added as a \code{$notes} element. The notes tree is keyed the same way as
#'   the definitions, so the notes for any value can be looked up with the keys
#'   that lead to it (see \code{get_notes}); notes attached to the definitions
#'   file as a whole are dropped.
#'
#' @noRd
#' @keywords internal
.split_off_notes <- function(info) {
  res <- .recurse_split_values_and_notes(info)
  res$values$notes <- res$notes[[.value_key]]
  res$values
}

#' Split One Node into its Value and its Notes
#'
#' Recursive worker for \code{.split_off_notes}, which should usually be called
#' instead. Splits a single \code{_notes_}/\code{_value_} node into the plain
#' value it holds and the notes attached to it, recursing into the value so that
#' everything it contains is split as well.
#'
#' @param info One node of an algorithm definition tree.
#'
#' @return A list with two elements: \code{values}, the plain value the node
#'   holds, and \code{notes}, a node of the same shape as \code{info} holding
#'   only notes (a \code{_notes_} element for this node, and, where the value is
#'   a list, a \code{_value_} element holding the notes for its entries).
#'
#' @noRd
#' @keywords internal
.recurse_split_values_and_notes <- function(info) {
  notes <- list()
  values <- list()

  split <- .extract_notes_and_value(info)
  notes[[.notes_key]] <- split$notes

  info <- split$value
  if (is.list(info)) {
    # Split every entry the value holds, keeping the values and the notes in
    # two trees of the same shape
    for (i in seq_along(info)) {
      key = names(info)[[i]]
      if (is.null(key))
        key <- i

      sub <- .recurse_split_values_and_notes(info[[key]])
      if (!.value_key %in% names(notes))
        notes[[.value_key]] <- list()
      notes[[.value_key]][[key]] <- sub$notes
      values[[key]] <- sub$values
    }

    # Annotating the entries of a YAML sequence forces it to be read as a list
    # of nodes rather than the vector it would otherwise be, so collapse it back
    # to a vector. The notes are unaffected: they keep their own entry per item.
    values <- .simplify_sequence(values)
  } else {
    # A value that is not a list has nothing below it to split
    values <- info
  }

  list(
    notes = notes,
    values = values
  )
}

#' Collapse a List of Scalars to a Vector
#'
#' Collapses an unnamed list of scalars, as a YAML sequence is read as, to the
#' vector holding the same values. This matches how \code{yaml::read_yaml} reads
#' a sequence whose entries are all scalars of one type, so that a sequence
#' whose entries carry notes (which is read as a list, since its entries are the
#' \code{_notes_}/\code{_value_} mappings the notes are written as) ends up in
#' the same form as the same sequence written without notes.
#'
#' Anything else is returned unchanged: named lists (which a YAML mapping, such
#' as a model or a reference group, is read as), and sequences whose entries are
#' not all scalars of one type, which \code{yaml::read_yaml} also leaves as a
#' list.
#'
#' @param values A value of an algorithm definition, with its notes already
#'   split off.
#'
#' @return The vector holding the entries of \code{values} if it is an unnamed
#'   list of scalars of one type, and \code{values} itself otherwise.
#'
#' @noRd
#' @keywords internal
.simplify_sequence <- function(values) {
  if (
    !is.list(values) ||
      length(values) == 0 ||
      !is.null(names(values))
  ) {
    return(values)
  }

  is_scalar <- vapply(
    values,
    function(value) is.atomic(value) && length(value) == 1,
    logical(1)
  )
  if (!all(is_scalar)) {
    return(values)
  }

  # Entries of more than one type are left alone, as a vector could only hold
  # them by coercing them all to the widest of those types
  types <- vapply(values, function(value) class(value)[[1]], character(1))
  if (length(unique(types)) > 1) {
    return(values)
  }

  unlist(values)
}

#' Get the Notes Attached to a Value
#'
#' Looks up the free-text notes attached to one value of an algorithm
#' definition, addressing that value by the sequence of keys that leads to it.
#' The notes are held in a tree of their own that mirrors the structure of the
#' definitions (see \code{.split_off_notes}), so the keys are the same ones used
#' to reach the value itself. For example, the notes on the age of the male
#' model's reference group are looked up with:
#'
#'   get_notes(info, list("models", "male", "reference_group", "clc_age"))
#'
#' A key may also be a number, which addresses the value at that position rather
#' than by name. This is how the entries of an unnamed list are reached, since
#' they have no names to address them by. For example, the notes on the second
#' allowable value of the male model's diabx predictor are looked up with:
#'
#'   get_notes(
#'     info,
#'     list("models", "male", "predictor_allowable_values", "diabx", 2)
#'   )
#'
#' Numbers address named values by position too, so
#' \code{list("models", 1)} reaches the notes on the first model. Positions are
#' 1-based, as elsewhere in R.
#'
#' @param info Model definitions list, as returned by
#'   \code{read_model_definitions} (must have a \code{$notes} element).
#' @param keys List of keys naming the path to the value, from the top of the
#'   model definitions down to the value itself. Each key is either a character
#'   string, naming the value at the current node, or a number, giving the
#'   position of the value within the current node. A character vector may be
#'   used instead when every key is a name, but a path that mixes names and
#'   positions must be a \code{list}, since \code{c()} would coerce the numbers
#'   in it to strings and they would then be looked up as names.
#'
#' @return The notes attached to the value at \code{keys} as a character string,
#'   or \code{NULL} if either the key path doesn't exist (including a position
#'   outside the current node) or there is no note attached to the key path.
#'
#' @noRd
#' @keywords internal
get_notes <- function(info, keys) {
  info <- info$notes
  last_notes <- NULL
  for (key in keys) {
    if (is.character(key)) {
      # Address the value by name
      if (!key %in% names(info)) {
        return(NULL)
      }
    } else if (is.numeric(key)) {
      # Address the value by position, which is the only way to reach the
      # entries of an unnamed list (such as an explicit list of allowable
      # values)
      if (key < 1 || key > length(info)) {
        return(NULL)
      }
    } else {
      # Anything else cannot name a value, so there are no notes to return
      return(NULL)
    }
    info <- info[[key]]
    last_notes <- info[[.notes_key]]
    info <- info[[.value_key]]
  }
  if (isTRUE(is.na(last_notes))) NULL else last_notes
}
