source("R/modules/reference_group.R")

initialize_reference_groups_env <- function() {
  rlang::env(
    # All reference group server modules. These allow us to retrieve the reference
    # group values
    reference_groups = list(),
    # Every time we call destroy_all_reference_groups, we increment this number.
    # The number gets appended to the reference group IDs, and ensures that
    # we never use the same ID twice.
    reference_groups_index = 0
  )
}

create_reference_group <- function(.env, model_data, extra_tag = NULL, change_trigger = NULL, model_name = NULL, show_model_color = TRUE) {
  id <- get_model_reference_group_id(.env, model_data, extra_tag = extra_tag)
  ui <- referenceGroupUI(id, model_data, model_name = model_name, show_model_color = show_model_color)
  server <- referenceGroupServer(id, model_data, change_trigger)

  .env$reference_groups[[id]] <- server

  list(
    ui = ui,
    server = server
  )
}

get_model_reference_group_id <- function(.env, model_data, extra_tag = NULL) {
  id <- paste0(model_data$model_id, "___", .env$reference_groups_index)
  if (!is.null(extra_tag)) {
    id <- paste0(id, "___", extra_tag)
  }
  id
}

destroy_all_reference_groups <- function(.env) {
  for (refgroup in .env$reference_groups) {
    if (!is.null(refgroup$destroy_module)) {
      refgroup$destroy_module()
    }
  }
  .env$reference_groups <- list()
  .env$reference_groups_index <- .env$reference_groups_index + 1
}

get_reference_group_values <- function(.env, model_data, extra_tag = NULL, rv_isolate = TRUE) {
  id <- get_model_reference_group_id(.env, model_data, extra_tag = extra_tag)
  refgroup <- .env$reference_groups[[id]]
  if (rv_isolate) {
    isolate(refgroup$rv_values())
  } else {
    refgroup$rv_values()
  }
}
