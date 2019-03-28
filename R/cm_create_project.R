cm_create_project <- function(path, ...) {

  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  params <- list(...)

  target_dir <- file.path(getwd(), path)
  cm_import_data(params$file_data, target_dir)

  cm_import_images(to = target_dir)

}
