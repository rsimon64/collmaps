cm_import_data <- function(file_name, to = getwd()) {
  if ((!is.null(file_name) & !is.null(to))) {

    target_dir <- file.path(to, "data", "original")
    if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)
    out <- file.copy(file_name,
                     to = file.path(target_dir, basename(file_name)),
                     overwrite = TRUE)
  }
}
