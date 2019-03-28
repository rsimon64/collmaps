cm_resize_image <- function(img_path, img_out, x_width = 200) {
  im <- imager::load.image(img_path)
  w <- dim(im)[1]
  h <- dim(im)[2]
  f <- x_width / w
  imr <- imager::resize(im, size_x = x_width, size_y = h * f)
  img_out <- file.path(img_out, basename(img_path))
  imager::save.image(imr, img_out)
}

cm_import_images <- function(from = NULL, to = getwd(), scale_to_width = 200,
                             include_original = TRUE) {
  init_dirs <- function(to) {
    target_dir <- file.path(to, "images", "original")
    if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)
    target_dir_popup <- file.path(to, "images", "popup")
    if (!dir.exists(target_dir_popup)) dir.create(target_dir_popup,
                                                  recursive = TRUE)
    list(target_dir = target_dir,
         target_dir_popup = target_dir_popup)
  }

  ok <- init_dirs(to)
  # message(ok$target_dir)
  # message(ok$target_dir_popup)

  if ((!is.null(from) & !is.null(to))) {
    images <- list.files(from, full.names = TRUE, pattern = ".png|*.jpg")
    if (!is.null(images)) {
      if(include_original) {
        out <- sapply(images, file.copy,  to = ok$target_dir, recursive = TRUE)
      }

      out <- sapply(images, cm_resize_image, img_out = ok$target_dir_popup,
                    x_width = scale_to_width)
    }
  }
}
