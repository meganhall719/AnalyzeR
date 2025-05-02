#' @title Analyze Image Based on Threshold
#' @description Processes an image using various thresholding techniques and compares it to blank references
#' @param image A magick image object to analyze
#' @param threshold Threshold value for image analysis (default: "60%")
#' @return A list containing percentage difference, processed images, and dimensional information
#' @keywords image analysis threshold
#' @export
#' @examples
#' \dontrun{
#' img <- magick::image_read("path/to/image.jpg")
#' analysis <- analyze_image(img, threshold = "65%")
#' print(analysis$formatted)
#' }
analyze_image <- function(image, threshold = "60%") {
  split_image <- magick::image_separate(image)
  split_image <- magick::image_threshold(split_image, type = "white", threshold = threshold)
  split_image <- magick::image_threshold(split_image, type = "black", threshold = threshold)
  final_image <- magick::image_combine(split_image)

  blank <- magick::image_threshold(image, type = "white", threshold = "0%")
  fill <- magick::image_threshold(image, type = "black", threshold = "1000%")

  comparison <- magick::image_compare_dist(final_image, blank, metric = "AE")
  info <- magick::image_info(image)

  width <- info$width
  height <- info$height
  percent <- (comparison$distortion / (width * height)) * 100

  return(list(
    percentage = percent,
    formatted = paste0(base::round(percent, 2), "%"),
    processed_image = final_image,
    blank_image = blank,
    filled_image = fill,
    comparison_result = comparison,
    width = width,
    height = height
  ))
}
