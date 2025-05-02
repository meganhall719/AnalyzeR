#' @title Batch Analysis of Multiple Images
#' @description Analyzes multiple images at once with a specified threshold after they have been grouped using group_loaded_images function.
#' @param image_result A list containing image data, output from the group_loaded_images function
#' @param threshold Threshold value for image analysis (default: "60%")
#' @return A list containing individual analysis results, average percentages, and a summary
#' @keywords images analysis batch
#' @export
#' @examples
#' \dontrun{
#' # First create grouped images
#' img1 <- magick::image_read("path/to/image1.jpg")
#' img2 <- magick::image_read("path/to/image2.jpg")
#' image_list <- list(img1, img2)
#' grouped_result <- group_loaded_images(image_list, process_loaded_images)
#' # Then analyze all images with the same threshold
#' analysis_results <- batch_analyze_images(grouped_result, threshold = "65%")
#' }
batch_analyze_images <- function(image_result, threshold = "60%") {
  if (!is.list(image_result) || is.null(image_result$images)) {
    stop("Expected input from group_loaded_images function with 'images' field")
  }

  images <- image_result$images
  results <- lapply(1:length(images), function(i) {
    if (!inherits(images[[i]], "magick-image")) {
      temp_file <- tempfile(fileext = ".tif")
      tiff::writeTIFF(images[[i]], temp_file)
      img <- magick::image_read(temp_file)
      file.remove(temp_file)
    } else {
      img <- images[[i]]
    }

    # Call the analyze_image function with proper namespace
    analysis <- analyze_image(img, threshold)
    analysis$image_index <- i
    analysis$image_name <- ifelse(exists(paste0("image_", i)), paste0("image_", i), paste0("Image ", i))

    return(analysis)
  })

  avg_percent <- base::mean(sapply(results, function(r) r$percentage))

  # Using message instead of cat for better package behavior
  base::message("Individual Image Analysis Results:")
  base::message("--------------------------------")

  for (r in results) {
    base::message(sprintf("Image %d (%s):", r$image_index, r$image_name))
    base::message(sprintf("  Dimensions: %d x %d pixels", r$width, r$height))
    base::message(sprintf("  Distortion value: %.2f", r$comparison_result$distortion))
    base::message(sprintf("  Percent difference from blank: %s\n", r$formatted))
  }

  return(list(
    individual_results = results,
    average_percentage = avg_percent,
    formatted_average = paste0(base::round(avg_percent, 2), "%"),
    count = base::length(images),
    summary = sprintf("Average percent difference across %d images: %s",
                      base::length(images), paste0(base::round(avg_percent, 2), "%"))
  ))
}

