#' @title Group and Process Loaded Images
#' @description Takes a list of pre-loaded image objects and applies a specified processor function to them.
#' @param image_objects A list of pre-loaded image objects to process
#' @param processor_function A function that will be applied to the image objects
#' @param ... Additional arguments to pass to the processor function
#' @return The result from the processor function
#' @keywords images processing
#' @export
#' @examples
#' \dontrun{
#' # Assuming img1, img2, and img3 are already loaded image objects
#' image_list <- list(img1, img2, img3)
#' result <- group_loaded_images(image_list, process_loaded_images, output_format = "png")
#' }
group_loaded_images <- function(image_objects, processor_function, ...) {
  # Validate inputs
  if (length(image_objects) == 0) {
    stop("No image objects provided")
  }

  if (!is.function(processor_function)) {
    stop("processor_function must be a function")
  }

  # Call the processor function with the grouped images and any additional arguments
  result <- processor_function(image_objects, ...)

  return(result)
}

#' @title Process a List of Pre-loaded Images
#' @description Processes a list of pre-loaded image objects with specified parameters.
#' @param image_list A list of pre-loaded image objects to process
#' @param output_format The desired output format for the processed images (default: "tif")
#' @param resize Optional parameter to resize the images (vector of width and height)
#' @return A list containing status, count, and the processed images
#' @keywords images processing
#' @export
#' @examples
#' \dontrun{
#' # Assuming img1, img2, img3 are already loaded magick image objects
#' img1 <- magick::image_read("path/to/image1.jpg")
#' img2 <- magick::image_read("path/to/image2.jpg")
#' img3 <- magick::image_read("path/to/image3.jpg")
#' image_list <- list(img1, img2, img3)
#' result <- process_loaded_images(image_list, output_format = "png", resize = c(800, 600))
#' }
process_loaded_images <- function(image_list, output_format = "tif", resize = NULL) {
  # Using base R function with no namespace needed (though could use base:: for consistency)
  message(sprintf("Processing %d pre-loaded images with output format: %s",
                  length(image_list), output_format))

  # Process images with proper namespace calls to magick package
  processed_images <- lapply(image_list, function(img) {
    # Apply resizing if specified
    if (!is.null(resize)) {
      img <- magick::image_resize(img, paste0(resize[1], "x", resize[2]))
    }

    # Convert to the specified format
    img <- magick::image_convert(img, format = output_format)

    return(img)
  })

  # Example of other magick functions with proper namespace prefixing
  # Create a montage of all images if more than one
  if (length(processed_images) > 1) {
    montage <- magick::image_append(magick::image_join(processed_images))
    return(list(
      status = "success",
      count = length(processed_images),
      images = processed_images,
      montage = montage
    ))
  }

  return(list(
    status = "success",
    count = length(processed_images),
    images = processed_images
  ))
}
