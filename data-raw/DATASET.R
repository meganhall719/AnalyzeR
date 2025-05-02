## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)
# Script to prepare cerebellum images for inclusion in the package
library(magick)

# Use raw GitHub URLs instead of web interface URLs
img1_url <- "https://raw.githubusercontent.com/kathrynnrhodes/CollaborativeProject/main/F_FXR_3_cerebellum_40x_1.tif"
img2_url <- "https://raw.githubusercontent.com/kathrynnrhodes/CollaborativeProject/main/F_FXR_3_cerebellum_40x_2.tif"
img3_url <- "https://raw.githubusercontent.com/kathrynnrhodes/CollaborativeProject/main/F_FXR_3_cerebellum_40x_3.tif"

temp_dir <- tempdir()
if (!all(file.exists(img1_path, img2_path, img3_path))) {
  stop("One or more image files not found. Check file paths.")
}
# Download the files
img1_path <- file.path(temp_dir, "img1.tif")
img2_path <- file.path(temp_dir, "img2.tif")
img3_path <- file.path(temp_dir, "img3.tif")

# Function to safely download
download_image <- function(url, dest_path) {
  tryCatch({
    download.file(url, dest_path, mode = "wb")
    return(TRUE)
  }, error = function(e) {
    message("Failed to download from ", url, ": ", e$message)
    return(FALSE)
  })
}

# Download images
download_status <- c(
  download_image(img1_url, img1_path),
  download_image(img2_url, img2_path),
  download_image(img3_url, img3_path)
)
img1 <- image_read(img1_path)
img2 <- image_read(img2_path)
img3 <- image_read(img3_path)

DATASET <- list(
  img1 = img1,
  img2 = img2,
  img3 = img3
)
cerebellum_images <- DATASET
usethis::use_data(DATASET, overwrite = TRUE)
usethis::use_data(cerebellum_images, overwrite = TRUE)
if (!all(download_status)) {
  stop("Failed to download one or more images. Check console for details.")
}
usethis::use_data(DATASET, overwrite = TRUE)
