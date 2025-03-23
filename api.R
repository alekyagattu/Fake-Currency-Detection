# Load required libraries
library(plumber)
library(imager)
library(EBImage)

# Preprocess image: Convert to grayscale, resize, and normalize
preprocess_image <- function(filepath) {
  img <- load.image(filepath)          # Load image
  img <- grayscale(img)                # Convert to grayscale
  img <- resize(img, 64, 64)           # Resize to 64x64 pixels
  return(as.matrix(img))               # Return as matrix
}

# Function to calculate correlation between two images
calculate_correlation <- function(A, B) {
  return(cor(as.vector(A), as.vector(B)))  # Flatten the matrices into vectors
}

# Function to find the maximum correlation within a training set
max_correlation <- function(train_set, test_note) {
  corrs <- sapply(train_set, function(img) calculate_correlation(img, test_note))
  return(max(corrs))  # Take the highest correlation
}

# Function to predict if a test note is Real or Fake
predict_currency <- function(test_note, train_real, train_fake) {
  cor_real <- max_correlation(train_real, test_note)
  cor_fake <- max_correlation(train_fake, test_note)
  if (cor_real > cor_fake && cor_real >= 0.5) {
    return("Real")  # Prediction: Real
  } else {
    return("Fake")  # Prediction: Fake
  }
}

# Training phase: Load training data
train_real <- list(
  preprocess_image("/Users/Alekya/Desktop/Fake Currency Detection/2000_s1.jpg"),
  preprocess_image("/Users/Alekya/Desktop/Fake Currency Detection/2000_s2.jpg"),
  preprocess_image("/Users/Alekya/Desktop/Fake Currency Detection/500_s1.jpg"),
  preprocess_image("/Users/Alekya/Desktop/Fake/500_my.jpeg"),
  preprocess_image("/Users/Alekya/Desktop/Fake Currency Detection/500_s4.jpg"),
  preprocess_image("/Users/Alekya/Desktop/Fake Currency Detection/500_s7.jpg")
)

train_fake <- list(
  preprocess_image("/Users/Alekya/Desktop/Fake Currency Detection/2000_f1.jpg"),
  preprocess_image("/Users/Alekya/Desktop/Fake Currency Detection/2000_f2.jpg"),
  preprocess_image("/Users/Alekya/Desktop/Fake Currency Detection/500_f1.jpg"),
  preprocess_image("/Users/Alekya/Desktop/Fake Currency Detection/500_f2.jpg")
)

# Create a Plumber API
api <- plumber$new()

# API Metadata
#' @apiTitle Fake Currency Detection API
#' @apiDescription Upload an image and get prediction (Real or Fake)

# Define predict endpoint
#' @post /predict
#' @param file:file Upload an image file
#' @serializer json
api$handle("POST", "/predict", function(req) {
  if (length(req$postBody) == 0) {
    return(list(status = "error", message = "No file uploaded"))
  }
  
  # Save uploaded file temporarily
  temp_file <- tempfile(fileext = ".jpg")
  writeBin(req$postBody, temp_file)
  
  # Preprocess the uploaded image
  test_note <- preprocess_image(temp_file)
  if (is.null(test_note)) {
    return(list(status = "error", message = "Image preprocessing failed"))
  }
  
  # Predict currency authenticity
  prediction <- predict_currency(test_note, train_real, train_fake)
  return(list(status = "success", prediction = prediction))
})

# Run API
api$run(host = "0.0.0.0", port = 8000)
