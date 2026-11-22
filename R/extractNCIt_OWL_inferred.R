#' Extract the inferred NCI Thesaurus OWL file
#'
#' Safely extracts `ThesaurusInferred.owl` from an NCI Thesaurus
#' inferred OWL ZIP archive.
#'
#' Before extraction, the archive contents are inspected to confirm that
#' the expected OWL file is present and that the archive does not contain
#' absolute or parent-relative paths.
#'
#' Only `ThesaurusInferred.owl` is extracted. Other files in the archive
#' are ignored.
#'
#' @param zip_file Character. Path to the downloaded NCI Thesaurus
#'   inferred OWL ZIP archive.
#'
#' @param path Character. Directory in which to extract the OWL file.
#'   Defaults to the directory containing `zip_file`.
#'
#' @param overwrite Logical. If `TRUE`, an existing extracted OWL file
#'   is overwritten. Defaults to `FALSE`.
#'
#' @return Invisibly returns the path to the extracted
#'   `ThesaurusInferred.owl` file.
#'
#' @examples
#' \dontrun{
#' zip_file <- downloadNCIt_OWL_inferred()
#' owl_file <- extractNCIt_OWL_inferred(zip_file)
#' }
#'
#' @export
extractNCIt_OWL_inferred <- function(
    zip_file,
    path = dirname(zip_file),
    overwrite = FALSE
) {
  
  # Check arguments
  stopifnot(
    is.character(zip_file),
    length(zip_file) == 1L,
    !is.na(zip_file),
    is.character(path),
    length(path) == 1L,
    !is.na(path),
    is.logical(overwrite),
    length(overwrite) == 1L,
    !is.na(overwrite)
  )
  
  # Check ZIP file exists
  if (!file.exists(zip_file)) {
    stop("ZIP file does not exist: ", zip_file)
  }
  
  # Inspect archive without extracting it
  zip_contents <- utils::unzip(
    zip_file,
    list = TRUE
  )
  
  # Check for unsafe paths
  unsafe_paths <- grepl(
    "^[/\\\\]|(^|[/\\\\])\\.\\.([/\\\\]|$)",
    zip_contents$Name
  )
  
  if (any(unsafe_paths)) {
    stop("ZIP archive contains unsafe file paths.")
  }
  
  # Expected NCIt OWL file
  expected_file <- "ThesaurusInferred.owl"
  
  # Confirm expected file is present exactly once
  matches <- zip_contents$Name == expected_file
  
  if (sum(matches) != 1L) {
    stop(
      "Expected exactly one ",
      expected_file,
      " in the ZIP archive."
    )
  }
  
  # Create destination directory if necessary
  if (!dir.exists(path)) {
    dir.create(
      path,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }
  
  owl_file <- file.path(path, expected_file)
  
  # Reuse existing extracted file unless overwrite requested
  if (file.exists(owl_file) && !overwrite) {
    message(
      "NCI Thesaurus inferred OWL file already exists: ",
      owl_file
    )
    
  return(invisible(owl_file))
  }
  
  # Extract only the expected OWL file
  utils::unzip(
    zip_file,
    files = expected_file,
    exdir = path,
    overwrite = overwrite
  )
  
  # Confirm extraction succeeded
  if (!file.exists(owl_file) || file.size(owl_file) == 0) {
    stop("NCI Thesaurus inferred OWL extraction failed.")
  }
  
  message(
    "NCI Thesaurus inferred OWL extracted to: ",
    owl_file
  )

invisible(owl_file)
}