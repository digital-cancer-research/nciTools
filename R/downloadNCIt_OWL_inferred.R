#' Download the NCI Thesaurus in inferred OWL format
#'
#' Downloads the current inferred OWL distribution of the NCI Thesaurus
#' (NCIt) from the NCI Enterprise Vocabulary Services (EVS).
#'
#' The downloaded file is stored in the nciTools user cache by default.
#' If the file already exists, it is not downloaded again unless
#' `overwrite = TRUE`.
#'
#' @param path Character. Directory in which to save the downloaded file.
#'   Defaults to the package-specific user cache directory returned by
#'   [tools::R_user_dir()].
#'
#' @param overwrite Logical. If `TRUE`, an existing file is overwritten.
#'   Defaults to `FALSE`.
#'
#' @return Invisibly returns the path to the downloaded ZIP file.
#'
#' @examples
#' \dontrun{
#' owl_file <- downloadNCIt_OWL_inferred()
#'
#' downloadNCIt_OWL_inferred(overwrite = TRUE)
#' }
#'
#' @export
downloadNCIt_OWL_inferred <- function(
    path = getOption(
      "nciTools.cache",
      tools::R_user_dir("nciTools", which = "cache")
    ),
    overwrite = FALSE
) {

  # Official NCI EVS source for the current inferred NCIt OWL release
  url <- "https://evs.nci.nih.gov/ftp1/NCI_Thesaurus/ThesaurusInf.OWL.zip"

  # Check arguments
  stopifnot(
    is.character(path),
    length(path) == 1L,
    !is.na(path),
    is.logical(overwrite),
    length(overwrite) == 1L,
    !is.na(overwrite)
  )

  # Create cache directory if it does not already exist
  if (!dir.exists(path)) {
    dir.create(
      path,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  # Determine destination file
  destfile <- file.path(
    path,
    basename(url)
  )

  # Use existing download unless explicitly asked to replace it
  if (file.exists(destfile) && !overwrite) {
    message(
      "NCI Thesaurus OWL file already exists: ",
      destfile
    )

    return(invisible(destfile))
  }

  # Download NCIt
  message("Downloading NCI Thesaurus inferred OWL...")

  utils::download.file(
    url = url,
    destfile = destfile,
    mode = "wb",
    quiet = FALSE
  )

  # Basic validation of downloaded file
  if (!file.exists(destfile) || file.size(destfile) == 0) {
    stop("NCI Thesaurus OWL download failed.")
  }

  message(
    "NCI Thesaurus OWL downloaded to: ",
    destfile
  )

  invisible(destfile)
}
