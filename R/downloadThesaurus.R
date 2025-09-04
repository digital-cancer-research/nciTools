#' Download and Extract the Latest NCI Thesaurus
#'
#' @description
#' The latest version of the NCI thesaurus is downloaded and unzipped to the data/raw subfolder.\cr
#' The NCI Thesaurus is a reference terminology covering a broad range of topics relevant to cancer and biomedical research.
#' @returns Path to the extracted NCI Thesaurus flat file.
#' @details
#' Thesaurus downloaded from `https://evs.nci.nih.gov/ftp1/NCI_Thesaurus/Thesaurus.FLAT.zip` - this should always be the latest version.\cr
#' @export
downloadThesaurus = function() {
  ## latest version should always be at this url...
  NCItURL <- "https://evs.nci.nih.gov/ftp1/NCI_Thesaurus/Thesaurus.FLAT.zip"

  # 1. Create a temporary file for the zip
  zip_file <- tempfile(fileext = ".zip")
  # 2. Download the file
  tryCatch(
    expr = {
      utils::download.file(NCItURL, destfile = zip_file, mode = "wb", quiet = TRUE)
    }, error = function(e) {
      print("NCIt download failed")
    })
  # 3. Create a temporary directory for extraction
  unzip_dir <- tempfile()
  dir.create(unzip_dir)
  # 4. Unzip into the temp directory
  utils::unzip(zip_file, exdir = unzip_dir)


  unzipped_filepath = paste(unzip_dir, "Thesaurus.txt", sep = "/")

  # 5. Load data into memory
  NCIt_raw <- utils::read.table(unzipped_filepath, header = FALSE, sep = "\t", comment.char = "", fill = TRUE, stringsAsFactors = FALSE, quote = "")
  # 6. Clean up immediately (zip + extracted files)
  unlink(c(zip_file, unzip_dir), recursive = TRUE, force = TRUE)

  ## download to data/raw subfolder
  # destFlatFilename <- paste(exdir, "NCIt_FLAT.zip", sep = "/")

  ## try to download
  # tryCatch(
  #   expr = {
  #     utils::download.file(url=NCItURL,destfile = destFlatFilename)
  #     utils::unzip(zipfile = destFlatFilename, exdir = exdir,)
  #     ## clean up, remove files
  #     file.remove(destFlatFilename)
  #   }, error = function(e) {
  #     print("NCIt download failed")
  #   }
  # )

  ## output file targets create files and return their paths
  # ex_filepath = paste(exdir, "Thesaurus.txt", sep = "/")
  # return(ex_filepath)

  return(NCIt_raw)

}
