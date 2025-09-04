#' Download and Extract the Latest NCI Thesaurus
#'
#' @description
#' The latest version of the NCI thesaurus is downloaded and unzipped to the data/raw subfolder.\cr
#' The NCI Thesaurus is a reference terminology covering a broad range of topics relevant to cancer and biomedical research.
#' @returns Path to the extracted NCI Thesaurus flat file.
#' @details
#' Thesaurus downloaded from `https://evs.nci.nih.gov/ftp1/NCI_Thesaurus/Thesaurus.FLAT.zip` - this should always be the latest version.\cr
#' @export
downloadThesaurus = function(exdir = "data/raw") {
  ## latest version should always be at this url...
  NCItURL <- "https://evs.nci.nih.gov/ftp1/NCI_Thesaurus/Thesaurus.FLAT.zip"

  ## download to data/raw subfolder
  destFlatFilename <- paste(exdir, "NCIt_FLAT.zip", sep = "/")

  ## try to download
  tryCatch(
    expr = {
      utils::download.file(url=NCItURL,destfile = destFlatFilename)
      utils::unzip(zipfile = destFlatFilename, exdir = exdir,)
      ## clean up, remove files
      file.remove(destFlatFilename)
    }, error = function(e) {
      print("NCIt download failed")
    }
  )

  ## output file targets create files and return their paths
  ex_filepath = paste(exdir, "Thesaurus.txt", sep = "/")
  return(ex_filepath)

}
