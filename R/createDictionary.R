#' Create a Specific Dictionary from NCI Thesaurus Data
#'
#' @description
#' This function constructs a dictionary for a specific set of semantic types from the NCI Thesaurus data.
#' It processes the source data to filter and organize terms based on the specified semantic types,
#' facilitating the creation of targeted dictionaries for use in annotation tasks. The output dictionary
#' is structured to associate unique codes with their corresponding synonyms, enhancing the annotation
#' process by allowing for the identification of terms through various synonymous expressions.
#'
#' @param source_data A dataset derived from the NCI Thesaurus, expected to contain detailed term
#' information including unique codes, synonyms, and semantic types. This data serves as the input
#' for generating the dictionary.
#'
#' @param semantic_types A character vector specifying the semantic types of interest. These types
#' define the scope of the dictionary, determining which terms from the source data are included based
#' on their semantic classification.
#'
#' @return Returns a dictionary object structured for annotation purposes. The dictionary is
#' specifically designed to map unique codes (representing specific terms) to a list of synonyms,
#' allowing for comprehensive term identification. This is particularly useful for annotating texts
#' with terms that may appear in various synonymous forms.
#'
#' @export
createDictionary = function(source_data, semantic_types) {

  ## creat dictionary
  source_data |>
    tidyr::unnest(cols = 'synonyms') |>
    tidyr::unnest(cols = 'semantic_type') |>
    as.data.frame() |>
    dplyr::filter(semantic_type %in% semantic_types) |>
    unique() |>
    utils::unstack(form = synonyms ~ code) |>         ## convert to named list where keys = codes, values = synonyms
    as.list() |>                               ## unstack() output is coerced to dataframe, dictionary() needs named list as input
    quanteda::dictionary()

}
