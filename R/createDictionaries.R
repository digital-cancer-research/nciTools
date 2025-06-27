#' Create dictionaries based on NCI thesaurus
#'
#' @description
#' Uses the `createDictionary` function to create `quanteda` dictionaries of cancer types, drugs, genes and molecular alterations.\cr
#' Dictionary keys are NCI thesaurus codes, dictionary values are NCI thesaurus synonyms.\cr
#' @param thesaurus (processed) NCI thesaurus.
#' @returns A list containing four dictionaries:
#' - `cancer_dict`: Entities of semantic type 'Neoplastic Process' .
#' - `drug_dict`: Entities of semantic type 'Pharmacologic Substance',
#'   'Biologically Active Substance', 'Clinical Drug', 'Steroid', 'Immunologic Factor',
#'   and 'Therapeutic or Preventive Procedure'.
#' - `gene_dict`: Entities of semantic type 'Gene or Genome'.
#' - `alteration_dict`: Entities of semantic type 'Cell or Molecular Dysfunction'.
#'
#' @export
createDictionaries = function(thesaurus) {

  ## cancer type dict
  cancer_dict = createDictionary(source_data = thesaurus, semantic_types = 'Neoplastic Process')

  drug_dict = createDictionary(source_data = thesaurus, semantic_types = c("Pharmacologic Substance", "Biologically Active Substance", "Clinical Drug", "Steroid", "Immunologic Factor", "Therapeutic or Preventive Procedure"))

  gene_dict = createDictionary(source_data = thesaurus, semantic_types = 'Gene or Genome')

  alteration_dict = createDictionary(source_data = thesaurus, semantic_types = 'Cell or Molecular Dysfunction')


  return(list(
    cancer_dict = cancer_dict,
    drug_dict = drug_dict,
    gene_dict = gene_dict,
    alteration_dict = alteration_dict
  )
  )

}
