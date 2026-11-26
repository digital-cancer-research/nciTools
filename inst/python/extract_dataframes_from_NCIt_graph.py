import pandas as pd

from rdflib import URIRef
from rdflib.namespace import RDF, RDFS, OWL


BASE = "http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#"


def _code(uri):
    """Extract an NCIt code from an NCIt URI."""
    if uri is None:
        return None

    uri = str(uri)

    if not uri.startswith(BASE):
        return None

    return uri.split("#")[-1]

def get_relationship_types(g):
    """
    Extract NCIt role and association types from a parsed RDF graph.
    """

    relationship_types = []

    definition_property = URIRef(BASE + "P97")

    # --------------------------------------------------------------
    # Roles
    # --------------------------------------------------------------

    for prop in g.subjects(RDF.type, OWL.ObjectProperty):

        code = _code(prop)

        if code is None or not code.startswith("R"):
            continue

        label = g.value(prop, RDFS.label)
        domain = g.value(prop, RDFS.domain)
        range_ = g.value(prop, RDFS.range)
        definition = g.value(prop, definition_property)

        relationship_types.append({
            "code": code,
            "label": str(label) if label is not None else "",
            "type": "role",
            "domain_code": _code(domain) or "",
            "range_code": _code(range_) or "",
            "definition": str(definition) if definition is not None else ""
        })

    # --------------------------------------------------------------
    # Associations
    # --------------------------------------------------------------

    for prop in g.subjects(RDF.type, OWL.AnnotationProperty):

        code = _code(prop)

        if code is None or not code.startswith("A"):
            continue

        label = g.value(prop, RDFS.label)
        definition = g.value(prop, definition_property)

        relationship_types.append({
            "code": code,
            "label": str(label) if label is not None else "",
            "type": "association",
            "domain_code": "",
            "range_code": "",
            "definition": str(definition) if definition is not None else ""
        })

    return relationship_types

def inspect_code(g, code):
    uri = URIRef(BASE + code)

    return {
        "as_subject": [
            (str(p), str(o))
            for p, o in g.predicate_objects(uri)
        ],
        "as_predicate": [
            (str(s), str(o))
            for s, o in g.subject_objects(uri)
        ],
        "as_object": [
            (str(s), str(p))
            for s, p in g.subject_predicates(uri)
        ]
    }


def get_relationships(g):
    """
    Extract NCIt roles and associations from a parsed RDF graph.

    Direct parent-child relationships are excluded.

    Returns
    -------
    list
        List of tuples containing:
        subject_code, predicate_code, object_code
    """

    relationships = []

    for concept in g.subjects(RDF.type, OWL.Class):

        subject_code = _code(concept)

        if subject_code is None or not subject_code.startswith("C"):
            continue

        # ----------------------------------------------------------
        # Roles (R codes)
        # ----------------------------------------------------------

        for parent in g.objects(concept, RDFS.subClassOf):

            if (parent, RDF.type, OWL.Restriction) not in g:
                continue

            predicate = g.value(
                parent,
                OWL.onProperty
            )

            object_ = g.value(
                parent,
                OWL.someValuesFrom
            )

            predicate_code = _code(predicate)
            object_code = _code(object_)

            if (
                predicate_code is not None
                and predicate_code.startswith("R")
                and object_code is not None
                and object_code.startswith("C")
            ):
                relationships.append(
                    (
                        subject_code,
                        predicate_code,
                        object_code
                    )
                )

        # ----------------------------------------------------------
        # Associations (A codes)
        # ----------------------------------------------------------

        for predicate, object_ in g.predicate_objects(concept):

            predicate_code = _code(predicate)
            object_code = _code(object_)

            if (
                predicate_code is not None
                and predicate_code.startswith("A")
                and object_code is not None
                and object_code.startswith("C")
            ):
                relationships.append(
                    (
                        subject_code,
                        predicate_code,
                        object_code
                    )
                )

    return relationships





# def get_relationships(g):
#     """
#     Extract NCIt relationships from a parsed RDF graph.
# 
#     Direct parent-child relationships are excluded.
#     """
# 
#     relationships = []
# 
#     for concept in g.subjects(RDF.type, OWL.Class):
# 
#         subject_code = _code(concept)
# 
#         if subject_code is None or not subject_code.startswith("C"):
#             continue
# 
#         for parent in g.objects(concept, RDFS.subClassOf):
# 
#             # Ignore direct parent-child relationships
#             parent_code = _code(parent)
# 
#             if parent_code is not None and parent_code.startswith("C"):
#                 continue
# 
#             # NCIt role represented as an OWL restriction
#             if (parent, RDF.type, OWL.Restriction) in g:
# 
#                 predicate = g.value(parent, OWL.onProperty)
#                 object_ = g.value(parent, OWL.someValuesFrom)
# 
#                 predicate_code = _code(predicate)
#                 object_code = _code(object_)
# 
#                 if (
#                     predicate_code is not None
#                     and object_code is not None
#                 ):
#                     relationships.append(
#                         (
#                             subject_code,
#                             predicate_code,
#                             object_code
#                         )
#                     )
# 
#     return relationships

