def get_ncit_relationships(g, code):
    from rdflib import URIRef
    from rdflib.namespace import RDFS, OWL

    base = "http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#"
    concept = URIRef(base + code)

    relationships = []

    for restriction in g.objects(concept, RDFS.subClassOf):

        if (restriction, None, OWL.Restriction) in g:

            property_ = g.value(restriction, OWL.onProperty)
            target = g.value(restriction, OWL.someValuesFrom)

            if property_ is not None and target is not None:
                relationships.append((
                    str(property_).split("#")[-1],
                    str(target).split("#")[-1]
                ))

    return relationships
