from rdflib import Graph

def parse_ncit_owl(owl_file):
    g = Graph()
    g.parse(owl_file)
    print(f"Loaded {len(g):,} RDF triples")
    return g
