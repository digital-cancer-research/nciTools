import sqlite3

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


def create_db_from_graph(g, db_file):
    """
    Create a SQLite database from a parsed NCIt RDF graph.

    Parameters
    ----------
    g : rdflib.Graph
        Parsed NCI Thesaurus RDF graph.

    db_file : str
        Path to the SQLite database to create.

    Returns
    -------
    str
        Path to the created SQLite database.
    """

    print("Creating NCIt SQLite database...")

    con = sqlite3.connect(db_file)
    cur = con.cursor()

    # Create tables
    cur.executescript(
        """
        DROP TABLE IF EXISTS concept;
        DROP TABLE IF EXISTS relationship_type;
        DROP TABLE IF EXISTS relationship;

        CREATE TABLE concept (
            code TEXT PRIMARY KEY,
            preferred_term TEXT
        );

        CREATE TABLE relationship_type (
            code TEXT PRIMARY KEY,
            label TEXT,
            domain_code TEXT,
            range_code TEXT,
            definition TEXT
        );

        CREATE TABLE relationship (
            subject_code TEXT NOT NULL,
            predicate_code TEXT NOT NULL,
            object_code TEXT NOT NULL
        );
        """
    )

    # --------------------------------------------------------------
    # Concepts
    # --------------------------------------------------------------

    print("Extracting concepts...")

    concepts = []

    for concept in g.subjects(RDF.type, OWL.Class):

        code = _code(concept)

        if code is None or not code.startswith("C"):
            continue

        label = g.value(concept, RDFS.label)

        concepts.append(
            (
                code,
                str(label) if label is not None else None
            )
        )

    cur.executemany(
        """
        INSERT OR REPLACE INTO concept (
            code,
            preferred_term
        )
        VALUES (?, ?)
        """,
        concepts
    )

    print(f"Stored {len(concepts):,} concepts")

    # --------------------------------------------------------------
    # Relationship types
    # --------------------------------------------------------------

    print("Extracting relationship types...")

    relationship_types = []

    definition_property = URIRef(BASE + "P97")

    for prop in g.subjects(RDF.type, OWL.ObjectProperty):

        code = _code(prop)

        if code is None:
            continue

        label = g.value(prop, RDFS.label)
        domain = g.value(prop, RDFS.domain)
        range_ = g.value(prop, RDFS.range)
        definition = g.value(prop, definition_property)

        relationship_types.append(
            (
                code,
                str(label) if label is not None else None,
                _code(domain),
                _code(range_),
                str(definition) if definition is not None else None
            )
        )

    cur.executemany(
        """
        INSERT OR REPLACE INTO relationship_type (
            code,
            label,
            domain_code,
            range_code,
            definition
        )
        VALUES (?, ?, ?, ?, ?)
        """,
        relationship_types
    )

    print(f"Stored {len(relationship_types):,} relationship types")

    # --------------------------------------------------------------
    # Relationships
    # --------------------------------------------------------------

    print("Extracting relationships...")

    relationships = []

    for concept in g.subjects(RDF.type, OWL.Class):

        subject_code = _code(concept)

        if subject_code is None or not subject_code.startswith("C"):
            continue

        for parent in g.objects(concept, RDFS.subClassOf):

            # Direct parent-child relationship
            parent_code = _code(parent)

            if parent_code is not None and parent_code.startswith("C"):

                relationships.append(
                    (
                        subject_code,
                        "is_a",
                        parent_code
                    )
                )

                continue

            # NCIt role represented as an OWL restriction
            if (parent, RDF.type, OWL.Restriction) in g:

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
                    and object_code is not None
                ):

                    relationships.append(
                        (
                            subject_code,
                            predicate_code,
                            object_code
                        )
                    )

    cur.executemany(
        """
        INSERT INTO relationship (
            subject_code,
            predicate_code,
            object_code
        )
        VALUES (?, ?, ?)
        """,
        relationships
    )

    print(f"Stored {len(relationships):,} relationships")

    # --------------------------------------------------------------
    # Indexes
    # --------------------------------------------------------------

    print("Creating indexes...")

    cur.executescript(
        """
        CREATE INDEX idx_relationship_subject
            ON relationship(subject_code);

        CREATE INDEX idx_relationship_predicate
            ON relationship(predicate_code);

        CREATE INDEX idx_relationship_object
            ON relationship(object_code);
        """
    )

    con.commit()
    con.close()

    print(f"NCIt SQLite database created: {db_file}")

    return db_file
