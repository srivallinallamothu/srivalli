'''5.write a function find matches (query:  str, db: dict) -> list that:

Takes a query Dna string and a dictionary of sequence IDs and sequences.

Returns a list of sequence IDs where the query is found as a substring.

Enter the query =“ATGC”
[“seq1” ,”seq10”] '''


def find_matches(query: str, db: dict) -> list:
    matches = []
    for seq_id, sequence in db.items():
        if query in sequence:
            matches.append(seq_id)
    return matches
database = {
    "seq1": "ATGCGTAC",
    "seq2": "TTAGGC",
    "seq3": "GGATGCCT",
    "seq4": "CGTATC",
    "seq10": "GATGCA",
}
    
query = "ATGC"
result = find_matches(query, database)
print("Matching sequences:", result)  



