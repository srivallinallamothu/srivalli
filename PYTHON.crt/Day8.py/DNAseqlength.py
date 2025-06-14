'''write a python script that:
compares two DNA sequence of the same length.
identifies all positions where the bases differ.
reports the index and the base change.

sample input: 
reference: "ATGCGTAGCTA"
patient: "ATGTCGTAGTTA"
EXPECTED OUTPUT:
snp at position 4: C-T
snp at position 10:C-T    '''     
    
    
reference = "ATGCGTAGCTA"
patient =   "ATGTCGTAGTTA"
if len(reference) != len(patient):
    print("Error: Sequences are of different lengths.")
else:
    for i in range(len(reference)):
        if reference[i] != patient[i]:
            print(f"snp at position {i+1}: {reference[i]}-{patient[i]}") 
