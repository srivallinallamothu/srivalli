'''write a loop to convert  a DNA string to its RNA format 
Enter  the string :"ATCGTAC"
converted string  : "AUCGUAC" '''


dna = input("Enter the DNA string: ")
rna = ""
for base in dna:
    rna += base.replace('T', 'U')
print("Converted RNA string:", rna) 