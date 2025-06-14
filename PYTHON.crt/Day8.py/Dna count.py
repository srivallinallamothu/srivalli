#count the DNA bases like a pro

'''write a ptogram that :
checks if the string has only vaild bases(A,T,C,G)
Then counts how many of each base there are.

enter the sample base value:"ATGCGATAAGCTTAA"
{'A':5,'T':4,'G':2,'C':2} '''

S=input("Enter the base sequence:")
A,C,G,T=0,0,0,0
sequence={1:'A',2:'C',3:'T',4:'G'}
for i in S:
    if i in sequence[1]:
     A+=1
    elif i in sequence[2]:
     C+=1   
    elif i in sequence[3]:
        T+=1
    elif  i in sequence[4]:
        G+=1
base={'A':A,'G':G,'C':C,'T':T}     
print(base)           



S=input("Enter the sequence in ATGC :")
base_count={'A':S.count('A'),'T':S.count('T'),'G':S.count('G'),'C':S.count('C')}
print(base_count) 


