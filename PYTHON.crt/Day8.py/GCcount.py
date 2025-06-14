'''write a program that:
takes a  DNA sequnce as a String
calculate the GC content as a percentage.
classifies the sequence as:
"High GC " if GC% >  60
"moderate GC if GC% is between 40 and 60"
"Low GC " if GC% <= 40'''  

str=input("Enter the DNA sequence: ")
gc_count = str.count('G') + str.count('C')
gc_percentage = (gc_count / len(str)) * 100
print(f"GC Content: {gc_percentage:.2f}%")
if gc_percentage > 60:
    print("High GC")
elif 40 <= gc_percentage <= 60:
    print("Moderate GC")
else:
    print("Low GC") 
    
    
sequence = input("Enter the DNA sequence: ")
gc_count = 0
for base in sequence:
    if base == 'G' or base == 'C':
        gc_count += 1
gc_percent = (gc_count / len(sequence)) * 100
print("GC Content: ", gc_percent, "%")
if gc_percent > 60:
    print("Classification: High GC")
elif gc_percent >= 40:
    print("Classification: Moderate GC")
else:
    print("Classification: Low GC") 
    