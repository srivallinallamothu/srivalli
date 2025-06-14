'''write a python program to read a string as input from the user and print the count of 

a)uppercase vowels
b)lowercase vowels
c)uppercase consonants
d)lowercase consonants '''

'''str=input("Enter the string :")
U_Vowels,L_Vowels,U_Consonants,L_Consonants=0,0,0,0
for ch in str:
    if(ch.isalpha() and ch.isupper()):
        if ch in 'AEIOU':
            U_Vowels+=1
        else:
            U_Consonants+=1
    if(ch.isalpha() and ch.islower()):
        if ch in 'aeiou':
            L_Vowels+=1
        else:
            L_Consonants+=1
print(f"Upper Case Vowel counts :{U_Vowels}")
print(f"Lower Case Vowel counts :{L_Vowels}")
print(f"Upper Case Consonants counts :{U_Consonants}")
print(f"Lower Case Consonants counts :{L_Consonants}") '''       


