'''write a python program to  read integer as input from the user and print no of 
A) uppercase letters or count of 
B) count of lower case letters
C) count of numerical values
D) print the count of special char '''



str=input("Enter the string:")
Uppercase_Alpha=0
Lowercase_Alpha=0
Numeric=0
special_char=0
for ch in str:
    if ch in str:
        if ch.isupper():
            Uppercase_Alpha+=1
        elif ch.islower():
            Lowercase_Alpha+=1
        elif ch.isdigit():
            Numeric+=1
    else:
        special_char+=1
print(f"count of Upper case letters:{Uppercase_Alpha}")
print(f"count of Lower case letters:{Lowercase_Alpha}")
print(f"count of Numeric characters:{Numeric}")
print(f"count of Special characters:{special_char}")  