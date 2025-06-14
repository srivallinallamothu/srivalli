'''7. write a python program to the print the multiplication table from 1 to n.'''

Num = int(input("Enter the value of n: "))
for i  in range(1,Num+1):
    print(f"\nMultiplication Table of {i}:")
    for j in range(1, 11):
        print(f"{i}x{j}={i*j}")