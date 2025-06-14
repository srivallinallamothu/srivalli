'''6. write a python program to read  an integer value  from the user & print the multiplication table of it.'''

num = int(input("Enter a number: "))
print(f"Multiplication Table of {num}:")
for i in range(1, 11):
    print(f"{num} x {i} = {num * i}")