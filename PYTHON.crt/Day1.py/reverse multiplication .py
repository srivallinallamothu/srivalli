'''8.write a python program to print the reversed  multiplication table of n.'''

num = int(input("Enter a number: "))
# Output: Print the reversed multiplication table for the given number
print(f"Reversed Multiplication Table of {num}:")
for i in range(10, 0, -1):
    print(f"{num} x {i} = {num * i}")