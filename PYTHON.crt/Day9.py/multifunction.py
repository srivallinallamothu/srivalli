#.#write a pythom program to bulit a function which prints the multiplication table of n

def print_multiplication_table(n):
    print(f"Multiplication Table of {n}:")
    for i in range(1, 11):
        print(f"{n} x {i} = {n * i}")
number = int(input("Enter a number: "))
print_multiplication_table(number)


4.#write a pythom program to bulit a function which prints the multiplication table of from 1 to n

def print_tables_upto(n):
    for num in range(1, n + 1):
        print(f"\nMultiplication Table of {num}:")
        for i in range(1, 11):
            print(f"{num} x {i} = {num * i}")
limit = int(input("Enter a number to print multiplication tables from 1 to that number: "))
print_tables_upto(limit) 
