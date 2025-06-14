'''3.write a python program to read the integer value as input from the user & checck whether it is a two digit number  or not  a two digit number'''

num = int(input("Enter a number: "))
# Check if the number is a two-digit number
if -99<=num<= -10 or 10<=num<= 99:
    print(f"{num} is a two-digit number.")
else:
    print(f"{num} is not a two-digit number.")