'''3. Find the Second Largest Number in a List
Given a list of integers, write a program to find and return the second largest unique number in the list. If no such number exists (due to insufficient unique values), handle it appropriately.'''

my_list = [4, 1, 7, 3, 7, 4, 9]
unique_numbers = list(set(my_list))

if len(unique_numbers) < 2:
    print("No second largest number exists.")
else:
    unique_numbers.sort(reverse=True) 
    print("Second largest number is:", unique_numbers[1]) 
