'''4. Swap the First and Last Value of the Given List
Write a program that swaps the first and last elements of a given list and returns the updated list. The list may contain elements of any data type.'''

my_list = [1, "oops", 3.5, True]

if len(my_list) >= 2:
    temp = my_list[0]
    my_list[0] = my_list[-1]
    my_list[-1] = temp

print("Updated list:", my_list) 