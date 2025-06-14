'''5. Calculate the Average of Numbers in a Given List
Write a program to calculate and return the average (mean) of a list of numbers (integers or floats). If the list is empty, handle the case with a suitable message. '''

list = [10, 20, 30, 40]

if len(list) == 0:
    print("The list is empty. Cannot calculate average.")
else:
    total = sum(list)
    average = total / len(list)
    print("Average:", average)  
