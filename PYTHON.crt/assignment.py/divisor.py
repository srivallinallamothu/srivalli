
'''7. Divide All Elements of a List by a Number
Write a program that accepts a list of numbers and a divisor. Divide each element of the list by the divisor and return a new list with the results. Ensure the divisor is not zero.'''

list = [10, 20, 30, 40]
divisor = 5 

if divisor == 0:
    print("Cannot divide by zero.")
else:
    result_list = []
    for num in list:
        result_list.append(num / divisor)

    print("Resulting list:", result_list)
