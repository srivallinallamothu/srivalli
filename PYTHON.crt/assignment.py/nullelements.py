'''1.Count the Number of Null Elements in a List
Write a program that takes a list containing various data types, including None values. Your task is to count how many None values are present in the list and return the count. '''

list = [1, None, "hello",3.1, None, 5, None]

count = 0
for value in list:
    if value is None:
        count += 1

print("Number of None elements:", count)