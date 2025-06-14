'''6. Print Sum of Negative Numbers, Positive Even Numbers, and Positive Odd Numbers in a List
Write a progthat takes a list of integers and calculates:

* The sum of all negative numbers
* The sum of all positive even numbers
* The sum of all positive odd numbers
  Display each sum clearly. '''

list = [10, -5, 3, -2, 8, 7, -1, 4, 0]

negative_sum = 0
positive_even_sum = 0
positive_odd_sum = 0

for num in list:
    if num < 0:
        negative_sum += num
    elif num > 0 and num % 2 == 0:
        positive_even_sum += num
    elif num > 0 and num % 2 != 0:
        positive_odd_sum += num

print("Sum of negative numbers:", negative_sum)
print("Sum of positive even numbers:", positive_even_sum)
print("Sum of positive odd numbers:", positive_odd_sum)  


