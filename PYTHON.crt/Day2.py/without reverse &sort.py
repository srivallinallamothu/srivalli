#write a python program to reverse a list of numbers without using reverse method.
#write a python program to sort a list of numbers without using sort method.

numbers = [10, 20, 30, 40, 50]
reversed_numbers = numbers[::-1]
print("Reversed List:", reversed_numbers)


numbers = [10, 20, 30, 40, 50]
left, right = 0, len(numbers) - 1
while left < right:
    numbers[left], numbers[right] = numbers[right], numbers[left]
    left += 1
    right -= 1
print("Reversed List:", numbers)  




numbers = [76, 23, 45, 12, 54, 9]
for i in range(len(numbers)):
    for j in range(i + 1, len(numbers)):
        if numbers[i] > numbers[j]:
            numbers[i], numbers[j] = numbers[j], numbers[i]
print("Sorted List:", numbers)  

