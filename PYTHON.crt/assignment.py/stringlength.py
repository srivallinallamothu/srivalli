'''2. Print Items from a List with Specific Length
You are given a list of strings and an integer n. Write a program that prints all the strings from the list that have a length exactly equal to n.'''


list=['java','python','c','oops','c++','dsa']
num=4
for word in list:
    if len(word) == num:
        print(word)  