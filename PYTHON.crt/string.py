'''write a  python program to read the string input from the user 
a) reverse the string
b) convert the string lower case
c) convert into upper case
d) convert the ch of strings to lower case if it is in uppercaseand convert the string into lower case
check whether the string is starting with  letter A print the count of the ch a from the given string  and replace all 'p' to 'j'  ''' 


Str=input("Enter the string: ")
print(Str[::-1])
print(Str.lower())
print(Str.upper())
print(Str.swapcase())
print(Str.startswith('P'))
print(Str.count('P'))
Str=Str.lower()
print(Str.replace('p','j'))  