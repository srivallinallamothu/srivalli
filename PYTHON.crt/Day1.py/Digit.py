'''2. write a python program to read the integer value as input from the user & checck whether it is a digit  or  number'''
'''sample test case :
15 is a number
1 is a digit'''

Num=int(input("Enter the integer: "))
if(Num>-9 and Num<9):
    print(f"{Num}is a digit")
else:
    print(f"{Num}is a Number")    
#using ternary 
 
result="Digit" if (Num>=-9 and Num<=9) else "Number"
print(f"{Num}is {result}")
