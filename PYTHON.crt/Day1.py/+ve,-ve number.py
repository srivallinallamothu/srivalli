'''1.write a python program to read the integer value as input from the user & checck whether it is a positive or negative number'''
'''sample test case :'''

'''Enter the integer value: 15'''
''' 15 is a +ve number '''

Num=int(input("Enter the integer: "))
#using if-else
if(Num>0):
    print(f"{Num}is a +ve number")
elif(Num<0):
    print(f"{Num}is a -ve number")
else:
    print(f"{Num}is 0")    

    #using ternery operator
    res="+ve Num" if(num>0) else "-ve Number" 