''' 10 . write a python program to read the integer value as input from the user & find the number of digits present in that particular number'''
'''sample test case
enter the integer value :18715
12345 has 5 digits
               rem
18715//10------->5 , 1871
1871//10------->1 , 187
187//10------->7 , 18
18//10------->8 , 1
1//10------->1 , 0 '''

Num=int(input("Enter the value of Num:"))
Temp=Num
DigitCount=0
while(Num!=0):
    Num=Num//10
    DigitCount+=1 #DigitCount=DigitCount+1
print(f"{Temp} has {DigitCount} digits")   