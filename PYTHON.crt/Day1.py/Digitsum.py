''' 11.write a python program to read the integer value as input from user and find the summation of  the digits
enter the integer value : 18715
22 is the summation of digits '''

'''18715----------->5
1871------------>1+5
187------------->7+6
18-------------->8+11
1--------------->1+21 '''

Num=int(input("Enter the value of Num :"))
DigitSum=0
Rem=0
while(Num!=0):
    Rem=Num%10
    DigitSum=DigitSum+Rem
    Num=Num//10
print(f"Summation is {DigitSum}") 