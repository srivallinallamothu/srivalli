''' 9. write a python program 

 a) to print natural numbers from 1 to n
 b)to print natural numbers from n to 1
 c)to print squares from 1 to n
 d) to print squares from n to  1   
 e) to print cubes '''


'''a) to print natural numbers from 1 to n'''

Num=int(input("Enter the value of Num: "))
print(f"Natural Numbers from 1 to {Num}: ")
for i in range (Num):
    print(i)

'''b)to print natural numbers from n to 1'''

Num=int(input("Enter the value of Num: "))
print(f"Natural Numbers from 1 to {Num}: ")
for i in range (Num):
    print(i)

'''c)to print squares from 1 to n'''

Num=int(input("Enter the value of Num:"))
for i in range(1,Num+1):
    print(i*i*i) 

'''d) to print squares from n to  1 '''

n = int(input("Enter a number: "))
print("Squares from", n, "to 1:")
for i in range(n, 0, -1):
    print(f"{i}^2 = {i*i}") 

''' e) to print cubes  '''

n = int(input("Enter a number: "))

print("Cubes from 1 to", n, ":")
for i in range(1, n + 1):
    print(f"{i}^3 = {i**3}")