# 2. write a python program to read list elements as input from user and display the list elements using for loop 

Size=int(input("Enter the Size of List: "))
Prog_Lang=[]
#reading the list elements as input
for i in range(Size):
    Temp=input("Enter a programming Lang :")
    Prog_Lang.append(Temp)
print("Elements of the List:")
print(Prog_Lang)    

'''Deleting '''

color=['white','Red','Blue','Black','Green']
print(color)
del color[2]
print(color)
del color
print(color)  