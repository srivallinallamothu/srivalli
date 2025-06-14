# 3.  write a python to read the size of list as input from the user and take the list elements also as input from the user 
# find the length of the list max element or num  present in the similarly the min element , the summation of elements and print the sorted list in ascending order 

Size=int(input("Enter the Size of List :"))
Num=[]
for i in range(Size):
    Temp=int(input(f"Enter the Element at{i} index:"))
    Num.append(Temp)
print(f"Given List : {Num}")    
print("Maximum Element :",max(Num))
print("Minimum Element :",min(Num))
print("Summation  :",sum(Num))
print("Sorted List :",sorted(Num)) 

Cartoons=['Tom&Jerry','Doremon','Shinchan','Oggy & Cockroaches','Heidi' ,'Horrid Henry']
print(Cartoons)
print("After Appending : ")
Cartoons.append('Heidi')
print(Cartoons)
Cartoons.insert(0,'Motu&Patlu')
print(Cartoons)
Cartoons.pop()
print(Cartoons)
Cartoons.pop(0)
print(Cartoons)
#add another element 

Cartoons=['Tom&Jerry','Doremon','Shinchan','Oggy & Cockroaches']
print(Cartoons)
Cartoons.remove('Oggy & Cockroaches')
print(Cartoons)

print(Cartoons.index('Tom&Jerry'))
#print(Cockroaches)   

Num=[11,22,33,44,55,66,77,88,99,100]
print(Num[0:])
print(Num[1:4+1])
print(Num[::1])
print(Num[::2])
print(Num[1::2])
print(Num[::-1])
