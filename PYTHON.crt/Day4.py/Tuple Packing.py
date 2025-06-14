#Tuple Packing
Tuple=10,20,30,40,50,60,70,80,90,100
print(Tuple)
print(type(Tuple))
#Tuple Unpacking
n1,n2,n3,n4,n,n6,n,n8,n9,n10=Tuple
print(n1,type(n1))
print(n2,type(n2))

Tuple=(('a','b','c'),('A','B','C'),(1,2,3),(-1,-2,-3))
print(Tuple)
for i in Tuple:
    print(i,type(Tuple))

Tuple=(10,25,5,15,17,30,35)
print(Tuple)
print("Maximum Number :",max(Tuple))
print("Minimum Number :",min(Tuple))
print("Summation :",sum(Tuple))
print("Sorted Tuple :",sorted(Tuple))
print("Reverse order:",reversed(Tuple)
