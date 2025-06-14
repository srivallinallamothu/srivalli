'''Str="Python"
print(f"Length of {Str} is {len(str)}")
#Accessing without index 
for i in Str:
    print(i,end=" ")
print()
#Accessing without index
for i in range(len(Str)):
    print(Str[i],end=" ") '''
    
    
    #0123456789
    #slicing string
    
'''Str="Python Program"
print(Str[1:6])
print(Str[0:1])
print(Str[7:11])
print(Str[10:])
print(Str[7:10])
print(Str[2:6])
print(Str[::-1])
print(Str[-9::-1])
print(Str[-1:-8:-1])
print(Str[-4:-8:-1])
print(Str[-7:-12:-1])
print(Str[-1:-5:-1])
print(Str[-1:-4:-1]) '''

#JOIN
Input=input("Enter the string :")
print(f"User Entered String :{Input}")
Str_List=Input.split()
Str="".join(Str_List)
print(f"string without spaces: {Str} ")



#ASSIGNMENT
'''Write a python program to read the string as input from the user
a) print the string as a list of individual characters
b) find the length of the String
c) find the minimum element after converting string into list
d) find the number of spaces present in the string without using any built-in methods or function'''