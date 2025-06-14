'''list (group of elements)'''
#         0  1  2  3  4  5  6  7  8
Num_List=[10,20,30,40,50,60,70,80,90]
#        -9  -8 -7 -6 -5 -4 -3 -2 -1
print(Num_List)
print("Acessing the List Elements using +ve Indexing")
print((Num_List[0]))
print((Num_List[1]))
print((Num_List[2]))
print((Num_List[3]))
print((Num_List[4]))
print((Num_List[5]))
print((Num_List[6]))
print((Num_List[7]))
print((Num_List[8]))
print("Acessing the List Elements using -ve Indexing")
print((Num_List[-9]))
print((Num_List[-8]))
print((Num_List[-7]))
print((Num_List[-6]))
print((Num_List[-5]))
print((Num_List[-4]))
print((Num_List[-3]))
print((Num_List[-2]))
print((Num_List[-1])) 


#without indexing
#         0  1  2  3  4  5  6  7  8
Num_List=[10,20,30,40,50,60,70,80,90]
#        -9  -8 -7 -6 -5 -4 -3 -2 -1
print(Num_List)
print("Acessing the List Elements using for loop without indexing")
for i in Num_List:
    print(i)
print("Acessing the List Elements using for loop with indexing")
# range(start,stop,staepsize),range(start,stop),range(stop)
for i in range(len(Num_List)):
    print(Num_List[i])
print("Acessing the list Elements using while loop ")
i=0
while(i<len(Num_List)): 
     print(Num_List[i])
       i+=1      
        