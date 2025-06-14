#list
'''loop through the list and assign the correct label to each number.
enter the count of data:5
user  entered list :[3.2,5.5,12.4,16.7,2.1]
["underexpressed", "normal","normal","overexpressed","underexpressed"]  '''

n = int(input("Enter the count of data: "))
list= []
list1=[]
for i in range(n):
    temp= float(input(f"Enter the values :"))
    list.append(temp)
for i in list:
    if i< 5:
        list1.append("underexpressed")
    elif i>=5 and i<=15:
        list1.append("normal")
    else:
        list1.append("overexpressed")

print("list:", list1)
