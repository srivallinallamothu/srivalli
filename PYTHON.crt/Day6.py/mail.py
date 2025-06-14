'''Write a python program to read mail id as input from the user and print user name and organisation name based on mail id (name@org.com)'''


Mail_id=input("Enter the Mail ID:")
list=Mail_id.split('@')
print(f"user Name: {list[0]}")
Org=list[1]
list=Org.split('.')
print(f"org Name:{list[0]}") 