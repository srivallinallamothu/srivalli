'''Write a python program to take name as input including prefix(Mr/Ms)
print the gender classification of the name on the bases of prefix '''


str=input("Enter the name with prefix (ms/mr):")
if str.startswith('ms'):
    print("Female")
else:
    print("Male")
    
    