#DICT 

stu={101:'Rahul',102:'Raj',103:'Sonam'}
fees={'rahul':'2000','raj':'3000','sonam':8000}
print(stu[101])
print(stu[102])
print(stu[103])
print(fees['rahul'])
print(fees['raj'])
print(fees['sonam']) 


Evencode={101:'Hackathon',102:'coding',103:'Project'}
print(Evencode)
#modification
Evencode[102]='Coding Challenge'
print(Evencode) 



Jobrole={101:'fullstack developer',102:'Data Engineer',102:'product manager',103:'Data Analyst'}
print(Jobrole)
Jobrole[105]='cloud engineer'
print(Jobrole)
Jobrole[106]='Data visualization'
print(Jobrole)
Jobrole[107]='scientist'
print(Jobrole)
Jobrole.pop(101)
print(Jobrole)   


person = {'city': 'guntur', 'age': '32', 'job': 'developer'}
#delete
del person['city']
print(person) #output:{'name':'john','age':'32','job':'developer'}
job=person.pop('job')
print(job)
print(person)
print(len(person))
print(person.items())   


Dict1={1:'a',2:'b',3:'c',4:'d'}
print(dict)
Dict2={5:'e',6:'f',7:'g'}
print(Dict2)
Dict1.update(Dict2)
print(Dict1) 