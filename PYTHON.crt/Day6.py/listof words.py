'''write a python program to read a sentence as a input from the user and print the list of words from the sentence'''


Sentence="We are learning python"
list=Sentence.split()
print(list)
for ch in range(len(Sentence)):
    print(Sentence[ch],end=" ")
    if(Sentence[ch+1]==""):
       print() 