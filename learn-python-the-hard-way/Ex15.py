# ex15 Reading Files
from sys import argv

script,filename = argv

txt = open(filename)
#txt.close()       Break it!

print(f"Here is your file {filename}")
print(txt.read())

print("Type the filename again:")
file_again = input("> ")

txt_again = open(file_again)

print(txt_again.read())

#python Ex15.py Ex15_sample.txt