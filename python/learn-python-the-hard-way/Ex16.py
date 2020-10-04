# ex16 Reading and Writing Files
'''
 close:Close the file.Like File->Save... in your editor
 read:Reads the contents of the file.You can assign the result to a variable.
 readline:Reads just one line of a text file.
 truncate:Empties the file.Watch out if you care about the file.
 write('stuff'):Writes "stuff" to the file.
 seek(0):Moves the read/write location to the beginning of the file.
'''

from sys import argv
 
script,filename = argv
 
print(f"We're going to erase {filename}")
print("If you don't want that, hit CTRL-C (^C).")
print("If you want that, hit RETURN.")
 
input("?")
 
print("Opening the file...")
target = open(filename, 'w')#the kind of mode for the file w-write r-read a-append

print("Truncating the file.  Goodbye!")
target.truncate()

print("Now I'm going to ask you for three lines.")

line1 = input("line1:")
line2 = input("line2:")
line3 = input("line3:")

print("I'm going to write these to the file.")

target.write(line1)
target.write("\n")
target.write(line2)
target.write("\n")
target.write(line3)
target.write("\n")

print("And finally,we close it.")
target.close()

#python Ex16.py Ex16_test.txt 