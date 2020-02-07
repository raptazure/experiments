import os
import json
import csv
import xml.etree.ElementTree as ET

'''File handling'''
# Opening File for reading
f = open('./files/reading_file_example.txt')
txt = f.read()  # read the whole text as string.
print(type(txt))    # <class 'str'>
print(txt)
line = f.readline()  # read only the first line
print(type(line))   # <class 'str'>
print(line)
lines = f.readlines()   # read all the text line by line
print(type(lines))  # <class 'list'>
print(lines)
lines = f.read().splitlines()   # read lines without enter
print(type(lines))  # <class 'list'>
print(lines)
f.close()

# Opening file for writing or updating
with open('./files/reading_file_example.txt', 'a') as f:
    f.write('This text has to be appended at the end')
with open('./files/writing_file_example.txt', 'w') as f:
    f.write('This text will be written in a newly created file')

# Deleting file
if os.path.exist('./files/example.txt'):
    os.remove('./files/example.txt')
else:
    os.remove('The file does not exist')

'''File Types'''
# Changing JSON to dictionary
person_json = '''{
    "name": "Asabeneh",
    "country": "Finland",
    "city": "Helsinki",
    "skills": ["JavaScrip", "React", "Python"]
}'''
person_dct = json.loads(person_json)    # Change JSON to dictionary
print(person_dct)
print(person_dct['name'])

# Changing dictionary to JSON
person = {
    "name": "Asabeneh",
    "country": "Finland",
    "city": "Helsinki",
    "skills": ["JavaScrip", "React", "Python"]
}
person_json = json.dumps(person, indent=4)  # Convert it to json
print(type(person_json))
print(person_json)

# Saving as JSON file
person = {
    "name": "Asabeneh",
    "country": "Finland",
    "city": "Helsinki",
    "skills": ["JavaScrip", "React", "Python"]
}
with open('./files/json_example.json', 'w', encoding='utf-8') as f:
    json.dump(person, f, ensure_ascii=False, indent=4)

# File with csv Extension
with open('./files/csv_example.csv') as f:
    # w use, reader method to read csv
    csv_reader = csv.reader(f, delimiter=',')
    line_count = 0
    for row in csv_reader:
        if line_count == 0:
            print(f'Column names are :{", ".join(row)}')
            line_count += 1
        else:
            print(
                f'\t{row[0]} is a teachers. He lives in {row[1]}, {row[2]}.')
            line_count += 1
    print(f'Number of lines:  {line_count}')

# File with xml Extension
tree = ET.parse('./files/xml_example.xml')
root = tree.getroot()
print('Root tag:', root.tag)
print('Attribute:', root.attrib)
for child in root:
    print('field: ', child.tag)
