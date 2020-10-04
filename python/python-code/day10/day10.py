''' While Loop '''
count = 0
while count < 5:
    print(count, end = '')
    count += 1
else:    # once the condition is no longer true
    print(count, end = '')
print('')

# break and continue
count = 0
while count < 5:
    print(count, end = '')
    count = count + 1
    if count == 3: break
print('')

count = 0
while count < 5:
    count += 1
    if count == 3:
        continue
    print(count, end = '')
print('')

''' For Loop '''
# list
numbers = [0, 1, 2, 3, 4, 5]
for number in numbers:
      print(number, end = '')
print('')

# string
language = 'Python'
for letter in language:
    print(letter, end = '')
print('')

# dict
person = {
      'name': 'Meow',
      'age': 250,
      'country': 'China',
      'skills': ['JavaScript', 'Python'],
      'address': {
          'street': 'Space street',
          'zipcode': '233'
      }
    }
for key in person:
    print(key, end = ' ')
print('')

for key, value in person.items():
    print(key, value, end = ' ')
print('')

# set
it_companies = {'Facebook', 'Google', 'Microsoft', 'Apple', 'IBM', 'Oracle', 'Amazon'}
for company in it_companies:
    print(company, end = " ")
print('')

# tuple
numbers = (0, 1, 2, 3, 4, 5)
for number in numbers:
    if number == 3:
        continue
    print(number, end = '')
print('')

# nested
for key in person:
    if key == 'skills':
        for skill in person['skills']:
            print(skill, end = ' ')
print('')

# else
for number in range(11):
    print(number, end = ' ')   # prints 0 to 10, not including 11
else:
    print('The loop stops at', number)

# pass
for i in numbers:
    pass

''' The range function '''
for number in range(5):
    print(number, end = ' ')
lst = list(range(0, 5, 2)) 
print(lst)



