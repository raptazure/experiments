dog = {}
dog['name'] = 'w'
dog['age'] = 2
print(dog)

student = {
    'name': 'miao',
    'skills': ['js', 'py', 'cpp'],
    'city': 'wei'
}
print(type(student['skills']))
student['skills'].append('meow')
print(student)

keys = student.keys()
print(keys)
values = student.values()
print(values)

lot = student.items()
print(lot)

student.pop('city')
print(student)
del student