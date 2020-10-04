for i in range(1, 8):
    print('#' * i)

for i in range(1, 9):
    for j in range(1, 9):
        print('#', end = ' ')
    print('')

for i in range(11):
    print(f"{i} x {i} = {i * i}")

skills = ['Python', 'Numpy','Pandas','Django', 'Flask']
for skill in skills:
    print(skill, end = ' ')

for i in range(0, 10, 2):
    print(i, end = ' ')
print('')

countries = [
  'Swaziland',
  'Sweden',
  'Switzerland',
  'Thailand',
]
lands = []
for i in range(len(countries)):
    if 'land' in countries[i]:
        lands.append(countries[i])
print(lands)

fruits, i = ['banana', 'orange', 'mango', 'lemon'], 0
for fruit in reversed(fruits):
    fruits[i] = fruit
    i += 1
print(fruits)
