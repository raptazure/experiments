# A tuple is a collection of different data types which is ordered and unchangeable(immutable).
''' Creating Tuple '''
empty_tuple = ()
empty_tuple = tuple()
tp1 = ('item1', 'item2', 'item3')
fruits = ('banana', 'orange', 'mango', 'lemon')

''' Tuple Length '''
print(len(tp1))

''' Accessing Tuple Items '''
last_index =len(fruits) - 1
last_fruit = fruits[last_index]
first_fruit = fruits[-4]
second_fruit = fruits[-3]
last_fruit = fruits[-1]
print(first_fruit, second_fruit, last_fruit)

''' Slicing Tuples '''
all_fruits = fruits[0:4]       # all items
all_fruits= fruits[0:]         # all items
orange_mango = fruits[1:3]     # doesn't include item at index 3
orange_to_the_rest = fruits[1:]
all_fruits = fruits[-4:]       # all items
orange_mango = fruits[-3:-1]   # doesn't include item at index 3
orange_to_the_rest = fruits[-3:]

''' Changing Tuples to List '''
fruits = list(fruits)
fruits[0] = 'apple'
print(fruits)
fruits = tuple(fruits)
print(fruits)

''' Checking an Item '''
print('apple' in fruits and 'item1' in tp1)

''' Joining Tuples '''
tp2 = ('item4', 'item5','item6')
tp3 = tp1 + tp2
print(tp3)

''' Deleting Tuple '''
del tp1
try: print(tp1)
except BaseException as e: print(e, "=>")

''' Excercises '''
brothers = ('azure', 'bro')
sisters = ('rapt', 'sis')
siblings = sisters + brothers
print(len(siblings))
family_members = siblings + ('papa', 'mama')
print(family_members)
middle = family_members[ (len(family_members)-1)//2 : len(family_members)//2 + 1 ]
print(middle)