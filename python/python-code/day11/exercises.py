def add_all_nums0(*nums):
    total = 0
    for num in nums:
        try:
            total += num
        except BaseException as e:
            print(e, ">> go on")
    return total
print(add_all_nums0(3, 4, 5, 6, 7, 's'))


def add_all_nums1(nums):
    total = 0
    for num in nums:
        total += num
    return total

lst = []
n = int(input("Enter number of elements : ")) 
for i in range(0, n):
    ele = int(input())
    lst.append(ele)
print(add_all_nums1(lst))


def reverse_list(lst):
    lst = lst[::-1]
    print(lst)
reverse_list(lst)


def capitalize_list_items(s: list):
    for i in range(0, len(s)):
        s[i] = s[i].upper()
    return s
sen = ['m', 'c', 'p', 'o']
print(capitalize_list_items(sen))


def add_item(lst, toAdd):
    lst.append(toAdd)
    return lst
def remove_item(lst, toRemove):
    lst.remove(toRemove)
    return lst
food_staff = ['Potato', 'Tomato', 'Mango', 'Milk']
print(add_item(food_staff, 'Meat'))
print(remove_item(food_staff, 'Meat'))


def is_unique(lst):
    lst_set = set(lst)
    if len(lst_set) == len(lst):
        return True
    return False
print(is_unique([1,1,2]))


import math
def is_prime(num):
    if num < 2:
        return False
    for i in range(2, int(math.sqrt(num)) + 1):
        if num % i == 0:
            return False
    return True
print(is_prime(7))


def is_same_data_type(lst: list):
    for i in range(1, len(lst)):
        if type(lst[i]) != type(lst[0]):
            return False
    return True
print(is_same_data_type([1, 2, 3, 4.5, 's']))


from keyword import iskeyword
def is_valid_variable_name(name):
    return name.isidentifier() and not iskeyword(name)
print(is_valid_variable_name('while'), is_valid_variable_name('123'))


import ast
with open('data.py', 'r') as f:
    mylist = ast.literal_eval(f.read())

def most_populated_countries(lst: list):
    return sorted(lst, key = lambda k: k['population'], reverse = True)

newlist = most_populated_countries(mylist)
sliced_list = newlist[0:15]

for i in range(0, 15):
    print(sliced_list[i].get('name'), end = ' ')
print('')

languages = set()
for conuntry in mylist:
    lan = conuntry.get('languages')
    for i in lan:
        languages.add(i)
lan_dct = dict()
for language in languages:
    lan_dct[language] = 0

for nation in mylist:
    _lan = nation.get('languages')
    for i in _lan:
        lan_dct[i] += 1

def most_spoken_languages(dct: dict):
    return sorted(dct.items(), key = lambda k: k[1], reverse = True)
print(most_spoken_languages(lan_dct)[0:15])