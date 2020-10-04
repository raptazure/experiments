import ast
import functools
countries = ['Estonia', 'Finland', 'Sweden', 'Denmark', 'Norway', 'Iceland']
numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

countries_upper = list(map(lambda country: country.upper(), countries))
print(countries_upper)
numbers_sq = list(map(lambda num: num ** 2, numbers))
print(numbers_sq)

lands = list(
    filter(lambda country: True if 'land' in country else False, countries))
print(lands)

six_or_more = list(
    filter(lambda country: True if len(country) >= 6 else False, countries))
print(six_or_more)

e_starter = list(
    filter(lambda country: True if country[0] == 'E' else False, countries))
print(e_starter)

lst = [2, 3, 's', 't']
get_string_lists = list(
    filter(lambda item: True if type(item) == str else False, lst))
print(get_string_lists)


def sum_numbers(a, b):
    return a + b


sum = functools.reduce(sum_numbers, numbers)
print(sum)


def linker(str1, str2):
    return str1 + ', ' + str2


concatenate = functools.reduce(linker, countries[0:-1])
print(concatenate + f' and {countries[-1]} are north European countris.')


def letter_counter(arr: list):
    letters = set()
    letter_dict = {}
    for item in arr:
        letters.add(item[0].lower())
    for letter in letters:
        letter_dict[letter] = 0
    for item in arr:
        letter_dict[item[0].lower()] += 1
    return letter_dict


print(letter_counter(countries))


with open('data.py', 'r') as f:
    mylist = ast.literal_eval(f.read())


def letter_cnt(mylist):
    letters = set()
    letter_dict = {}
    for item in mylist:
        s = item.get('name')
        letters.add(s[0].lower())
    for letter in letters:
        letter_dict[letter] = 0
    for item in mylist:
        s = item.get('name')
        letter_dict[s[0].lower()] += 1
    sorted_dct = sorted(letter_dict.items(), key=lambda kv: kv[1])
    return sorted_dct


print(letter_cnt(mylist)[-1])


def most_populated_countries(lst: list):
    return sorted(lst, key=lambda k: k['population'], reverse=True)


newlist = most_populated_countries(mylist)
sliced_list = newlist[0:10]

for i in range(0, 10):
    print(sliced_list[i].get('name'), end=' ')
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
    return sorted(dct.items(), key=lambda k: k[1], reverse=True)


print(most_spoken_languages(lan_dct)[0:10])
