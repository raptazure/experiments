'''Exception Handling'''
try:
    name = input('Enter your name:')
    year_born = input('Year you born:')
    age = 2019 - int(year_born)
    print(f'You are {name}. And your age is {age}.')
except TypeError:
    print('Type error occur')
except ValueError:
    print('Value error occur')
except ZeroDivisionError:
    print('zero division error occur')
else:
    print('I usually run with the try block')
finally:
    print('I alway run.')

'''Packing and Unpacking Arguments in Python'''
#Unpacking list (* for tuples)
def sum_of_five_nums(a, b, c, d, e):
    return a + b + c + d + e
lst = [1,2,3,4,5]
print(sum_of_five_nums(*lst))  # 15

#Unpacking dictionary (** for dictionaries)
def unpacking_person_info(name, country, city, age):
    return f'{name} lives in {country}, {city}. He is {age} year old.'
dct = {'name':'Asabeneh', 'country':'Finland', 'city':'Helsinki', 'age':250}
print(unpacking_person_info(**dct))        # Asabeneh lives in Finland, Helsinki. He is 250 years old.

#Packing list (* for tuples)
def sum_all(*args):
    s = 0
    for i in args:
        s += i
    return s 
print(sum_all(1, 2, 3))             # 6
print(sum_all(1, 2, 3, 4, 5, 6, 7)) # 28

#Packing dictionary (** for dictionaries)
def packing_person_info(**kwargs):
    for key in kwargs:
        print(f"{key} = {kwargs[key]}")
    return kwargs        #kwargs now is a dictionary
print(packing_person_info(name="Asabeneh",
      country="Finland", city="Helsinki", age=250))

'''Spreading in Python'''
lst_one = [1, 2, 3]
lst_two = [4, 5, 6,7]
lst = [0, *lst_one, *lst_two]
print(lst)        # [0, 1, 2, 3, 4, 5, 6, 7]
country_lst_one = ['Finland', 'Sweden', 'Norway']
country_lst_two = ['Denmark', 'Iceland']
nordic_countries = [*country_lst_one, *country_lst_two]
print(nordic_countries)        #['Finland', 'Sweden', 'Norway', 'Denmark', 'Iceland']

'''Enumerate'''
countries = ['Finland', 'Sweden', 'Norway', 'Denmark', 'Iceland']
for index, i in enumerate(countries):
    print('hi')
    if i == 'Finland':
        print(f'The country {i} has been found at index {index}')        

'''Zip'''
fruits = ['banana', 'orange', 'mango', 'lemon']                    
vegetables = ['Tomato', 'Potato', 'Cabbage','Onion', 'Carrot']
fruits_and_veges = []
for f, v in zip(fruits, vegetables):
    fruits_and_veges.append({'fruit':f, 'veg':v})
print(fruits_and_veges)        