from mypackage import arithmetics  # mypackage is created by yourself
from mypackage import greet
import numpy    # install the packages under pip before inporting
import pandas
import webbrowser
import requests

'''Python PIP'''
# using numpy
numpy.version.version   # Select multiple lines and then press shift and enter to transform into interactive mode
lst = [1, 2, 3, 4, 5]
np_arr = numpy.array(lst)
np_arr
len(np_arr)
np_arr * 2
np_arr + 2

# using webbrowser
url_lists = [
    'http://www.python.org',
    'https://www.linkedin.com/in/asabeneh/',
    'https://twitter.com/Asabeneh',
    'https://twitter.com/Asabeneh',
]
for url in url_lists:
    webbrowser.open_new_tab(url)

# Reading from URL
url = 'https://www.w3.org/TR/PNG/iso_8859-1.txt'  # text from a website
response = requests.get(url)  # opening a network and fetching a data
print(response)
print(response.status_code)  # status code, success:200
print(response.headers)     # headers information
print(response.text)  # gives all the text from the page

# Creating a package
def add_numbers(*args):
    total = 0
    for num in args:
        total += num
    return total
def subtract(a, b):
    return (a - b)
def multiple(a, b):
    return a * b
def division(a, b):
    return a / b
def remainder(a, b):
    return a % b
def power(a, b):
    return a ** b
def greet_person(firstname, lastname):
    return f'{firstname} {lastname}, welcome to 30DaysOfPython Challenge!'
arithmetics.add_numbers(1, 2, 3, 5)
arithmetics.subtract(5, 3)
arithmetics.multiple(5, 3)
arithmetics.division(5, 3)
arithmetics.remainder(5, 3)
arithmetics.power(5, 3)
greet.greet_person('Asabeneh', 'Yetayeh')
