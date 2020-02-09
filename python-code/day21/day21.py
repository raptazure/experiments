'''Classes and Objects'''
# Everything in python is class
num = 10
print(type(num))    # <class 'int'>
string = 'string'
print(type(string))  # <class 'str'>
boolean = True
print(type(boolean))   # <class 'bool'>
lst = []
print(type(lst))    # <class 'list'>
tpl = ()
print(type(tpl))    # <class 'tuple'>
set1 = set()
print(type(set1))   # <class 'set'>
dct = {}
print(type(dct))   # <class 'dict'>

# Class Constructor
class Person:
    def __init__(self, firstname, lastname, age, country, city):
        self.firstname = firstname
        self.lastname = lastname
        self.age = age
        self.country = country
        self.city = city
p = Person('Asabeneh', 'Yetayeh', 250, 'Finland', 'Helsinki')
print(p.firstname)  # Asabeneh
print(p.lastname)   # Yetayeh
print(p.age)    # 250
print(p.country)    # Finland
print(p.city)   # Helsinki

# Object Methods
class Person:
    def __init__(self, firstname='Asabeneh', lastname='Yetayeh', age=250, country='Finland', city='Helsinki'):
        self.firstname = firstname
        self.lastname = lastname
        self.age = age
        self.country = country
        self.city = city
    def person_info(self):
        return f'{self.firstname} {self.lastname} is {self.age} year old. He lives in {self.city}, {self.country}.'
p1 = Person()
print(p1.person_info())    # Asabeneh Yetayeh is 250 year old. He lives in Helsinki, Finland. 
p2 = Person('John', 'Doe', 30, 'Nomanland', 'Noman city')
print(p2.person_info())    # John Doe is 30 year old. He lives in Noman city, Nomanland.

# Method to modify class default values
class Person:
    def __init__(self, firstname='Asabeneh', lastname='Yetayeh', age=250, country='Finland', city='Helsinki'):
        self.firstname = firstname
        self.lastname = lastname
        self.age = age
        self.country = country
        self.city = city
        self.skills = []
    def person_info(self):
        return f'{self.firstname} {self.lastname} is {self.age} year old. He lives in {self.city}, {self.country}.'
    def add_skill(self, skill):
        self.skills.append(skill)
p1 = Person()
p1.add_skill('HTML')
p1.add_skill('CSS')
p1.add_skill('JavaScript')
p2 = Person('John', 'Doe', 30, 'Nomanland', 'Noman city')
print(p1.skills)    # ['HTML', 'CSS', 'JavaScript']
print(p2.skills)    # []

# Inheritance
class Person:
    def __init__(self, firstname='Asabeneh', lastname='Yetayeh', age=250, country='Finland', city='Helsinki'):
        self.firstname = firstname
        self.lastname = lastname
        self.age = age
        self.country = country
        self.city = city
        self.skills = []
    def person_info(self):
        return f'{self.firstname} {self.lastname} is {self.age} year old. He lives in {self.city}, {self.country}.'
    def add_skill(self, skill):
        self.skills.append(skill)
class Student(Person):
    pass
s1 = Student('Eyob', 'Yetayeh', 30, 'Finland', 'Helsinki')
print(s1.person_info())    # Eyob Yetayeh is 30 year old. He lives in Helsinki, Finland.
s1.add_skill('JavaScript')
s1.add_skill('React')
s1.add_skill('Python')
print(s1.skills)    # ['JavaScript', 'React', 'Python']

# Overriding parent method
class Person:
    def __init__(self, firstname='Asabeneh', lastname='Yetayeh', age=250, country='Finland', city='Helsinki'):
        self.firstname = firstname
        self.lastname = lastname
        self.age = age
        self.country = country
        self.city = city
        self.skills = []
    def person_info(self):
        return f'{self.firstname} {self.lastname} is {self.age} year old. He lives in {self.city}, {self.country}.'
    def add_skill(self, skill):
        self.skills.append(skill)
class Student(Person):
    def __init__(self, firstname='Asabeneh', lastname='Yetayeh', age=250, country='Finland', city='Helsinki', gender='male'):
        self.gender = gender
        super().__init__(firstname, lastname, age, country, city)
    def person_info(self):
        gender = 'He' if self.gender == 'male' else 'She'
        return f'{self.firstname} {self.lastname} is {self.age} year old. {gender} lives in {self.city}, {self.country}.'
s1 = Student('Eyob', 'Yetayeh', 30, 'Finland', 'Helsinki', 'male')
print(s1.person_info())    # Eyob Yetayeh is 30 year old. He lives in Helsinki, Finland.
s1.add_skill('JavaScript')
s1.add_skill('React')
s1.add_skill('Python')
print(s1.skills)    # ['JavaScript', 'React', 'Python']
