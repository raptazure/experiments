age = int(input("enter your age: "))
if age >= 18:
    print("You are old enough to drive.")
else:
    print(f"You are left with {18 - age} years to drive")

fruits = ['banana', 'orange', 'mango', 'lemon']
fruit = input("enter a fruit name: ")
if fruit in fruits:
    print("already there")
else:
    fruits.append(fruit)
    print(fruits)

person = {
'first_name': 'Asabeneh',
'last_name': 'Yetayeh',
'age': 250,
'country': 'Finland',
'is_married': True,
'skills': ['Node', 'MongoDB', 'Python'],
'address': {
    'street': 'Space street',
    'zipcode': '02210'
    }
}

def findMiddle(lst: list):
    return lst[len(lst) // 2]

if 'skills' in person:
    print(f"the middle one is {findMiddle(person['skills'])}")

if 'skills' in person:
    if 'Python' in person['skills']:
        print('Python')

if set(person['skills']) == {'JavaScript', 'React'}:
    print('He is a front end developer.')
elif set(person['skills']) == {'Node', 'Python', 'MongoDB'}:
    print('He is a backend developer')
elif 'React' in person['skills'] and 'Node' in person['skills'] and 'MongoDB' in person['skills']:
    print('fullstack')
else:
    print('unkonwn')

if person['is_married'] == True:
    print(f"{person['first_name']} {person['last_name']} lives in {person['country']}. He is married")