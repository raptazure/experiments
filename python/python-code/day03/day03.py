print('Multiplying complex number: ',(1+1j) * (1-1j))
# Calculating area of a circle
radius = 10                                 
area_of_circle = 3.14 * radius ** 2          
print('Area of a circle:', area_of_circle)

print('1 is 1', 1 is 1)                   # True - because the data values are the same
print('1 is not 2', 1 is not 2)           # True - because 1 is not 2
print('A in Asabeneh', 'A' in 'Asabeneh') # True - A found in the string
print('B in Asabeneh', 'B' in 'Asabeneh') # False -there is no uppercase B
print('coding' in 'coding for all') # True - because coding for all has the word coding
print('a in an:', 'a' in 'an')      # True
print('4 is 2 ** 2:', 4 is 2 **2)   # True
print(not 3 > 2)     # False - because 3 > 2 is true

# Exercises
base = input("Enter base: ")
height = input("Enter height: ")
print(f"The area of the triangle is {int(base) * int(height) * 0.5}")
for i in range(10):
    x = i - 10
    if x ** 2 + 6 * x + 9 == 0:
        print(x)
print('on' in 'python' and 'on' in 'jargon')

