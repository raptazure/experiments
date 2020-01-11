# ex33 While Loops

i = 0
numbers = []
while i < 6:
    print(f"At the top i is {i}")
    numbers.append(i)
    i += 1
    print("Numbers now: ",numbers)
    print(f"At the bottom i is {i}")
print("The numbers: ")
for num in numbers:
    print(num)

# ex34 Accessing Elements of Lists

animals = ['bear', 'tiger']
bear = animals[0]