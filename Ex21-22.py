# ex21 Functions Can Return Something
def add(a,b):
    print(f"ADDING {a} + {b}")
    return a + b

def subtract(a,b):
    print(f"SUBTRACTING {a} - {b}") 
    return a - b

def mutiply(a,b):
    print(f"MULTIPLYING {a} * {b}")
    return a * b

def devide(a,b):
    print(f"DEVIDING {a} / {b}")
    return a / b

print("Let's do some math with just functions!")

age = add(30,5)
height = subtract(78,4)
weight = mutiply(90,2)
iq = devide(100,2)

print(f"Age: {age}, Height: {height}, Weight:{weight}, IQ: {iq}")

print("Here is a puzzle.")
what = add(age,subtract(height,mutiply(weight,devide(iq,2))))
print("That becomes:", what, "Can you do it by hand?")

# ex22 What Do You Know So Far?
# Make Flash Cards to remember what everything is going to do.
# The most important thing when doing this exercise is :"There is no failure, only trying."