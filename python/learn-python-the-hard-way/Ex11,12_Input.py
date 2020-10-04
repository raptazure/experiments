#ex11 Asking Questions
print("Which university are you in?",end = " ")
university = input()
age = input("How old are you? ")
print(">>> age=",repr(age))
print(f"So, you are in {university} and you're {age}.")

# ex12 Promoting People
age = int(input("How old are you? "))
print(">>> age=",repr(age)) #String - find out the type of a variable
height = input(f"You're {age}? Nice. How tall are you?")
weight = input("How much do you weigh?")
print(f"So, you are {age} old,{height} tall and {weight} heavy.")
 