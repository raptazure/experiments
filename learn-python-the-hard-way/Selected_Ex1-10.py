# ex3  Numbers and Math
print("Hens",25+30)
print("Is it true that 3+2<5-7?")
print(3+2<5-7)
print("What is 3+2",3+2)
print("Is it greater?",5>-2)

# ex4 Vairables and Names
passengers = 90
print("We have",passengers, "to carpool today")

# ex5 More Variables and Printing
my_name = 'Raptazure'
my_age = 18
my_weight = 92
my_height = 173
print(f"Let's talk about {my_name}.")
total = my_age+my_height+my_weight
print(f"If I add {my_age},{my_height},and {my_weight} I get {total}")

# ex6 Strings and Text
types_of_people = 10
x = f"There are {types_of_people} types of people."
binary = "binary"
do_not = "don't"
y = f"Those who know {binary} and those who {do_not}."
print(x)
print(y)
print(f"I said: {x}")
print(f"I also said: '{y}'")
hilarious = False 
joke_evaluation = "Isn't that joke so funny?!{}"
print(joke_evaluation.format(hilarious))
w = "This is the left side of..."
e = "a string with a right side."
print(w + e)

# ex7 More Printing
print("Mary had a little lamb.")
print("Its fleece was white as {}".format('snow'))
print("." * 10)
end1 = 'C'
end2 = 'h'
end3 = 'e'
end4 = 'e'
end5 = 's'
end6 = 'e'
end7 = 'B'
end8 = 'u'
end9 = 'r'
end10 = 'g'
end11 = 'e'
end12 = 'r'
#watch end = ' ' at the end.
print(end1 + end2 + end3 + end4 + end5 +end6 , end=' ')
print(end7 + end8 + end9 + end10 + end11 + end12)

# ex8 Printing,Printing
formatter = "{} {} {} {}"
print(formatter.format(1,2,3,4))
print(formatter.format(True,False,False,True))
print(formatter.format(formatter,formatter,formatter,formatter))
# from video-Debug:repr()  
# The repr() method returns a printable representational string of the given object.
# It returns a string that would yield an object with the same value when passed to eval().
# var='foo'
# repr(var)
print(formatter.format(
    "Try your",
    "Own text here",
    "Maybe a poem",
    "Or a song about fear"
))

# ex9 Printing,Printing,Printing
days = "Mon Tue Wed Thu Fri Sat Sun"
months = "Jan\nFeb\nMar\nApr\nMay\nJun\nJul\nAug"
print('Here are the days: ',days)
print('Here are the months: ',months)
print("""
There is something going on here.
With the three double-quotes.
we'll be able to type as much as we like.
Even 4 lines if we want,or 5,or 6""")

# ex10 What You Should See
tabby_cat = "\tI'm tabbed in."
persian_cat = "I'm split\non a line."
backslash_cat = "I'm \\ a \\ cat."
fat_cat = """
I'll do a list:
\t* Cat food
\t* Fishies
\t* Catnip\n\t* Grass
"""
print(tabby_cat)
print(persian_cat)
print(backslash_cat)
print(fat_cat)
