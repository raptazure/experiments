# ex31 Making Decisions
print("""You enter a dark room with two doors.
Do you go through door #1 or door #2?""")

door = input("> ")
if door == "1":
    print("There is a giant bear here eating a cheese cake.")
    print("what do you do?")
    print("1.take the cake")
    print("2.scream at the bear")

    bear = input("> ")
    if bear == "1":
        print("The bear eats your face off. Good job!")
    elif bear == "2":
        print("The bear eats your legs off. Good job!")
    else:
        print("Well, doing {bear} is probably better")

elif door == "2":
    print("You stare into the endless abyss at Cthulhu's retina.")
    print("1. B;ueberries.")
    print("2. Yellow jacket clothespins.")
    print("3. Understading revolvers yelling melodies.")

    insanity = input("> ")

    if insanity == "1" or insanity == "2":
        print("Your body survives powered by a mind of jello.")
        print("Goood")
    else:
        print("The insanity rots your eyes into a pool of muck")    
else:
    print("You stumble around and fall on a knife and die.")