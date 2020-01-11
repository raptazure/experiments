# ex19 Functions and Variables
def cheese_and_crakers(cheese_count, boxes_of_crakers):
    print(f"You have {cheese_count} cheeses!")
    print(f"You have {boxes_of_crakers} boxes of crakers!")
    print("Man that's enough for a party!")
    print("Get a blanket.\n")

print("We can just give the function numbers directly.")
amount_of_cheese = 10
amount_of_crakers = 50

cheese_and_crakers(amount_of_cheese,amount_of_crakers)

print("We can even do math inside too:")
cheese_and_crakers(10+20, 5+6)

print("And we can combine the two, variables and math:")
cheese_and_crakers(amount_of_cheese+100,amount_of_crakers+1000)



    