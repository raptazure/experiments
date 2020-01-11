# ex44 Inheritance 

class Parent(object):
    def override(self):
        print("PARENT overried()")
    def implicit(self):
        print("PARENT implicit()")
    def alerted(self):
        print("PARENT altered()")
class Child(Parent):
    def override(self):
        print("CHILD override()")
    def altered(self):
        print("CHILD, BEFORE PARENT altered()")
        super(Child, self).alerted()
        print("CHILD, AFTER PARENT altered()")

dad = Parent()
son = Child()

dad.implicit()
son.implicit()

dad.override()
son.override()

dad.alerted()
son.alerted()