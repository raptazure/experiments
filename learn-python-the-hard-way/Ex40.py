# ex40 Modules, Classes, and Objects

# Think about a module as a specialized dictionary that can store Python code
# so you can access it with the . operator. Python also has another construct 
# that serves a similar purpose called a class. A class is a way to take a a
# grouping of functions and data and place them inside a container so you can
# access them with the dot operator

class Song(object):
    
    def __init__(self, lyrics):
        self.lyrics = lyrics
    
    def sing_me_a_song(self):
        for line in self.lyrics:
            print(line)

happy_bday = Song(["Happy birthday to you",
                "I don't want to get sued",
                "So I'll stop right there"])

bulls_on_parade = Song(["They rally around tha family",
                        "With pockets full of shells"])


happy_bday.sing_me_a_song()
bulls_on_parade.sing_me_a_song()