class Student:
    bloodgroup = "A+"
    def __init__(self, age, marks):
        self.age = age
        self.marks = marks

    # GETTER FUNCTIONS
    def getAge(self):
        return self.age
    def getMarks(self):
        return self.marks
    

    # SETTER FUNCTIONS
    def setMarks(self, marks):
        self.marks = marks
    def setAge(self, age):
        self.age = age


    def __str__(self):
        return f"The students age is {self.age}. The students marks is {self.marks}."

    def __del__(self):
        print("hahahahaah")

avni = Student(18, 99)
vivaan = Student(18, 99)
print(avni.__str__())
print(vivaan)
print(avni.__dict__)
del avni
try:
    print(avni)
except Exception:
    print("not found :)")