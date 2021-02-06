package models

case class Student(index: Int, name: String, year: Short)

case class Lecture(id: String, name: String)

case class Enrollment(id: String, students: List[Student])
