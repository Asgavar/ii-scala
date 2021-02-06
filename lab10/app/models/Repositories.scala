package models

import com.google.inject.ImplementedBy

@ImplementedBy(classOf[HardcodedZapisyRepository])
trait ZapisyRepository {
  def getAllStudents(): List[Student]
  def getStudent(index: Int): Option[Student]
  def getAllLectures(): List[Lecture]
  def getLecture(id: String): Option[Lecture]
  def getEnrollment(id: String): Option[Enrollment]
}

class HardcodedZapisyRepository extends ZapisyRepository {
  def getAllStudents(): List[Student] = DB.students.values.toList
  def getStudent(index: Int): Option[Student] = DB.students get index
  def getAllLectures(): List[Lecture] = DB.lectures.values.toList
  def getLecture(id: String): Option[Lecture] = DB.lectures get id
  def getEnrollment(id: String): Option[Enrollment] = DB.enrollments get id
}
