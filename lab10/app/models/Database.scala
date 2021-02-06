package models

object DB {
  val students = Map(
    299970 -> Student(299970, "Artur Juraszek", 2017),
    111111 -> Student(111111, "Jane Doe", 2000),
    222222 -> Student(222222, "John Kovalsky", 1999),
    333333 -> Student(333333, "Functional Programming Enjoyer", 2020),
    444444 -> Student(444444, "A Guy Who Rather Stays With C", 1990),
    555555 -> Student(555555, "Kernels Design Appreciator", 2005),
    666666 -> Student(666666, "Hardcore GCC Hacker", 1985),
    777777 -> Student(777777, "Lifetime Fan Of OCaml", 2014),
    888888 -> Student(888888, "Undecided Newcomer", 2021),
    999999 -> Student(999999, "Unlucky Lad Who Missed His Enrollment Time", 2018)
  )

  val lectures = Map(
    "sip-1234" -> Lecture("sip-1234", "Scala in Practice"),
    "os-5678" -> Lecture("os-5678", "Operating Systems"),
    "cc-9876543210" -> Lecture("cc-9876543210", "Compilers Construction"),
    "aisd-1337" -> Lecture("aisd-1337", "Algorithms and Data Structures")
  )

  val enrollments = Map(
    "sip-1234" -> Enrollment("sip-1234", students.values.toList),
    "os-5678" -> Enrollment("sip-1234", List(students(111111), students(444444), students(555555))),
    "cc-9876543210" -> Enrollment("cc-9876543210", List(students(111111), students(222222), students(333333), students(666666), students(777777)))
  )
}
