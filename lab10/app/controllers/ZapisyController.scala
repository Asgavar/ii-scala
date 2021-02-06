package controllers

import javax.inject._
import play.api._
import play.api.mvc._

@Singleton
class ZapisyController @Inject()(val controllerComponents: ControllerComponents,
                               val repository: models.ZapisyRepository) extends BaseController {

  def listStudents() = Action {
    Ok(views.html.students(repository.getAllStudents()))
  }

  def retrieveStudent(index: Int) = Action {
    repository.getStudent(index) match {
      case Some(student) => Ok(views.html.student(student))
      case None => NotFound("no such student")
    }
  }

  def listLectures() = Action {
    Ok(views.html.lectures(repository.getAllLectures()))
  }

  def retrieveLecture(id: String) = Action {
    repository.getLecture(id) match {
      case Some(lecture) => Ok(
        views.html.lecture(lecture, repository.getEnrollment(id).getOrElse(models.Enrollment(id, Nil)).students))
      case None => NotFound("no such lecture")
    }
  }
}
