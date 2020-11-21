package plugins {
  trait Pluginable {
    def plugin(s: String): String = s
  }

  class CompositePlugin(plugin1: Pluginable, plugin2: Pluginable) extends Pluginable {
    override def plugin(s: String): String = plugin2.plugin(plugin1.plugin(s))
  }

  trait Reverting extends Pluginable {
    override def plugin(s: String): String = super.plugin(s).reverse
  }

  trait LowerCasing extends Pluginable {
    override def plugin(s: String): String = super.plugin(s).toLowerCase
  }

  trait SingleSpacing extends Pluginable {
    override def plugin(s: String): String = "  +".r.replaceAllIn(super.plugin(s), " ")
  }

  trait NoSpacing extends Pluginable {
    override def plugin(s: String): String = super.plugin(s).replace(" ", "")
  }

  trait DuplicateRemoval extends Pluginable {
    override def plugin(s: String): String = {
      val ss = super.plugin(s)
      ss.filter(letter => ss.count(_ == letter) == 1)
    }
  }

  trait Rotating extends Pluginable {
    override def plugin(s: String): String = {
      val ss = super.plugin(s)
      s"${ss.last}${ss.dropRight(1)}"
    }
  }

  trait Doubling extends Pluginable {
    override def plugin(s: String): String = duplicate(super.plugin(s), false, "")

    def duplicate(ss: String, doCopy: Boolean, acc: String): String = {
      (ss, doCopy) match {
        case ("", _) => acc.reverse
        case (ss, false) => duplicate(ss.tail, true, s"${ss.head}$acc")
        case (ss, true) => duplicate(ss.tail, false, s"${ss.head}${ss.head}$acc")
      }
    }
  }

  trait Shortening extends Pluginable {
    override def plugin(s: String): String = shorten(super.plugin(s), false, "")

    def shorten(ss: String, doRemove: Boolean, acc: String): String = {
      (ss, doRemove) match {
        case ("", _) => acc.reverse
        case (ss, false) => shorten(ss.tail, true, s"${ss.head}$acc")
        case (ss, true) => shorten(ss.tail, false, s"$acc")
      }
    }
  }
}

object Actions {
  import plugins._

  val actionA = new Pluginable with SingleSpacing with Doubling with Shortening
  val actionB = new Pluginable with NoSpacing with Shortening with Doubling
  val actionC = new Pluginable with LowerCasing with Doubling
  val actionD = new Pluginable with DuplicateRemoval with Rotating
  val actionE = new Pluginable with NoSpacing with Shortening with Doubling with Reverting

  private val rotatingPlugin = new Pluginable with Rotating
  val actionF = new CompositePlugin(rotatingPlugin,
                                    new CompositePlugin(new CompositePlugin(rotatingPlugin, rotatingPlugin),
                                                        new CompositePlugin(rotatingPlugin, rotatingPlugin)))
  val actionG = new CompositePlugin(actionA, actionB)
}

object Lab05 {
  def main(args: Array[String]) = {
    import plugins._

    println(Actions.actionA.plugin("a   sEnTENce   fULl of     SPAces"))
    println(Actions.actionB.plugin("a   sEnTENce   fULl of     SPAces"))
    println(Actions.actionC.plugin("quiCK brOwn FOX"))
    println(Actions.actionD.plugin("abcdefghabcd"))
    println(Actions.actionE.plugin("no   spacES, nooo   spaces   at   all"))
    println(Actions.actionF.plugin("123456789"))
    println(Actions.actionG.plugin("ABC   def   jk o"))
  }
}
