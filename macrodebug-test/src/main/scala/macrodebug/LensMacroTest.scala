package macrodebug

import org.junit.Test

class LensMacroTest {

    import macrodebug.LensMacro._

    @Test
    def apply(): Unit = {
        case class Person(name: String, age: Int)
        val nameLens = lens[Person].name
    }
}
