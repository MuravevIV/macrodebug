package macrodebug

import org.junit.Test

class TraceMacroTest {

    import macrodebug.TraceMacro._

    @Test
    def apply(): Unit = {
        def plus(a: Int, b: Int) = a + b
        trace(plus(1, plus(2, plus(3, 4))))
    }
}
