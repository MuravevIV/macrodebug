package macrodebug

import org.junit.Test

class DesugarMacroTest {

    import macrodebug.DesugarMacro._

    @Test
    def apply(): Unit = {
        desugar("foo".map(_ + 1))
    }
}
