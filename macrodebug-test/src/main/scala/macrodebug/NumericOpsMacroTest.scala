package macrodebug

import org.junit.Test

class NumericOpsMacroTest {

    import macrodebug.NumericOpsMacro._

    @Test
    def apply(): Unit = {
        object A { def foo[T: Numeric](t: T) = (-t * t).abs }
    }
}
