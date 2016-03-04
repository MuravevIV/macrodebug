package macrodebug

import org.junit.Test

//noinspection EmptyParenMethodAccessedAsParameterless
class RichStringMacroTest {

    import macrodebug.RichStringMacro._

    @Test
    def apply(): Unit = {
        b"101010"
    }
}
