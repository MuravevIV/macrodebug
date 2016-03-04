package macrodebug

import org.junit.Test

class RegexMacroTest {

    import macrodebug.RegexMacro._

    @Test
    def apply(): Unit = {
        regex(".*")
    }
}
