package macrodebug

import org.junit.Test

class LogMacroTest {

    import macrodebug.LogMacro._

    @Test
    def apply(): Unit = {
        log("".isEmpty)
    }
}
