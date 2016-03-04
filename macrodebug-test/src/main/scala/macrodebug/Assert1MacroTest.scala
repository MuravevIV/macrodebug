package macrodebug

import org.junit.Test

class Assert1MacroTest {

    import macrodebug.Assert1Macro._

    @Test(expected = classOf[AssertionError])
    def apply(): Unit = {
        assert1("foo".reverse == "off")
    }
}
