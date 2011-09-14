package de.fosd.typechef.typesystem.linker

import org.junit._
import de.fosd.typechef.parser.c.{TestHelper, TranslationUnit}
import java.io.{File, InputStream, FileNotFoundException}

class InterfaceInferenceTest extends TestHelper {

    val folder = "testfiles/"
    private def parse(filename: String): TranslationUnit = {
        val start = System.currentTimeMillis
        println("parsing " + filename)
        var inputStream: InputStream = getClass.getResourceAsStream("/" + folder + filename)
        if (inputStream == null) {
            throw new FileNotFoundException("Input file not found: " + filename)
        }
        val ast = parseFile(inputStream, filename, folder)
        val parsed = System.currentTimeMillis
        println("parsed " + filename + " (" + (parsed - start) + ")")
        ast
    }


    private def checkSerialization(i: CInterface) {
        val inf = new CInferInterface {}
        val f = new File("tmp.interface")
        inf.writeInterface(i, f)
        val interface2 = inf.readInterface(f)


        //
        assert(i equals interface2)
        assert(!(i eq interface2))
    }


    @Test
    def testMini {
        val ast = parse("mini.pi")
        val interface = new CInferInterface {}.inferInterface(ast)
        println(interface)
        checkSerialization(interface)

        //find imported function
        assert(interface.imports.exists(_.name == "bar"))
        assert(interface.imports.exists(_.name == "printf"))
        assert(interface.imports.exists(_.name == "local"))
        //foo declared but not used, should not show up in interface
        assert(!interface.imports.exists(_.name == "foo"))
        assert(!interface.imports.exists(_.name == "unusedlocal"))
        //local variables should not show up in interfaces
        assert(!interface.imports.exists(_.name == "a"))
        //main should be exported
        assert(interface.exports.exists(_.name == "main"))
        assert(interface.exports.exists(_.name == "foobar"))
        assert(interface.exports.exists(_.name == "partialA"))
        assert(interface.exports.exists(_.name == "partialB"))
        //exported methods should not be imported
        assert(!interface.imports.exists(_.name == "main"))
        assert(!interface.imports.exists(_.name == "foobar"))
        assert(!interface.imports.exists(_.name == "partialB"))
        assert(interface.imports.exists(_.name == "partialA"))
        //local variables should not be exported
        assert(!interface.exports.exists(_.name == "a"))
    }

    @Test
    def testBoa {
        val ast = parse("boa.pi")
        val interface = new CInferInterface {}.inferInterface(ast)
        println(interface)
        checkSerialization(interface)
    }

    @Test
    def testAr {
        val ast = parse("ar.pi")
        val interface = new CInferInterface {}.inferInterface(ast)
        println(interface)
        checkSerialization(interface)
    }

    //       @Test
    //    def testFork {
    //        val ast = parse("fork_.pi")
    //        val interface = new CInferInterface {}.inferInterface(ast)
    //        println(interface)
    //    }
}