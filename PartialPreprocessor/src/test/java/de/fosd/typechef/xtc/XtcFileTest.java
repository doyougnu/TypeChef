package de.fosd.typechef.xtc;

import de.fosd.typechef.jcpp.AbstractCheckTests;
import de.fosd.typechef.lexer.LexerException;
import org.junit.Ignore;
import org.junit.Test;

import java.io.IOException;

//TODO write a proper differential testing infrastructure with gcc/cpp

/**
 * equivalent tests to JcppFileTest, using the same infrastructure
 */
public class XtcFileTest extends AbstractCheckTests {

    protected boolean useXtc() { return true; }

    @Test
    public void testNestingDead() throws LexerException, IOException {
        testFile("nestingdead.c");
    }

    @Test
    public void testDeadElse() throws LexerException, IOException {
        testFile("deadelse.h");
    }

    @Test
    public void testIncludeGuard() throws LexerException, IOException {
        testFile("in1.c");
    }

    @Test
    public void testUnlikely() throws LexerException, IOException {
        testFile("unlikely.h");
    }

    @Test
    public void testByteOrder() throws LexerException, IOException {
        testFile("byteorder.h");
    }


    @Test@Ignore("xtc bug!?")
    public void testAlternativeMacros() throws LexerException, IOException {
        testFile("macro2.c", true);
    }

    @Test
    public void testIncludeGuards() throws LexerException, IOException {
        testFile("includeguards.c");
    }

    @Test
    public void testIncludeGuards2() throws LexerException, IOException {
        testFile("includeguards2.h");
    }

    @Test@Ignore("not supposed to work")
    public void testDefDefined() throws LexerException, IOException {
        testFile("defdefined.c", true);
    }

    @Test
    public void testAlternativeDef() throws LexerException, IOException {
        testFile("alternativedef.c");
    }

    @Test
    public void testHiddenBaseAndDead() throws LexerException, IOException {
        testFile("hiddenDeadAndBase.c");
    }

//	@Test
//	@Ignore
//	public void testMultiInclude() throws LexerException, IOException {
//		// XXX this is not supported right now. let's see whether we will need
//		// it.
//		testFile("multiinclude.c");
//	}

    @Test
    public void testIfCondition() throws LexerException, IOException {
        testFile("ifcondition.c");
    }

    @Test
    public void testBeispielJoerg() throws LexerException, IOException {
        testFile("beispielJoerg.c");
    }

    @Test
    public void testNumericIfAlternative() throws LexerException, IOException {
        testFile("ifdefnumeric.c");
    }

    @Test
    public void testLinuxTestFLock() throws LexerException, IOException {
        testFile("linuxtestflock.c");
    }

    @Test
    public void testElIfChain() throws LexerException, IOException {
        testFile("elifchain.c");
    }

    @Test@Ignore("Termination bug in Xtc!?")
    public void testSelfDef() throws LexerException, IOException {
        testFile("selfdef.c");
    }

    @Test
    public void testNonTautologicExpansions() throws LexerException,
            IOException {
        testFile("non_tautologic.c");
    }

    @Test@Ignore("Whitespace bug in Xtc!?")
    public void testVariadic() throws LexerException, IOException {
        testFile("variadic.c",true);
    }

    @Test
    public void testIncompMacroExp() throws LexerException, IOException {
        testFile("incompatibleMacroExp.c");
    }

    @Test
    public void testRedef() throws LexerException, IOException {
        testFile("redef.h");
    }

    //jiffies contains complex calculations; from the linux kernel headers
    @Test
    public void testJiffies() throws LexerException, IOException {
        testFile("jiffiesTest.h");
    }

    @Test
    public void testIncludeMacros() throws LexerException, IOException {
        testFile("includemacro.c");
    }

    @Test
    public void testRecursiveMacro() throws LexerException, IOException {
        testFile("recursivemacro.h");
    }

    @Test
    public void testStringifyNl() throws LexerException, IOException {
        testFile("stringifyNl.c");
    }

    @Test
    public void testUseCondDef() throws LexerException, IOException {
        testFile("useconddef.c");
    }

    @Test
    public void testDivByZero() throws LexerException, IOException {
        testFile("test_div_by_zero.c");
    }

    @Test@Ignore("Evaluation order bug in Xtc")
    public void testDivByZero2() throws LexerException, IOException {
        testFile("test_div_by_zero2.c");
    }

    @Test
    public void testDivByZero3() throws LexerException, IOException {
        testFile("test_div_by_zero3.c");
    }

    @Test
    public void testDivByZero4() throws LexerException, IOException {
        testFile("test_div_by_zero4.c",true);
    }


    @Test
    public void testMacroPNF() throws LexerException, IOException {
        testFile("macroPFN.c");
    }

    @Test
    public void testParametricMacro() throws LexerException, IOException {
        testFile("parametricmacro.h");
    }

    @Test
    public void testParametricMacro2() throws LexerException, IOException {
        testFile("parametricmacro2.h");
    }

    @Test
    public void testKBuildStr() throws LexerException, IOException {
        testFile("kbuildstr.c");
    }

    @Test
    public void testStringify() throws LexerException, IOException {
        testFile("stringify.c");
    }

    @Test@Ignore("Whitespace bug in Xtc!?")
    public void testAlternativeDifferentArities1() throws LexerException, IOException {
        testFile("alternDiffArities1.c",true,true);
    }

    @Test
    public void testAlternativeDifferentArities2() throws LexerException, IOException {
        testFile("alternDiffArities2.c",true);
    }

    @Test
    public void testDateTime() throws LexerException, IOException {
        testFile("dateTime.c");
    }

    @Test
    public void testNumbers() throws LexerException, IOException {
        testFile("numbers.c");
    }

    @Test
    public void testConcatVarargs() throws LexerException, IOException {
        testFile("concatVarargs.c");
    }

    @Test
    public void testDeadcomparison() throws LexerException, IOException {
        testFile("deadcomparison.c");
    }

    @Test
    public void testExpandWithinExpand() throws LexerException, IOException {
        testFile("expandWithinExpand.c", false, true);
    }

    @Test
    @Ignore //TODO fix
    public void testLinebreaks() throws LexerException, IOException {
        testFile("linebreaks.c", false, true);
    }

    //TODO investigate, lexer crashes
    @Test@Ignore("Investigate: Lexer crashes")
    public void testLinebreaks2() throws LexerException, IOException {
        testFile("linebreaks2.c", false, true);
    }

    @Test
    public void testFileBaseFile() throws LexerException, IOException {
        testFile("filebasefile.c", false, true);
    }

    @Test
    public void testBnx2() throws LexerException, IOException {
        testFile("bnx2.c", false, true);
    }

    @Test
    @Ignore("bug in lexer, see issue #10")
    public void testBnx() throws LexerException, IOException {
        testFile("bnx.c", false, true);
    }

    @Test
    public void testVarargs() throws LexerException, IOException {
        testFile("varargs.c", false, true);
    }

    @Test
    public void testConditionalError() throws LexerException, IOException {
        testFile("conditionalerror.c");
    }


    @Test
    public void testSimpleCompare() throws LexerException, IOException {
        testFile("simplecompare.c");
    }

}