package de.fosd.typechef

/*
* temporarily copied from PreprocessorFrontend due to technical problems
*/


import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem._
import de.fosd.typechef.crewrite._
import de.fosd.typechef.featureexpr.FeatureModel
import lexer.options.OptionException
import java.io.{FileWriter, File}
import parser.c.TranslationUnit
import de.fosd.typechef.parser.TokenReader

object Frontend {

    private var storedAst: AST = null
    private var featureModel: FeatureModel = null

    def main(args: Array[String]) {
        // load options
        val opt = new FrontendOptionsWithConfigFiles()
        try {
            try {
                opt.parseOptions(args)
            } catch {
                case o: OptionException => if (!opt.isPrintVersion) throw o
            }

            if (opt.isPrintVersion) {
                var version = "development build"
                try {
                    val cl = Class.forName("de.fosd.typechef.Version")
                    version = "version " + cl.newInstance().asInstanceOf[VersionInfo].getVersion
                } catch {
                    case e: ClassNotFoundException =>
                }

                println("TypeChef " + version)
                return
            }
        } catch {
            case o: OptionException =>
                println("Invocation error: " + o.getMessage)
                println("use parameter --help for more information.")
                return
        }

        processFile(opt)
    }

    private class StopWatch {
        var lastStart: Long = 0
        var currentPeriod: String = "none"
        var times: Map[String, Long] = Map()

        private def measure(checkpoint: String) {
            times = times + (checkpoint -> System.currentTimeMillis())
        }

        def start(period: String) {
            val now = System.currentTimeMillis()
            val lastTime = now - lastStart
            times = times + (currentPeriod -> lastTime)
            lastStart = now
            currentPeriod = period
        }

        def get(period: String): Long = times.getOrElse(period, 0)

        override def toString = {
            var res = "timing "
            val switems = times.toList.filterNot(x => x._1 == "none" || x._1 == "done")

            if (switems.size > 0) {
                res = res + "("
                res = res + switems.map(_._1).reduce(_ + ", " + _)
                res = res + ")\n"
                res = res + switems.map(_._2.toString).reduce(_ + ";" + _)
            }
            res
        }
    }


    def processFile(opt: FrontendOptions) {
        val errorXML = new ErrorXML(opt.getErrorXMLFile)
        opt.setRenderParserError(errorXML.renderParserError)

        val stopWatch = new StopWatch()
        stopWatch.start("loadFM")

        val fm = opt.getFeatureModel.and(opt.getLocalFeatureModel).and(opt.getFilePresenceCondition)
        opt.setFeatureModel(fm) //otherwise the lexer does not get the updated feature model with file presence conditions
        /*
        // create dimacs file from feature model
        opt.getFeatureModelTypeSystem.asInstanceOf[SATFeatureModel].writeToDimacsFile(new File(
            "/tmp/BB_fm.dimacs"
        ))

        System.exit(0)
        */
        if (!opt.getFilePresenceCondition.isSatisfiable(fm)) {
            println("file has contradictory presence condition. existing.") //otherwise this can lead to strange parser errors, because True is satisfiable, but anything else isn't
            return
        }

        stopWatch.start("lexing")
        val in = lex(opt)


        if (opt.parse) {
            stopWatch.start("parsing")

            val parserMain = new ParserMain(new CParser(fm))
            val ast = parserMain.parserMain(in, opt)

            stopWatch.start("serialize")
            if (ast != null && opt.serializeAST)
                serializeAST(ast, opt.getSerializedASTFilename)

            if (ast != null) {
                storedAst = ast
                val fm_ts = opt.getFeatureModelTypeSystem.and(opt.getLocalFeatureModel).and(opt.getFilePresenceCondition)
                featureModel = fm_ts
                val ts = new CTypeSystemFrontend(ast.asInstanceOf[TranslationUnit], fm_ts)

                /** I did some experiments with the TypeChef FeatureModel of Linux, in case I need the routines again, they are saved here. */
                //Debug_FeatureModelExperiments.experiment(fm_ts)

                if (opt.typecheck || opt.writeInterface) {
                    //ProductGeneration.typecheckProducts(fm,fm_ts,ast,opt,
                    //logMessage=("Time for lexing(ms): " + (t2-t1) + "\nTime for parsing(ms): " + (t3-t2) + "\n"))
                    //ProductGeneration.estimateNumberOfVariants(ast, fm_ts)

                    stopWatch.start("typechecking")
                    println("type checking.")
                    ts.errors.map(errorXML.renderTypeError(_))
                }
                if (opt.writeInterface) {
                    stopWatch.start("interfaces")
                    val interface = ts.getInferredInterface().and(opt.getFilePresenceCondition)

                    stopWatch.start("writeInterfaces")
                    ts.writeInterface(interface, new File(opt.getInterfaceFilename))
                    if (opt.writeDebugInterface)
                        ts.debugInterface(interface, new File(opt.getDebugInterfaceFilename))
                }
                if (opt.dumpcfg) {
                    stopWatch.start("dumpCFG")

                    val cf = new CAnalysisFrontend(ast.asInstanceOf[TranslationUnit], fm_ts)
                    cf.dumpCFG()
                }
                if (opt.doublefree) {
                    stopWatch.start("doublefree")
                    val df = new CAnalysisFrontend(ast.asInstanceOf[TranslationUnit], fm_ts)
                    df.doubleFree()
                }
                if (opt.uninitializedmemory) {
                    stopWatch.start("uninitializedmemory")
                    val uv = new CAnalysisFrontend(ast.asInstanceOf[TranslationUnit], fm_ts)
                    uv.uninitializedMemory()
                }
                if (opt.xfree) {
                    stopWatch.start("xfree")
                    val xf = new CAnalysisFrontend(ast.asInstanceOf[TranslationUnit], fm_ts)
                    xf.xfree()
                }
                if (opt.danglingswitchcode) {
                    stopWatch.start("danglingswitchcode")
                    val ds = new CAnalysisFrontend(ast.asInstanceOf[TranslationUnit], fm_ts)
                    ds.danglingSwitchCode()
                }

            }

        }
        stopWatch.start("done")
        errorXML.write()
        if (opt.recordTiming)
            println(stopWatch)

    }


    def lex(opt: FrontendOptions): TokenReader[CToken, CTypeContext] = {
        val tokens = new lexer.Main().run(opt, opt.parse)
        val in = CLexer.prepareTokens(tokens)
        in
    }

    def serializeAST(ast: AST, filename: String) {
        val fw = new FileWriter(filename)
        fw.write(ast.toString)
        fw.close()
    }
    def getAST = storedAst
}
