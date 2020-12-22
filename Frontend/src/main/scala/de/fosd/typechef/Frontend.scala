package de.fosd.typechef


import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem._
import de.fosd.typechef.crewrite._
import java.io._
import java.nio.file._ // for Files, Path
import parser.TokenReader
import de.fosd.typechef.options.{FrontendOptionsWithConfigFiles, FrontendOptions, OptionException}
import de.fosd.typechef.parser.c.CTypeContext
import de.fosd.typechef.parser.c.TranslationUnit
import de.fosd.typechef.featureexpr.FeatureExpr
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.io.Source

import scala.sys.process.Process
import scala.collection.immutable.HashMap

import de.fosd.typechef.featureexpr.sat.VSATMode
import de.fosd.typechef.featureexpr.sat.VSATMissionControl

object Frontend extends EnforceTreeHelper {

    def main(args: Array[String]) {
        // [VSAT] By inspecting jcpp.sh, we found that the current file will always be the last argument.
        VSATMissionControl.setSessionFile(args(args.length - 1));
        VSATMissionControl.init();
//      vsat_clean_env()
      //vsat_initialise_cache_file()
        // load options
        VSATMissionControl.setCurrentMode(VSATMode.ArgParse);
        val opt = new FrontendOptionsWithConfigFiles()
        try {
            try {
                opt.parseOptions(args)
            } catch {
                case o: OptionException => if (!opt.isPrintVersion) throw o
            }

            if (opt.isPrintVersion) {
                println("TypeChef " + getVersion)
                return
            }
            if (opt.isPrintIncludes)
                opt.printInclude()
        }

        catch {
            case o: OptionException =>
                println("Invocation error: " + o.getMessage)
                println("use parameter --help for more information.")
                return
        }
        VSATMissionControl.cleanCurrentMode();
        processFile(opt)
        VSATMissionControl.terminate();
    }


    def getVersion: String = {
        var version = "development build"
        try {
            val cl = Class.forName("de.fosd.typechef.Version")
            version = "version " + cl.newInstance().asInstanceOf[VersionInfo].getVersion
        } catch {
            case e: ClassNotFoundException =>
        }
        version
    }

    private class StopWatch {
        var lastStart: Long = 0
        var currentPeriod: String = "none"
        var currentPeriodId: Int = 0
        var times: Map[(Int, String), Long] = Map()

        private def genId(): Int = { currentPeriodId += 1; currentPeriodId }

        private def measure(checkpoint: String) {
            times = times + ((genId(), checkpoint) -> System.currentTimeMillis())
        }

        def start(period: String) {
            val now = System.currentTimeMillis()
            val lastTime = now - lastStart
            times = times + ((genId(), currentPeriod) -> lastTime)
            lastStart = now
            currentPeriod = period
        }

        def get(period: String): Long = times.filter(v => v._1._2 == period).headOption.map(_._2).getOrElse(0)

        override def toString = {
            var res = "timing "
            val switems = times.toList.filterNot(x => x._1._2 == "none" || x._1._2 == "done").sortBy(_._1._1)

            if (switems.size > 0) {
                res = res + "("
                res = res + switems.map(_._1._2).reduce(_ + ", " + _)
                res = res + ")\n"
                res = res + switems.map(_._2.toString).reduce(_ + ";" + _)
            }
            res
        }
    }
////////////////////////////////////////////////////////////////////////////////
///////////////////////////// VSAT Helpers /////////////////////////////////////
//// Abandon all hope, ye who enter here
//// Logic files because we never figured out env variables:
  // VSAT_MODE -- ^ stores a string which captures the mode or phase of the
  // variational analysis that the sat query was recorded in

  // FEAT_MODEL_CNTR -- ^ stores the integer which serves as a UUID for a given
  // feature model. We use this to create subdirectories that associate a
  // feature model to the queries which were produced with that FM

  // VSAT_ENV -- ^ stores the current path that all the queries should be dumped
  // into

  ////////////////////////// VSAT Mode //////////////////////////////////////
//  def vsat_set_mode(mode: String) : Unit = {
//      println("[VSAT] Set mode to " + mode)
//    val mode_file_name = "./VSAT_MODE"
//    val output         = new BufferedWriter(new FileWriter(mode_file_name, false))
//    output.write(mode)
//    output.close()
//  }

//  def vsat_clean_mode() : Unit = {
//    vsat_set_mode("NO_MODE")
//  }

//  def vsat_initialise_cache_file() {
//    val fmPath = "VSAT_CACHE_HITS.txt"
//    val fmOut = new BufferedWriter(new FileWriter(fmPath,false))
//    val headers = "problem,count\n"
//    fmOut.write(headers)
//    fmOut.close()
//  }

  ////////////////////////// VSAT FM Counter ////////////////////////////////
//  def vsat_get_fm_cntr() : Integer = {
//    val fpath = "./FEAT_MODEL_CNTR"
//    val file = new File(fpath)
//    val file_not_present = !file.exists()
//
//    // then make it
//    if (file_not_present) {
//      val output = new BufferedWriter(new FileWriter(fpath, false))
//      output.write("0") // initialize to 0, this makes me cringe
//      output.close()
//    }
//
//    val src  = Source.fromFile(fpath)
//    val cntr = src.getLines.take(1).toList.head.toInt
//    src.close()
//    cntr
//  }
//
//  // does scala have lenses or profunctors?
//  def vsat_on_fm_cntr(f : Integer => Integer) {
//    val i      = vsat_get_fm_cntr()
//    val fPath  = "./FEAT_MODEL_CNTR"
//    val output = new BufferedWriter(new FileWriter(fPath, false))
//    output.write(f(i).toString)
//    output.close()
//  }
//
//  def vsat_set_fm_cntr(i: Integer) {
//    vsat_on_fm_cntr((_:Any) => i)
//  }
//
//  def vsat_increment_fm_cntr() {
//    vsat_on_fm_cntr(_ + 1)
//  }
//
//  ////////////////////////// VSAT Environment ////////////////////////////////
//  // the feature models that have been observed thus far
//  type ObservedFMs = HashMap[String, Integer]
//  type FeatModelID      = Integer


  /////////////////////////// VSAT Queries ///////////////////////////////////
//  def vsat_initialise_new_dir() {
//    val fPath = "./VSAT_sat_queries/"
//    val fm_counter = vsat_get_fm_cntr()
//
//    // create any directories on the path
//    Files.createDirectories(Paths.get(fPath + fm_counter))
//  }

//  def vsat_initialise_plain_dir() {
//    val fPath = "./VSAT_sat_queries/"
//
//    // create any directories on the path
//    Files.createDirectories(Paths.get(fPath + "plain"))
//  }

//  def vsat_make_query_path(id: FeatModelID) : String = {
//    "./VSAT_sat_queries/" + id + "/"
//  }

  // check if the feature model is novel, if so then add it to the observed
  // feature models and increment the UUID counter
//  def vsat_update_with_fm(observed: ObservedFMs, fm: String) : FeatModelID = {
//    if (observed.contains(fm)) { // then we have seen the feature model before
//      observed(fm)               // then get the ID and return it
//    } else {                     // new feature model
//      // update observed feature models
//      val cntr = vsat_get_fm_cntr()
//      observed + (fm -> cntr)
//
//      // create the sub dir for the queries
//      vsat_initialise_new_dir()
//
//      // return
//      vsat_increment_fm_cntr()
//      cntr
//    }
//  }

//  def vsat_get_env() : String = {
//    val fpath = "./VSAT_ENV"
//    val file_not_present = !(Files.isRegularFile(Paths.get(fpath)))
//    val go = () => Source.fromFile(fpath).getLines.mkString
//
//    // make the file if it doesn't exist
//    if (file_not_present) {
//      val file = new File(fpath)
//      file.createNewFile()
//    }
//
//    // let's go, one wonders what continuation passing style in scala is like
//    go()
//  }

//  def vsat_on_env(f : String => String) {
//    val e      = vsat_get_env()
//    val fPath  = "./VSAT_ENV"
//    val output = new BufferedWriter(new FileWriter(fPath, false))
//    output.write(f(e))
//    output.close()
//  }

  // set the VSAT_ENV file variable, this variable holds the appropriate path to
  // dispatch sat queries for a particular feature model
//  def vsat_set_env(new_env: String) {
//    val env_fname = "./VSAT_ENV"
//    vsat_on_env((_:Any) => new_env)
//  }

//  def vsat_clean_env(){
//    vsat_set_env("VSAT_sat_queries/plain/")
//    vsat_initialise_plain_dir
//  }

///////////////////////////// End Helpers //////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

    def processFile(opt: FrontendOptions) {

        // [VSAT]: Added to track feature models and dispatch feature models to
        // the queries there associated sat queries
//        val observed = new ObservedFMs()

        val errorXML = new ErrorXML(opt.getErrorXMLFile)
        opt.setRenderParserError(errorXML.renderParserError)

        val stopWatch = new StopWatch()
        VSATMissionControl.setCurrentMode(VSATMode.LoadFM);
        stopWatch.start("loadFM")

 // println("[VSAT]: Feature Model Stuff!!!!! ")
 // vsat_set_mode("FEATURE_MODEL") // [VSAT]: Set env variable to indicate parsing queries

        val smallFM = opt.getSmallFeatureModel().and(opt.getLocalFeatureModel).and(opt.getFilePresenceCondition)
        opt.setSmallFeatureModel(smallFM) //otherwise the lexer does not get the updated feature model with file presence conditions
        val fullFM = opt.getFullFeatureModel.and(opt.getLocalFeatureModel).and(opt.getFilePresenceCondition)
        opt.setFullFeatureModel(fullFM) // should probably be fixed in how options are read

        VSATMissionControl.cleanCurrentMode();


 // have to wrap this in a val because scala can't do ((f .) . g) apparently?
 // val f = vsat_update_with_fm(observed,_:String)

 // read left to right, we do:

  // f:
  // with string version of fm, check if we've seen it, if not update the
  // observed list

  // vsat_make_query_path:
  // construct a string to represent the path the queries should be stored in

  // vsat_set_env:
  // finally set VSAT_ENV to the path so that the solver can log the queries
  // correctly

 // [VSAT]: Jeff: I'm not sure if we need to log small??? these seem to be empty always
 // [VSAT]: Jeff: If you enable the smalls then the odd numbers will be fullFMs fyi
 // (f andThen vsat_make_query_path andThen vsat_set_env)(smallFM.toString)
 // (f andThen vsat_make_query_path andThen vsat_set_env)(fullFM.toString)

        if (!opt.getFilePresenceCondition.isSatisfiable(fullFM)) {
            println("file has contradictory presence condition. existing.") //otherwise this can lead to strange parser errors, because True is satisfiable, but anything else isn't
            return
        }


        var ast: TranslationUnit = null
        if (opt.reuseAST && opt.parse && new File(opt.getSerializedASTFilename).exists()) {
            VSATMissionControl.setCurrentMode(VSATMode.LoadAST);
            println("loading AST.")
            try {
            ast = loadSerializedAST(opt.getSerializedASTFilename)
            ast = prepareAST[TranslationUnit](ast)
            } catch {
                case e: Throwable => println(e.toString);e.printStackTrace(); ast=null
            }
            if (ast == null)
                println("... failed reading AST\n")
            VSATMissionControl.cleanCurrentMode();
        }

// [VSAT]: Jeff and Paul: log the feature models from the options
// fullFM.exportFM2DNF(fullFM, "FullFeatureModels.txt")
// smallFM.exportFM2DNF(smallFM, "SmallFeatureModels.txt")


        VSATMissionControl.setCurrentMode(VSATMode.Lexing);
        stopWatch.start("lexing")
        //no parsing if read serialized ast
        val in = if (ast == null) {
            println("#lexing")
            lex(opt)
        } else null
        VSATMissionControl.cleanCurrentMode();

        if (opt.parse) {
            VSATMissionControl.setCurrentMode(VSATMode.Parsing);
            println("#parsing")
            stopWatch.start("parsing")

          // [VSAT]: Jeff and Paul: Parsing begins after lexing of course. Here
          // is the block where if the parsing is successful then it moves to
          // type checking
            if (ast == null) {
                //no parsing and serialization if read serialized ast
                val parserMain = new ParserMain(new CParser(smallFM))
                ast = parserMain.parserMain(in, opt, fullFM)
                ast = prepareAST[TranslationUnit](ast)
                // checkPositionInformation(ast)

                if (ast != null && opt.serializeAST) {
                    VSATMissionControl.setCurrentMode(VSATMode.Serialize);
                    stopWatch.start("serialize")
                    serializeAST(ast, opt.getSerializedASTFilename)
                }

            }

//  vsat_clean_mode()
            VSATMissionControl.cleanCurrentMode();
          // [VSAT]: Jeff and Paul: Parsing ends
            if (ast != null) {

              // [VSAT]: Jeff and Paul: Typechecking begins
                // some dataflow analyses require typing information
                VSATMissionControl.setCurrentMode(VSATMode.TypeSystemInit);
                val ts = if (opt.typechecksa)
                            new CTypeSystemFrontend(ast, fullFM, opt) with CTypeCache with CDeclUse
                         else
                            new CTypeSystemFrontend(ast, fullFM, opt)
                VSATMissionControl.cleanCurrentMode();


                /** I did some experiments with the TypeChef FeatureModel of Linux, in case I need the routines again, they are saved here. */
                //Debug_FeatureModelExperiments.experiment(fm_ts)

                if (opt.typecheck || opt.writeInterface || opt.typechecksa) {
                    //ProductGeneration.typecheckProducts(fm,fm_ts,ast,opt,
                    //logMessage=("Time for lexing(ms): " + (t2-t1) + "\nTime for parsing(ms): " + (t3-t2) + "\n"))
                    //ProductGeneration.estimateNumberOfVariants(ast, fm_ts)

                    VSATMissionControl.setCurrentMode(VSATMode.TypeChecking);
                    stopWatch.start("typechecking")
                    println("#type checking")
                    ts.checkAST(printResults = true)
                    ts.errors.map(errorXML.renderTypeError)
                }
                VSATMissionControl.cleanCurrentMode();
                if (opt.writeInterface) {
                    VSATMissionControl.setCurrentMode(VSATMode.Interfaces);
                    stopWatch.start("interfaces")
                    val interface = ts.getInferredInterface().and(opt.getFilePresenceCondition)

                    VSATMissionControl.setCurrentMode(VSATMode.WriteInterfaces);
                    stopWatch.start("writeInterfaces")
                    ts.writeInterface(interface, new File(opt.getInterfaceFilename))
                    if (opt.writeDebugInterface)
                        ts.debugInterface(interface, new File(opt.getDebugInterfaceFilename))
                }
                VSATMissionControl.cleanCurrentMode();
                if (opt.dumpcfg) {
                    VSATMissionControl.setCurrentMode(VSATMode.CallGraph);
                    println("#call graph")
                    stopWatch.start("dumpCFG")

                    //run without feature model, because otherwise too expensive runtimes in systems such as linux
                    val cf = new CInterAnalysisFrontend(ast/*, fm_ts*/)
                    val writer = new CFGCSVWriter(new FileWriter(new File(opt.getCCFGFilename)))
                    val dotwriter = new DotGraph(new FileWriter(new File(opt.getCCFGDotFilename)))
                    cf.writeCFG(opt.getFile, new ComposedWriter(List(dotwriter, writer)))
                }
                VSATMissionControl.cleanCurrentMode();

              // [VSAT]: Jeff and Paul: Static Analysis begins
                if (opt.staticanalyses) {
                    VSATMissionControl.setCurrentMode(VSATMode.StaticAnalysis);
                    println("#static analysis")
                    val sa = new CIntraAnalysisFrontend(ast, ts.asInstanceOf[CTypeSystemFrontend with CTypeCache with CDeclUse], fullFM)
                    if (opt.warning_double_free) {
                        stopWatch.start("doublefree")
                        sa.doubleFree()
                    }
                    if (opt.warning_uninitialized_memory) {
                        stopWatch.start("uninitializedmemory")
                        sa.uninitializedMemory()
                    }
                    if (opt.warning_case_termination) {
                        stopWatch.start("casetermination")
                        sa.caseTermination()
                    }
                    if (opt.warning_xfree) {
                        stopWatch.start("xfree")
                        sa.xfree()
                    }
                    if (opt.warning_dangling_switch_code) {
                        stopWatch.start("danglingswitchcode")
                        sa.danglingSwitchCode()
                    }
                    if (opt.warning_cfg_in_non_void_func) {
                        stopWatch.start("cfginnonvoidfunc")
                        sa.cfgInNonVoidFunc()
                    }
                    if (opt.warning_stdlib_func_return) {
                        stopWatch.start("checkstdlibfuncreturn")
                        sa.stdLibFuncReturn()
                    }
                    if (opt.warning_dead_store) {
                        stopWatch.start("deadstore")
                        sa.deadStore()
                    }
                }
                VSATMissionControl.cleanCurrentMode();
            }

        }
        stopWatch.start("done")
        errorXML.write()
        if (opt.recordTiming)
            println(stopWatch)

    }


    def lex(opt: FrontendOptions): TokenReader[CToken, CTypeContext] = {
        val tokens = new lexer.LexerFrontend().run(opt, opt.parse)
        val in = CLexerAdapter.prepareTokens(tokens)
        in
    }

    def serializeAST(ast: AST, filename: String) {
        val fw = new ObjectOutputStream(new GZIPOutputStream(new FileOutputStream(filename)))
        fw.writeObject(ast)
        fw.close()
    }

    def loadSerializedAST(filename: String): TranslationUnit = try {
        val fr = new ObjectInputStream(new GZIPInputStream(new FileInputStream(filename))) {
            override protected def resolveClass(desc: ObjectStreamClass) = { /*println(desc);*/ super.resolveClass(desc) }
        }
        val ast = fr.readObject().asInstanceOf[TranslationUnit]
        fr.close()
        ast
    } catch {
        case e:ObjectStreamException => System.err.println("failed loading serialized AST: "+e.getMessage); null
    }
}
