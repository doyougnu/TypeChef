package de.fosd.typechef.featureexpr.sat

import org.sat4j.core.VecInt

import collection.mutable.WeakHashMap
import org.sat4j.specs.{ContradictionException, IConstr}
import org.sat4j.minisat.SolverFactory

import scala.Predef._
import java.io._
import java.nio.file.{Files, Paths}

import scala.collection.immutable.HashMap
import scala.collection.mutable.{HashMap => MHashMap}
import scala.io.Source


/**
 * connection to the SAT4j SAT solver
 *
 * may reuse instance
 */


class SatSolver {
  // [VSAT] Caching: Hey Jeff look at this. Some caching is disabled by the TypeChef devs.
  /**
   * caching can reuse SAT solver instances, but experience
   * has shown that it can lead to incorrect results,
   * hence caching is currently disabled
   */
  val CACHING = false


  val output = new BufferedWriter(new FileWriter("SatSolverDebugLog.txt", true))


  def isSatisfiable(exprCNF: SATFeatureExpr, featureModel: SATFeatureModel = SATNoFeatureModel): Boolean = {
    // output.write("[Debug::SatSolver::isSatisfiable]: Do we have fm? ==> " + (nfm(featureModel) != SATNoFeatureModel) + "\n")
    // output.write("[Debug::SatSolver::isSatisfiable]: Will we have a cache hit? ==> " + (CACHING && (nfm(featureModel) != SATNoFeatureModel)) + "\n")
    // output.flush()
    (if (CACHING && (nfm(featureModel) != SATNoFeatureModel))
      SatSolverCache.get(nfm(featureModel))
    else
      new SatSolverImpl(nfm(featureModel),false)).isSatisfiable(exprCNF)
  }

  /**
   * Basically a clone of isSatisfiable(..) that also returns the satisfying assignment (if available).
   * The return value is a Pair where the first element is a list of the feature names set to true.
   * The second element is a list of feature names set to false.
   */
  def getSatAssignment(featureModel: SATFeatureModel, exprCNF: SATFeatureExpr): Option[(List[String], List[String])] = {
    val solver =
      (if (CACHING && (nfm(featureModel) != SATNoFeatureModel))
        SatSolverCache.get(nfm(featureModel))
      else
        new SatSolverImpl(nfm(featureModel), false))

    if (solver.isSatisfiable(exprCNF, exprCNF != True)) {
      return Some(solver.getLastModel())
    } else {
      return None
    }
  }

  private def nfm(fm: SATFeatureModel) = if (fm == null) SATNoFeatureModel else fm
}

// [VSAT] Caching: As CACHING is disabled in the SatSolver (see Line 23) this should not be used ever.
// So we do not have to worry about this I guess.
private object SatSolverCache {
  val cache: WeakHashMap[SATFeatureModel, SatSolverImpl] = new WeakHashMap()

  def get(fm: SATFeatureModel) =
  /*if (fm == NoFeatureModel) new SatSolverImpl(fm)
 else */ cache.getOrElseUpdate(fm, new SatSolverImpl(fm, true))
}

private class SatSolverImpl(featureModel: SATFeatureModel, isReused: Boolean) {

  import SatSolver._

  /**Type Aliases for Readability */
  type CNF = SATFeatureExpr
  type OrClause = SATFeatureExpr
  type Literal = SATFeatureExpr
  type Flag = DefinedExpr

  val PROFILING = false

  val cacheHitTracker: MHashMap[CNF, Integer] = new MHashMap() //.withDefaultValue(0)

  /**init / constructor */
  val solver = SolverFactory.newDefault();
  solver.setTimeoutMs(10000);
//  solver.setTimeoutOnConflicts(100000)

  assert(featureModel != null)
  solver.addAllClauses(featureModel.clauses)
  var uniqueFlagIds: Map[String, Int] = featureModel.variables


  ////////////////////////// VSAT FM Counter ////////////////////////////////
  def vsat_get_fm_cntr() : Integer = {
    val fpath = "./FEAT_MODEL_CNTR"
    val file = new File(fpath)
    val file_not_present = !file.exists()

    // then make it
    if (file_not_present) {
      val output = new BufferedWriter(new FileWriter(fpath, false))
      output.write("0") // initialize to 0, this makes me cringe
      output.close()
    }

    val src  = Source.fromFile(fpath)
    val cntr = src.getLines.take(1).toList.head.toInt
    src.close()
    cntr
  }


  // does scala have lenses or profunctors?
  def vsat_on_fm_cntr(f : Integer => Integer) {
    val i      = vsat_get_fm_cntr()
    val fPath  = "./FEAT_MODEL_CNTR"
    val output = new BufferedWriter(new FileWriter(fPath, false))
    output.write(f(i).toString)
    output.close()
  }


  def vsat_set_fm_cntr(i: Integer) {
    vsat_on_fm_cntr((_:Any) => i)
  }



  def vsat_increment_fm_cntr() {
    vsat_on_fm_cntr(_ + 1)
  }



  /////////////////////////// VSAT Queries ///////////////////////////////////
  def vsat_initialise_new_dir() {
    val fPath = "./sat_queries/"
    val fm_counter = vsat_get_fm_cntr()

    // create any directories on the path
    Files.createDirectories(Paths.get(fPath + fm_counter))
  }

  def vsat_initialise_plain_dir() {
    val fPath = "./sat_queries/"

    // create any directories on the path
    Files.createDirectories(Paths.get(fPath + "plain"))
  }


  // def vsat_make_query_path(id: FeatModelID) : String = {
  //   "./sat_queries/" + id + "/"
  // }


  // // check if the feature model is novel, if so then add it to the observed
  // // feature models and increment the UUID counter
  // def vsat_update_with_fm(observed: ObservedFMs, fm: String) : FeatModelID = {
  //   if (observed.contains(fm)) { // then we have seen the feature model before
  //     observed(fm)               // then get the ID and return it
  //   } else {                     // new feature model
  //     // update observed feature models
  //     val cntr = vsat_get_fm_cntr()
  //     observed + (fm -> cntr)

  //     // create the sub dir for the queries
  //     vsat_initialise_new_dir()

  //     // return
  //     vsat_increment_fm_cntr()
  //     cntr
  //   }
  // }


  // [VSAT]: get the vsat query mode environment variable which indicates which
  // query belongs to parsing, type checking etc.
  def vsat_get_mode(): String = {
    Source.fromFile("./VSAT_MODE").getLines.mkString
  }

//  def vsat_get_env(): String = {
//    Source.fromFile("./VSAT_ENV").getLines.mkString
//  }

  def vsat_update_cache_hits(cacheHits:MHashMap[CNF, Integer], the_query:CNF) {
    cacheHits.get(the_query) match {
      case Some(i) => cacheHits.update(the_query, i + 1)
      case None    => cacheHits.put(the_query, 0)
    }
  }

  def vsat_make_query_path(id: String) : String = {
    "./sat_queries/" + id + "/"
  }

  def vsat_log_cache_hits(cacheHits:MHashMap[CNF,Integer]) {
    val fmPath = "VSAT_CACHE_HITS.txt"
    val fmOut = new BufferedWriter(new FileWriter(fmPath,true))

    val records: Seq[String] = cacheHits.toSeq.map {
      case (key: CNF, i : Integer) => key.toString + "," + i.toString() + "\n"
    }

    val csv: String               = records.reduceLeft(_ + _)

    fmOut.write(csv)
    fmOut.close()
  }

  def getDirFor(featureModel: SATFeatureModel) : String = {
    val path = vsat_make_query_path(
      if (featureModel == SATNoFeatureModel) {
        "plain"
      } else {
        featureModel.toString
      }
    )
    Files.createDirectories(Paths.get(path))
    path
  }

  def vsat_record_query(cacheHits:MHashMap[CNF,Integer], the_query: CNF, featureModel: SATFeatureModel) {
    val dir  = getDirFor(featureModel) // vsat_get_env()
    val mode = vsat_get_mode()

    if (featureModel != SATNoFeatureModel) { // if we have a fm
       val fmPath = dir + "FEATURE_MODEL.txt"
       val fmOut = new BufferedWriter(new FileWriter(fmPath, false))
       fmOut.write(featureModel.decreate().toString())
       fmOut.close()
    }

    vsat_update_cache_hits(cacheHits, the_query)
    vsat_log_cache_hits(cacheHits)

    val output = new BufferedWriter(new FileWriter(dir + "SAT_problems_" + mode + ".txt", true))
    output.write(the_query + "\n")
    output.close()
  }


  /**
   * determines whether
   * (exprCNF AND featureModel) is satisfiable
   *
   * featureModel is optional
   */
  def isSatisfiable(exprCNF: CNF, optimizeSimpleExpression: Boolean = true): Boolean = {
    this.lastModel = null // remove model from last satisfiability check
    assert(CNFHelper.isCNF(exprCNF))

    if (optimizeSimpleExpression) {
      if (exprCNF == True) {
        // if the expression is True, then the result is true, and we need a model. The model is cached lazily.
        if (this.trueModel == null) {
          isSatisfiable(exprCNF, false)
          this.trueModel = getLastModel()
        } else {
          lastModel = trueModel
        }
        return true
      }
      if (exprCNF == False) return false
    }
    //as long as we do not consider feature models, expressions with a single variable
    //are always satisfiable
    if ((featureModel == SATNoFeatureModel) && (CNFHelper.isLiteralExternal(exprCNF))) {
      exprCNF match {
        //one of these cases has to match, because we have a literal expression
        case x: DefinedExternal => lastModel = (List(x.satName), List())
        case Not(x: DefinedExternal) => lastModel = (List(), List(x.satName))
        case _ => sys.error("This really should not be possible")
      }
      return true
    }

    val startTime = System.currentTimeMillis();


// print("THE MODE: " + vsat_get_mode())
vsat_record_query(cacheHitTracker, exprCNF, featureModel)

    if (PROFILING)
      print("<SAT " + countClauses(exprCNF) + " with " + countFlags(exprCNF) + " flags; ")

    val startTimeSAT = System.currentTimeMillis();
    try {

      //find used macros, combine them by common expansion
      val cnfs: List[CNF] = prepareFormula(exprCNF, PROFILING)

      if (PROFILING)
        print(";")
      for (cnf <- cnfs; clause <- CNFHelper.getCNFClauses(cnf))
        for (literal <- CNFHelper.getDefinedExprs(clause))
          if (!uniqueFlagIds.contains(literal.satName))
            uniqueFlagIds = uniqueFlagIds + ((literal.satName, uniqueFlagIds.size + 1))
      if (PROFILING)
        print(";" + cnfs.size + "-" + uniqueFlagIds.size)

      //update max size (nothing happens if smaller than previous setting)
      solver.newVar(uniqueFlagIds.size)

      /**
       * to reuse SAT solver state, use the following strategy for adding
       * (adopted from Thomas Thuems solution in FeatureIDE):
       *
       * clauses with only a single literal are added to "assumptions" and can be
       * checked as paramter to isSatisfiable
       * all other clauses are added to the Solver but remembered in "constraintGroup"
       * which is removed from the solver at the end
       */

      var constraintGroup: Set[IConstr] = Set()
      try {
        val assumptions = new VecInt();
        try {
          for (cnf <- cnfs; clause <- CNFHelper.getCNFClauses(cnf)) {
            if (CNFHelper.isLiteral(clause))
              // [VSAT]: Jeff and Paul, this is the push, record the clause
              assumptions.push(getAtomicId(uniqueFlagIds, clause))
            else {
              val constr = solver.addClause(getClauseVec(uniqueFlagIds, clause))
              if (isReused && constr != null)
                constraintGroup = constraintGroup + constr
            }
          }
        } catch {
          case e: ContradictionException => return false;
        }

        if (PROFILING)
          print(";")

        // [VSAT]: Jeff, Paul, connection to sat4j here!
        // [VSAT]: Jeff, Paul, what is this actually?
        val result = solver.isSatisfiable(assumptions)
        // print reason for unsatisfiability
        /*
        if (result == false) {
            println("Unsat Explanation:")
            val ex:IVecInt = solver.unsatExplanation()
            for ((fName, modelID) <- uniqueFlagIds) {
                if (ex.contains(modelID))
                    println(fName + " &&")
                else if (ex.contains(-modelID))
                    println("! " + fName + " &&")
            }
        }
        */
        if (result == true) {
          // scanning the model (storing the satisfiable assignment for later retrieval)
          val model = solver.model()
          var trueList: List[String] = List()
          var falseList: List[String] = List()
          for ((fName, modelID) <- uniqueFlagIds) {
            if (solver.model(modelID))
              trueList ::= fName
            else
              falseList ::= fName
          }
          lastModel = (trueList, falseList)
        }
        if (PROFILING)
          print(result + ";")
        return result
      } finally {
        if (isReused)
          for (constr <- constraintGroup)
            // [VSAT]: Jef and paul this is a pop, we should record the constr
            assert(solver.removeConstr(constr))
      }
    } finally {
      if (PROFILING)
        println(" in " + (System.currentTimeMillis() - startTimeSAT) + " ms>")
    }
  }

  /**
   * This pair contains the model that was constructed during the last isSatisfiable call (if the result was true).
   * The first element contains the names of the features set to true, the second contains the names of the false features.
   */
  var lastModel: (List[String], List[String]) = null
  // model that satisfies the FM (when a TRUE Expression is passed to the solver)
  // this is cached after first creation
  var trueModel: (List[String], List[String]) = null

  def getLastModel() = lastModel
}

private object SatSolver {
  /**Type Aliases for Readability */
  type CNF = SATFeatureExpr
  type OrClause = SATFeatureExpr
  type Literal = SATFeatureExpr
  type Flag = DefinedExpr


  def countClauses(expr: CNF) = CNFHelper.getCNFClauses(expr).size

  def countFlags(expr: CNF) = {
    var flags = Set[String]()
    for (clause <- CNFHelper.getCNFClauses(expr))
      for (literal <- CNFHelper.getDefinedExprs(clause))
        flags = flags + literal.satName
    flags.size
  }

  def getClauseVec(uniqueFlagIds: Map[String, Int], orClause: OrClause) = {
    val literals = CNFHelper.getLiterals(orClause)
    val clauseArray: Array[Int] = new Array(literals.size)
    var i = 0
    for (literal <- literals) {
      literal match {
        case x: DefinedExpr => clauseArray(i) = uniqueFlagIds(x.satName)
        case Not(x: DefinedExpr) => clauseArray(i) = -uniqueFlagIds(x.satName)
        case _ => throw new NoLiteralException(literal)
      }
      i = i + 1;
    }
    new VecInt(clauseArray)
  }

  def getAtomicId(uniqueFlagIds: Map[String, Int], literal: Literal) = literal match {
    case x: DefinedExpr => uniqueFlagIds(x.satName)
    case Not(x: DefinedExpr) => -uniqueFlagIds(x.satName)
    case _ => throw new NoLiteralException(literal)
  }

  /**
   * DefinedExternal translates directly into a flag for the SAT solver
   *
   * DefinedMacro is more complicated, because it is equivalent to a whole formula.
   * to avoid transforming a large formula into CNF, we use the following strategy
   *
   * DefinedMacro("X",expr) expands to the following
   * DefinedExternal(freshName) -- and a new formula DefinedExternal(freshName) <=> expr
   * Both are independent clauses fed to the SAT solver
   *
   * Actually, DefinedMacro already contains an expression name <=> expr as CNF, where we
   * just need to replace the Macro name by a fresh name.
   *
   * We first collect all expansions and detect identical ones
   */
  def prepareFormula(expr: CNF, PROFILING: Boolean): List[CNF] = {
    import scala.collection.mutable.Map
    var macroExpansions: Map[String, SATFeatureExpr] = Map()
    val cache: Map[SATFeatureExpr, SATFeatureExpr] = Map()

    def prepareLiteral(literal: DefinedExpr): DefinedExpr = {
      literal match {
        case DefinedMacro(name, _, expansionName, expansionNF) => {
          if (!macroExpansions.contains(expansionName)) {
            if (PROFILING)
              print(expansionName)
            val f = expansionNF.apply()
            assert(f.isInstanceOf[SATFeatureExpr])
            val e: CNF = f.asInstanceOf[SATFeatureExpr]
            if (PROFILING)
              print(":")
            //recursively expand formula (dummy is necessary to avoid accidental recursion)
            macroExpansions = macroExpansions + (expansionName -> False /*dummy*/)
            val preparedExpansion = prepareFormulaInner(e)
            macroExpansions = macroExpansions + (expansionName -> preparedExpansion)

            if (PROFILING)
              print(".")
          }
          FExprBuilder.definedExternal(expansionName)
        }
        case e => e
      }
    }
    def prepareFormulaInner(formula: CNF): CNF = {
      formula.mapDefinedExpr(prepareLiteral, cache)
    }

    val targetExpr = prepareFormulaInner(expr)

    List(targetExpr) ++ macroExpansions.values
  }

}
