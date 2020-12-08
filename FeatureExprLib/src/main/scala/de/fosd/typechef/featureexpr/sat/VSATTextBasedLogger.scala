package de.fosd.typechef.featureexpr.sat

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths}
import scala.collection.mutable.HashMap

import scala.io.Source

object VSATTextBasedLogger {
//    val cacheHitTracker: MHashMap[CNF, Integer] = new MHashMap() //.withDefaultValue(0)
//
//    ////////////////////////// VSAT FM Counter ////////////////////////////////
//    def get_fm_cntr() : Integer = {
//        val fpath = "./FEAT_MODEL_CNTR"
//        val file = new File(fpath)
//        val file_not_present = !file.exists()
//
//        // then make it
//        if (file_not_present) {
//            val output = new BufferedWriter(new FileWriter(fpath, false))
//            output.write("0") // initialize to 0, this makes me cringe
//            output.close()
//        }
//
//        val src  = Source.fromFile(fpath)
//        val cntr = src.getLines.take(1).toList.head.toInt
//        src.close()
//        cntr
//    }
//
//    // does scala have lenses or profunctors?
//    def on_fm_cntr(f : Integer => Integer) {
//        val i      = get_fm_cntr()
//        val fPath  = "./FEAT_MODEL_CNTR"
//        val output = new BufferedWriter(new FileWriter(fPath, false))
//        output.write(f(i).toString)
//        output.close()
//    }
//
//    def set_fm_cntr(i: Integer) {
//        on_fm_cntr((_:Any) => i)
//    }
//
//    def increment_fm_cntr() {
//        on_fm_cntr(_ + 1)
//    }
//
//    /////////////////////////// VSAT Queries ///////////////////////////////////
//    def initialise_new_dir() {
//        val fPath = "./sat_queries/"
//        val fm_counter = get_fm_cntr()
//
//        // create any directories on the path
//        Files.createDirectories(Paths.get(fPath + fm_counter))
//    }
//
//    def initialise_plain_dir() {
//        val fPath = "./sat_queries/"
//
//        // create any directories on the path
//        Files.createDirectories(Paths.get(fPath + "plain"))
//    }

    // def make_query_path(id: FeatModelID) : String = {
    //   "./sat_queries/" + id + "/"
    // }

    // // check if the feature model is novel, if so then add it to the observed
    // // feature models and increment the UUID counter
    // def update_with_fm(observed: ObservedFMs, fm: String) : FeatModelID = {
    //   if (observed.contains(fm)) { // then we have seen the feature model before
    //     observed(fm)               // then get the ID and return it
    //   } else {                     // new feature model
    //     // update observed feature models
    //     val cntr = get_fm_cntr()
    //     observed + (fm -> cntr)

    //     // create the sub dir for the queries
    //     initialise_new_dir()

    //     // return
    //     increment_fm_cntr()
    //     cntr
    //   }
    // }
    

    //  def get_env(): String = {
    //    Source.fromFile("./VSAT_ENV").getLines.mkString
    //  }
    
    def make_query_path(id: String) : String = {
        "./VSAT_sat_queries/" + id + "/"
    }

    /// The two caching methods are buggy because they do not consider the FM.
//    def update_cache_hits(the_query:CNF) {
//        cacheHitTracker.get(the_query) match {
//            case Some(i) => cacheHits.update(the_query, i + 1)
//            case None    => cacheHits.put(the_query, 0)
//        }
//    }
//
//    def log_cache_hits() {
//        val fmPath = "VSAT_CACHE_HITS.txt"
//        val fmOut = new BufferedWriter(new FileWriter(fmPath,true))
//
//        val records: Seq[String] = cacheHitTracker.toSeq.map {
//            case (key: CNF, i : Integer) => key.toString + "," + i.toString() + "\n"
//        }
//
//        val csv: String               = records.reduceLeft(_ + _)
//
//        fmOut.write(csv)
//        fmOut.close()
//    }

    def getDirFor(featureModel: SATFeatureModel) : String = {
        val path = make_query_path(
            if (featureModel == SATNoFeatureModel) {
                "plain"
            } else {
                featureModel.toString
            }
        )
        Files.createDirectories(Paths.get(path))
        path
    }

    // [VSAT]: get the vsat query mode environment variable which indicates which
    // query belongs to parsing, type checking etc.
    def get_mode(): String = {
        Source.fromFile("./VSAT_MODE").getLines.mkString
    }

    def cache_hit(the_query: SATFeatureExpr, featureModel: SATFeatureModel): Unit = {
        /**
         * Choose red (memory friendly) or blue pill (runtime friendly).
         */

        // red pill
        //cache_hit_memoryfriendly(the_query, featureModel);
        //blue pill
        cache_hit_runtimefriendly(the_query, featureModel);
    }

    /**
     * Red pill:
     * This method is implemented quite stupidly.
     * Whenever we have a cache hit, we write the query to the end of a text file.
     * So the occurences of the formula in that file gives the number of cache hits on that formula.
     * I chose this stupid logging of cache hits because it does not require us to keep all that data in typechefs
     * memory.
     * Instead, on a postprocessing step, we could merge Cache_hits.txt with SAT_problems.txt.
     */
    private def cache_hit_memoryfriendly(the_query: SATFeatureExpr, featureModel: SATFeatureModel) : Unit = {
        val mode = get_mode()
        val dir  = getDirFor(featureModel)
        val output = new BufferedWriter(new FileWriter(dir + "Cache_hits_" + mode + ".txt", true))
        output.write(the_query + "\n")
        output.close()
    }

    // @Jeff: You used a MHashMap? What's that and what's the difference to HashMap?
    private var cache_hits : HashMap[SATFeatureModel, HashMap[SATFeatureExpr, Integer]] = new HashMap();
    private def cache_hit_runtimefriendly(the_query: SATFeatureExpr, featureModel: SATFeatureModel) : Unit = {
        var innerMap : HashMap[SATFeatureExpr, Integer] = cache_hits.getOrElseUpdate(featureModel, new HashMap());

        innerMap.get(the_query) match {
            case Some(i) => innerMap.update(the_query, i + 1)
            case None    => innerMap.put(the_query, 1)
        }

        log_cache_hits_for(featureModel)
    }

    def log_cache_hits(): Unit = {
        for ((featureModel, innerMap) <- cache_hits) {
            log_cache_hits_for(featureModel);
        }
    }

    def log_cache_hits_for(featureModel: SATFeatureModel) : Unit = {
        val mode = get_mode()
        var innerMap : HashMap[SATFeatureExpr, Integer] = cache_hits.getOrElseUpdate(featureModel, new HashMap());
        val dir = getDirFor(featureModel) + mode;
        val fmPath = dir + "/" + "VSAT_CACHE_HITS.txt";
        Files.createDirectories(Paths.get(dir))

        val fmOut = new BufferedWriter(new FileWriter(fmPath,false));

        val records: Seq[String] = innerMap.toSeq.map {
            case (key: SATFeatureExpr, i : Integer) => key.toString + "," + i.toString() + "\n"
        }

        val csv: String = records.reduceLeft(_ + _)

        fmOut.write(csv)
        fmOut.close()
    }
    
    /**
     * @param the_query The query to log in CNF.
     * @param featureModel The featuremodel under which the query is made. Might be empty in form of SATNoFeatureModel
     * @param sendToSat True when the query was sent to the SAT solver by TypeChef.
     *                  False, if TypeChef could answer the satisfiability query directly.
     *                  This happens if the query is simple enough (e.g., when it is an atomic 'true' or 'false').
     *                  @Jeff: Depending on this value we might want to drop queries immediately.
     *                         I included them for now because we will get cache_hits on these queries, too.
     */
    def record_query(the_query: SATFeatureExpr, featureModel: SATFeatureModel, sentToSat : Boolean) {
        // [VSATDB] This is where queries are recorded.
        val mode = get_mode()
        val dir  = getDirFor(featureModel) // get_env()
        val fmPath = dir + "FEATURE_MODEL.txt"

        // if we have a fm and didn't write it to disk yet
        if (featureModel != SATNoFeatureModel
            && !Files.exists(Paths.get(fmPath)))
        {
            val fmOut = new BufferedWriter(new FileWriter(fmPath, false))
            fmOut.write(featureModel.decreate().toString())
            fmOut.close()
        }

        /**
         * We basically want to write out a table with the following columns
         * query : String | fm : String | cacheHits : Int | sentToSAT : Bool
         *
         * We achieve this by doing the following.
         * 1.) We create a directory for each FM and group queries on that FM in that directory.
         * 2.) We log all queries (those passed as arguments to this method) and the value of sentToSAT
         *     in "SAT_problems_<MODE>.txt", where <MODE> is the current mode of Typechef.
         * 3.) On a cache hit (see method cache_hit), we write the formula on which we got a cache hit to "Cache_hits_<MODE>.txt".
         */
        val satproblemsdir = dir + mode;
        Files.createDirectories(Paths.get(satproblemsdir))
        val output = new BufferedWriter(new FileWriter(satproblemsdir + "/" + "SAT_problems.txt", true))
        output.write(the_query + "; " + sentToSat + "\n")
        output.close()
    }
}