package de.fosd.typechef.featureexpr.sat

import com.sun.xml.internal.ws.developer.StreamingAttachmentFeature

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths}
import scala.collection.mutable.HashMap
import scala.io.Source

object VSATTextBasedLogger {
    import VSATMode._

    private val logDir : String = "sat_queries/";
    private val cacheFileName : String = "VSAT_CACHE_HITS.txt";
    private val queriesFileName : String = "SAT_PROBLEMS.txt";
    private val featureModelFileName : String = "FEATURE_MODEL.txt";
    private var directoryForCurrentIteration : String =
//        "unknownfile/";
        "";

    def init() : Boolean = {
        println("[VSATTextBasedLogger.init]");
        true
    }

    def setSessionFile(file : String) : Unit = {
        println("[VSATTextBasedLogger.setSessionFile]");

//        directoryForCurrentIteration = file + "/";

        // Save a list of all files that where processed by init across TypeChef runs.
        val runsFile = VSATMissionControl.metadatadir + "runs.txt";
        val output = new BufferedWriter(new FileWriter(runsFile, true))
        output.write(file + "\n")
        output.close()
    }

    def terminate() : Boolean = {
        println("[VSATTextBasedLogger.terminate] Writing cache log ...");
        log_cache_hits();
        println("[VSATTextBasedLogger.terminate] done");
        true
    }

    def make_query_path(id: String) : String = {
        VSATMissionControl.metadatadir + logDir + directoryForCurrentIteration + id + "/"
    }

    def getFMDir(featureModel: SATFeatureModel) : String = {
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

    def getCurrentQueryDir(featureModel: SATFeatureModel) : String = {
        getQueryDir(featureModel, VSATMissionControl.getCurrentMode())
    }

    def getQueryDir(featureModel: SATFeatureModel, mode : VSATMode) : String = {
        val path = getFMDir(featureModel) + mode + "/";
        Files.createDirectories(Paths.get(path))
        path
    }

    def cache_hit(the_query: SATFeatureExpr, featureModel: SATFeatureModel): Unit = {
        /**
         * Choose exactly one of red (memory friendly) or blue pill (runtime friendly).
         */

        // red pill
        //cache_hit_memoryfriendly(the_query, featureModel);
        // blue pill
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
        val dir = getCurrentQueryDir(featureModel)
        val output = new BufferedWriter(new FileWriter(dir + cacheFileName, true))
        output.write(the_query + "\n")
        output.close()
    }

    private var cache_hits : HashMap[SATFeatureModel, HashMap[VSATMode, HashMap[SATFeatureExpr, Integer]]] = new HashMap();

    private def getCacheOf(featureModel: SATFeatureModel, mode : VSATMode): HashMap[SATFeatureExpr, Integer] = {
        cache_hits
            .getOrElseUpdate(featureModel, new HashMap())
            .getOrElseUpdate(VSATMissionControl.getCurrentMode(), new HashMap())
    }

    private def cache_hit_runtimefriendly(the_query: SATFeatureExpr, featureModel: SATFeatureModel) : Unit = {
        var innerMap : HashMap[SATFeatureExpr, Integer] = getCacheOf(featureModel, VSATMissionControl.getCurrentMode());

        innerMap.get(the_query) match {
            case Some(i) => innerMap.update(the_query, i + 1)
            case None    => innerMap.put(the_query, 1)
        }

        // Rewriting the entire hashmap to disk is quite expensive.
        // So we don't write here but on program end (see terminate).
        /// log_cache_hits_for(featureModel)
    }

    def log_cache_hits(): Unit = {
        for ((featureModel, innerMap) <- cache_hits) {
            log_cache_hits_for(featureModel);
        }
    }

    def log_cache_hits_for(featureModel: SATFeatureModel) : Unit = {
        for (mode <- VSATMode.values) {
            log_cache_hits_for(featureModel, mode);
        }
    }

    def log_cache_hits_for(featureModel: SATFeatureModel, mode : VSATMode) : Unit = {
        var innerMap : HashMap[SATFeatureExpr, Integer] = getCacheOf(featureModel, mode);

        val records: Seq[String] = innerMap.toSeq.map {
            case (key: SATFeatureExpr, i : Integer) => key.toString + "," + i.toString() + "\n"
        }

        if (records.nonEmpty) {
            val csv: String = records.reduceLeft(_ + _)
            val path = getQueryDir(featureModel, mode) + cacheFileName;
            val fmOut = new BufferedWriter(new FileWriter(path, false));
            fmOut.write(csv)
            fmOut.close()
        }
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
        val fmPath = getFMDir(featureModel) + featureModelFileName

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
         *     in "<MODE>/SAT_problems.txt", where <MODE> is the current mode of Typechef.
         * 3.) On a cache hit (see method cache_hit), we write the formula on which we got a cache hit to "<MODE>/VSAT_CACHE_HITS.txt".
         */
        val problemsPath = getCurrentQueryDir(featureModel) + queriesFileName;
        val output = new BufferedWriter(new FileWriter(problemsPath, true))
        output.write(the_query + ", " + sentToSat + "\n")
        output.close()
    }
}