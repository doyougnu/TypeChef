package de.fosd.typechef.featureexpr.sat

import de.fosd.typechef.featureexpr.bdd._
import org.sat4j.specs.{IVec, IVecInt};

import java.io.{BufferedWriter, FileWriter}
import java.nio.file.{Files, Paths}

import scala.io.Source

case class VSATBDDQueryMetadata(
                               // True when BDDFeatureExpr.isSatisfiable was invoked.
                               // False when BDDFeatureExpr.isSatisfiable2 was invoked.
//                               invokedOnSat1 : Boolean
//                               ,
                               // True when the formula was sent to the solving stage.
                               // False when the formula was simple enough to be solved immediately by typechef.
                               sentToSat : Boolean,
                               // information on where this metadata was created
                               origin : String
                           );

object VSATMissionControl {
    import VSATMode._

    /// Configure the logging here.
    private val withTextBasedLogging : Boolean = false;
    private val withDatabaseLogging : Boolean = true;
    val DEBUG : Boolean = false; // Will print more information (mostly used for database logging)

    /// Do not touch these
    private var currentMode : VSATMode = VSATMode.Unknown;
    private val noFeatureModelHash : String = "NoFM";
    private val noSessionFile : String = "None";
    var metadatadir : String = "./VSAT_metadata/"
    val runIndexFile : String = metadatadir + "runno.txt";
    val startRunNumber : Int = 1;
    var runNumber : Int = -1;
    var sessionFile : String = "";

    /**
     * Invoked once on program startup.
     */
    def init() : Unit = {
        println("[VSATMissionControl.init] Ready for take-off!");
        cleanCurrentMode();

        // Increment run index number
        runNumber = startRunNumber;
        Files.createDirectories(Paths.get(metadatadir));
        if (Files.exists(Paths.get(runIndexFile))) {
            val source = Source.fromFile(runIndexFile);
            runNumber = 1 + source.mkString.toInt;
            source.close();
        }
        val output = new BufferedWriter(new FileWriter(runIndexFile, false));
        output.write("" + runNumber);
        output.close();

        metadatadir = metadatadir + runNumber + "/";
        Files.createDirectories(Paths.get(metadatadir));

        if (withTextBasedLogging) {
            VSATTextBasedLogger.init();
        }
        if (withDatabaseLogging) {
            VSATDatabase.init();
        }
    }

    def setSessionFile(file : String) : Unit = {
        println("[VSATMissionControl.setSessionFile] to " + file);
        sessionFile = file;

        if (withTextBasedLogging) {
            VSATTextBasedLogger.setSessionFile(file);
        }
    }

    def getSessionFile(): String = {
        if (sessionFile.isEmpty) {
            noSessionFile
        } else {
            sessionFile
        }
    }

    def terminate(): Unit = {
        println("[VSATMissionControl.terminate] Mission Completed!");
        if (withTextBasedLogging) {
            VSATTextBasedLogger.terminate();
        }
        if (withDatabaseLogging) {
            VSATDatabase.terminate();
        }
    }

    def isFirstRun(): Boolean = startRunNumber == runNumber;

    /// Hashing of Feature Models

    private def fmhash_tostr(fm : SATFeatureModel) : String = fm.toString.substring("de.fosd.typechef.featureexpr.sat.SATFeatureModel@".length)

    private def fmhash_javastrhashcode(fmclauses : IVec[IVecInt]) : Int = fmhash_shortformula(fmclauses).hashCode

    private def fmhash_shortformula(fmclauses : IVec[IVecInt]) : String = {
        val orConnective : String = "+";
        val andConnective : String = "*";

        def clauseToStr(clause : IVecInt) : String = {
            val clauseIndicesArray : Array[Int] = new Array[Int](clause.size());
            clause.copyTo(clauseIndicesArray);
            "(" + clauseIndicesArray.mkString(orConnective) + ")"
        };

        val cnf : Array[IVecInt] = new Array[IVecInt](fmclauses.size());
        fmclauses.copyTo(cnf);
        cnf.map(clauseToStr).mkString(andConnective);
    }

    private def fmhash_arithmetic(fmclauses : IVec[IVecInt]) : Long = {
        // inspired by https://cp-algorithms.com/string/string-hashing.html
        val peterPrime : Long = 53L;
        val hashLimit : Long = 1099511627689L; // greatest prime smaller than (2^40)

        def hashClause(clause : IVecInt) : Long = {
            val clauseIndicesArray: Array[Int] = new Array[Int](clause.size());
            clause.copyTo(clauseIndicesArray);
            clauseIndicesArray
                .map(x => {
                    // make everything positive
                    if (x < 0) {
                        (2 * (-x)) - 1
                    } else {
                        2 * x
                    }
                })
                .map(i => i.toLong)
                .sum % hashLimit
        }

        var currentPeterPrime : Long = 1L;
        val cnf : Array[IVecInt] = new Array[IVecInt](fmclauses.size());
        fmclauses.copyTo(cnf);
        cnf
            .map(i => {
                val ret : Long = hashClause(i) * currentPeterPrime;
                currentPeterPrime = (currentPeterPrime * peterPrime) % hashLimit;
                ret % hashLimit
            })
            .foldLeft(0L)((a, b) => (a + b) % hashLimit);
    }

    def hash(fm : SATFeatureModel) : String = {
        if (fm == null || fm == SATNoFeatureModel) {
            noFeatureModelHash
        } else {
            val clauses = fm.clauses;
            fmhash_javastrhashcode(clauses) + "_" + fmhash_arithmetic(clauses) //+ "_" + runNumber + "_" + fmhash_java(fm)
        }
    }

    def hash(fm : BDDFeatureModel) : String = {
        if (fm == null || fm == BDDNoFeatureModel) {
            noFeatureModelHash
        } else {
            val clauses = fm.clauses;
            fmhash_javastrhashcode(clauses) + "_" + fmhash_arithmetic(clauses)
        }
    }

    /// Get and Set the VSAT_MODE here

    def setCurrentMode(newMode : VSATMode) : Unit = {
        println("[VSATMissionControl.setCurrentMode] to " + newMode)
        currentMode = newMode;
    }

    def cleanCurrentMode() : Unit = {
        setCurrentMode(VSATMode.Unknown)
    }

    def getCurrentMode() : VSATMode = { currentMode }

    /// Query Logging from here

    def sat_cache_hit(the_query: SATFeatureExpr, featureModel: SATFeatureModel) : Unit = {
        if (withTextBasedLogging) {
            VSATTextBasedLogger.sat_cache_hit(the_query, featureModel);
        }
        if (withDatabaseLogging) {
            VSATDatabase.sat_cache_hit(the_query, featureModel);
        }
    }

    def sat_record_query(the_query: SATFeatureExpr, featureModel: SATFeatureModel, sentToSat : Boolean) : Unit = {
        if (withTextBasedLogging) {
            VSATTextBasedLogger.sat_record_query(the_query, featureModel, sentToSat);
        }
        if (withDatabaseLogging) {
            VSATDatabase.sat_record_query(the_query, featureModel, sentToSat);
        }
    }

    def bdd_cache_hit(the_query: BDDFeatureExpr, featureModel: BDDFeatureModel, metadata : VSATBDDQueryMetadata) : Unit = {
        if (withDatabaseLogging) {
            VSATDatabase.bdd_cache_hit(the_query, featureModel, metadata);
        }
    }

    def bdd_record_query(the_query: BDDFeatureExpr, featureModel: BDDFeatureModel, metadata : VSATBDDQueryMetadata) : Unit = {
        if (withDatabaseLogging) {
            VSATDatabase.bdd_record_query(the_query, featureModel, metadata);
        }
    }
}