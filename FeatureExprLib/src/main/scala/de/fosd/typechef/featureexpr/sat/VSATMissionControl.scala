package de.fosd.typechef.featureexpr.sat

import java.io.{BufferedWriter, FileWriter}
import java.nio.file.{Files, Paths}

import scala.io.Source

object VSATMissionControl {
    import VSATMode._

    /// Configure the logging here.
    private val withTextBasedLogging : Boolean = false;
    private val withDatabaseLogging : Boolean = true;
    val DEBUG : Boolean = false; // Will print more information (mostly used for database logging)

    /// Do not touch these
    private var currentMode : VSATMode = VSATMode.Unknown;
    var metadatadir : String = "./VSAT_metadata/"
    val runIndexFile : String = metadatadir + "runno.txt";
    val startRunNumber : Int = 1;
    var runNumber : Int = -1;

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
            runNumber = 1 + Source.fromFile(runIndexFile).mkString.toInt;
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

        if (withTextBasedLogging) {
            VSATTextBasedLogger.setSessionFile(file);
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

    def isFirstRun() : Boolean = startRunNumber == runNumber;

    /// Hashing of Feature Models

    private def fmhash_java(fm : SATFeatureModel) : String = fm.toString.substring("de.fosd.typechef.featureexpr.sat.SATFeatureModel@".length)

    private def fmhash_shortformula(fm : SATFeatureModel) : String = {
        import org.sat4j.specs.IVecInt;

        val mempty : String = "";
        val orConnective : String = "+";
        val andConnective : String = "*";

        def clauseToStr(clause : IVecInt) : String = {
            var clauseIndicesArray : Array[Int] = new Array[Int](clause.size());
            clause.copyTo(clauseIndicesArray);
            "(" + clauseIndicesArray.mkString(orConnective) + ")"
        };

        var cnf : Array[IVecInt] = new Array[IVecInt](fm.clauses.size());
        fm.clauses.copyTo(cnf);
        cnf.map(clauseToStr).mkString(andConnective);
    }

    private def fmhash_arithmetic(fm : SATFeatureModel) : Long = {
        import org.sat4j.specs.IVecInt;

        if (fm == SATNoFeatureModel) {
            return 0;
        }

        // inspired by https://cp-algorithms.com/string/string-hashing.html
        var globalIndex : Long = 0L;
        val peterPrime : Long = 53L;
        val hashLimit : Long = 1099511627689L; // greatest prime smaller than (2^40)

        def hashClause(clause : IVecInt) : Long = {
            var clauseIndicesArray: Array[Int] = new Array[Int](clause.size());
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

        var cnf : Array[IVecInt] = new Array[IVecInt](fm.clauses.size());
        fm.clauses.copyTo(cnf);
        cnf
            .map(i => {
                val ret : Long = hashClause(i) * scala.math.pow(peterPrime, globalIndex).longValue();
                globalIndex += 1L;
                ret % hashLimit
            })
            .foldLeft(0L)((a, b) => (a + b) % hashLimit);
    }

    def hash(fm : SATFeatureModel) : String = {
        "" + fmhash_arithmetic(fm) //+ "_" + runNumber + "_" + fmhash_java(fm)
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

    def cache_hit(the_query: SATFeatureExpr, featureModel: SATFeatureModel) : Unit = {
        if (withTextBasedLogging) {
            VSATTextBasedLogger.cache_hit(the_query, featureModel);
        }
        if (withDatabaseLogging) {
            VSATDatabase.cache_hit(the_query, featureModel);
        }
    }

    def record_query(the_query: SATFeatureExpr, featureModel: SATFeatureModel, sentToSat : Boolean) : Unit = {
        if (withTextBasedLogging) {
            VSATTextBasedLogger.record_query(the_query, featureModel, sentToSat);
        }
        if (withDatabaseLogging) {
            VSATDatabase.record_query(the_query, featureModel, sentToSat);
        }
    }
}