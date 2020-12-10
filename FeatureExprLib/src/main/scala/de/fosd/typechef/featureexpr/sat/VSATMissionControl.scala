package de.fosd.typechef.featureexpr.sat

import java.io.{BufferedWriter, FileWriter}
import java.nio.file.{Files, Paths}

import scala.io.Source

object VSATMissionControl {
    import VSATMode._

    /// Configure where to log here.
    private val withTextBasedLogging : Boolean = false;
    private val withDatabaseLogging : Boolean = true;
    private var currentMode : VSATMode = VSATMode.Unknown;

    /// Configure debug settings here
    val DEBUG : Boolean = false;

    /// Do not touch these
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