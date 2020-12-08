package de.fosd.typechef.featureexpr.sat

import java.io.{BufferedWriter, FileWriter}
import java.nio.file.{Files, Paths}

import scala.io.Source

object VSATMissionControl {
    import VSATMode._

    /// Configure where to log here.
    private val withTextBasedLogging : Boolean = true;
    private val withDatabaseLogging : Boolean = false;
    private var currentMode : VSATMode = VSATMode.Unknown;

    var metadatadir : String = "./VSAT_metadata/"

    /**
     * Invoked once on program startup.
     */
    def init() : Unit = {
        println("[VSATMissionControl.init] Ready for take-off!");
        cleanCurrentMode();

        // Increment run index number
        Files.createDirectories(Paths.get(metadatadir));
        val runIndexFile = metadatadir + "runno.txt";
        var currentRunIndex : Int = 1;
        if (Files.exists(Paths.get(runIndexFile))) {
            currentRunIndex = 1 + Source.fromFile(runIndexFile).mkString.toInt;
        }
        val output = new BufferedWriter(new FileWriter(runIndexFile, false));
        output.write("" + currentRunIndex);
        output.close();

        metadatadir = metadatadir + currentRunIndex + "/";
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