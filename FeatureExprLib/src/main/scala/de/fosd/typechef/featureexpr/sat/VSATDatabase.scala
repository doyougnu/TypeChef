package de.fosd.typechef.featureexpr.sat

// Use H2Profile to connect to an H2 database
import slick.jdbc.H2Profile.api._

// This is deprecated:
//import slick.driver.H2Driver.api._

//import scala.slick.driver.H2Driver.simple._

import java.io.{PrintWriter, StringWriter}

import scala.concurrent.{Future, Await}
import scala.util.{Success,Failure,Try}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

import slick.jdbc.GetResult

import de.fosd.typechef.featureexpr.bdd._

object VSATDatabase {
    // Select the database profile in application.conf that you want to use.
    private val databaseProfileToUse : String = "h2localhostpaul";

    // Neither touch these
    private val satQueriesTableName : String = "SATQUERIES";
    private val bddQueriesTableName : String = "BDDQUERIES";
    private val featureModelsTableName : String = "FEATUREMODELS";
    private val errorsTableName : String = "ERRORS";
    private val tableLine : String = "----------------------------------------";

    // Nor these
    private var running : Boolean = false;
    private var db : Database = null;
    private var errorCounter : Int = 0;

    /// Possible TODO:
    /// Currently, we often wait for futures to complete.
    /// Instead we should use monadic bind to queue new actions asynchronously.
    /// I just wanted to get it working first and fancify it afterwards.

    def init() : Boolean = {
        println("[VSATDatabase.init]");
        if (running) {
            println("[Database.init] Database connection is already established.")
            return false
        }

        errorCounter = 0;

        db = Database.forConfig(databaseProfileToUse)

        setupTable(satQueriesTableName, createSatQueriesTable _);
        setupTable(bddQueriesTableName, createBddQueriesTable _);
        setupTable(featureModelsTableName, createFeatureModelsTable _);
        setupTable(errorsTableName, createErrorsTable _);

        if (VSATMissionControl.DEBUG) {
            runSyncForced(printTableNames());
        }

        running
    }

    def terminate() : Boolean = {
//        if (VSATMissionControl.DEBUG) {
//            runSync(showSatQueriesTable());
//            runSync(showFeatureModelsTable());
//        }
        db.close();
        println("[Database.terminate] Database connection terminated")
        running = false
        running
    }

    def setupTable(name : String, createTableQuery : () => DBIO[Int]): Unit = {
        // Creating tables synchronously and crash if something fails.
        var exists : Boolean = evalForced(tableExists(name));
        if (exists && VSATMissionControl.isFirstRun()) {
            println("[VSATDatabase.setupTable] Cleaning existing table " + name + " because this is the first run.");
            runSyncForced(dropTable(name));
            exists = false;
        }
        if (!exists) {
            println("[VSATDatabase.setupTable] Creating table " + name + ".");
            runSyncForced(createTableQuery());
        }
    }


    /// Table Entry Datatypes ///

    case class FMRecord(hash : String, formula : String)
    implicit val getFMRecordResult = GetResult(r => FMRecord(r.<<, r.<<))

    // For Slick you dont need an extra class for Primary Keys. Its just for us.
    case class SATQueryPrimaryKey(formula : String, fmhash : String, mode : String)
    case class SATQueryRecord(formula : String, fmhash : String, mode : String, tcCacheHits : Int, dbCacheHits : Int, sentToSAT : Boolean)
    implicit val getSATQueryRecordResult = GetResult(r => SATQueryRecord(r.<<, r.<<, r.<<, r.<<, r.<<, r.<<))

    val fromSATPrimaryKey : (SATQueryPrimaryKey, Boolean) => SATQueryRecord =
        (k, sentToSAT) => SATQueryRecord(k.formula, k.fmhash, k.mode, 0, 0, sentToSAT)

    val toSATPrimaryKey : SATQueryRecord => SATQueryPrimaryKey =
        s => SATQueryPrimaryKey(s.formula, s.fmhash, s.mode)

    case class BDDQueryPrimaryKey(hash : String, fmhash : String, mode : String)
    case class BDDQueryRecord(hash : String, fmhash : String, mode : String, tcCacheHits : Int, dbCacheHits : Int, sentToSAT : Boolean);
    implicit val getBDDQueryRecordResult = GetResult(r => BDDQueryRecord(r.<<, r.<<, r.<<, r.<<, r.<<, r.<<))

    val fromBDDPrimaryKey : (BDDQueryPrimaryKey, Boolean) => BDDQueryRecord =
        (k, sentToSAT) => BDDQueryRecord(k.hash, k.fmhash, k.mode, 0, 0, sentToSAT)

    val toBDDPrimaryKey : BDDQueryRecord => BDDQueryPrimaryKey =
        s => BDDQueryPrimaryKey(s.hash, s.fmhash, s.mode)

    /// VSAT logging ///

    def sat_cache_hit(the_query: SATFeatureExpr, featureModel: SATFeatureModel): Unit =
        tryWait(incTcCacheHits(
            SATQueryPrimaryKey(
                the_query.toString,
                VSATMissionControl.hash(featureModel),
                VSATMissionControl.getCurrentMode().toString)))

    def sat_record_query(the_query: SATFeatureExpr, featureModel: SATFeatureModel, sentToSat : Boolean): Unit = {
        // We assume that the given query is not yet stored in the DB
        // If it is, we have a cache hit that was missed by TypeChef.
        // This might happen due to repeated runs of TypeChef.
        // If we get such a cache hit, we store it in dbCacheHits.

        // 1.) Store FM if not stored already
        var fmHash : String = VSATMissionControl.hash(featureModel)
        if (featureModel != SATNoFeatureModel) {
            val existingFM : Option[FMRecord] = runSyncForced(sql"SELECT * FROM #$featureModelsTableName WHERE hash = $fmHash".as[FMRecord].headOption);
            if (existingFM.isEmpty) {
                val fmFormula = featureModel.decreate().toString();
                val fmrecord = FMRecord(fmHash, fmFormula);
                if (VSATMissionControl.DEBUG) {
                    println("[Database.sat_record_query] inserting new feature model " + fmrecord.hash);
                }
                runSafe(sqlu"INSERT INTO #$featureModelsTableName VALUES (${fmrecord.hash}, ${fmrecord.formula});");
            }
        }

        // 2.) Store the query or update it if we have an accidental cache hit
        val key = SATQueryPrimaryKey(the_query.toString, fmHash, VSATMissionControl.getCurrentMode().toString)

        val existingQuery : Option[SATQueryRecord] = evalForced(getSATQuery(key));
        if (existingQuery.isEmpty) {
            // The default case: This is a new query and we store it
            runSafe(insertSATQuery(fromSATPrimaryKey(key, sentToSat)));
        } else {
            // Surprising case: TypeChef told us to record a new query but we actually have a cache hit!
            // Such a cache hit remains unobserved by TypeChef.
            // Hypothesis: This happens because TypeChef might discard its cache inbetween different runs of the main method.
            if (VSATMissionControl.DEBUG) {
                println("[VSATDatabase.sat_record_query] Cache hit in database that was unnoticed by TypeChef.")
            }
            incDbCacheHits(key);
        }
    }

    def bdd_cache_hit(the_query: BDDFeatureExpr, featureModel: BDDFeatureModel, metadata : VSATBDDQueryMetadata) : Unit =
        tryWait(incTcCacheHits(
            BDDQueryPrimaryKey(
                "" + the_query.hashCode,
                VSATMissionControl.hash(featureModel),
                VSATMissionControl.getCurrentMode().toString)))

    def bdd_record_query(the_query: BDDFeatureExpr, featureModel: BDDFeatureModel, metadata : VSATBDDQueryMetadata) : Unit = {
        // ignore the feature mode for now
        val key = BDDQueryPrimaryKey("" + the_query.hashCode, "nokey_"+VSATMissionControl.hash(featureModel), VSATMissionControl.getCurrentMode().toString)

        val existingQuery : Option[BDDQueryRecord] = evalForced(getBDDQuery(key));
        if (existingQuery.isEmpty) {
            // The default case: This is a new query and we store it
            runSafe(insertBDDQuery(fromBDDPrimaryKey(key, metadata.sentToSat)));
        } else {
            // Surprising case: TypeChef told us to record a new query but we actually have a cache hit!
            // Such a cache hit remains unobserved by TypeChef.
            // Hypothesis: This happens because TypeChef might discard its cache inbetween different runs of the main method.
            if (VSATMissionControl.DEBUG) {
                println("[VSATDatabase.bdd_record_query] Cache hit in database that was unnoticed by TypeChef.")
            }
            incDbCacheHits(key);
        }
    }

    /// Running Database Actions ///

    private def runSafe[T](action : DBIO[T]) : T = tryWait(runAsync(action))
    private def runAsync[T](action : DBIO[T]) : Future[T] = db.run(action)
    private def runSync[T](action : DBIO[T]) : Try[T] = eval(runAsync(action))
    // Like runSync but throws an exception if the action failed
    private def runSyncForced[T](action : DBIO[T]) : T = evalForced(runAsync(action))

    private def eval[T](f : Future[T]) : Try[T] = Await.ready(f, Duration.Inf).value.get
    // like eval but throws an exception when the future fails
    private def evalForced[T](f : Future[T]) : T = Await.result(f, Duration.Inf)

    private def tryWait[T](f : Future[T]) : T = {
        try {
            evalForced(f)
        } catch {
            case e : Exception => {
                logError(e.getMessage);
                throw e
            };
        }
    }


    /// Create SQL Queries ///

    private def dropTable(table : String) : DBIO[Int] = sqlu"DROP TABLE #$table"

    /// sqlu produces a DBIO[Int] action where the returned int is the row cound (I guess)
    private def createSatQueriesTable() : DBIO[Int] =
        sqlu"""CREATE TABLE #$satQueriesTableName (
               formula varchar(max) NOT NULL,
               fmhash varchar(255) NOT NULL,
               mode varchar(50) NOT NULL,
               tcCacheHits int NOT NULL,
               dbCacheHits int NOT NULL,
               sentToSAT bool NOT NULL,
               PRIMARY KEY(formula, fmhash, mode)
               );"""//CONSTRAINT pkey

    private def createFeatureModelsTable() : DBIO[Int] =
        sqlu"""CREATE TABLE #$featureModelsTableName (
               hash varchar(255) NOT NULL,
               formula varchar(max) NOT NULL,
               PRIMARY KEY(hash)
               );"""
    
    private def createBddQueriesTable() : DBIO[Int] =
        sqlu"""CREATE TABLE #$bddQueriesTableName (
               hash varchar(255) NOT NULL,
               fmhash varchar(255) NOT NULL,
               mode varchar(50) NOT NULL,
               tcCacheHits int NOT NULL,
               dbCacheHits int NOT NULL,
               sentToSAT bool NOT NULL,
               PRIMARY KEY(hash, fmhash, mode)
               );"""//CONSTRAINT pkey

    private def createErrorsTable() : DBIO[Int] =
        sqlu"""CREATE TABLE #$errorsTableName (
               file varchar(255) NOT NULL,
               no int NOT NULL,
               message varchar(max) NOT NULL,
               stacktrace varchar(max),
               PRIMARY KEY(file, no)
               );"""

    private def tableExists(tablename : String) : Future[Boolean] = {
        runAsync(sql"""SELECT TABLE_NAME
               FROM INFORMATION_SCHEMA.TABLES
               WHERE TABLE_SCHEMA = 'PUBLIC'
               AND TABLE_NAME = $tablename""".as[String].headOption // look at this ugly method name 'headoption'
        ).map(o => o.nonEmpty)
    }

    /// Functions to create WHERE statements according to e.g. primary keys

    private val regardlessOfModeSAT : SATQueryPrimaryKey => String
        = key => "formula = '" + key.formula + "' AND fmhash = '" + key.fmhash + "'";

    private val regardlessOfModeBDD : BDDQueryPrimaryKey => String
        = key => "hash = '" + key.hash + "' AND fmhash = '" + key.fmhash + "'";

    private val bySATPrimaryKey : SATQueryPrimaryKey => String
        = key => regardlessOfModeSAT(key) + " AND mode = '" + key.mode + "'"

    private val byBDDPrimaryKey : BDDQueryPrimaryKey => String
        = key => regardlessOfModeBDD(key) + " AND mode = '" + key.mode + "'"

    /// Functions to get rows

    // This cannot be combined into a single generic function because "as" takes an implicit parameter which does not work with generics
    private def getRow(tableName : String, key : SATQueryPrimaryKey, identifier : SATQueryPrimaryKey => String) : Future[Option[SATQueryRecord]] =
        runAsync(sql"SELECT * FROM #$tableName WHERE #${identifier(key)}".as[SATQueryRecord].headOption)
    private def getRow(tableName : String, key : BDDQueryPrimaryKey, identifier : BDDQueryPrimaryKey => String) : Future[Option[BDDQueryRecord]] =
        runAsync(sql"SELECT * FROM #$tableName WHERE #${identifier(key)}".as[BDDQueryRecord].headOption)

    private def getSATQuery(key: SATQueryPrimaryKey) : Future[Option[SATQueryRecord]] = getRow(satQueriesTableName, key, bySATPrimaryKey)
    private def getSATQueryRegardlessOfMode(key: SATQueryPrimaryKey) : Future[Option[SATQueryRecord]] = getRow(satQueriesTableName, key, regardlessOfModeSAT)

    private def getBDDQuery(key: BDDQueryPrimaryKey) : Future[Option[BDDQueryRecord]] = getRow(bddQueriesTableName, key, byBDDPrimaryKey)
    private def getBDDQueryRegardlessOfMode(key: BDDQueryPrimaryKey) : Future[Option[BDDQueryRecord]] = getRow(bddQueriesTableName, key, regardlessOfModeBDD)

    /// Functions to insert rows

    private def logError(message : String) : Future[Int] = {
        val no : Int = errorCounter;
        errorCounter = errorCounter + 1;

        val sw = new StringWriter;
        (new Throwable()).printStackTrace(new PrintWriter(sw));

        runAsync(sqlu"insert into #$errorsTableName values (${VSATMissionControl.getSessionFile}, $no, $message, ${sw.toString});")
    }

    private def insertSATQuery(q : SATQueryRecord) : DBIO[Int] =
        sqlu"insert into #$satQueriesTableName values (${q.formula}, ${q.fmhash}, ${q.mode}, ${q.tcCacheHits}, ${q.dbCacheHits}, ${q.sentToSAT});"

    private def insertBDDQuery(q : BDDQueryRecord) : DBIO[Int] =
        sqlu"insert into #$bddQueriesTableName values (${q.hash}, ${q.fmhash}, ${q.mode}, ${q.tcCacheHits}, ${q.dbCacheHits}, ${q.sentToSAT});"

    // Invoked when the given key does not exist in the table but should do so.
    // This happens when we have a tcCacheHit but the row does not exist.
    // This can happen because the initial query was made in another mode (which is part of the primary key).
    // Maybe, there is an entry with the same formula and featuremodel but a different mode.
    // We saw that on text based logging:
    // Lots of queries were made during LEXING and had only cache hits in the later modes.
    // So instead of throwing an exception (which we will miss in the big text output):
    private def insertSATQueryThatWeSawPreviouslyInAnotherMode(key: SATQueryPrimaryKey) : Future [Int] =
    // 1.) Get the entries with the same formula and fm (but different mode).
    //     (If there is no such entry, we have a crucial bug! Maybe we should write that into an errors table!)
        getSATQueryRegardlessOfMode(key).flatMap {
            case Some(row) =>
                // 2.) Get the sentToSAT entry from these rows (should be the same because formula is equal for all).
                // 3.) Create a new entry with the given primary key and the retrieved sentToSAT value.
                runAsync(insertSATQuery(fromSATPrimaryKey(key, row.sentToSAT)))
            case None => logError("SAT query [" + key + "] is not stored in the database yet!");
        }

    private def insertBDDQueryThatWeSawPreviouslyInAnotherMode(key: BDDQueryPrimaryKey) : Future [Int] =
        getBDDQueryRegardlessOfMode(key).flatMap {
            case Some(row) => runAsync(insertBDDQuery(fromBDDPrimaryKey(key, row.sentToSAT)))
            case None => logError("BDD query [" + key + "] is not stored in the database yet!");
        }

    /// Functions to update rows

    private def updateAttribute[T, U](element : Future[Option[T]], tableName : String, identifier : T => String,
                                      attribName : String, update : T => U, errorCase : Future[Int]) : Future[Int] =
        element.flatMap {
            case Some(row) => {
                val location : String = identifier(row);
                val newAttribVal : String = update(row).toString;
                runAsync(sqlu"UPDATE #$tableName SET #$attribName = $newAttribVal WHERE #$location ;")
            };
            case None => errorCase;
        }

    private def updateSATAttribute[U](key: SATQueryPrimaryKey, attribName : String, update : SATQueryRecord => U, errorCase : Future[Int]) : Future[Int] =
        updateAttribute(getSATQuery(key), satQueriesTableName, bySATPrimaryKey compose toSATPrimaryKey,
            attribName, update, errorCase)

    private def updateBDDAttribute[U](key: BDDQueryPrimaryKey, attribName : String, update : BDDQueryRecord => U, errorCase : Future[Int]) : Future[Int] =
        updateAttribute(getBDDQuery(key), bddQueriesTableName, byBDDPrimaryKey compose toBDDPrimaryKey,
            attribName, update, errorCase)

    /// cache hit counters

    private def incTcCacheHits(key: SATQueryPrimaryKey) : Future[Int] =
        updateSATAttribute(key, "tcCacheHits", (s : SATQueryRecord) => 1 + s.tcCacheHits,
            insertSATQueryThatWeSawPreviouslyInAnotherMode(key))

    private def incDbCacheHits(key: SATQueryPrimaryKey) : Future[Int] =
        updateSATAttribute(key, "dbCacheHits", (s : SATQueryRecord) => 1 + s.dbCacheHits,
            insertSATQueryThatWeSawPreviouslyInAnotherMode(key))

    private def incTcCacheHits(key: BDDQueryPrimaryKey) : Future[Int] =
        updateBDDAttribute(key, "tcCacheHits", (b : BDDQueryRecord) => 1 + b.tcCacheHits,
            insertBDDQueryThatWeSawPreviouslyInAnotherMode(key))

    private def incDbCacheHits(key: BDDQueryPrimaryKey) : Future[Int] =
        updateBDDAttribute(key, "dbCacheHits", (b : BDDQueryRecord) => 1 + b.dbCacheHits,
            insertBDDQueryThatWeSawPreviouslyInAnotherMode(key))


    /// Debug SQL Queries ///

    private def showSatQueriesTable() : DBIO[Unit] = {
        val sep = " | ";
        sql"SELECT * FROM #$satQueriesTableName".as[SATQueryRecord].map { satqueries =>
            println("[VSATDatabase.showSatQueriesTable] SAT_QUERIES:")
            println(tableLine)
            println("  formula"+sep+"fmhash"+sep+"mode"+sep+"cacheHits"+sep+"sentToSAT")
            println(tableLine)
            for(q <- satqueries)
                println("  " + longStringPreview(q.formula) + sep + q.fmhash + sep + q.mode + sep + q.tcCacheHits + sep + q.dbCacheHits + sep + q.sentToSAT)
            println(tableLine + "\n")
        }
    }

    private def showFeatureModelsTable() : DBIO[Unit] = {
        val sep = " | ";
        sql"SELECT * FROM #$featureModelsTableName".as[FMRecord].map { fms =>
            println("[VSATDatabase.showFeatureModelsTable] FEATURE_MODELS:")
            println(tableLine)
            println("  hash"+sep+"formula")
            println(tableLine)
            for(fm <- fms)
                println("  " + fm.hash + sep + longStringPreview(fm.formula))
            println(tableLine + "\n")
        }
    }

    private def printTableNames() : DBIO[Unit] = {
        sql"""SELECT TABLE_NAME
               FROM INFORMATION_SCHEMA.TABLES
               WHERE TABLE_SCHEMA = 'PUBLIC'""".as[String].map {
            names =>
                println("[VSATDatabase.printTableNames]")
                for(t <- names)
                    println("  " + t)
        }
    }


    /// Util ///

    def longStringPreview(s : String) : String = {
        val limit = 20;
        if (s.length() > limit) {
            s.substring(0, limit-3) + "..."
        } else {
            s
        }
    }
}
