package de.fosd.typechef.featureexpr.sat

// Use H2Profile to connect to an H2 database
import slick.jdbc.H2Profile.api._

// This is deprecated:
//import slick.driver.H2Driver.api._

//import scala.slick.driver.H2Driver.simple._

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
    private val noFeatureModel : String = "NoFeatureModel";
    private val tableLine : String = "----------------------------------------";

    // Nor these
    private var running : Boolean = false;
    private var db : Database = null;

    /// TODO:
    /// Currently, we often wait for futures to complete.
    /// Instead we should use monadic bind to queue new actions asynchronously.
    /// I just wanted to get it working first and fancify it afterwards.

    /// TODO:
    /// There is lots of clone-and-own in the queries.
    /// We could try to get rid of that but we dont want to win a price so that's low priority.

    def init() : Boolean = {
        println("[VSATDatabase.init]");
        if (running) {
            println("[Database.init] Database connection is already established.")
            return false
        }

        db =
//            Database.forConfig("h2inmem")
            Database.forConfig(databaseProfileToUse)


        setupTable(satQueriesTableName, createSatQueriesTable _, dropSatQueriesTable _);
        setupTable(bddQueriesTableName, createBddQueriesTable _, dropBddQueriesTable _);
        setupTable(featureModelsTableName, createFeatureModelsTable _, dropFeatureModelsTable _);

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

    def setupTable(name : String, createTableQuery : () => DBIO[Int], dropTableQuery : () => DBIO[Int]): Unit = {
        // Creating tables synchronously and crash if something fails.
        var exists : Boolean = evalForced(tableExists(name));
        if (exists && VSATMissionControl.isFirstRun()) {
            println("[VSATDatabase.init] Cleaning existing table " + name + " because this is the first run.");
            runSyncForced(dropTableQuery());
            exists = false;
        }
        if (!exists) {
            println("[VSATDatabase.init] Creating table " + name + ".");
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

    def fromPrimaryKey(k : SATQueryPrimaryKey, sentToSAT : Boolean) : SATQueryRecord =
        SATQueryRecord(k.formula, k.fmhash, k.mode, 0, 0, sentToSAT)

    case class BDDQueryPrimaryKey(hash : String, fmhash : String, mode : String)
    case class BDDQueryRecord(hash : String, fmhash : String, mode : String, tcCacheHits : Int, dbCacheHits : Int, sentToSAT : Boolean);
    implicit val getBDDQueryRecordResult = GetResult(r => BDDQueryRecord(r.<<, r.<<, r.<<, r.<<, r.<<, r.<<))

    def fromPrimaryKey(k : BDDQueryPrimaryKey, sentToSAT : Boolean) : BDDQueryRecord =
        BDDQueryRecord(k.hash, k.fmhash, k.mode, 0, 0, sentToSAT)

    /// VSAT logging ///

    def sat_cache_hit(the_query: SATFeatureExpr, featureModel: SATFeatureModel): Unit = {
        //println("[Database.sat_cache_hit]")
        incTcCacheHits(
            SATQueryPrimaryKey(
                the_query.toString,
                VSATMissionControl.hash(featureModel),
                VSATMissionControl.getCurrentMode().toString));
    }

    def sat_record_query(the_query: SATFeatureExpr, featureModel: SATFeatureModel, sentToSat : Boolean): Unit = {
        // We assume that the given query is not yet stored in the DB
        // If it is, we have a cache hit that was missed by TypeChef.
        // This might happen due to repeated runs of TypeChef.
        // If we get such a cache hit, we store it in dbCacheHits.

        // 1.) Store FM if not stored already
        var fmHash : String = VSATMissionControl.hash(featureModel)
        if (featureModel == SATNoFeatureModel) {
            //println("[Database.sat_record_query] with SATNoFeatureModel");
            fmHash = noFeatureModel;
        } else {
            val existingFM : Option[FMRecord] = runSyncForced(sql"SELECT * FROM FEATUREMODELS WHERE hash = $fmHash".as[FMRecord].headOption);
            if (existingFM.isEmpty) {
                val fmFormula = featureModel.decreate().toString();
                val fmrecord = FMRecord(fmHash, fmFormula);
                if (VSATMissionControl.DEBUG) {
                    println("[Database.sat_record_query] inserting new feature model " + fmrecord.hash);
                }
                runAsync(sqlu"INSERT INTO FEATUREMODELS VALUES (${fmrecord.hash}, ${fmrecord.formula});");
            }
        }

        // 2.) Store the query or update it if we have an accidental cache hit
        val key = SATQueryPrimaryKey(the_query.toString, fmHash, VSATMissionControl.getCurrentMode().toString)

        val existingQuery : Option[SATQueryRecord] = evalForced(getSATQuery(key));
        if (existingQuery.isEmpty) {
            // The default case: This is a new query and we store it
            runAsync(insertSATQuery(fromPrimaryKey(key, sentToSat)));
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

    def bdd_cache_hit(the_query: BDDFeatureExpr, featureModel: BDDFeatureModel, metadata : VSATBDDQueryMetadata) : Unit = {
//        hashCode
        incTcCacheHits(
            BDDQueryPrimaryKey(
                "" + the_query.hashCode,
                "nokey_"+VSATMissionControl.hash(featureModel),
                VSATMissionControl.getCurrentMode().toString));
    }

    def bdd_record_query(the_query: BDDFeatureExpr, featureModel: BDDFeatureModel, metadata : VSATBDDQueryMetadata) : Unit = {
        // ignore the feature mode for now
        val key = BDDQueryPrimaryKey("" + the_query.hashCode, "nokey_"+VSATMissionControl.hash(featureModel), VSATMissionControl.getCurrentMode().toString)

        val existingQuery : Option[BDDQueryRecord] = evalForced(getBDDQuery(key));
        if (existingQuery.isEmpty) {
            // The default case: This is a new query and we store it
            runAsync(insertBDDQuery(fromPrimaryKey(key, metadata.sentToSat)));
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

    /// Database utility ///

    private def runAsync[T](action : DBIO[T]) : Future[T] = db.run(action)
    private def runSync[T](action : DBIO[T]) : Try[T] = eval(runAsync(action))
    // Like runSync but throws an exception if the action failed
    private def runSyncForced[T](action : DBIO[T]) : T = evalForced(runAsync(action))

    private def eval[T](f : Future[T]) : Try[T] = Await.ready(f, Duration.Inf).value.get
    // like eval but throws an exception when the future fails
    private def evalForced[T](f : Future[T]) : T = Await.result(f, Duration.Inf)


    /// SQL Queries ///

    private def dropSatQueriesTable() : DBIO[Int] = sqlu"DROP TABLE SATQUERIES"
    private def dropBddQueriesTable() : DBIO[Int] = sqlu"DROP TABLE BDDQUERIES"
    private def dropFeatureModelsTable() : DBIO[Int] = sqlu"DROP TABLE FEATUREMODELS"

    /// sqlu produces a DBIO[Int] action where the returned int is the row cound (I guess)
    private def createSatQueriesTable() : DBIO[Int] =
        sqlu"""CREATE TABLE SATQUERIES (
               formula varchar(max) NOT NULL,
               fmhash varchar(255) NOT NULL,
               mode varchar(50) NOT NULL,
               tcCacheHits int NOT NULL,
               dbCacheHits int NOT NULL,
               sentToSAT bool NOT NULL,
               PRIMARY KEY(formula, fmhash, mode)
               );"""//CONSTRAINT pkey

    private def createFeatureModelsTable() : DBIO[Int] =
        sqlu"""CREATE TABLE FEATUREMODELS (
               hash varchar(255) NOT NULL,
               formula varchar(max) NOT NULL,
               PRIMARY KEY(hash)
               );"""
    
    private def createBddQueriesTable() : DBIO[Int] =
        sqlu"""CREATE TABLE BDDQUERIES (
               hash varchar(255) NOT NULL,
               fmhash varchar(255) NOT NULL,
               mode varchar(50) NOT NULL,
               tcCacheHits int NOT NULL,
               dbCacheHits int NOT NULL,
               sentToSAT bool NOT NULL,
               PRIMARY KEY(hash, fmhash, mode)
               );"""//CONSTRAINT pkey

    private def tableExists(tablename : String) : Future[Boolean] = {
        runAsync(sql"""SELECT TABLE_NAME
               FROM INFORMATION_SCHEMA.TABLES
               WHERE TABLE_SCHEMA = 'PUBLIC'
               AND TABLE_NAME = $tablename""".as[String].headOption // look at this ugly method name 'headoption'
        ).map(o => o.nonEmpty)
    }

    private def getSATQuery(key: SATQueryPrimaryKey) : Future[Option[SATQueryRecord]] =
        runAsync(sql"SELECT * FROM SATQUERIES WHERE formula = ${key.formula} AND fmhash = ${key.fmhash} AND mode = ${key.mode}".as[SATQueryRecord].headOption)

    private def insertSATQuery(q : SATQueryRecord) : DBIO[Int] =
        sqlu"insert into SATQUERIES values (${q.formula}, ${q.fmhash}, ${q.mode}, ${q.tcCacheHits}, ${q.dbCacheHits}, ${q.sentToSAT});"

    private def getBDDQuery(key: BDDQueryPrimaryKey) : Future[Option[BDDQueryRecord]] =
        runAsync(sql"SELECT * FROM BDDQUERIES WHERE hash = ${key.hash} AND fmhash = ${key.fmhash} AND mode = ${key.mode}".as[BDDQueryRecord].headOption)

    private def insertBDDQuery(q : BDDQueryRecord) : DBIO[Int] =
        sqlu"insert into BDDQUERIES values (${q.hash}, ${q.fmhash}, ${q.mode}, ${q.tcCacheHits}, ${q.dbCacheHits}, ${q.sentToSAT});"

    private def incTcCacheHits(key: SATQueryPrimaryKey) : Future[Int] = {
        getSATQuery(key).flatMap {
            case Some(satQuery) => {
                val newTcCacheHits = 1 + satQuery.tcCacheHits;
                runAsync(
                    sqlu"""UPDATE SATQUERIES
                          SET tcCacheHits=$newTcCacheHits
                          WHERE formula = ${satQuery.formula}
                          AND fmhash = ${satQuery.fmhash}
                          AND mode = ${satQuery.mode};""")
            };
            case None => throw new Exception("[VSATDatabase.incTcCacheHits] The given query is not stored in the database yet!");
        }
    }

    // #clone-and-own
    private def incDbCacheHits(key: SATQueryPrimaryKey) : Future[Int] = {
        getSATQuery(key).flatMap {
            case Some(satQuery) => {
                val newDbCacheHits = 1 + satQuery.dbCacheHits;
                runAsync(
                    sqlu"""UPDATE SATQUERIES
                          SET dbCacheHits=$newDbCacheHits
                          WHERE formula = ${satQuery.formula}
                          AND fmhash = ${satQuery.fmhash}
                          AND mode = ${satQuery.mode};""")
            };
            case None => throw new Exception("[VSATDatabase.incDbCacheHits] The given query is not stored in the database yet!");
        }
    }

    private def incTcCacheHits(key: BDDQueryPrimaryKey) : Future[Int] = {
        getBDDQuery(key).flatMap {
            case Some(bddQuery) => {
                val newTcCacheHits = 1 + bddQuery.tcCacheHits;
                runAsync(
                    sqlu"""UPDATE BDDQUERIES
                          SET tcCacheHits=$newTcCacheHits
                          WHERE hash = ${bddQuery.hash}
                          AND fmhash = ${bddQuery.fmhash}
                          AND mode = ${bddQuery.mode};""")
            };
            case None => throw new Exception("[VSATDatabase.incTcCacheHits] The given query is not stored in the database yet!");
        }
    }

    // #clone-and-own
    private def incDbCacheHits(key: BDDQueryPrimaryKey) : Future[Int] = {
        getBDDQuery(key).flatMap {
            case Some(bddQuery) => {
                val newDbCacheHits = 1 + bddQuery.dbCacheHits;
                runAsync(
                    sqlu"""UPDATE BDDQUERIES
                          SET dbCacheHits=$newDbCacheHits
                          WHERE hash = ${bddQuery.hash}
                          AND fmhash = ${bddQuery.fmhash}
                          AND mode = ${bddQuery.mode};""")
            };
            case None => throw new Exception("[VSATDatabase.incDbCacheHits] The given query is not stored in the database yet!");
        }
    }


    /// Debug SQL Queries ///

    private def showSatQueriesTable() : DBIO[Unit] = {
        val sep = " | ";
        sql"SELECT * FROM SATQUERIES".as[SATQueryRecord].map { satqueries =>
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
        sql"SELECT * FROM FEATUREMODELS".as[FMRecord].map { fms =>
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


    /// Test SQL Queries ///

    private def createTestTable() : DBIO[Unit] = {
        DBIO.seq(sqlu"CREATE TABLE TEST (id int NOT NULL, name varchar(255) NOT NULL);")
    }

    private def showTestTable() : DBIO[Unit] = {
        sql"SELECT * FROM TEST".as[(Int, String)].map { persons =>
            println("Persons:")
            for((id, name) <- persons)
                println("* " + id + ", " + name)
        }
    }

    private var test_id = 1;
    private def insertAgentSmithIntoTest(): DBIO[Unit] = {
        test_id += 1;
        DBIO.seq(sqlu"INSERT INTO TEST VALUES (${test_id}, 'Agent Smith');")
    }
}
