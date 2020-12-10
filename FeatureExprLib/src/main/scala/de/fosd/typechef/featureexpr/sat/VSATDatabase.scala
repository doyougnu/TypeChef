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

object VSATDatabase {
    val queriesTableName : String = "Queries";
    val featureModelsTableName : String = "FeatureModels";
    val noFeatureModel : String = "NoFeatureModel"

    val dbDebug : Boolean = false;

    var running : Boolean = false;
    var db : Database = null;

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

        db = Database.forConfig("h2mem1")

//        try {
//            /**
//             *  :profiles/prod {
//             *      :env {
//             *          :database-url "jdbc:postgresql://uqigickuuzalxs:4a983bbd18d1788fc5187d0f24ab6c37b33dabdab444293723dab3601f095ad2@ec2-184-73-196-65.compute-1.amazonaws.com:5432/d2t468ltf9ppbg"
//             *          :neo4j-db-url "https://app87302872-N2QaDg:b.4rsLGYPKQfiz.w7QaJuQk1c4ocmxv@hobby-cjlamlpgpijggbkenjcimnal.dbs.graphenedb.com:24780"
//             *      }
//             *  }
//             */
//            db = Database.forURL(
//                //"jdbc:h2:C:/Users/Paul Bittner/Documents/Software/VSAT/typechefqueries;DB_CLOSE_DELAY=-1",
//                //"jdbc:h2://sa:vsat@192.168.1.227:8082/typechefqueries",
//                // jdbc:h2:file:~/sample;USER=sa;PASSWORD=123
////                "jdbc:h2:/mnt/c/Users/Paul\\ Bittner/Documents/Software/VSAT/typechefqueries",
////                "jdbc:h2:file:/mnt/c/Users/Paul\\ Bittner/Documents/Software/VSAT;databaseName=typechefqueries;USER=sa;PASSWORD=vsat",
//                "jdbc:h2:mem:test1",
//                user = "sa",
//                password = "vsat",
//                //"thisTestURLCannotWork",
//                driver = "org.h2.Driver"
////                connectionPool = "disabled"
////              driver = "scala.slick.driver.H2Driver"
//            );
//
//            if (db != null) {
//                println("[Database.init] Database connection established");
//                running = true;
//            } else {
//                running = false;
//                println("[Database.init] Database connection failed due to a yet unknown reason.");
//            }
//        } catch {
//            case e: Exception => {
//                println("[Database.init] Database connection failed. Reason:");
//                println(e);
//                running = false;
//            }
//        }


        // Creating tables synchronously and crash if something fails.
        if (!evalForced(tableExists(queriesTableName))) {
            runSyncForced(createQueriesTable())
        }
        if (!evalForced(tableExists(featureModelsTableName))) {
            runSyncForced(createFeatureModelsTable())
        }
        // Create tables asynchronously and fail silently. This might be a bad idea.
        //        tableExists(queriesTableName).foreach(exists => {
        //            if (!exists) {
        //                runAsync(createQueriesTable())
        //            }
        //        })
        //
        //        tableExists(featureModelsTableName).foreach(exists => {
        //            if (!exists) {
        //                runAsync(createFeatureModelsTable())
        //            }
        //        })

//        runSyncForced(printTableNames());

        running
    }

    def terminate() : Boolean = {
        runSync(showQueriesTable());
        runSync(showFeatureModelsTable());
        db.close();
        println("[Database.terminate] Database connection terminated")
        running = false
        running
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


    /// VSAT logging ///

    def cache_hit(the_query: SATFeatureExpr, featureModel: SATFeatureModel): Unit = {
        //println("[Database.cache_hit]")
        incTcCacheHits(
            SATQueryPrimaryKey(
                the_query.toString,
                featureModel.toString,
                VSATMissionControl.getCurrentMode().toString));
    }

    def record_query(the_query: SATFeatureExpr, featureModel: SATFeatureModel, sentToSat : Boolean): Unit = {
        // We assume that the given query is not yet stored in the DB
        // If it is, we have a cache hit that was missed by TypeChef.
        // This might happen due to repeated runs of TypeChef.
        // If we get such a cache hit, we store it in dbCacheHits.

        // 1.) Store FM if not stored already
        var fmHash : String = featureModel.toString
        if (featureModel == SATNoFeatureModel) {
            //println("[Database.record_query] with SATNoFeatureModel");
            fmHash = noFeatureModel;
        } else {
            val existingFM : Option[FMRecord] = runSyncForced(sql"SELECT * FROM FeatureModels WHERE hash = $fmHash".as[FMRecord].headOption);
            if (existingFM.isEmpty) {
                val fmrecord = FMRecord(fmHash, featureModel.decreate().toString());
                println("[Database.record_query] inserting new feature model " + fmrecord.hash);
                runSyncForced(sqlu"INSERT INTO FeatureModels VALUES (${fmrecord.hash}, ${fmrecord.formula});");
            } else {
                //println("[Database.record_query] fm " + fmHash + " is already stored in db.");
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
            // Hyothesis: This happens because TypeChef might discard its cache inbetween different runs of the main method.
            //println("[VSATDatabase.record_query] WE ACTUALLY GOT A CACHE HIT! AND NOW IMPLEMENT A WAY TO HANDLE THAT!")
            incDbCacheHits(key);
        }
    }


    /// Database utility ///

    def runAsync[T](action : DBIO[T]) : Future[T] = {
        val f: Future[T] = db.run(action);

        if (dbDebug) {
            f.onComplete {
                case Success(s) => println(s"Result: $s")
                case Failure(t) => t.printStackTrace()
            }
        }

        f
    }

    def runSync[T](action : DBIO[T]) : Try[T] = eval(runAsync(action))
    // Like runSync but throws an exception if the action failed
    def runSyncForced[T](action : DBIO[T]) : T = evalForced(runAsync(action))

    def eval[T](f : Future[T]) : Try[T] = Await.ready(f, Duration.Inf).value.get
    // like eval but throws an exception when the future fails
    def evalForced[T](f : Future[T]) : T = Await.result(f, Duration.Inf)


    /// SQL Queries ///

    /// sqlu produces a DBIO[Int] action where the returned int is the row cound (I guess)
    def createQueriesTable() : DBIO[Int] =
        sqlu"""CREATE TABLE Queries (
               formula varchar(max) NOT NULL,
               fmhash varchar(255) NOT NULL,
               mode varchar(50) NOT NULL,
               tcCacheHits int NOT NULL,
               dbCacheHits int NOT NULL,
               sentToSAT bool NOT NULL,
               CONSTRAINT pkey PRIMARY KEY(formula, fmhash, mode)
               );"""

    def createFeatureModelsTable() : DBIO[Int] =
        sqlu"""CREATE TABLE FeatureModels (
               hash varchar(255) NOT NULL,
               formula varchar(max) NOT NULL,
               PRIMARY KEY(hash)
               );"""

    def tableExists(tablename : String) : Future[Boolean] = {
        runAsync(sql"""SELECT TABLE_NAME
               FROM INFORMATION_SCHEMA.TABLES
               WHERE TABLE_SCHEMA = 'PUBLIC'
               AND TABLE_NAME = $tablename""".as[String].headOption // look at this ugly method name 'headoption'
        ).map(o => o.nonEmpty)
    }

    def getSATQuery(key: SATQueryPrimaryKey) : Future[Option[SATQueryRecord]] =
        runAsync(sql"SELECT * FROM Queries WHERE formula = ${key.formula} AND fmhash = ${key.fmhash} AND mode = ${key.mode}".as[SATQueryRecord].headOption)

    def insertSATQuery(q : SATQueryRecord) : DBIO[Int] =
        sqlu"insert into Queries values (${q.formula}, ${q.fmhash}, ${q.mode}, ${q.tcCacheHits}, ${q.dbCacheHits}, ${q.sentToSAT});"

    def incTcCacheHits(key: SATQueryPrimaryKey) : Future[Int] = {
        getSATQuery(key).flatMap {
            case Some(satQuery) => {
                val newTcCacheHits = 1 + satQuery.tcCacheHits;
                runAsync(
                    sqlu"""UPDATE Queries
                          SET tcCacheHits=$newTcCacheHits
                          WHERE formula = ${satQuery.formula}
                          AND fmhash = ${satQuery.fmhash}
                          AND mode = ${satQuery.mode};""")
            };
            case None => throw new Exception("[VSATDatabase.incTcCacheHits] The given query is not stored in the database yet!");
        }
    }

    // #clone-and-own
    def incDbCacheHits(key: SATQueryPrimaryKey) : Future[Int] = {
        getSATQuery(key).flatMap {
            case Some(satQuery) => {
                val newDbCacheHits = 1 + satQuery.dbCacheHits;
                runAsync(
                    sqlu"""UPDATE Queries
                          SET dbCacheHits=$newDbCacheHits
                          WHERE formula = ${satQuery.formula}
                          AND fmhash = ${satQuery.fmhash}
                          AND mode = ${satQuery.mode};""")
            };
            case None => throw new Exception("[VSATDatabase.incDbCacheHits] The given query is not stored in the database yet!");
        }
    }


    /// Debug SQL Queries ///

    def showQueriesTable() : DBIO[Unit] = {
        val sep = " | ";
        sql"SELECT * FROM Queries".as[SATQueryRecord].map { satqueries =>
            println("[VSATDatabase.showQueriesTable] SAT_QUERIES:")
            println("  formula"+sep+"fmhash"+sep+"mode"+sep+"cacheHits"+sep+"sentToSAT")
            for(q <- satqueries)
                println("  " + longStringPreview(q.formula) + sep + q.fmhash + sep + q.mode + sep + q.tcCacheHits + sep + q.dbCacheHits + sep + q.sentToSAT)
        }
    }

    def showFeatureModelsTable() : DBIO[Unit] = {
        val sep = " | ";
        sql"SELECT * FROM FeatureModels".as[FMRecord].map { fms =>
            println("[VSATDatabase.showFeatureModelsTable] FEATURE_MODELS:")
            println("  hash"+sep+"formula")
            for(fm <- fms)
                println("  " + fm.hash + sep + longStringPreview(fm.formula))
        }
    }

    def printTableNames() : DBIO[Unit] = {
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

    def createTestTable() : DBIO[Unit] = {
        DBIO.seq(sqlu"CREATE TABLE TEST (id int NOT NULL, name varchar(255) NOT NULL);")
    }

    def showTestTable() : DBIO[Unit] = {
        sql"SELECT * FROM TEST".as[(Int, String)].map { persons =>
            println("Persons:")
            for((id, name) <- persons)
                println("* " + id + ", " + name)
        }
    }

    var test_id = 1;
    def insertAgentSmithIntoTest(): DBIO[Unit] = {
        test_id += 1;
        DBIO.seq(sqlu"INSERT INTO TEST VALUES (${test_id}, 'Agent Smith');")
    }
}
