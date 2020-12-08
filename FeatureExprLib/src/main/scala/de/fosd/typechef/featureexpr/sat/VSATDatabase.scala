package de.fosd.typechef.featureexpr.sat

// Use H2Profile to connect to an H2 database
import slick.jdbc.H2Profile.api._

// This is deprecated:
//import slick.driver.H2Driver.api._

//import scala.slick.driver.H2Driver.simple._

import scala.concurrent.Future
import scala.util.{Success,Failure}
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * The shape of the VSAT database is of the following:
 *
 * CREATE TABLE Queries (
 *     hash varchar(255) NOT NULL,
 *     formula varchar(255) NOT NULL,
 *     occurences int NOT NULL,
 *     fmhash varchar(255) NOT NULL,
 *     cacheHits varchar(255) NOT NULL,
 *     sentToSAT bool NOT NULL,
 *     CONSTRAINT pkey PRIMARY KEY(hash, fmhash)
 * );
 *
 * CREATE TABLE FeatureModels (
 *     hash varchar(255) NOT NULL,
 *     formula varchar(255) NOT NULL,
 *     PRIMARY KEY(hash)
 * );
 */
object VSATDatabase {
    var running : Boolean = false;
    var db : Database = null;

    def init() : Boolean = {
        if (running) {
            println("[Database.init] Database connection is already established.")
            return false
        }

        try {
            /**
             *  :profiles/prod {
             *      :env {
             *          :database-url "jdbc:postgresql://uqigickuuzalxs:4a983bbd18d1788fc5187d0f24ab6c37b33dabdab444293723dab3601f095ad2@ec2-184-73-196-65.compute-1.amazonaws.com:5432/d2t468ltf9ppbg"
             *          :neo4j-db-url "https://app87302872-N2QaDg:b.4rsLGYPKQfiz.w7QaJuQk1c4ocmxv@hobby-cjlamlpgpijggbkenjcimnal.dbs.graphenedb.com:24780"
             *      }
             *  }
             */
            db = Database.forURL(
                //"jdbc:h2:C:/Users/Paul Bittner/Documents/Software/VSAT/typechefqueries;DB_CLOSE_DELAY=-1",
                //"jdbc:h2://sa:vsat@192.168.1.227:8082/typechefqueries",
                // jdbc:h2:file:~/sample;USER=sa;PASSWORD=123
//                "jdbc:h2:/mnt/c/Users/Paul\\ Bittner/Documents/Software/VSAT/typechefqueries",
//                "jdbc:h2:file:/mnt/c/Users/Paul\\ Bittner/Documents/Software/VSAT;databaseName=typechefqueries;USER=sa;PASSWORD=vsat",
                "jdbc:h2:mem:test1",
                user = "sa",
                password = "vsat",
                //"thisTestURLCannotWork",
                driver = "org.h2.Driver",
//                connectionPool = "disabled"
//              driver = "scala.slick.driver.H2Driver"
            );

            if (db != null) {
                println("[Database.init] Database connection established");
                running = true;
            } else {
                running = false;
                println("[Database.init] Database connection failed due to a yet unknown reason.");
            }
        } catch {
            case e: Exception => {
                println("[Database.init] Database connection failed. Reason:");
                println(e);
                running = false;
            }
        }

        running
    }

    def terminate() : Boolean = {
//        println("[Database.init] Database connection terminated")
//        db.close();
//        running = false
        running
    }


    var test_id = 1;
    def insertAgentSmithIntoTest(): DBIO[Unit] = {
        test_id += 1;
        DBIO.seq(sqlu"INSERT INTO TEST VALUES (${test_id}, 'Agent Smith');")
    }

    def spawnAgentSmith() : Unit = {
        val f: Future[Unit] = db.run(insertAgentSmithIntoTest());

        f.onComplete {
            case Success(s) => println(s"Result: $s")
            case Failure(t) => t.printStackTrace()
        }
    }

    def cache_hit(the_query: SATFeatureExpr, featureModel: SATFeatureModel): Unit = {
        //println("[Database.cache_hit]")
        if (!running) {
            init();
        }
        spawnAgentSmith();
    }

    def record_query(the_query: SATFeatureExpr, featureModel: SATFeatureModel, sentToSat : Boolean): Unit = {
        //println("[Database.query_test]")
        if (!running) {
            init();
        }
        spawnAgentSmith();
    }
}
