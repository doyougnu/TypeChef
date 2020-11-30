package de.fosd.typechef.featureexpr.sat

// Use H2Profile to connect to an H2 database
import slick.jdbc.H2Profile.api._

import scala.concurrent.ExecutionContext.Implicits.global

/**
 * The shape of the VSAT database is of the following:
 *
 * CREATE TABLE Queries (
 *     hash varchar(255) NOT NULL,
 *     formula varchar(255) NOT NULL,
 *     occurences int NOT NULL,
 *     fmhash varchar(255) NOT NULL,
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

    var test_id = 1;

    def insertTest(): DBIO[Unit] = {
        test_id += 1;
        DBIO.seq(sqlu"INSERT INTO Test VALUES (${test_id}, Agent Smith})")
    }

    def init() : Boolean = {
        if (running) {
            println("[Database.init] Database connection is already established.")
            return false
        }

        db = Database.forURL(
            //"jdbc:h2:C:/Users/Paul Bittner/Documents/Software/VSAT/typechefqueries;DB_CLOSE_DELAY=-1",
            "http://sa:vsat@192.168.1.227:8082",
            driver="org.h2.Driver")

        println("[Database.init] Database connection established")
        running = true
        true
    }

    def terminate() : Boolean = {
        println("[Database.init] Database connection terminated")
        db.close();
        running = false
        running
    }

    def query_test() : Unit = {
        println("[Database.query_test]")
        if (!running) {
            init();
        }
        db.run(insertTest());
    }
}