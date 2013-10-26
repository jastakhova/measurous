package web.snippet

/**
 * Created with IntelliJ IDEA.
 * User: Julia
 * Date: 10/25/13
 */
import com.google.api.client.auth.oauth2.Credential
import com.google.api.client.extensions.java6.auth.oauth2.AuthorizationCodeInstalledApp
import com.google.api.client.extensions.jetty.auth.oauth2.LocalServerReceiver
import com.google.api.client.googleapis.auth.oauth2.GoogleAuthorizationCodeFlow
import com.google.api.client.googleapis.auth.oauth2.GoogleClientSecrets
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport
import com.google.api.client.http.HttpTransport
import com.google.api.client.json.JsonFactory
import com.google.api.client.json.jackson2.JacksonFactory
import com.google.api.client.util.store.DataStoreFactory
import com.google.api.client.util.store.FileDataStoreFactory
import com.google.api.services.analytics.Analytics
import com.google.api.services.analytics.AnalyticsScopes

import com.google.api.services.analytics.model.GaData
import java.io.IOException
import java.io.InputStreamReader
import java.util.HashSet
import java.util.Set
import scala.collection.JavaConversions._

import net.liftweb.http._
import js.JE.ValById
import js.JsCmds.SetHtml
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._
import net.liftweb.http.js.JsCmds
import xml.NodeSeq
import web.AnalyticsData
import model.{EnrichedRow, EnrichedData, Dimension}

/**
 * Main class for the Google Analytics API command line sample.
 * Demonstrates how to make an authenticated API call using OAuth 2 helper classes.
 */
object AnalyticsLoader {

  /**
   * Be sure to specify the name of your application. If the application name is {@code null} or
   * blank, the application will log a warning. Suggested format is "MyCompany-ProductName/1.0".
   */
  val APPLICATION_NAME = ""

  /** Directory to store user credentials. */
  val DATA_STORE_DIR = new java.io.File(System.getProperty("user.home"), ".store/analytics_sample")

  /**
   * Global instance of the {@link DataStoreFactory}. The best practice is to make it a single
   * globally shared instance across your application.
   */
  var dataStoreFactory : Option[FileDataStoreFactory] = None

  /** Global instance of the JSON factory. */
  val JSON_FACTORY = JacksonFactory.getDefaultInstance()

  /** Global instance of the HTTP transport. */
  var httpTransport : Option[HttpTransport] = None

  var client : Option[Analytics] = None

  /** Authorizes the installed application to access user's protected data. */
  def authorize: Credential = {
    // load client secrets
    val clientSecrets = GoogleClientSecrets.load(JSON_FACTORY,
      new InputStreamReader(getClass.getClassLoader.getResourceAsStream("client_secrets.json")))
    if (clientSecrets.getDetails().getClientId().startsWith("Enter") ||
      clientSecrets.getDetails().getClientSecret().startsWith("Enter ")) {
      println(
        "Overwrite the src/main/resources/client_secrets.json file with the client secrets file "
          + "you downloaded from the Quickstart tool or manually enter your Client ID and Secret "
          + "from https://code.google.com/apis/console/?api=analytics#project:1050783689346 "
          + "into src/main/resources/client_secrets.json")
      System.exit(1)
    }

    // Set up authorization code flow.
    // Ask for only the permissions you need. Asking for more permissions will
    // reduce the number of users who finish the process for giving you access
    // to their accounts. It will also increase the amount of effort you will
    // have to spend explaining to users what you are doing with their data.
    // Here we are listing all of the available scopes. You should remove scopes
    // that you are not actually using.
    val scopes = new HashSet[String]()
    scopes.add(AnalyticsScopes.ANALYTICS)
    scopes.add(AnalyticsScopes.ANALYTICS_MANAGE_USERS)
    scopes.add(AnalyticsScopes.ANALYTICS_READONLY)

    val flow = new GoogleAuthorizationCodeFlow.Builder(httpTransport.get, JSON_FACTORY, clientSecrets, scopes)
      .setDataStoreFactory(dataStoreFactory.get)
      .build()
    // authorize
    new AuthorizationCodeInstalledApp(flow, new LocalServerReceiver()).authorize("user")
  }

  def load(startDate: String, endDate: String, fields: List[String], dimensions: List[String]): GaData  = {
      // initialize the transport
      httpTransport = Some(GoogleNetHttpTransport.newTrustedTransport())

      // initialize the data store factory
      dataStoreFactory = Some(new FileDataStoreFactory(DATA_STORE_DIR))

      // authorization
      val credential = authorize

      // set up global Analytics instance
      client = Some(new Analytics.Builder(httpTransport.get, JSON_FACTORY, credential)
        .setApplicationName(APPLICATION_NAME).build())

      val management = client.get.management()

      val accounts = management.accounts().list().execute()        //www.keplers.com
      val profileId = accounts.getItems.flatMap(account => management.webproperties().list(account.getId()).execute().getItems
        .flatMap(property => management.profiles().list(account.getId, property.getId).execute().getItems()
        .map(profile => profile.getId()))).head

      def toGA(fields: List[String]) = fields.map("ga:" + _).mkString(",")

      client.get.data().ga().get("ga:" + profileId, startDate, endDate, toGA(fields))
        .setDimensions(toGA(dimensions)).setSort("-" + toGA(List("visits"))).setMaxResults(100).execute()
  }

  def formTable(enriched: EnrichedData): List[List[String]] = AnalyticsData.currentLayout.layout match {
    case Left(tagLayouts) => enriched.data.groupBy(row => row.tags.intersect(tagLayouts.map(_.rule.name)))
      .filter(!_._1.isEmpty).map{case (tags, rows) =>
      tags.mkString(",") :: enriched.headers.map(header => rows.foldLeft(0){case (res, row) =>
        res + row.data.get(header).getOrElse("0").toInt}).map(_.toString)}.toList
    case Right(dimensionLayout) => enriched.data.map(row =>
      (dimensionLayout.dimension :: enriched.headers).flatMap(dimension => row.data.get(dimension)))
  }

  def dataBoardNode(data: GaData) = {
      val enriched = EnrichedData.fromRaw(data)

      <table class="table">
      <tr> <td class="notop"><strong>{AnalyticsData.currentLayout.getName}</strong></td>
        {
          enriched.headers.map(header => <td class="notop"><strong>{header.getUIName}</strong></td>)
        }
      </tr>
      {
        formTable(enriched).take(10).map(row =>
          <tr>
            {
              row.map(value => <td>{value}</td>)
            }
          </tr>
        )
      }
    </table>
  }

  def startDate = "2013-09-25"
  def endDate = "2013-10-25"

  def render = {
    val data = load(startDate, endDate, List("visits", "visitors", "bounces"), List("source", "keyword", "medium"))
    "@dataBoard" #> dataBoardNode(data) &
    "@startDate" #> text(startDate, ValById(_), "type" -> "date") &
    "@endDate" #> text(endDate, ValById(_), "type" -> "date") &
    "@dataName" #> <h3>Report for profile "www.keplers.com"</h3>
  }

  def main(args: Array[String]) {
    println(formTable(EnrichedData.fromRaw(load(startDate, endDate, List("visits"), List("source", "keyword", "medium")))))
  }
}