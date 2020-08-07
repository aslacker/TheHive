package org.thp.elastictool

import java.nio.file.{Files, Paths}
import java.security.KeyStore

import com.sksamuel.elastic4s.{ElasticClient, ElasticNodeEndpoint, ElasticProperties}
import com.sksamuel.elastic4s.http.JavaClient
import com.typesafe.config.Config
import javax.net.ssl.{KeyManagerFactory, SSLContext, TrustManagerFactory}
import org.apache.http.HttpHost
import org.apache.http.auth.{AuthScope, UsernamePasswordCredentials}
import org.apache.http.client.CredentialsProvider
import org.apache.http.client.config.RequestConfig
import org.apache.http.impl.client.BasicCredentialsProvider
import org.apache.http.impl.nio.client.HttpAsyncClientBuilder
import org.elasticsearch.client.{Request, RestClient}
import org.elasticsearch.client.RestClientBuilder.{HttpClientConfigCallback, RequestConfigCallback}
import play.api.Configuration
import play.api.libs.json.{JsObject, Json}

import scala.util.parsing.combinator._
import scala.collection.JavaConverters._
import scala.collection.immutable

class ElasticSearchClient(config: Configuration) {

  def requestConfigCallback: RequestConfigCallback = (requestConfigBuilder: RequestConfig.Builder) ⇒ {
    requestConfigBuilder.setAuthenticationEnabled(credentialsProviderMaybe.isDefined)
    config.getOptional[Boolean]("search.circularRedirectsAllowed").foreach(requestConfigBuilder.setCircularRedirectsAllowed)
    config.getOptional[Int]("search.connectionRequestTimeout").foreach(requestConfigBuilder.setConnectionRequestTimeout)
    config.getOptional[Int]("search.connectTimeout").foreach(requestConfigBuilder.setConnectTimeout)
    config.getOptional[Boolean]("search.contentCompressionEnabled").foreach(requestConfigBuilder.setContentCompressionEnabled)
    config.getOptional[String]("search.cookieSpec").foreach(requestConfigBuilder.setCookieSpec)
    config.getOptional[Boolean]("search.expectContinueEnabled").foreach(requestConfigBuilder.setExpectContinueEnabled)
    //    config.getOptional[InetAddress]("search.localAddress").foreach(requestConfigBuilder.setLocalAddress)
    config.getOptional[Int]("search.maxRedirects").foreach(requestConfigBuilder.setMaxRedirects)
    //    config.getOptional[Boolean]("search.proxy").foreach(requestConfigBuilder.setProxy)
    config.getOptional[Seq[String]]("search.proxyPreferredAuthSchemes").foreach(v ⇒ requestConfigBuilder.setProxyPreferredAuthSchemes(v.asJava))
    config.getOptional[Boolean]("search.redirectsEnabled").foreach(requestConfigBuilder.setRedirectsEnabled)
    config.getOptional[Boolean]("search.relativeRedirectsAllowed").foreach(requestConfigBuilder.setRelativeRedirectsAllowed)
    config.getOptional[Int]("search.socketTimeout").foreach(requestConfigBuilder.setSocketTimeout)
    config.getOptional[Seq[String]]("search.targetPreferredAuthSchemes").foreach(v ⇒ requestConfigBuilder.setTargetPreferredAuthSchemes(v.asJava))
    requestConfigBuilder
  }

  lazy val credentialsProviderMaybe: Option[CredentialsProvider] =
    for {
      user     ← config.getOptional[String]("search.user")
      password ← config.getOptional[String]("search.password")
    } yield {
      val provider    = new BasicCredentialsProvider
      val credentials = new UsernamePasswordCredentials(user, password)
      provider.setCredentials(AuthScope.ANY, credentials)
      provider
    }

  lazy val sslContextMaybe: Option[SSLContext] = config.getOptional[String]("search.keyStore.path").map { keyStore ⇒
    val keyStorePath     = Paths.get(keyStore)
    val keyStoreType     = config.getOptional[String]("search.keyStore.type").getOrElse(KeyStore.getDefaultType)
    val keyStorePassword = config.getOptional[String]("search.keyStore.password").getOrElse("").toCharArray
    val keyInputStream   = Files.newInputStream(keyStorePath)
    val keyManagers = try {
      val keyStore = KeyStore.getInstance(keyStoreType)
      keyStore.load(keyInputStream, keyStorePassword)
      val kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm)
      kmf.init(keyStore, keyStorePassword)
      kmf.getKeyManagers
    } finally {
      keyInputStream.close()
    }

    val trustManagers = config
      .getOptional[String]("search.trustStore.path")
      .map { trustStorePath ⇒
        val keyStoreType       = config.getOptional[String]("search.trustStore.type").getOrElse(KeyStore.getDefaultType)
        val trustStorePassword = config.getOptional[String]("search.trustStore.password").getOrElse("").toCharArray
        val trustInputStream   = Files.newInputStream(Paths.get(trustStorePath))
        try {
          val keyStore = KeyStore.getInstance(keyStoreType)
          keyStore.load(trustInputStream, trustStorePassword)
          val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
          tmf.init(keyStore)
          tmf.getTrustManagers
        } finally {
          trustInputStream.close()
        }
      }
      .getOrElse(Array.empty)

    // Configure the SSL context to use TLS
    val sslContext = SSLContext.getInstance("TLS")
    sslContext.init(keyManagers, trustManagers, null)
    sslContext
  }

  def httpClientConfig: HttpClientConfigCallback = (httpClientBuilder: HttpAsyncClientBuilder) ⇒ {
    sslContextMaybe.foreach(httpClientBuilder.setSSLContext)
    credentialsProviderMaybe.foreach(httpClientBuilder.setDefaultCredentialsProvider)
    httpClientBuilder
  }

  val client: RestClient = {
    val props = ElasticProperties(config.get[String]("search.uri"))
    val hosts = props.endpoints.map {
      case ElasticNodeEndpoint(protocol, host, port, _) ⇒ new HttpHost(host, port, protocol)
    }
    RestClient
      .builder(hosts: _*)
      .setRequestConfigCallback(requestConfigCallback)
      .setHttpClientConfigCallback(httpClientConfig)
      .build()
  }

  def listIndexes: Seq[Index] = {
    val request = new Request("GET", "/_cat/indices")
    request.addParameter("h", "health,status,index,uuid,rep,docsCount,storeSize")
    val response = client.performRequest(request)
    Index.parseStream(response.getEntity.getContent)
  }

  def listExtendedIndexes: Seq[ExtendedIndex] =
    listIndexes.map { index ⇒
      println(s"request index information for $index")
      val request = new Request("GET", index.name)
      try {
        val response        = client.performRequest(request)
        val data            = Json.parse(response.getEntity.getContent)
        val mappings        = (data \ index.name \ "mappings").as[JsObject]
        val settings        = (data \ index.name \ "settings").as[JsObject]
        val versionCreated  = (settings \ "index" \ "version" \ "created").as[String]
        val versionUpgraded = (settings \ "index" \ "version" \ "upgraded").asOpt[String]
        new ExtendedIndex(index, mappings, settings, versionCreated, versionUpgraded)
      } catch {
        case error: Throwable ⇒
          println(error.getMessage)
          new ExtendedIndex(index, JsObject.empty, JsObject.empty, "", None)
      }
    }
}
