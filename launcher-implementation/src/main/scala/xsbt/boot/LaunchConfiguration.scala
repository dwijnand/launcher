/* sbt -- Simple Build Tool
 * Copyright 2008, 2009, 2010  Mark Harrah
 */
package xsbt.boot

import Pre._
import java.io.File
import java.net.URL
import scala.collection.immutable.List

//TODO: use copy constructor, check size change
final case class LaunchConfiguration(
  scalaVersion: Value[String],
  ivyConfiguration: IvyOptions,
  app: Application,
  boot: BootSetup,
  logging: Logging,
  appProperties: List[AppProperty],
  serverConfig: Option[ServerConfiguration]
) {

  def isServer: Boolean = serverConfig.isDefined

  def getScalaVersion: Option[String] = {
    val sv = Value.get(scalaVersion)
    if (sv == "auto") None else Some(sv)
  }

  def withScalaVersion(newScalaVersion: String): LaunchConfiguration = {
    val newScalaVersion1 = new Explicit(newScalaVersion)
    LaunchConfiguration(newScalaVersion1, ivyConfiguration, app, boot, logging, appProperties, serverConfig)
  }

  def withApp(app: Application): LaunchConfiguration =
    LaunchConfiguration(scalaVersion, ivyConfiguration, app, boot, logging, appProperties, serverConfig)

  def withAppVersion(newAppVersion: String): LaunchConfiguration = {
    val newApp = app.withVersion(new Explicit(newAppVersion))
    LaunchConfiguration(scalaVersion, ivyConfiguration, newApp, boot, logging, appProperties, serverConfig)
  }

  // TODO: withExplicit
  def withVersions(
    newScalaVersion: String,
    newAppVersion: String,
    classifiers0: Classifiers
  ): LaunchConfiguration = {
    val newScalaVersion1 = new Explicit(newScalaVersion)
    val newIvyConfig = ivyConfiguration.copy(classifiers = classifiers0)
    val newApp = app.withVersion(new Explicit(newAppVersion))
    LaunchConfiguration(newScalaVersion1, newIvyConfig, newApp, boot, logging, appProperties, serverConfig)
  }

  def map(f: File => File) =
    LaunchConfiguration(
      scalaVersion,
      ivyConfiguration.map(f),
      app.map(f),
      boot.map(f),
      logging,
      appProperties,
      serverConfig.map(_ map f)
    )

}

object LaunchConfiguration {
  // Saves a launch configuration into a file. This is only safe if it is loaded by the *same* launcher version.
  def save(config: LaunchConfiguration, f: File): Unit =
    andClose(new java.io.ObjectOutputStream(new java.io.FileOutputStream(f)))(_ writeObject config)

  // Restores a launch configuration from a file. This is only safe if it is loaded by the *same* launcher version.
  def restore(url: URL): LaunchConfiguration = {
    val obj = andClose(new java.io.ObjectInputStream(url.openConnection.getInputStream))(_.readObject)
    obj.asInstanceOf[LaunchConfiguration]
  }

  private[this] def andClose[A <: java.io.Closeable, B](x: A)(f: A => B): B =
    try f(x)
    finally x.close()
}

final case class ServerConfiguration(lockFile: File, jvmArgs: Option[File], jvmPropsFile: Option[File]) {
  def map(f: File => File): ServerConfiguration =
    ServerConfiguration(f(lockFile), jvmArgs map f, jvmPropsFile map f)
}

final case class IvyOptions(
  ivyHome: Option[File],
  classifiers: Classifiers,
  repositories: List[Repository.Repository],
  checksums: List[String],
  isOverrideRepositories: Boolean
) {
  def map(f: File => File): IvyOptions =
    IvyOptions(ivyHome.map(f), classifiers, repositories, checksums, isOverrideRepositories)
}

sealed trait Value[T] extends Serializable

final class Explicit[T](val value: T) extends Value[T] {
  override def toString = value.toString
}

final class Implicit[T](val name: String, val default: Option[T]) extends Value[T] {
  require(isNonEmpty(name), "Name cannot be empty")
  override def toString = name + default.fold("")(d => s"[$d]")
}

object Value {
  def get[T](v: Value[T]): T = v match {
    case e: Explicit[T] => e.value
    case _              => throw new BootException(s"Unresolved version: $v")
  }

  def readImplied[T](s: String, name: String, default: Option[String])(implicit read: String => T): Value[T] =
    if (s == "read") new Implicit(name, default map read) else Pre.error(s"Expected 'read', got '$s'")
}

final case class Classifiers(forScala: Value[List[String]], app: Value[List[String]])

object Classifiers {
  def apply(forScala: List[String], app: List[String]): Classifiers =
    Classifiers(new Explicit(forScala), new Explicit(app))
}

object LaunchCrossVersion {
  def apply(s: String): xsbti.CrossValue = s match {
    case _ if CrossVersionUtil.isFull(s)     => xsbti.CrossValue.Full
    case _ if CrossVersionUtil.isBinary(s)   => xsbti.CrossValue.Binary
    case _ if CrossVersionUtil.isDisabled(s) => xsbti.CrossValue.Disabled
    case _                                   => Pre.error(s"Unknown value '$s' for property 'cross-versioned'")
  }
}

final case class Application(
  groupID: String,
  name: String,
  version: Value[String],
  main: String,
  components: List[String],
  crossVersioned: xsbti.CrossValue,
  classpathExtra: Array[File]
) {
  def getVersion: String = Value.get(version)

  def withVersion(newVersion: Value[String]): Application =
    Application(groupID, name, newVersion, main, components, crossVersioned, classpathExtra)

  def toID: AppID = AppID(groupID, name, getVersion, main, toArray(components), crossVersioned, classpathExtra)

  def map(f: File => File): Application =
    Application(groupID, name, version, main, components, crossVersioned, classpathExtra.map(f))
}

final case class AppID(
  groupID: String,
  name: String,
  version: String,
  mainClass: String,
  mainComponents: Array[String],
  crossVersionedValue: xsbti.CrossValue,
  classpathExtra: Array[File]
) extends xsbti.ApplicationID {
  def crossVersioned: Boolean = crossVersionedValue != xsbti.CrossValue.Disabled
}

object Application {
  def apply(id: xsbti.ApplicationID): Application = {
    import id._
    val version = new Explicit(id.version)
    val mainComponents = id.mainComponents.toList
    Application(groupID, name, version, mainClass, mainComponents, crossVersionedValue, classpathExtra)
  }
}

object Repository {
  trait Repository extends xsbti.Repository { def bootOnly: Boolean }

  final case class Maven(id: String, url: URL, bootOnly: Boolean = false)
    extends xsbti.MavenRepository
    with Repository

  final case class Ivy(
    id: String,
    url: URL,
    ivyPattern: String,
    artifactPattern: String,
    mavenCompatible: Boolean,
    bootOnly: Boolean = false,
    descriptorOptional: Boolean = false,
    skipConsistencyCheck: Boolean = false
  ) extends xsbti.IvyRepository
    with Repository

  final case class Predefined(id: xsbti.Predefined, bootOnly: Boolean = false)
    extends xsbti.PredefinedRepository
    with Repository

  object Predefined {
    def apply(s: String): Predefined = new Predefined(xsbti.Predefined.toValue(s), false)
    def apply(s: String, bootOnly: Boolean): Predefined = new Predefined(xsbti.Predefined.toValue(s), bootOnly)
  }

  def isMavenLocal(repo: xsbti.Repository): Boolean = repo match {
    case p: xsbti.PredefinedRepository => p.id == xsbti.Predefined.MavenLocal
    case _                             => false
  }

  def defaults: List[xsbti.Repository] = xsbti.Predefined.values.map(Predefined(_)).toList
}

final case class Search(tpe: Search.Value, paths: List[File])

object Search extends Enumeration {
  val Only = value("only")
  val RootFirst = value("root-first")
  val Nearest = value("nearest")
  val Current = value("none")

  def apply(s: String, paths: List[File]): Search = Search(toValue(s), paths)
  def none: Search = Search(Current, Nil)
}

final case class BootSetup(
  directory: File,
  lock: Boolean,
  properties: File,
  search: Search,
  promptCreate: String,
  enableQuick: Boolean,
  promptFill: Boolean
) {
  def map(f: File => File): BootSetup =
    BootSetup(f(directory), lock, f(properties), search, promptCreate, enableQuick, promptFill)
}

final case class AppProperty(name: String)(
  val quick: Option[PropertyInit],
  val create: Option[PropertyInit],
  val fill: Option[PropertyInit]
)

sealed trait PropertyInit
final class SetProperty(val value: String) extends PropertyInit
final class PromptProperty(val label: String, val default: Option[String]) extends PropertyInit

final class Logging(level: LogLevel.Value) extends Serializable {
  def log(s: => String, at: LogLevel.Value): Unit =
    if (level.id <= at.id) stream(at).println("[" + at + "] " + s)

  def debug(s: => String): Unit = log(s, LogLevel.Debug)

  private def stream(at: LogLevel.Value) = if (at == LogLevel.Error) System.err else System.out
}

object LogLevel extends Enumeration {
  val Debug = value("debug", 0)
  val Info = value("info", 1)
  val Warn = value("warn", 2)
  val Error = value("error", 3)

  def apply(s: String): Logging = new Logging(toValue(s))
}

final class AppConfiguration(
  val arguments: Array[String],
  val baseDirectory: File,
  val provider: xsbti.AppProvider
) extends xsbti.AppConfiguration
