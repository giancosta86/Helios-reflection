/*§
  ===========================================================================
  Helios - Reflection
  ===========================================================================
  Copyright (C) 2016 Gianluca Costa
  ===========================================================================
  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
  ===========================================================================
*/

package info.gianlucacosta.helios.services

import java.io.File
import java.lang.reflect.Modifier
import java.net.URLClassLoader

import org.reflections.Reflections
import org.reflections.util.{ClasspathHelper, ConfigurationBuilder}

import scala.collection.JavaConversions._

/**
  * Discovers services from both the current ClassLoader and the JAR files
  * within a directory.
  *
  * For performance reasons, files and reflection information are cached upon creation;
  * should the external files change, a new instance must be created.
  *
  * @param externalDirectoryOption The directory whose JAR files contain external services.
  *                                If None, only internal services will be scanned.
  */
class ServiceExplorer(
                       externalDirectoryOption: Option[File]
                     ) {
  private val externalJarFiles: Array[File] =
    externalDirectoryOption
      .map(externalDirectory => {
        if (externalDirectory.isDirectory)
          externalDirectory
            .listFiles()
            .filter(_.getName.toLowerCase.endsWith(".jar"))
        else
          Array[File]()
      })
      .getOrElse(
        Array[File]()
      )


  private val reflectionsClassLoader = new URLClassLoader(
    externalJarFiles.map(_.toURI.toURL),
    getClass.getClassLoader
  )


  private val reflections: Reflections = {
    val configuration =
      new ConfigurationBuilder()
        .addClassLoader(reflectionsClassLoader)
        .setUrls(ClasspathHelper.forClassLoader(reflectionsClassLoader))

    new Reflections(configuration)
  }


  /**
    * Returns instantiated services from both the internal context and the external JAR files.
    *
    * Non-abstract service classes will be instantiated using the default (no-args) constructor.
    *
    * @param serviceRootClass The root class (or interface) for the service
    * @tparam TService The generic service type
    * @return A list of instantiated services
    */
  def findServicesOfType[TService](
                                              serviceRootClass: Class[TService]
                                            ): List[TService] = {
    val noArgsCreator: Class[_ <: TService] => TService =
      serviceClass => serviceClass.newInstance()

    findServicesOfType(
      serviceRootClass,
      noArgsCreator
    )
  }


  /**
    * Returns instantiated services from both the internal context and the external JAR files.
    *
    * @param serviceRootClass The root class (or interface) for the service
    * @param serviceCreator   Function receiving a TService subclass and returning an instantiated TService.
    *                         It may also throw exceptions - other services won't be affected.
    * @tparam TService The generic service type
    * @return A list of instantiated services
    */
  def findServicesOfType[TService](
                                              serviceRootClass: Class[TService],
                                              serviceCreator: Class[_ <: TService] => TService
                                            ): List[TService] = {
    reflections
      .getSubTypesOf(serviceRootClass)
      .filter(potentialClass =>
        !Modifier.isAbstract(potentialClass.getModifiers) &&
          !potentialClass.isInterface
      )
      .map(serviceClass =>
        try {
          Some(serviceCreator(serviceClass))
        } catch {
          case ex: Exception =>
            ex.printStackTrace(System.err)
            None
        }
      )
      .filter(serviceOption =>
        serviceOption.isDefined
      )
      .map(_.get)
      .toList
  }
}