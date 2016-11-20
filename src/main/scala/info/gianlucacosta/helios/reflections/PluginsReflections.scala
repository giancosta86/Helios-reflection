/*^
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

package info.gianlucacosta.helios.reflections

import java.io.File
import java.net.URLClassLoader

import org.reflections.Reflections
import org.reflections.util.{ClasspathHelper, ConfigurationBuilder}


object PluginsReflections {
  /**
    * Creates a Reflections object for:
    *
    * @param internalRootPackage    The given root package, which is considered <i>internal</i> - that is,
    *                               available even when the plugins directory is missing or with no files
    * @param pluginsDirectoryOption an optional (flat) directory containing plugin JAR files
    *
    */
  def apply(internalRootPackage: String, pluginsDirectoryOption: Option[File]): Reflections = {
    val externalJarFiles: Array[File] =
      pluginsDirectoryOption
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

    val reflectionsClassLoader = new URLClassLoader(
      externalJarFiles.map(_.toURI.toURL),
      getClass.getClassLoader
    )

    val configuration =
      new ConfigurationBuilder()
        .addClassLoader(reflectionsClassLoader)
        .addUrls(ClasspathHelper.forPackage(internalRootPackage))
        .addUrls(externalJarFiles.map(_.toURI.toURL): _*)

    new Reflections(configuration)
  }
}
