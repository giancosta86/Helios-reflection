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

package info.gianlucacosta.helios.services

import java.lang.reflect.Modifier

import org.reflections.Reflections

import scala.collection.JavaConversions._

/**
  * Discovers services from the given Reflections object
  */
class ServiceExplorer(reflections: Reflections) {
  /**
    * Returns instantiated services after discovering them.
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
    * Returns instantiated services after discovering them.
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