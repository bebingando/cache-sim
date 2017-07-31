/**
 * Copyright 2017 Steve Black
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.bebingando.cachesim

import java.time.Instant

import scala.collection.mutable.MutableList

class System(
  mem: MainMemory[Int,Int],
  val l1: L1Cache[Int,Int],
  val l2: L2Cache[Int,Int],
  val l3: L3Cache[Int,Int]
) {
  def requestData(dataKey: Int): Option[Int] = {
    println("Requesting " + dataKey)
    val toUpdate: MutableList[Int] = MutableList.empty
    val t1 = Instant.now().toEpochMilli()
    val vm = l1.get(dataKey) match {
      case Some(vm) => Some(vm._1)
      case None => {
        toUpdate += 1
        l2.get(dataKey) match {
          case Some(vm) => Some(vm._1)
          case None => {
            toUpdate += 2
            l3.get(dataKey) match {
              case Some(vm) => Some(vm._1)
              case None => {
                toUpdate += 3
                mem.get(dataKey).map(_._1)
              }
            }
          }
        }
      }
    }
    println("Took " + (Instant.now().toEpochMilli() - t1) + " ms to get value " + vm)
    vm.foreach(v => toUpdate.foreach(i => i match {
      case 1 => l1 += ((dataKey, v))
      case 2 => l2 += ((dataKey, v))
      case 3 => l3 += ((dataKey, v))
      case _ => println("Nothing to update")
    }))
    vm
  }

  def saveData(dataKey: Int, dataValue: Int) = {
    
  }
}