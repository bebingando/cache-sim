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

object Application {
  def main(args: Array[String]) {
    println("Start...")

    val rp: ReplacementPolicy.Value = {
      if (args.isDefinedAt(0)) {
        try {
          ReplacementPolicy.withName(args(0))
        } catch {
          case _: Throwable => 
            println("invalid Replacement Policy specified; using FIFO")
            ReplacementPolicy.FIFO  
        }
      } else {
        println("No Replacement Policy specified; using FIFO")
        ReplacementPolicy.FIFO
      }
    }

    // Populate Main Memory with a bunch of data (lowest "cache")
    val mm: MainMemory[Int,Int] = new MainMemory[Int,Int](replacementPolicy = rp)
    mm.loadData

    import ReplacementPolicy._
    val sys: System = new System(
                            mm,
                            new L1Cache[Int,Int](replacementPolicy = rp),
                            new L2Cache[Int,Int](replacementPolicy = rp),
                            new L3Cache[Int,Int](replacementPolicy = rp)
                          )

    (1 until 20).foreach{ sys.requestData(_) }
    println("***** 1 to 20 done *****")
    
    (10 until 90 by 4).foreach{ sys.requestData(_) }
    println("***** 10 to 90 bye 4 done *****")

    (40 until 0 by -2).foreach{ sys.requestData(_) }
    println("***** 40 to 0 (by 2) done *****")
    
    (20 until 80 by 3).foreach{ sys.requestData(_) }
    println("***** 20 to 80 (by 3) done *****")


    def kvAsString(kv: (Int, (Int, Long))): String = "key:" + kv._1 + ", value:" + kv._2._1 + ", marker:" + kv._2._2

    println("L1Cache:")
    sys.l1.toList.sortBy(_._1).map(kvAsString(_)).foreach(d => println("  " + d))
    println("")

    println("L2Cache:")
    sys.l2.toList.sortBy(_._1).map(kvAsString(_)).foreach(d => println("  " + d))
    println("")

    println("L3Cache:")
    sys.l3.toList.sortBy(_._1).map(kvAsString(_)).foreach(d => println("  " + d))
    println("")

    println("Finish...")
  }
}
