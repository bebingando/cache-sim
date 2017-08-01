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

import scala.collection.generic.{CanBuildFrom, MutableMapFactory}
import scala.collection.mutable.{HashMap, ListMap, MapLike}
import scala.collection.mutable.{Map => MutableMap}
import scala.collection.AbstractMap

object ReplacementPolicy extends Enumeration {
  type ReplacementPolicy = Value
  val FIFO = Value("FIFO")
  val LIFO = Value("LIFO")
  val LRU = Value("LRU")
  val MRU = Value("MRU")
  val LFU = Value("LFU")
  val RR = Value("RR")
}

abstract class DelayedCache[A,B] extends HashMap[A,(B,Long)] with MapLike[A,(B,Long),DelayedCache[A,B]] with MutableMap[A,(B,Long)] {
  val name: String
  val max: Int
  val delay: Int
  val replacementPolicy: ReplacementPolicy.Value

  override def empty: DelayedCache[A,B] = super.empty.asInstanceOf[DelayedCache[A,B]]

  private def selectVictim: Option[A] = {
    import ReplacementPolicy._
    replacementPolicy match {
      case FIFO | LRU | LFU => if (this.nonEmpty) Option(this.minBy({case (k,v) => v._2})._1) else None
      case LIFO | MRU => if (this.nonEmpty) Option(this.maxBy({case (k,v) => v._2})._1) else None
      case RR => {
        val s = this.size
        val r = scala.util.Random
        val idx = r.nextInt(size)
        Option(this.toList(idx)._1)
      }
    }
  }
  
  /** Removes a key from this map.
   *  @param    key the key to be removed
   *  @return   the map itself.
   */
  override def -=(key: A): this.type = {
    println("-= " + key)
    Thread.sleep(delay)
    super.get(key) match {
      case Some(_) => super.-=(key)
      case None => this
    }
    this
  }

  /** Adds a new key/value pair to this map.
   *  If the map already contains a
   *  mapping for the key, it will be overridden by the new value.
   *  @param    kv the key/value pair.
   *  @return   the map itself
   */
  def +=(kv: (A, B)): Long = {
    import ReplacementPolicy._
    val m: Long = replacementPolicy match {
      case FIFO | LIFO | LRU | MRU => Instant.now().toEpochMilli()
      case LFU => super.get(kv._1).map(_._2 + 1L).getOrElse(0)
      case RR => 0L
    }
    this += ((kv._1, (kv._2, m)))
    m
  }

  override def +=(kv: (A, (B, Long))): this.type = {
    Thread.sleep(delay)
    super.get(kv._1) match {
      case Some(v) => {
        println("Found " + kv._1 + " in " + name + " ...was " + v + "; updating it in the map with value " + kv._2)
        super.update(kv._1, kv._2)
      }
      case None if (super.size >= max) => {
        /** Entry is not in cache, but cache is full; remove an entry from the cache to make way for the new entry */
        val victimKey = selectVictim
        println(kv._1 + " not found in " + name + ", and map is full; " + victimKey.getOrElse("<none>") + " will be removed to make way")
        victimKey.foreach(k => super.-=(k))
        super.put(kv._1, kv._2)
      }
      case None => {
        /** Entry is not in the cache; cache has room to add an entry */
        println(kv._1 + " not found in " + name + "; adding it to the map (no deletions)")
        super.put(kv._1, kv._2)
      }
    }
    this
  }

  /** Optionally returns the value associated with a key.
   *
   *  @param  key    the key value
   *  @return an option value containing the value associated with `key` in this map,
   *          or `None` if none exists.
   */
  override def get(key: A): Option[(B,Long)] = {
    Thread.sleep(delay)
    val valueAndMarker = super.get(key)
    import ReplacementPolicy._
    replacementPolicy match {
      case LRU | MRU => valueAndMarker.foreach(vm => super.update(key, (vm._1, Instant.now().toEpochMilli())))
      case LFU => valueAndMarker.foreach(vm => super.update(key, (vm._1, vm._2+1)))
      case FIFO | LIFO | RR => ()
    }
    valueAndMarker match {
      case Some(vm) => println("HIT: in " + name + ", found k=" + key + ", v=" + vm._1)
      case None => println("MISS: in " + name + ", did not find k=" + key)
    }
    valueAndMarker
  }
}

class L1Cache[A,B](
  val max: Int = 8, //64, 
  val replacementPolicy: ReplacementPolicy.Value = ReplacementPolicy.FIFO
) extends DelayedCache[A,B] {
  val name: String = "L1Cache"
  val delay: Int = 2 // 10
}

class L2Cache[A,B](
  val max: Int = 64, // 512, 
  val replacementPolicy: ReplacementPolicy.Value = ReplacementPolicy.FIFO
) extends DelayedCache[A,B] {
  val name: String = "L2Cache"
  val delay: Int = 10 // 50
}

class L3Cache[A,B](
  val max: Int = 1024, // 8192, 
  val replacementPolicy: ReplacementPolicy.Value = ReplacementPolicy.FIFO
) extends DelayedCache[A,B] {
  val name: String = "L3Cache"
  val delay: Int = 20 // 100
}

class L4Cache[A,B](
  val max: Int = 32768, // 262144, 
  val replacementPolicy: ReplacementPolicy.Value = ReplacementPolicy.FIFO
) extends DelayedCache[A,B] {
  val name: String = "L4Cache"
  val delay: Int = 50 // 250
}

class MainMemory[A,B](val replacementPolicy: ReplacementPolicy.Value = ReplacementPolicy.FIFO) extends DelayedCache[A,B] {
  val name: String = "MainMemory"
  val max: Int = 524288 // java.lang.Integer.MAX_VALUE / 64
  val delay: Int = 100 // 500

  private def generateValue(key: A, value: B): (A, (B, Long)) = {
    import ReplacementPolicy._
    val m: Long = replacementPolicy match {
      case FIFO | LIFO | LRU | MRU => Instant.now().toEpochMilli()
      case LFU => super.get(key).map(_._2 + 1L).getOrElse(0)
      case RR => 0L
    }
    (key, (value, m))
  }

  def loadData = (1 until (max/64)).map(i => generateValue(i.asInstanceOf[A], i.asInstanceOf[B])).foreach(i => super.put(i._1, i._2))
}
