/* BSD 2-Clause License:
 * Copyright (c) 2009 - 2017
 * Software Technology Group
 * Department of Computer Science
 * Technische Universität Darmstadt
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  - Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *  - Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
package extract.methods

import java.io.{File, FileReader}
import java.net.URL
import java.security.MessageDigest
import java.util.Properties

import org.opalj.br.{Method, ObjectType}
import org.opalj.br.analyses.Project
import scala.sys.process._

object Utils {

  def extractInterfaceIR(set: org.opalj.collection.immutable.UIDSet[ObjectType], theProject: Project[URL]): String =
    if (set.isEmpty)
      "Ljava/lang/Object"
    else {
      set.filter(theProject.isLibraryType).map(o => "L" + o.fqn).toSeq.sorted[String](Ordering.String).mkString(", ")
    }

  def extractParamTypes(pt: IndexedSeq[org.opalj.br.FieldType], theProject: Project[URL]): String = pt.collect {
    case p => extractTypeIR(p, theProject)
  }.sorted[String](Ordering.String).mkString(", ")

  def extractMethodParamLibTypes(a: Method, theProject: Project[URL]): String = extractParamTypes(a.parameterTypes, theProject: Project[URL])

  def extractTypeIR(t: org.opalj.br.Type, theProject: Project[URL]): String =
    if (t.isObjectType) extractInterfaceIR(theProject.classHierarchy.allSuperinterfacetypes(t.asObjectType), theProject)
    else if (t.isArrayType && t.asArrayType.elementType.isObjectType)
      extractInterfaceIR(theProject.classHierarchy.allSuperinterfacetypes(t.asArrayType.elementType.asObjectType), theProject) + "[]"
    else t.toString

  def getInstructionCount(m: Method): Int = if (m.body.isEmpty) 0 else m.body.get.instructions.count { x => x != null }

  def sha1Bytes(text: String): Array[Byte] = {
    val md1 = MessageDigest.getInstance("SHA-1")
    md1.update(text.getBytes("UTF-8"))
    md1.digest()
  }


  def getMethodLoC(ms: Iterable[Method]): Int = ms.map { m => getInstructionCount(m) }.sum

  def sha256Text(text: String): String = {
    val md1 = MessageDigest.getInstance("SHA-256")
    md1.update(text.getBytes("UTF-8"))
    val digest = md1.digest()
    String.format("%064x", new java.math.BigInteger(1, digest))
  }

  private val props = new Properties()

  def getConfigValue(value: String): String = {
    if (props.isEmpty) {
      props.load(new FileReader("config.txt"))
    }
    props.getProperty(value)
  }

  def enjarify(apkPath: String): File = {
    val file = File.createTempFile("myJar", ".jar")
    (getConfigValue("ENJARIFY_EXEC_CMD") + " -f " + apkPath + " -o " + file.getAbsolutePath).!!
    file
  }
}

class MethodData(val bcode: Array[Byte], val aar: Array[Byte], val nlr: Array[Byte],
                 val sar: Array[Byte], val fuzzySar: String, val pkg: String, val fqn: String,
                 val msig: String, val methodLen: Int, val methodCount: Int, var score: Long, val classLoc: Long, val repr: String) {

  val bcodeStr: String = byteToSHA1(bcode)

  def byteToSHA1(bcode: Array[Byte]): String = String.format("%040x", new java.math.BigInteger(1, bcode))

  def this(bcode: Array[Byte], methodLen: Int, msig: String, fqn: String, classLoc: Long, methodCount: Int, score: Long, repr: String) {
    this(bcode, Array[Byte](), Array[Byte](), Array[Byte](), "",
      if (fqn.contains(".")) fqn.substring(0, fqn.lastIndexOf(".")) else "",
      fqn.replace("/", "."), msig, methodLen, methodCount, score, classLoc, repr)
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case md: MethodData =>
        if (md == null)
          return false
        return md.fqn == fqn && md.bcodeStr == bcodeStr
      case _ =>
    }
    false
  }

  override def hashCode(): Int = (fqn + bcodeStr).hashCode()

}
