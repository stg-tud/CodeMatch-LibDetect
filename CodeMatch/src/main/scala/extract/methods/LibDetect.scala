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
import java.sql.{DriverManager, PreparedStatement, ResultSet}
import java.util.Properties

import eu.scape_project.bitwiser.utils.{FuzzyHash, SSDeep}
import extract.methods.Utils.{extractMethodParamLibTypes, extractTypeIR, getInstructionCount, sha1Bytes}
import org.opalj.br._
import org.opalj.br.analyses.{AnalysisExecutor, BasicReport, OneStepAnalysis, Project}
import org.opalj.br.instructions.{ANEWARRAY, CHECKCAST, INSTANCEOF, MULTIANEWARRAY, MethodInvocationInstruction, NEW}
import org.opalj.log.{ConsoleOPALLogger, OPALLogger}

import scala.collection.mutable

object LibDetect extends AnalysisExecutor {

  val METHOD_LENGTH_THRESHOLD: Int = 10
  private val USER_NAME = Utils.getConfigValue("USER_NAME")
  private val PASSWD = Utils.getConfigValue("PASSWD")
  private val DB_URL = Utils.getConfigValue("DB_URL")

  private val conn = DriverManager.getConnection(DB_URL, USER_NAME, PASSWD)

  private val selection = "m.bcode as bcode,m.mlen as mlen,m.msig as msig, c.fqn as fqn, c.loc as loc, c.mcount as mcount"
  private val tables = "androidlib.mthdtbl m, androidlib.classxmthd cm, androidlib.classtbl c"

  private val bcodeP = conn.prepareStatement(
    "Select " + selection + " from " + tables + " where m.bcode=? and cm.bcode_id=m.bcode and cm.uniqid_cl=c.uniqid")
  private val aarP = conn.prepareStatement(
    "Select " + selection + " from " + tables + " where m.aar=? and cm.bcode_id=m.bcode and cm.uniqid_cl=c.uniqid")
  private val bcodePFqn = conn.prepareStatement(
    "Select " + selection + " from " + tables + " where m.bcode=? and cm.bcode_id=m.bcode and cm.uniqid_cl=c.uniqid and c.fqn=?")
  private val aarPFqn = conn.prepareStatement(
    "Select " + selection + " from " + tables + " where m.aar=? and cm.bcode_id=m.bcode and cm.uniqid_cl=c.uniqid and c.fqn=?")
  private val nlrP = conn.prepareStatement(
    "Select " + selection + " from " + tables + " where m.nlr=? and cm.bcode_id=m.bcode and cm.uniqid_cl=c.uniqid")
  private val sarP = conn.prepareStatement(
    "Select " + selection + " from " + tables + " where m.sar=? and cm.bcode_id=m.bcode and cm.uniqid_cl=c.uniqid")
  private val fuzzySarP = conn.prepareStatement(
    "Select " + selection + " from " + tables + " where (fHash1=? or fHash2=?) and cm.bcode_id=m.bcode and cm.uniqid_cl=c.uniqid")

  var results = mutable.HashMap.empty[String, (String, Double)]

  var debug = false

  var mainPkg = ""

  override def checkAnalysisSpecificParameters(parameters: Seq[String]): Traversable[String] = {
    if (parameters.length == 1) {
      for (p <- parameters) {
        if (p.startsWith("-in=") && new File(p.substring(4)).exists()) {
          return Nil
        }
      }
    }
    parameters.map("unknown parameter: " + _)
  }

  val analysis = new OneStepAnalysis[URL, BasicReport] {


    override def doAnalyze(
                            theProject: Project[URL],
                            parameters: Seq[String],
                            isInterrupted: () => Boolean): BasicReport = {
      OPALLogger.updateLogger(theProject.logContext, new ConsoleOPALLogger(true, org.opalj.log.Error))
      results = mutable.HashMap.empty[String, (String, Double)]

      if (theProject.allProjectClassFiles.nonEmpty) {

        for (pkg <- theProject.projectPackages) {
          var uniqClazzes = Set.empty[String]
          var globalPkg = Map.empty[String, Long]
          for (cf <- theProject.allProjectClassFiles.filter { f => f.thisType.packageName == pkg }) {
            var mLen: Long = 0
            var set = Set.empty[MethodData]
            var candidates = Map.empty[String, (Long, Set[String])]
            for (m@MethodWithBody(body) <- cf.methods if body.instructions.length > METHOD_LENGTH_THRESHOLD && m.body.isDefined) {

              val matchedMethods = findBestMatchingMethods(m, cf, theProject)
              if (matchedMethods.nonEmpty) {
                mLen += getInstructionCount(m)
                for (md <- matchedMethods) {
                  if (!set.contains(md)) {
                    var count: Long = md.methodLen
                    if (globalPkg.contains(md.pkg)) {
                      count += globalPkg(md.pkg)
                    }
                    globalPkg += ((md.pkg, count))
                    var score: Long = md.score
                    var bcodes = Set.empty[String]
                    if (candidates.contains(md.fqn)) {
                      val p1: (Long, Set[String]) = candidates(md.fqn)
                      bcodes = p1._2
                      val size: Int = bcodes.size
                      bcodes += md.bcodeStr.toLowerCase
                      if (bcodes.size > size) {
                        score += p1._1
                      }
                    }
                    candidates += ((md.fqn, (score, bcodes)))
                  }
                  set += md
                }
              }
            } // end methods

            var maxFqn: String = ""
            var clazzLen: Long = 0
            var max: Long = 0
            for (p <- candidates) {
              val fqnLib: String = p._1
              val pkgLib: String = if (fqnLib.contains(".")) fqnLib.substring(0, fqnLib.lastIndexOf(".")) else ""
              if (globalPkg(pkgLib) > max && Math.abs(clazzLen - mLen) > Math.abs(p._2._1 - mLen)
                && !uniqClazzes.contains(fqnLib)) {
                maxFqn = fqnLib
                max = globalPkg(pkgLib)
                clazzLen = p._2._1
                uniqClazzes += fqnLib
              }
            }

            if (max > 0) {
              val matching: Double = (clazzLen.toDouble
                / mLen.toDouble)
              results += (cf.fqn.replace("/", ".") -> (maxFqn, if (matching > 1.0) 1.0 else matching))
              if (debug)
                println(cf.fqn + ";" + maxFqn + ";" + (if (matching > 1.0) 1.0 else matching))

            }
          }
        }
      }
      BasicReport(results.size + " library classes found")
    }
  }

  def findBestMatchingMethods(m: Method, cf: ClassFile, theProject: Project[URL]): Set[MethodData] = {
    var matchedMethods = mutable.HashSet.empty[MethodData]
    val bcode = sha1Bytes(m.toJava() + "\n" + m.body.get)

    matchedMethods = getMatchingMethods(bcodePFqn, bcode, cf.fqn, 1.0d, "bcodeFQN")
    if (matchedMethods.isEmpty) {
      matchedMethods = getMatchingMethods(bcodeP, bcode, 1.0d, "bcode")
      if (matchedMethods.isEmpty) {
        val irMethod = AddressLessRepresentation.extractAddressLessCode(m.body.get, theProject)
        val aar = sha1Bytes(AddressLessRepresentation.removeModifiers(m.toJava()) + "\n" + irMethod)
        matchedMethods = getMatchingMethods(aarPFqn, aar, cf.fqn, 1.0d, "aarFQN")
        if (matchedMethods.isEmpty) {
          matchedMethods = getMatchingMethods(aarP, aar, 1.0d, "aar")
          if (matchedMethods.isEmpty) {
            val msigR = new StringBuilder(1000000).append(extractTypeIR(m.descriptor.returnType, theProject)).append(", ").append(extractMethodParamLibTypes(m, theProject)).append("\n").toString()
            val text2 = new StringBuilder(1000000).append(msigR).append(NameLessRepresentation.extractNameLessCode(m.body.get, theProject).mkString("\n")).append("\n").toString()
            val nlr = sha1Bytes(text2)
            matchedMethods = getMatchingMethods(nlrP, nlr, 0.95d, "nlr")
            if (matchedMethods.isEmpty) {
              val text3 = new StringBuilder(1000000).append(msigR).append(StructurePreservingRepresentation.extractStructurePreservingCode(m.body.get, theProject).mkString("\n")).append("\n").toString()
              val sar = sha1Bytes(text3)
              matchedMethods = getMatchingMethods(sarP, sar, 0.95d, "spr")
              if (matchedMethods.isEmpty) {
                val fh: FuzzyHash = new SSDeep().fuzzyHashContext(text2, "1")
                matchedMethods = getMatchingFuzzyMethods(fh.getHash, fh.getHash2, 0.95d, "fuzzy spr")
              }
            }
          }
        }
      }
    }
    matchedMethods.toSet
  }

  def getMatchingMethods(ps: PreparedStatement, rep: Array[Byte], fqn: String, factor: Double, repr: String): mutable.HashSet[MethodData] = {
    var res = mutable.HashSet.empty[MethodData]
    ps.setBytes(1, rep)
    if (fqn.length() > 0)
      ps.setString(2, fqn)
    val rs: ResultSet = ps.executeQuery()
    while (rs.next()) {
      res += createMethodData(rs, factor, repr)
    }
    res
  }

  def getMatchingFuzzyMethods(fhash1Str: String, fhash2Str: String, factor: Double, repr: String): mutable.HashSet[MethodData] = {
    val fhash1: Array[Byte] = sha1Bytes(fhash1Str)
    val fhash2: Array[Byte] = sha1Bytes(fhash2Str)
    var res = mutable.HashSet.empty[MethodData]
    fuzzySarP.setBytes(1, fhash1)
    fuzzySarP.setBytes(2, fhash2)
    val rs: ResultSet = fuzzySarP.executeQuery()
    while (rs.next()) {
      res += createMethodData(rs, factor, repr)
    }
    res
  }

  def getMatchingMethods(ps: PreparedStatement, rep: Array[Byte], factor: Double, repr: String): mutable.HashSet[MethodData] = getMatchingMethods(ps, rep, "", factor, repr)


  def createMethodData(rs: ResultSet, factor: Double, repr: String): MethodData = {
    new MethodData(rs.getBytes("bcode"), rs.getInt("mlen"), rs.getString("msig"), rs.getString("fqn"), rs.getLong("loc"),
      rs.getInt("mcount"), (factor * rs.getInt("mlen").toDouble).toLong, repr)
  }

  def usesType(method: Method, cl: ObjectType): Boolean = method.returnType == cl ||
    method.parameterTypes.exists(p => p.isObjectType && p.asObjectType == cl) ||
    (method.body.isDefined && method.body.get.instructions.exists(i => (i.isInstanceOf[MethodInvocationInstruction] &&
      i.asInstanceOf[MethodInvocationInstruction].declaringClass == cl) ||
      (i.isInstanceOf[NEW] &&
        i.asInstanceOf[NEW].objectType == cl) ||
      (i.isInstanceOf[ANEWARRAY] &&
        i.asInstanceOf[ANEWARRAY].componentType == cl) ||
      (i.isInstanceOf[MULTIANEWARRAY] &&
        i.asInstanceOf[MULTIANEWARRAY].arrayType.componentType == cl) ||
      (i.isInstanceOf[CHECKCAST] &&
        i.asInstanceOf[CHECKCAST].referenceType == cl) ||
      (i.isInstanceOf[INSTANCEOF] &&
        i.asInstanceOf[INSTANCEOF].referenceType == cl)
    ))

  def directUsageTypes(cl: ObjectType, theProject: Project[URL]): Set[ObjectType] =
    theProject.allProjectClassFiles.filter(cf => cf.thisType != cl &&
      (cf.methods.exists(m => usesType(m, cl)) ||
        cf.fields.exists(f => f.fieldType == cl) ||
        cf.interfaceTypes.contains(cl) ||
        (cf.superclassType.isDefined &&
          cf.superclassType.get == cl))).map(cf => cf.thisType).toSet
}

