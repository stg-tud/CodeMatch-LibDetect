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
import extract.methods.Utils._
import org.opalj.br.analyses.{AnalysisExecutor, BasicReport, OneStepAnalysis, Project}
import org.opalj.log.{ConsoleOPALLogger, OPALLogger}

object DBWriter extends AnalysisExecutor {

  private val USER_NAME = Utils.getConfigValue("USER_NAME")
  private val PASSWD = Utils.getConfigValue("PASSWD")
  private val DB_URL = Utils.getConfigValue("DB_URL")
  Class.forName("com.mysql.jdbc.Driver")

  private val conn = DriverManager.getConnection(DB_URL, USER_NAME, PASSWD)
  private val pkgInsert: PreparedStatement = conn.prepareStatement("insert into jarXclass (jarPath,pkg,uniqid_cl,classCount) values (?,?,?,?)")
  private val clInsert: PreparedStatement = conn.prepareStatement("insert into classtbl (uniqid,fqn,loc,mcount) values (?,?,?,?)")
  private val clSelect: PreparedStatement = conn.prepareStatement("select uniqid from classtbl where uniqid=?")

  private val clXmInsert: PreparedStatement = conn.prepareStatement("insert into classXmthd (uniqid_cl,bcode_id) values (?,?)")
  private val mthdInsert: PreparedStatement = conn.prepareStatement("insert into mthdtbl (bcode,aar,nlr,sar,fuzzySAR,fHash1,fHash2,mlen,msig) values (?,?,?,?,?,?,?,?,?)")
  private val mthdSelect: PreparedStatement = conn.prepareStatement("select bcode from mthdtbl where bcode=?")

  override def checkAnalysisSpecificParameters(parameters: Seq[String]): Traversable[String] = {
    if (parameters.length == 1)
      for (p <- parameters)
        if (p.startsWith("-out=") && new File(p.substring(5)).exists())
          return Nil
    parameters.map("unknown parameter: " + _)
  }

  val analysis = new OneStepAnalysis[URL, BasicReport] {

    override def doAnalyze(
                            theProject: Project[URL],
                            parameters: Seq[String],
                            isInterrupted: () => Boolean): BasicReport = {

      OPALLogger.updateLogger(theProject.logContext, new ConsoleOPALLogger(true, org.opalj.log.Error))

      if (theProject.allProjectClassFiles.nonEmpty) {

        var pathToJar: String = ""

        for (p <- parameters) {
          if (p.startsWith("-out=")) {
            pathToJar = p.substring(5)
          }
        }
        for {
          cf <- theProject.allProjectClassFiles
          pkgClassCount: Int = theProject.allProjectClassFiles.count { c => c.fqn.equals(cf.fqn) }
          uniqId: String = sha256Text(theProject.source(cf).get.toString)
        } {
          try {
            val cltxt = sha1Bytes(new StringBuilder(1000000).append(cf.fqn).append(
              cf.fields.map { x => x.toJava })
              .append(cf.methods.map { m =>
                new StringBuilder(1000000).append(m.toJava()).append(
                  if (m.body.isDefined && m.body.nonEmpty) m.body.get.toString() else "").toString()
              }).toString())
            clSelect.setBytes(1, cltxt)

            val rscl: ResultSet = clSelect.executeQuery()
            pkgInsert.setString(1, pathToJar)
            pkgInsert.setString(2, cf.thisType.packageName)
            pkgInsert.setBytes(3, cltxt)

            pkgInsert.setInt(4, pkgClassCount)
            if (!rscl.next()) {
              clInsert.setBytes(1, cltxt) //uniqid
              clInsert.setString(2, cf.fqn.replace("/", ".")) //fqn
              clInsert.setInt(3, getMethodLoC(cf.methods)) // class loc
              clInsert.setInt(4, cf.methods.count { x => x.isMethod }) // method count
              if (clInsert.executeUpdate() == 1) {
                for (m <- cf.methods) {
                  val bcode = sha1Bytes(m.toJava() + "\n" + (if (m.body.isDefined && m.body.nonEmpty) m.body.get else ""))
                  mthdSelect.setBytes(1, bcode)
                  val rsmthd: ResultSet = mthdSelect.executeQuery()
                  clXmInsert.setBytes(1, cltxt)
                  clXmInsert.setBytes(2, bcode)
                  if (!rsmthd.next()) {
                    val irMethod = if (m.body.isDefined && m.body.nonEmpty) AddressLessRepresentation.extractAddressLessCode(m.body.get, theProject) else ""
                    var sb: StringBuilder = new StringBuilder(1000000)
                    sb.append(extractTypeIR(m.descriptor.returnType, theProject)).append(", ").append(extractMethodParamLibTypes(m, theProject)).append("\n")
                    val msigR = sb.toString()
                    sb = new StringBuilder(1000000)
                    sb.append(msigR)
                    if (m.body.isDefined && m.body.nonEmpty)
                      StructurePreservingRepresentation.extractStructurePreservingCode(m.body.get, theProject).foreach { i => if (i != null) sb.append(i).append("\n") }
                    else
                      sb.append("")

                    val text2 = sb.toString()

                    sb = new StringBuilder(1000000)
                    sb.append(msigR)
                    if (m.body.isDefined && m.body.nonEmpty)
                      NameLessRepresentation.extractNameLessCode(m.body.get, theProject).foreach { i => if (i != null) sb.append(i).append("\n") }
                    else
                      sb.append("")

                    val text3 = sb.toString()
                    val s: SSDeep = new SSDeep()
                    val fh: FuzzyHash = s.fuzzyHashContext(text2, uniqId + ".txt")

                    mthdInsert.setBytes(1, bcode) // byte code
                    mthdInsert.setBytes(2, sha1Bytes(AddressLessRepresentation.removeModifiers(m.toJava()) + "\n" + irMethod)) // ar
                    mthdInsert.setBytes(3, sha1Bytes(text3)) // nlr
                    mthdInsert.setBytes(4, sha1Bytes(text2)) // spr
                    val fhash1: String = fh.getHash
                    val fhash2: String = fh.getHash2
                    mthdInsert.setString(5, fh.getBlocksize + ":" + fhash1 + ":" + fhash2) // fuzzySPR
                    mthdInsert.setBytes(6, sha1Bytes(fhash1))
                    mthdInsert.setBytes(7, sha1Bytes(fhash2)) // fuzzySPR
                    mthdInsert.setInt(8, getInstructionCount(m))
                    mthdInsert.setString(9, m.toJava())
                    mthdInsert.execute()
                  }
                  clXmInsert.executeUpdate()
                }
              }
            }
            pkgInsert.executeUpdate()

          } catch {
            case p: Throwable => p.printStackTrace()
          }

        }

      }
      BasicReport("Saved all methods")
    }
  }
}

