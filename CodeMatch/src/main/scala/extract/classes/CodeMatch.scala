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
package extract.classes

import java.io.File
import java.net.URL

import eu.scape_project.bitwiser.utils.{FuzzyHash, SSDeep}
import extract.methods.StructurePreservingRepresentation
import extract.methods.Utils.{extractInterfaceIR, extractMethodParamLibTypes, extractTypeIR}
import org.opalj.br.MethodWithBody
import org.opalj.br.analyses.{AnalysisExecutor, BasicReport, OneStepAnalysis, Project}
import org.opalj.log.{ConsoleOPALLogger, OPALLogger}

import scala.collection.mutable.ListBuffer

object CodeMatch extends AnalysisExecutor {

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

  var libs = Set.empty[String]
  var pkgNames = Set.empty[String]
  var state: FuzzyHash = _
  var count: Long = 0

  val analysis = new OneStepAnalysis[URL, BasicReport] {

    override def doAnalyze(
                            theProject: Project[URL],
                            parameters: Seq[String],
                            isInterrupted: () => Boolean): BasicReport = {

      OPALLogger.updateLogger(theProject.logContext, new ConsoleOPALLogger(true, org.opalj.log.Error))

      var pathToJar: String = ""
      if (theProject.allProjectClassFiles.nonEmpty) {

        for (p <- parameters) {
          if (p.startsWith("-in=")) {
            pathToJar = p.substring(4)
          }
        }
        var list = ListBuffer.empty[String]
        for (cf <- theProject.allProjectClassFiles.filter { x => !isLibrary(x.fqn) }) {
          var methods = cf.methods.filter { m => m.body.isDefined && m.body.get.instructionsCount > 10 }
          if (methods.nonEmpty) {
            var classList = ListBuffer.empty[String]
            var classBuilder = new StringBuilder(100000000)
            classBuilder.append(extractInterfaceIR(theProject.classHierarchy.allSuperinterfacetypes(cf.thisType.asObjectType, reflexive = false), theProject) + "\n")
            cf.fields.foreach { x => classBuilder.append("\t" + extractTypeIR(x.fieldType, theProject) + "\n") }
            for (m@MethodWithBody(body) <- methods) {
              count += body.instructionsCount
              var sb: StringBuilder = new StringBuilder(1000000)
              sb.append(extractTypeIR(m.descriptor.returnType, theProject)).append(", ").append(extractMethodParamLibTypes(m, theProject)).append("\n")
              val msigR = sb.toString()
              sb = new StringBuilder(1000000)
              sb.append("\t").append(msigR)
              StructurePreservingRepresentation.extractStructurePreservingCode(m.body.get, theProject).foreach { i => if (i != null) sb.append("\t\t").append(i).append("\n") }

              classList += sb.toString()
            }
            classBuilder.append(classList.sortBy { x => x.split("\n").length }.mkString("\n"))
            list += classBuilder.toString()
          }
        }
        val fh: FuzzyHash = new SSDeep().fuzzyHashContext(list.sortBy { x => x.split("\n").length }.mkString("\n\n\n"), pathToJar)
        state = fh
      }
      BasicReport(state.toString)
    }
  }

  def isLibrary(className: String): Boolean = {
    if (pkgNames.isEmpty && libs.nonEmpty) {
      pkgNames = libs.collect { case x => if (x.contains("/")) x.substring(0, x.lastIndexOf("/")) else "" }
    }
    var pkgName = if (className.contains("/")) className.substring(0, className.lastIndexOf("/")) else ""
    pkgNames.contains(pkgName)
  }
}



