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
package main

import java.io.{BufferedReader, File, FileReader}
import java.text.SimpleDateFormat
import java.util.Date

import eu.scape_project.bitwiser.utils.FuzzyHash
import extract.classes.CodeMatch
import extract.methods.LibDetect
import extract.methods.Utils.enjarify
import org.opalj.log.{ConsoleOPALLogger, GlobalLogContext, OPALLogger}

import scala.collection.mutable

object CodeMatchStandaloneExecutor {

  private val generatedApps = mutable.HashSet.empty[String]

  def main(args: Array[String]) {
    OPALLogger.updateLogger(GlobalLogContext, new ConsoleOPALLogger(true, org.opalj.log.Error))
    if (args.length != 1 || !new File(args(0)).exists()) {
      println("Usage: filePath")
      println("Example: write path/to/file/with/app/pairs.txt")
      println("Example: write path/to/file/with/apps/to/compare/with/F2S2/db.txt")
    }
    else {

      Class.forName("com.mysql.jdbc.Driver")

      val buff = new BufferedReader(new FileReader(args(0)))
      var line: String = buff.readLine()
      var count1: Long = 0
      var count2: Long = 0
      var author1: String = ""
      var author2: String = ""
      var lineCount = 0
      var usesF2S2 = false
      while (line != null) {
        var isMadeApp = false
        var classMap1: FuzzyHash = null
        var classMap2: FuzzyHash = null
        if (line.length() > 0) {
          if (line.contains(";")) {
            val first = line.split(";")(0)
            val second = line.split(";")(1)
            if (new File(first).exists() && new File(second).exists() && first.endsWith(".apk") && second.endsWith(".apk")) {
              var file = enjarify(first)
              val mainPkg1 = MainPkgResolver.readManifestPackage(first)
              isMadeApp = isGeneratedApp(mainPkg1)
              author1 = AuthorKeyExtractor.extractAuthorKey(first)
              if (file.exists() && !isMadeApp) {
                val libs: Set[String] = identifyLibraries(file, mainPkg1)

                resetCodeMatch(libs)
                CodeMatch.main(Array("-cp=" + file.getAbsolutePath, "-libcp=android.jar", "-in=" + first + ""))
                count1 = CodeMatch.count
                classMap1 = CodeMatch.state

                file = enjarify(second)
                val mainPkg2 = MainPkgResolver.readManifestPackage(second)
                isMadeApp = isGeneratedApp(mainPkg2)
                author2 = AuthorKeyExtractor.extractAuthorKey(second)
                if (file.exists() && !isMadeApp) {
                  val libs2: Set[String] = identifyLibraries(file, mainPkg2)
                  resetCodeMatch(libs2)
                  CodeMatch.main(Array("-cp=" + file.getAbsolutePath, "-libcp=android.jar", "-in=" + second + ""))
                  count2 = CodeMatch.count
                  classMap2 = CodeMatch.state

                  lineCount += 1
                }
              }
            }
          } else {
            val filePath = line
            if (new File(filePath).exists() && filePath.endsWith(".apk")) {
              var file = enjarify(filePath)
              val mainPkg1 = MainPkgResolver.readManifestPackage(filePath)
              isMadeApp = isGeneratedApp(mainPkg1)
              author1 = AuthorKeyExtractor.extractAuthorKey(filePath)
              if (file.exists() && !isMadeApp) {
                val libs: Set[String] = identifyLibraries(file, mainPkg1)
                resetCodeMatch(libs)
                CodeMatch.main(Array("-cp=" + file.getAbsolutePath, "-libcp=android.jar", "-in=" + filePath + ""))
                count1 = CodeMatch.count
                classMap1 = CodeMatch.state


                lineCount += 1
              }
            }
          }
        }
        val dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss")
        //get current date time with Date()
        val date = new Date()
        if (!usesF2S2) {
          if (isMadeApp)
            println(lineCount + ". " + dateFormat.format(date) + " ---> " + line + "; one of the apps is generated")
          else if (count1 < 1278 || count2 < 1278) println(lineCount + ". " + dateFormat.format(date) + " ---> " + line + "; one of the apps is too small")
          else if (author1.equals(author2)) println(lineCount + ". " + dateFormat.format(date) + " ---> " + line + "; the apps are from the same author")
          else {
            val cloneSim = isAppClone(classMap1, classMap2, 0.3f)
            println(lineCount + ". " + dateFormat.format(date) + " ---> " + line + ";" + cloneSim)
          }
        }
        else {
          if (isMadeApp)
            println(lineCount + ". " + dateFormat.format(date) + " ---> " + line + "; the app is generated")
          else if (count1 < 1278 || count2 < 1278) println(lineCount + ". " + dateFormat.format(date) + " ---> " + line + "; the app is too small for comparison")
          else {
            println("author key: " + author1)
            println("fuzzy hash: " + classMap1)
            println(lineCount + ". " + dateFormat.format(date) + " ---> processed " + line)
          }
        }
        line = buff.readLine()
      }
      buff.close()
    }

  }

  def resetCodeMatch(libs: Set[String]): Unit = {
    CodeMatch.state = null
    CodeMatch.count = 0
    CodeMatch.libs = libs
    CodeMatch.pkgNames = Set.empty[String]
  }

  def isGeneratedApp(mainPkg: String): Boolean = {
    if (generatedApps.isEmpty) {
      val buff = new BufferedReader(new FileReader("appMaker.txt"))
      var line = buff.readLine()
      while (line != null) {
        if (line.length > 0) generatedApps.add(line)
        line = buff.readLine()
      }
      buff.close()
    }
    for (maker <- generatedApps) {
      if (mainPkg.contains(maker)) {
        return true
      }
    }
    false
  }


  def isAppClone(fh1: FuzzyHash, fh2: FuzzyHash, fl: Float): Boolean = FuzzyHash.compare(fh1, fh2) >= fl * 100

  def getLibrarySet(filename: String): Set[String] = {
    val libPackageList = scala.collection.mutable.Set.empty[String]
    if (libPackageList.isEmpty && new File(filename).exists()) {
      val buff = new BufferedReader(new FileReader(filename))
      var line = buff.readLine()
      while (line != null) {
        if (line.length() > 0) {
          line = line.split(";")(0)
          libPackageList ++ line.replace(".", "/")
        }
        line = buff.readLine()
      }
      buff.close()
    }
    libPackageList.toSet
  }

  private def identifyLibraries(file: File, mainPkg1: String) = {
    LibDetect.mainPkg = mainPkg1
    LibDetect.results = mutable.HashMap.empty[String, (String, Double)]
    LibDetect.main(Array("-cp=" + file.getAbsolutePath + "", "-libcp=android.jar", "-in=" + file.getAbsolutePath + ""))
    val libs: Set[String] = LibDetect.results.keys.toSet
    libs
  }

}
