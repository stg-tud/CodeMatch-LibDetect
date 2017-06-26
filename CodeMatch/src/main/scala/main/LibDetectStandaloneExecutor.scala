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

import extract.methods.{DBWriter, LibDetect}
import extract.methods.Utils.enjarify
import org.opalj.log.{ConsoleOPALLogger, GlobalLogContext, OPALLogger}


object LibDetectStandaloneExecutor {

  def main(args: Array[String]) {
    OPALLogger.updateLogger(GlobalLogContext, new ConsoleOPALLogger(true, org.opalj.log.Error))
    if (args.length != 2 || !(args(0).equalsIgnoreCase("write") || args(0).equalsIgnoreCase("identify")) || !new File(args(1)).exists()) {
      println("Usage: [write|identify] filePath")
      println("Example: write path/to/file/with/library/list")
      println("Example: identify path/to/file/with/app/list")
    } else {

      Class.forName("com.mysql.jdbc.Driver")


      val buff = new BufferedReader(new FileReader(args(1)))
      var line: String = buff.readLine()
      var count: Int = 0
      while (line != null) {
        count += 1
        var file = new File(line)
        if (line.length() > 0 && file.exists() && file.isFile) {
          if (file.getName.endsWith(".apk"))
            file = enjarify(file.getAbsolutePath)
          if (args(0).equalsIgnoreCase("write"))
            DBWriter.main(Array("-cp=" + file.getAbsolutePath + "", "-libcp=android.jar", "-in=" + file.getAbsolutePath))
          else
            LibDetect.main(Array("-cp=" + file.getAbsolutePath + "", "-libcp=android.jar", "-in=" + file.getAbsolutePath))
        }
        val dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss")
        val date = new Date()
        println("no." + count + ", " + dateFormat.format(date) + " ---> " + line)
        line = buff.readLine()
      }
      buff.close()
    }
  }


}