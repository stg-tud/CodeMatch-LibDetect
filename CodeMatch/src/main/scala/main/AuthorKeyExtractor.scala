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

import java.io._
import java.security.MessageDigest
import java.util
import java.util.zip.ZipInputStream
import java.math._
import extract.methods.Utils

object AuthorKeyExtractor {

  def extractAuthorKey(fileName: String): String = {
    val unZip = new UnZip
    val certD = new File("cert")
    if (certD.exists) certD.delete
    val certpem = new File("cert.pem")
    if (certpem.exists) certpem.delete
    unZip.unZipIt(fileName)
    val process1 = Runtime.getRuntime.exec("cmd /c C:/OpenSSL-Win32/bin/openssl.exe pkcs7 -in \"" + new File("cert").getAbsolutePath.replace("\\", "/") + "\" -inform DER -print_certs -out \"" + pySubString(new File(".").getAbsolutePath.replace("\\", "/"), 0, -2) + "/cert.pem\"")
    process1.waitFor
    val list = new util.ArrayList[String]()
    (Utils.getConfigValue("OPENSSL_EXEC_CMD") + " x509 -pubkey -noout -in " + pySubString(new File(".").getAbsolutePath.replace("\\", "/"), 0, -2) + "/cert.pem").split(" ").foreach(i => list.add(i))
    val pb = new ProcessBuilder(list)
    pb.directory(new File("."))
    pb.environment.put("OPENSSL_CONF", Utils.getConfigValue("OPENSSL_CONF_PATH"))
    pb.redirectErrorStream(true)
    val process = pb.start
    val is = process.getInputStream
    val certbf = new BufferedReader(new InputStreamReader(is))
    val sb = new StringBuilder
    var line2 = certbf.readLine
    while (line2 != null) {
      sb.append(line2 + "\n")
      line2 = certbf.readLine()
    }
    certbf.close()
    val p = hashSHA1(sb.toString)
    p
  }


  def hashSHA1(text: String): String = {
    val md1 = MessageDigest.getInstance("SHA-1")
    md1.update(text.getBytes("UTF-8"))
    val digest = md1.digest
    val p = String.format("%040x", new BigInteger(1, digest))
    p
  }

  def pySubString(str: String, begin: Int, end: Int): String = str.substring(begin, if (end < 0) {
    str.length + end
  }
  else {
    end
  })

}

class UnZip {

  def unZipIt(zipFile: String): Unit = {
    val buffer = new Array[Byte](1024)
    val zis = new ZipInputStream(new FileInputStream(zipFile))
    var ze = zis.getNextEntry
    while (ze != null) {
      var fileName = ze.getName
      if (!ze.isDirectory) {
        fileName = new File(fileName).getName
        if (fileName.toLowerCase.endsWith(".rsa") || fileName.toLowerCase.endsWith(".dsa")) {
          val newFile = new File("cert")

          val fos = new FileOutputStream(newFile)
          var len = zis.read(buffer)
          while (len > 0) {
            fos.write(buffer, 0, len)
            len = zis.read(buffer)
          }
          fos.close()
        }
      }
      ze = zis.getNextEntry
    }
    zis.closeEntry()
    zis.close()

  }
}
