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
import java.util.logging.{Level, Logger}
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.xpath.{XPathConstants, XPathFactory}

import brut.androlib.ApkOptions
import brut.androlib.res.AndrolibResources
import brut.androlib.res.data.ResPackage
import brut.androlib.res.decoder.{ResAttrDecoder, XmlPullStreamDecoder}
import brut.androlib.res.util.ExtFile
import org.xml.sax.InputSource

/**
  * Created by Admin on 17.05.2017.
  */
object MainPkgResolver {

  def hasManifest(fileName: String): Boolean = new File(fileName).exists && new ExtFile(new File(fileName))
    .getDirectory.containsFile("AndroidManifest.xml")

  def decodeManifest(fileName: String): String = {
    val apkFile = new ExtFile(new File(fileName))
    if (apkFile.getDirectory.containsFile("AndroidManifest.xml")) {
      val androLibRes = new AndrolibResources()
      androLibRes.apkOptions = new ApkOptions()
      val aXmlResourceParser = androLibRes.getManifestFileDecoder.m2
      aXmlResourceParser.setAttrDecoder(new ResAttrDecoder())
      val attrDecoder = aXmlResourceParser.getAttrDecoder
      attrDecoder.setCurrentPackage(new ResPackage(androLibRes.getResTable(apkFile, true), 0, null))
      val decoder = new XmlPullStreamDecoder(aXmlResourceParser, androLibRes.getResXmlSerializer)
      val in = apkFile.getDirectory.getFileInput("AndroidManifest.xml")
      val out = new ByteArrayOutputStream()
      decoder.decodeManifest(in, out)
      return out.toString
    }
    ""
  }

  def readManifestPackage(fileName: String): String = {
    val factory = DocumentBuilderFactory.newInstance
    val builder = factory.newDocumentBuilder

    val doc = builder.parse(new InputSource(new ByteArrayInputStream(decodeManifest(fileName).getBytes("UTF-8"))))
    val xPathfactory = XPathFactory.newInstance()
    val xpath = xPathfactory.newXPath()
    val expr = xpath.compile("/manifest/@package")
    expr.evaluate(doc, XPathConstants.STRING).asInstanceOf[String]
  }

  def shutLogger(): Unit = {
    Logger.getLogger("brut.androlib.res.AndrolibResources").setLevel(Level.OFF)
  }
}

