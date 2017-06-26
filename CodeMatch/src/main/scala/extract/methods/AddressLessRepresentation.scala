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

import java.net.URL

import org.opalj.br.Code
import org.opalj.br.analyses.Project
import org.opalj.br.instructions.{GOTO, GOTO_W, IFGE, IFGT, IFLE, IFLT, IFNE, IFNONNULL, IFNULL, IF_ACMPEQ, IF_ACMPNE, IF_ICMPEQ, IF_ICMPGE, IF_ICMPGT, IF_ICMPLE, IF_ICMPLT, IF_ICMPNE, JSR, JSR_W}

object AddressLessRepresentation {

  def extractAddressLessCode(n: Code, theProject: Project[URL]): Seq[String] = n.collectWithIndex {
    case (_, IF_ACMPEQ(direction)) => "IF_ACMPEQ " + (if (direction > 0) "along" else "back")
    case (_, IF_ACMPNE(direction)) => "IF_ACMPNE " + (if (direction > 0) "along" else "back")
    case (_, IF_ICMPEQ(direction)) => "IF_ICMPEQ " + (if (direction > 0) "along" else "back")
    case (_, IF_ICMPGE(direction)) => "IF_ICMPGE " + (if (direction > 0) "along" else "back")
    case (_, IF_ICMPGT(direction)) => "IF_ICMPGT " + (if (direction > 0) "along" else "back")
    case (_, IF_ICMPLE(direction)) => "IF_ICMPLE " + (if (direction > 0) "along" else "back")
    case (_, IF_ICMPLT(direction)) => "IF_ICMPLT " + (if (direction > 0) "along" else "back")
    case (_, IF_ICMPNE(direction)) => "IF_ICMPNE " + (if (direction > 0) "along" else "back")
    case (_, IFGE(direction)) => "IFGE " + (if (direction > 0) "along" else "back")
    case (_, IFGT(direction)) => "IFGT " + (if (direction > 0) "along" else "back")
    case (_, IFLE(direction)) => "IFLE " + (if (direction > 0) "along" else "back")
    case (_, IFLT(direction)) => "IFLT " + (if (direction > 0) "along" else "back")
    case (_, IFNE(direction)) => "IFNE " + (if (direction > 0) "along" else "back")
    case (_, IFNONNULL(direction)) => "IFNONNULL " + (if (direction > 0) "along" else "back")
    case (_, IFNULL(direction)) => "IFNULL " + (if (direction > 0) "along" else "back")
    case (_, JSR(direction)) => "JSR " + (if (direction > 0) "along" else "back")
    case (_, JSR_W(direction)) => "JSR_W " + (if (direction > 0) "along" else "back")
    case (_, GOTO(direction)) => "GOTO " + (if (direction > 0) "along" else "back")
    case (_, GOTO_W(direction)) => "GOTO_W " + (if (direction > 0) "along" else "back")
    case (_, p) => p.toString()
  }

  def removeModifiers(sig: String): String = {
    var newSig = sig
    newSig = removeModifiers(newSig, "public ")
    newSig = removeModifiers(newSig, "protected ")
    newSig = removeModifiers(newSig, "private ")
    newSig = removeModifiers(newSig, "abstract ")
    newSig = removeModifiers(newSig, "final ")
    newSig = removeModifiers(newSig, "native ")
    newSig = removeModifiers(newSig, "static ")
    newSig = removeModifiers(newSig, "synchronized ")
    newSig = removeModifiers(newSig, "transient ")
    newSig.trim()
  }

  def removeModifiers(sig: String, rep: String): String = {
    if (sig.startsWith(rep))
      removeModifiers(sig.replace(rep, ""))
    else sig
  }

}
