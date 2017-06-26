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

import extract.methods.Utils.{extractParamTypes, extractTypeIR}
import org.opalj.br.Code
import org.opalj.br.analyses.Project
import org.opalj.br.instructions.{ANEWARRAY, CHECKCAST, GETFIELD, GETSTATIC, GOTO, GOTO_W, IFGE, IFGT, IFLE, IFLT, IFNE, IFNONNULL, IFNULL, IF_ACMPEQ, IF_ACMPNE, IF_ICMPEQ, IF_ICMPGE, IF_ICMPGT, IF_ICMPLE, IF_ICMPLT, IF_ICMPNE, INSTANCEOF, JSR, JSR_W, MULTIANEWARRAY, MethodInvocationInstruction, NEW, NEWARRAY, PUTFIELD, PUTSTATIC}

object NameLessRepresentation {

  def extractNameLessCode(n: Code, theProject: Project[URL]): Seq[String] = n.collectWithIndex {
    case (_, PUTSTATIC(clazz, _, ft)) => "putstatic " + extractTypeIR(clazz, theProject) + ", " + extractTypeIR(ft, theProject)
    case (_, PUTFIELD(clazz, _, ft)) => "putfield " + extractTypeIR(clazz, theProject) + ", " + extractTypeIR(ft, theProject)

    case (_, GETFIELD(clazz, _, ft)) => "getfield " + extractTypeIR(clazz, theProject) + ", " + extractTypeIR(ft, theProject)
    case (_, GETSTATIC(clazz, _, ft)) => "getstatic " + extractTypeIR(clazz, theProject) + ", " + extractTypeIR(ft, theProject)


    case (_, invoke: MethodInvocationInstruction) => "invoke " + extractTypeIR(invoke.declaringClass, theProject) +
      ", ret: " + extractTypeIR(invoke.methodDescriptor.returnType, theProject) +
      ", params: " + extractParamTypes(invoke.methodDescriptor.parameterTypes, theProject)

    case (_, NEW(clazz)) => "new " + extractTypeIR(clazz, theProject)

    case (_, ANEWARRAY(clazz)) => "anewarray " + extractTypeIR(clazz, theProject)
    case (_, MULTIANEWARRAY(clazz, _)) => "multianewarray " + extractTypeIR(clazz, theProject)
    case (_, NEWARRAY(clazz)) => "newarray " + extractTypeIR(clazz, theProject)

    case (_, CHECKCAST(clazz)) => "checkcast " + extractTypeIR(clazz, theProject)
    case (_, INSTANCEOF(clazz)) => "instanceof " + extractTypeIR(clazz, theProject)

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

}
