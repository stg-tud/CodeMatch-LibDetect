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
import org.opalj.br.instructions.{ACONST_NULL, ANEWARRAY, ARRAYLENGTH, ATHROW, AddInstruction, ArrayLoadInstruction, BIPUSH, CHECKCAST, DCMPG, DCMPL, DCONST_0, DCONST_1, DUP, DUP2, DUP2_X1, DUP2_X2, DUP_X1, DUP_X2, FCMPG, FCMPL, FCONST_0, FCONST_1, FCONST_2, FloatingPointDivideInstruction, FloatingPointRemainderInstruction, GETFIELD, GETSTATIC, GOTO, GOTO_W, IAND, ICONST_0, ICONST_1, ICONST_2, ICONST_3, ICONST_4, ICONST_5, ICONST_M1, IFGE, IFGT, IFLE, IFLT, IFNE, IFNONNULL, IFNULL, IF_ACMPEQ, IF_ACMPNE, IF_ICMPEQ, IF_ICMPGE, IF_ICMPGT, IF_ICMPLE, IF_ICMPLT, IF_ICMPNE, IINC, INSTANCEOF, IOR, IXOR, IntegerDivideInstruction, IntegerRemainderInstruction, JSR, JSR_W, LAND, LCMP, LCONST_0, LCONST_1, LDC, LDC2_W, LDC_W, LOOKUPSWITCH, LOR, LXOR, LoadLocalVariableInstruction, MONITORENTER, MONITOREXIT, MULTIANEWARRAY, MethodInvocationInstruction, MultiplyInstruction, NEW, NEWARRAY, NOP, NegateInstruction, NumericConversionInstruction, POP, POP2, PUTFIELD, PUTSTATIC, PrimitiveArrayStoreInstruction, RET, ReturnValueInstruction, SIPUSH, SWAP, ShiftInstruction, StoreLocalVariableInstruction, SubtractInstruction, TABLESWITCH, WIDE}

object StructurePreservingRepresentation {

  def extractStructurePreservingCode(n: Code, theProject: Project[URL]): Seq[String] = n.collectWithIndex {
    case (_, PUTSTATIC(clazz, _, ft)) => "put " + extractTypeIR(clazz, theProject) + ", " + extractTypeIR(ft, theProject)
    case (_, PUTFIELD(clazz, _, ft)) => "put " + extractTypeIR(clazz, theProject) + ", " + extractTypeIR(ft, theProject)

    case (_, GETFIELD(clazz, _, ft)) => "get " + extractTypeIR(clazz, theProject) + ", " + extractTypeIR(ft, theProject)
    case (_, GETSTATIC(clazz, _, ft)) => "get " + extractTypeIR(clazz, theProject) + ", " + extractTypeIR(ft, theProject)

    case (_, LDC(_)) => "ldc"
    case (_, LDC2_W(_)) => "ldc"
    case (_, LDC_W(_)) => "ldc"

    case (_, invoke: MethodInvocationInstruction) => "invoke " + extractTypeIR(invoke.declaringClass, theProject) +
      ", ret: " + extractTypeIR(invoke.methodDescriptor.returnType, theProject) +
      ", params: " + extractParamTypes(invoke.methodDescriptor.parameterTypes, theProject)

    case (_, NEW(clazz)) => "new " + extractTypeIR(clazz, theProject)

    case (_, ANEWARRAY(clazz)) => "newarray " + extractTypeIR(clazz, theProject)
    case (_, MULTIANEWARRAY(clazz, _)) => "newarray " + extractTypeIR(clazz, theProject)
    case (_, NEWARRAY(clazz)) => "newarray " + extractTypeIR(clazz, theProject)
    case (_, ARRAYLENGTH) => "arraylength"

    case (_, ATHROW) => "athrow"

    case (_, BIPUSH(_)) => "push"
    case (_, SIPUSH(_)) => "push"

    case (_, NOP) => "nop"

    case (_, CHECKCAST(clazz)) => "check " + extractTypeIR(clazz, theProject)
    case (_, INSTANCEOF(clazz)) => "check " + extractTypeIR(clazz, theProject)

    case (_, _: NumericConversionInstruction) => "to"

    case (_, _: LoadLocalVariableInstruction) => "load"

    case (_, _: ArrayLoadInstruction) => "ArrayLoad"

    case (_, _: StoreLocalVariableInstruction) => "store"

    case (_, _: PrimitiveArrayStoreInstruction) => "ArrayStore"

    case (_, _: ReturnValueInstruction) => "return"
    case (_, RET(_)) => "return"

    case (_, ACONST_NULL) | (_, DCONST_0) | (_, DCONST_1) | (_, FCONST_0) | (_, FCONST_1) |
         (_, FCONST_2) | (_, ICONST_M1) | (_, ICONST_0) | (_, ICONST_1) | (_, ICONST_2) |
         (_, ICONST_3) | (_, ICONST_4) | (_, ICONST_5) | (_, LCONST_0) | (_, LCONST_1) => "const"

    case (_, _: AddInstruction) => "add"
    case (_, IINC(_, _)) => "add"

    case (_, _: FloatingPointDivideInstruction) => "div"
    case (_, _: IntegerDivideInstruction) => "div"

    case (_, _: MultiplyInstruction) => "mul"
    case (_, _: ShiftInstruction) => "mul"

    case (_, _: NegateInstruction) => "neg"

    case (_, _: FloatingPointRemainderInstruction) => "rem"
    case (_, _: IntegerRemainderInstruction) => "rem"

    case (_, _: SubtractInstruction) => "sub"

    case (_, DCMPG) => "if along"
    case (_, DCMPL) => "if along"
    case (_, FCMPG) => "if along"
    case (_, FCMPL) => "if along"
    case (_, LCMP) => "if along"
    case (_, IF_ACMPEQ(direction)) => "if " + (if (direction > 0) "along" else "back")
    case (_, IF_ACMPNE(direction)) => "if " + (if (direction > 0) "along" else "back")
    case (_, IF_ICMPEQ(direction)) => "if " + (if (direction > 0) "along" else "back")
    case (_, IF_ICMPGE(direction)) => "if " + (if (direction > 0) "along" else "back")
    case (_, IF_ICMPGT(direction)) => "if " + (if (direction > 0) "along" else "back")
    case (_, IF_ICMPLE(direction)) => "if " + (if (direction > 0) "along" else "back")
    case (_, IF_ICMPLT(direction)) => "if " + (if (direction > 0) "along" else "back")
    case (_, IF_ICMPNE(direction)) => "if " + (if (direction > 0) "along" else "back")
    case (_, IFGE(direction)) => "if " + (if (direction > 0) "along" else "back")
    case (_, IFGT(direction)) => "if " + (if (direction > 0) "along" else "back")
    case (_, IFLE(direction)) => "if " + (if (direction > 0) "along" else "back")
    case (_, IFLT(direction)) => "if " + (if (direction > 0) "along" else "back")
    case (_, IFNE(direction)) => "if " + (if (direction > 0) "along" else "back")
    case (_, IFNONNULL(direction)) => "if " + (if (direction > 0) "along" else "back")
    case (_, IFNULL(direction)) => "if " + (if (direction > 0) "along" else "back")
    case (_, JSR(direction)) => "if " + (if (direction > 0) "along" else "back")
    case (_, JSR_W(direction)) => "if " + (if (direction > 0) "along" else "back")
    case (_, GOTO(direction)) => "if " + (if (direction > 0) "along" else "back")
    case (_, GOTO_W(direction)) => "if " + (if (direction > 0) "along" else "back")

    case (_, DUP) | (_, DUP_X1) | (_, DUP_X2) | (_, DUP2) | (_, DUP2_X1) | (_, DUP2_X2) => "dup"

    case (_, IAND) => "and"
    case (_, LAND) => "and"

    case (_, IOR) => "or"
    case (_, LOR) => "or"

    case (_, IXOR) => "xor"
    case (_, LXOR) => "xor"

    case (_, LOOKUPSWITCH(_, _)) => "switch"
    case (_, TABLESWITCH(_, _, _, _)) => "switch"

    case (_, MONITORENTER) => "monitor"
    case (_, MONITOREXIT) => "monitor"

    case (_, POP) => "pop"
    case (_, POP2) => "pop"

    case (_, SWAP) => "swap"

    case (_, WIDE) => "wide"

    case (_, p) => p.toString()
  }

}
