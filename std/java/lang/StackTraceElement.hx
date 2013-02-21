package java.lang;
/*
* Copyright (c) 2000, 2011, Oracle and/or its affiliates. All rights reserved.
* DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
*
* This code is free software; you can redistribute it and/or modify it
* under the terms of the GNU General Public License version 2 only, as
* published by the Free Software Foundation.  Oracle designates this
* particular file as subject to the "Classpath" exception as provided
* by Oracle in the LICENSE file that accompanied this code.
*
* This code is distributed in the hope that it will be useful, but WITHOUT
* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
* FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
* version 2 for more details (a copy is included in the LICENSE file that
* accompanied this code).
*
* You should have received a copy of the GNU General Public License version
* 2 along with this work; if not, write to the Free Software Foundation,
* Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
*
* Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
* or visit www.oracle.com if you need additional information or have any
* questions.
*/
/**
* An element in a stack trace, as returned by {@link
* Throwable#getStackTrace()}.  Each element represents a single stack frame.
* All stack frames except for the one at the top of the stack represent
* a method invocation.  The frame at the top of the stack represents the
* execution point at which the stack trace was generated.  Typically,
* this is the point at which the throwable corresponding to the stack trace
* was created.
*
* @since  1.4
* @author Josh Bloch
*/
@:require(java4) extern class StackTraceElement implements java.io.Serializable
{
	/**
	* Creates a stack trace element representing the specified execution
	* point.
	*
	* @param declaringClass the fully qualified name of the class containing
	*        the execution point represented by the stack trace element
	* @param methodName the name of the method containing the execution point
	*        represented by the stack trace element
	* @param fileName the name of the file containing the execution point
	*         represented by the stack trace element, or {@code null} if
	*         this information is unavailable
	* @param lineNumber the line number of the source line containing the
	*         execution point represented by this stack trace element, or
	*         a negative number if this information is unavailable. A value
	*         of -2 indicates that the method containing the execution point
	*         is a native method
	* @throws NullPointerException if {@code declaringClass} or
	*         {@code methodName} is null
	* @since 1.5
	*/
	@:require(java5) @:overload public function new(declaringClass : String, methodName : String, fileName : String, lineNumber : Int) : Void;
	
	/**
	* Returns the name of the source file containing the execution point
	* represented by this stack trace element.  Generally, this corresponds
	* to the {@code SourceFile} attribute of the relevant {@code class}
	* file (as per <i>The Java Virtual Machine Specification</i>, Section
	* 4.7.7).  In some systems, the name may refer to some source code unit
	* other than a file, such as an entry in source repository.
	*
	* @return the name of the file containing the execution point
	*         represented by this stack trace element, or {@code null} if
	*         this information is unavailable.
	*/
	@:overload public function getFileName() : String;
	
	/**
	* Returns the line number of the source line containing the execution
	* point represented by this stack trace element.  Generally, this is
	* derived from the {@code LineNumberTable} attribute of the relevant
	* {@code class} file (as per <i>The Java Virtual Machine
	* Specification</i>, Section 4.7.8).
	*
	* @return the line number of the source line containing the execution
	*         point represented by this stack trace element, or a negative
	*         number if this information is unavailable.
	*/
	@:overload public function getLineNumber() : Int;
	
	/**
	* Returns the fully qualified name of the class containing the
	* execution point represented by this stack trace element.
	*
	* @return the fully qualified name of the {@code Class} containing
	*         the execution point represented by this stack trace element.
	*/
	@:overload public function getClassName() : String;
	
	/**
	* Returns the name of the method containing the execution point
	* represented by this stack trace element.  If the execution point is
	* contained in an instance or class initializer, this method will return
	* the appropriate <i>special method name</i>, {@code <init>} or
	* {@code <clinit>}, as per Section 3.9 of <i>The Java Virtual
	* Machine Specification</i>.
	*
	* @return the name of the method containing the execution point
	*         represented by this stack trace element.
	*/
	@:overload public function getMethodName() : String;
	
	/**
	* Returns true if the method containing the execution point
	* represented by this stack trace element is a native method.
	*
	* @return {@code true} if the method containing the execution point
	*         represented by this stack trace element is a native method.
	*/
	@:overload public function isNativeMethod() : Bool;
	
	/**
	* Returns a string representation of this stack trace element.  The
	* format of this string depends on the implementation, but the following
	* examples may be regarded as typical:
	* <ul>
	* <li>
	*   {@code "MyClass.mash(MyClass.java:9)"} - Here, {@code "MyClass"}
	*   is the <i>fully-qualified name</i> of the class containing the
	*   execution point represented by this stack trace element,
	*   {@code "mash"} is the name of the method containing the execution
	*   point, {@code "MyClass.java"} is the source file containing the
	*   execution point, and {@code "9"} is the line number of the source
	*   line containing the execution point.
	* <li>
	*   {@code "MyClass.mash(MyClass.java)"} - As above, but the line
	*   number is unavailable.
	* <li>
	*   {@code "MyClass.mash(Unknown Source)"} - As above, but neither
	*   the file name nor the line  number are available.
	* <li>
	*   {@code "MyClass.mash(Native Method)"} - As above, but neither
	*   the file name nor the line  number are available, and the method
	*   containing the execution point is known to be a native method.
	* </ul>
	* @see    Throwable#printStackTrace()
	*/
	@:overload public function toString() : String;
	
	/**
	* Returns true if the specified object is another
	* {@code StackTraceElement} instance representing the same execution
	* point as this instance.  Two stack trace elements {@code a} and
	* {@code b} are equal if and only if:
	* <pre>
	*     equals(a.getFileName(), b.getFileName()) &&
	*     a.getLineNumber() == b.getLineNumber()) &&
	*     equals(a.getClassName(), b.getClassName()) &&
	*     equals(a.getMethodName(), b.getMethodName())
	* </pre>
	* where {@code equals} has the semantics of {@link
	* java.util.Objects#equals(Object, Object) Objects.equals}.
	*
	* @param  obj the object to be compared with this stack trace element.
	* @return true if the specified object is another
	*         {@code StackTraceElement} instance representing the same
	*         execution point as this instance.
	*/
	@:overload public function equals(obj : Dynamic) : Bool;
	
	/**
	* Returns a hash code value for this stack trace element.
	*/
	@:overload public function hashCode() : Int;
	
	
}
