package java.lang;
/*
* Copyright (c) 1994, 2011, Oracle and/or its affiliates. All rights reserved.
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
* The class {@code Exception} and its subclasses are a form of
* {@code Throwable} that indicates conditions that a reasonable
* application might want to catch.
*
* <p>The class {@code Exception} and any subclasses that are not also
* subclasses of {@link RuntimeException} are <em>checked
* exceptions</em>.  Checked exceptions need to be declared in a
* method or constructor's {@code throws} clause if they can be thrown
* by the execution of the method or constructor and propagate outside
* the method or constructor boundary.
*
* @author  Frank Yellin
* @see     java.lang.Error
* @jls 11.2 Compile-Time Checking of Exceptions
* @since   JDK1.0
*/
@:require(java0) extern class Exception extends java.lang.Throwable
{
	/**
	* Constructs a new exception with {@code null} as its detail message.
	* The cause is not initialized, and may subsequently be initialized by a
	* call to {@link #initCause}.
	*/
	@:overload public function new() : Void;
	
	/**
	* Constructs a new exception with the specified detail message.  The
	* cause is not initialized, and may subsequently be initialized by
	* a call to {@link #initCause}.
	*
	* @param   message   the detail message. The detail message is saved for
	*          later retrieval by the {@link #getMessage()} method.
	*/
	@:overload public function new(message : String) : Void;
	
	/**
	* Constructs a new exception with the specified detail message and
	* cause.  <p>Note that the detail message associated with
	* {@code cause} is <i>not</i> automatically incorporated in
	* this exception's detail message.
	*
	* @param  message the detail message (which is saved for later retrieval
	*         by the {@link #getMessage()} method).
	* @param  cause the cause (which is saved for later retrieval by the
	*         {@link #getCause()} method).  (A <tt>null</tt> value is
	*         permitted, and indicates that the cause is nonexistent or
	*         unknown.)
	* @since  1.4
	*/
	@:require(java4) @:overload public function new(message : String, cause : java.lang.Throwable) : Void;
	
	/**
	* Constructs a new exception with the specified cause and a detail
	* message of <tt>(cause==null ? null : cause.toString())</tt> (which
	* typically contains the class and detail message of <tt>cause</tt>).
	* This constructor is useful for exceptions that are little more than
	* wrappers for other throwables (for example, {@link
	* java.security.PrivilegedActionException}).
	*
	* @param  cause the cause (which is saved for later retrieval by the
	*         {@link #getCause()} method).  (A <tt>null</tt> value is
	*         permitted, and indicates that the cause is nonexistent or
	*         unknown.)
	* @since  1.4
	*/
	@:require(java4) @:overload public function new(cause : java.lang.Throwable) : Void;
	
	/**
	* Constructs a new exception with the specified detail message,
	* cause, suppression enabled or disabled, and writable stack
	* trace enabled or disabled.
	*
	* @param  message the detail message.
	* @param cause the cause.  (A {@code null} value is permitted,
	* and indicates that the cause is nonexistent or unknown.)
	* @param enableSuppression whether or not suppression is enabled
	*                          or disabled
	* @param writableStackTrace whether or not the stack trace should
	*                           be writable
	* @since 1.7
	*/
	@:require(java7) @:overload private function new(message : String, cause : java.lang.Throwable, enableSuppression : Bool, writableStackTrace : Bool) : Void;
	
	
}
