package java.lang;
/*
* Copyright (c) 1994, 2001, Oracle and/or its affiliates. All rights reserved.
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
* The abstract class <code>Number</code> is the superclass of classes
* <code>BigDecimal</code>, <code>BigInteger</code>,
* <code>Byte</code>, <code>Double</code>, <code>Float</code>,
* <code>Integer</code>, <code>Long</code>, and <code>Short</code>.
* <p>
* Subclasses of <code>Number</code> must provide methods to convert
* the represented numeric value to <code>byte</code>, <code>double</code>,
* <code>float</code>, <code>int</code>, <code>long</code>, and
* <code>short</code>.
*
* @author      Lee Boynton
* @author      Arthur van Hoff
* @see     java.lang.Byte
* @see     java.lang.Double
* @see     java.lang.Float
* @see     java.lang.Integer
* @see     java.lang.Long
* @see     java.lang.Short
* @since   JDK1.0
*/
@:require(java0) extern class Number implements java.io.Serializable
{
	/**
	* Returns the value of the specified number as an <code>int</code>.
	* This may involve rounding or truncation.
	*
	* @return  the numeric value represented by this object after conversion
	*          to type <code>int</code>.
	*/
	@:overload @:abstract public function intValue() : Int;
	
	/**
	* Returns the value of the specified number as a <code>long</code>.
	* This may involve rounding or truncation.
	*
	* @return  the numeric value represented by this object after conversion
	*          to type <code>long</code>.
	*/
	@:overload @:abstract public function longValue() : haxe.Int64;
	
	/**
	* Returns the value of the specified number as a <code>float</code>.
	* This may involve rounding.
	*
	* @return  the numeric value represented by this object after conversion
	*          to type <code>float</code>.
	*/
	@:overload @:abstract public function floatValue() : Single;
	
	/**
	* Returns the value of the specified number as a <code>double</code>.
	* This may involve rounding.
	*
	* @return  the numeric value represented by this object after conversion
	*          to type <code>double</code>.
	*/
	@:overload @:abstract public function doubleValue() : Float;
	
	/**
	* Returns the value of the specified number as a <code>byte</code>.
	* This may involve rounding or truncation.
	*
	* @return  the numeric value represented by this object after conversion
	*          to type <code>byte</code>.
	* @since   JDK1.1
	*/
	@:require(java1) @:overload public function byteValue() : java.StdTypes.Int8;
	
	/**
	* Returns the value of the specified number as a <code>short</code>.
	* This may involve rounding or truncation.
	*
	* @return  the numeric value represented by this object after conversion
	*          to type <code>short</code>.
	* @since   JDK1.1
	*/
	@:require(java1) @:overload public function shortValue() : java.StdTypes.Int16;
	
	
}
