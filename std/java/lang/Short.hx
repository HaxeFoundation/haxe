package java.lang;
/*
* Copyright (c) 1996, 2009, Oracle and/or its affiliates. All rights reserved.
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
* The {@code Short} class wraps a value of primitive type {@code
* short} in an object.  An object of type {@code Short} contains a
* single field whose type is {@code short}.
*
* <p>In addition, this class provides several methods for converting
* a {@code short} to a {@code String} and a {@code String} to a
* {@code short}, as well as other constants and methods useful when
* dealing with a {@code short}.
*
* @author  Nakul Saraiya
* @author  Joseph D. Darcy
* @see     java.lang.Number
* @since   JDK1.1
*/
@:require(java1) extern class Short extends java.lang.Number implements java.lang.Comparable<Short>
{
	/**
	* A constant holding the minimum value a {@code short} can
	* have, -2<sup>15</sup>.
	*/
	public static var MIN_VALUE(default, null) : java.StdTypes.Int16;
	
	/**
	* A constant holding the maximum value a {@code short} can
	* have, 2<sup>15</sup>-1.
	*/
	public static var MAX_VALUE(default, null) : java.StdTypes.Int16;
	
	/**
	* The {@code Class} instance representing the primitive type
	* {@code short}.
	*/
	public static var TYPE(default, null) : Class<Short>;
	
	/**
	* Returns a new {@code String} object representing the
	* specified {@code short}. The radix is assumed to be 10.
	*
	* @param s the {@code short} to be converted
	* @return the string representation of the specified {@code short}
	* @see java.lang.Integer#toString(int)
	*/
	@:native('toString') @:overload public static function _toString(s : java.StdTypes.Int16) : String;
	
	/**
	* Parses the string argument as a signed {@code short} in the
	* radix specified by the second argument. The characters in the
	* string must all be digits, of the specified radix (as
	* determined by whether {@link java.lang.Character#digit(char,
	* int)} returns a nonnegative value) except that the first
	* character may be an ASCII minus sign {@code '-'}
	* (<code>'&#92;u002D'</code>) to indicate a negative value or an
	* ASCII plus sign {@code '+'} (<code>'&#92;u002B'</code>) to
	* indicate a positive value.  The resulting {@code short} value
	* is returned.
	*
	* <p>An exception of type {@code NumberFormatException} is
	* thrown if any of the following situations occurs:
	* <ul>
	* <li> The first argument is {@code null} or is a string of
	* length zero.
	*
	* <li> The radix is either smaller than {@link
	* java.lang.Character#MIN_RADIX} or larger than {@link
	* java.lang.Character#MAX_RADIX}.
	*
	* <li> Any character of the string is not a digit of the
	* specified radix, except that the first character may be a minus
	* sign {@code '-'} (<code>'&#92;u002D'</code>) or plus sign
	* {@code '+'} (<code>'&#92;u002B'</code>) provided that the
	* string is longer than length 1.
	*
	* <li> The value represented by the string is not a value of type
	* {@code short}.
	* </ul>
	*
	* @param s         the {@code String} containing the
	*                  {@code short} representation to be parsed
	* @param radix     the radix to be used while parsing {@code s}
	* @return          the {@code short} represented by the string
	*                  argument in the specified radix.
	* @throws          NumberFormatException If the {@code String}
	*                  does not contain a parsable {@code short}.
	*/
	@:overload public static function parseShort(s : String, radix : Int) : java.StdTypes.Int16;
	
	/**
	* Parses the string argument as a signed decimal {@code
	* short}. The characters in the string must all be decimal
	* digits, except that the first character may be an ASCII minus
	* sign {@code '-'} (<code>'&#92;u002D'</code>) to indicate a
	* negative value or an ASCII plus sign {@code '+'}
	* (<code>'&#92;u002B'</code>) to indicate a positive value.  The
	* resulting {@code short} value is returned, exactly as if the
	* argument and the radix 10 were given as arguments to the {@link
	* #parseShort(java.lang.String, int)} method.
	*
	* @param s a {@code String} containing the {@code short}
	*          representation to be parsed
	* @return  the {@code short} value represented by the
	*          argument in decimal.
	* @throws  NumberFormatException If the string does not
	*          contain a parsable {@code short}.
	*/
	@:overload public static function parseShort(s : String) : java.StdTypes.Int16;
	
	/**
	* Returns a {@code Short} object holding the value
	* extracted from the specified {@code String} when parsed
	* with the radix given by the second argument. The first argument
	* is interpreted as representing a signed {@code short} in
	* the radix specified by the second argument, exactly as if the
	* argument were given to the {@link #parseShort(java.lang.String,
	* int)} method. The result is a {@code Short} object that
	* represents the {@code short} value specified by the string.
	*
	* <p>In other words, this method returns a {@code Short} object
	* equal to the value of:
	*
	* <blockquote>
	*  {@code new Short(Short.parseShort(s, radix))}
	* </blockquote>
	*
	* @param s         the string to be parsed
	* @param radix     the radix to be used in interpreting {@code s}
	* @return          a {@code Short} object holding the value
	*                  represented by the string argument in the
	*                  specified radix.
	* @throws          NumberFormatException If the {@code String} does
	*                  not contain a parsable {@code short}.
	*/
	@:overload public static function valueOf(s : String, radix : Int) : Short;
	
	/**
	* Returns a {@code Short} object holding the
	* value given by the specified {@code String}. The argument
	* is interpreted as representing a signed decimal
	* {@code short}, exactly as if the argument were given to
	* the {@link #parseShort(java.lang.String)} method. The result is
	* a {@code Short} object that represents the
	* {@code short} value specified by the string.
	*
	* <p>In other words, this method returns a {@code Short} object
	* equal to the value of:
	*
	* <blockquote>
	*  {@code new Short(Short.parseShort(s))}
	* </blockquote>
	*
	* @param s the string to be parsed
	* @return  a {@code Short} object holding the value
	*          represented by the string argument
	* @throws  NumberFormatException If the {@code String} does
	*          not contain a parsable {@code short}.
	*/
	@:overload public static function valueOf(s : String) : Short;
	
	/**
	* Returns a {@code Short} instance representing the specified
	* {@code short} value.
	* If a new {@code Short} instance is not required, this method
	* should generally be used in preference to the constructor
	* {@link #Short(short)}, as this method is likely to yield
	* significantly better space and time performance by caching
	* frequently requested values.
	*
	* This method will always cache values in the range -128 to 127,
	* inclusive, and may cache other values outside of this range.
	*
	* @param  s a short value.
	* @return a {@code Short} instance representing {@code s}.
	* @since  1.5
	*/
	@:require(java5) @:overload public static function valueOf(s : java.StdTypes.Int16) : Short;
	
	/**
	* Decodes a {@code String} into a {@code Short}.
	* Accepts decimal, hexadecimal, and octal numbers given by
	* the following grammar:
	*
	* <blockquote>
	* <dl>
	* <dt><i>DecodableString:</i>
	* <dd><i>Sign<sub>opt</sub> DecimalNumeral</i>
	* <dd><i>Sign<sub>opt</sub></i> {@code 0x} <i>HexDigits</i>
	* <dd><i>Sign<sub>opt</sub></i> {@code 0X} <i>HexDigits</i>
	* <dd><i>Sign<sub>opt</sub></i> {@code #} <i>HexDigits</i>
	* <dd><i>Sign<sub>opt</sub></i> {@code 0} <i>OctalDigits</i>
	* <p>
	* <dt><i>Sign:</i>
	* <dd>{@code -}
	* <dd>{@code +}
	* </dl>
	* </blockquote>
	*
	* <i>DecimalNumeral</i>, <i>HexDigits</i>, and <i>OctalDigits</i>
	* are as defined in section 3.10.1 of
	* <cite>The Java&trade; Language Specification</cite>,
	* except that underscores are not accepted between digits.
	*
	* <p>The sequence of characters following an optional
	* sign and/or radix specifier ("{@code 0x}", "{@code 0X}",
	* "{@code #}", or leading zero) is parsed as by the {@code
	* Short.parseShort} method with the indicated radix (10, 16, or
	* 8).  This sequence of characters must represent a positive
	* value or a {@link NumberFormatException} will be thrown.  The
	* result is negated if first character of the specified {@code
	* String} is the minus sign.  No whitespace characters are
	* permitted in the {@code String}.
	*
	* @param     nm the {@code String} to decode.
	* @return    a {@code Short} object holding the {@code short}
	*            value represented by {@code nm}
	* @throws    NumberFormatException  if the {@code String} does not
	*            contain a parsable {@code short}.
	* @see java.lang.Short#parseShort(java.lang.String, int)
	*/
	@:overload public static function decode(nm : String) : Short;
	
	/**
	* Constructs a newly allocated {@code Short} object that
	* represents the specified {@code short} value.
	*
	* @param value     the value to be represented by the
	*                  {@code Short}.
	*/
	@:overload public function new(value : java.StdTypes.Int16) : Void;
	
	/**
	* Constructs a newly allocated {@code Short} object that
	* represents the {@code short} value indicated by the
	* {@code String} parameter. The string is converted to a
	* {@code short} value in exactly the manner used by the
	* {@code parseShort} method for radix 10.
	*
	* @param s the {@code String} to be converted to a
	*          {@code Short}
	* @throws  NumberFormatException If the {@code String}
	*          does not contain a parsable {@code short}.
	* @see     java.lang.Short#parseShort(java.lang.String, int)
	*/
	@:overload public function new(s : String) : Void;
	
	/**
	* Returns the value of this {@code Short} as a
	* {@code byte}.
	*/
	@:overload override public function byteValue() : java.StdTypes.Int8;
	
	/**
	* Returns the value of this {@code Short} as a
	* {@code short}.
	*/
	@:overload override public function shortValue() : java.StdTypes.Int16;
	
	/**
	* Returns the value of this {@code Short} as an
	* {@code int}.
	*/
	@:overload override public function intValue() : Int;
	
	/**
	* Returns the value of this {@code Short} as a
	* {@code long}.
	*/
	@:overload override public function longValue() : haxe.Int64;
	
	/**
	* Returns the value of this {@code Short} as a
	* {@code float}.
	*/
	@:overload override public function floatValue() : Single;
	
	/**
	* Returns the value of this {@code Short} as a
	* {@code double}.
	*/
	@:overload override public function doubleValue() : Float;
	
	/**
	* Returns a {@code String} object representing this
	* {@code Short}'s value.  The value is converted to signed
	* decimal representation and returned as a string, exactly as if
	* the {@code short} value were given as an argument to the
	* {@link java.lang.Short#toString(short)} method.
	*
	* @return  a string representation of the value of this object in
	*          base&nbsp;10.
	*/
	@:overload public function toString() : String;
	
	/**
	* Returns a hash code for this {@code Short}; equal to the result
	* of invoking {@code intValue()}.
	*
	* @return a hash code value for this {@code Short}
	*/
	@:overload public function hashCode() : Int;
	
	/**
	* Compares this object to the specified object.  The result is
	* {@code true} if and only if the argument is not
	* {@code null} and is a {@code Short} object that
	* contains the same {@code short} value as this object.
	*
	* @param obj       the object to compare with
	* @return          {@code true} if the objects are the same;
	*                  {@code false} otherwise.
	*/
	@:overload public function equals(obj : Dynamic) : Bool;
	
	/**
	* Compares two {@code Short} objects numerically.
	*
	* @param   anotherShort   the {@code Short} to be compared.
	* @return  the value {@code 0} if this {@code Short} is
	*          equal to the argument {@code Short}; a value less than
	*          {@code 0} if this {@code Short} is numerically less
	*          than the argument {@code Short}; and a value greater than
	*           {@code 0} if this {@code Short} is numerically
	*           greater than the argument {@code Short} (signed
	*           comparison).
	* @since   1.2
	*/
	@:require(java2) @:overload public function compareTo(anotherShort : Short) : Int;
	
	/**
	* Compares two {@code short} values numerically.
	* The value returned is identical to what would be returned by:
	* <pre>
	*    Short.valueOf(x).compareTo(Short.valueOf(y))
	* </pre>
	*
	* @param  x the first {@code short} to compare
	* @param  y the second {@code short} to compare
	* @return the value {@code 0} if {@code x == y};
	*         a value less than {@code 0} if {@code x < y}; and
	*         a value greater than {@code 0} if {@code x > y}
	* @since 1.7
	*/
	@:require(java7) @:overload public static function compare(x : java.StdTypes.Int16, y : java.StdTypes.Int16) : Int;
	
	/**
	* The number of bits used to represent a {@code short} value in two's
	* complement binary form.
	* @since 1.5
	*/
	@:require(java5) public static var SIZE(default, null) : Int;
	
	/**
	* Returns the value obtained by reversing the order of the bytes in the
	* two's complement representation of the specified {@code short} value.
	*
	* @return the value obtained by reversing (or, equivalently, swapping)
	*     the bytes in the specified {@code short} value.
	* @since 1.5
	*/
	@:require(java5) @:overload public static function reverseBytes(i : java.StdTypes.Int16) : java.StdTypes.Int16;
	
	
}
