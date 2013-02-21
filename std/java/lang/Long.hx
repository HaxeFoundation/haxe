package java.lang;
/*
* Copyright (c) 1994, 2009, Oracle and/or its affiliates. All rights reserved.
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
* The {@code Long} class wraps a value of the primitive type {@code
* long} in an object. An object of type {@code Long} contains a
* single field whose type is {@code long}.
*
* <p> In addition, this class provides several methods for converting
* a {@code long} to a {@code String} and a {@code String} to a {@code
* long}, as well as other constants and methods useful when dealing
* with a {@code long}.
*
* <p>Implementation note: The implementations of the "bit twiddling"
* methods (such as {@link #highestOneBit(long) highestOneBit} and
* {@link #numberOfTrailingZeros(long) numberOfTrailingZeros}) are
* based on material from Henry S. Warren, Jr.'s <i>Hacker's
* Delight</i>, (Addison Wesley, 2002).
*
* @author  Lee Boynton
* @author  Arthur van Hoff
* @author  Josh Bloch
* @author  Joseph D. Darcy
* @since   JDK1.0
*/
@:require(java0) extern class Long extends java.lang.Number implements java.lang.Comparable<Long>
{
	/**
	* A constant holding the minimum value a {@code long} can
	* have, -2<sup>63</sup>.
	*/
	public static var MIN_VALUE(default, null) : haxe.Int64;
	
	/**
	* A constant holding the maximum value a {@code long} can
	* have, 2<sup>63</sup>-1.
	*/
	public static var MAX_VALUE(default, null) : haxe.Int64;
	
	/**
	* The {@code Class} instance representing the primitive type
	* {@code long}.
	*
	* @since   JDK1.1
	*/
	@:require(java1) public static var TYPE(default, null) : Class<Long>;
	
	/**
	* Returns a string representation of the first argument in the
	* radix specified by the second argument.
	*
	* <p>If the radix is smaller than {@code Character.MIN_RADIX}
	* or larger than {@code Character.MAX_RADIX}, then the radix
	* {@code 10} is used instead.
	*
	* <p>If the first argument is negative, the first element of the
	* result is the ASCII minus sign {@code '-'}
	* (<code>'&#92;u002d'</code>). If the first argument is not
	* negative, no sign character appears in the result.
	*
	* <p>The remaining characters of the result represent the magnitude
	* of the first argument. If the magnitude is zero, it is
	* represented by a single zero character {@code '0'}
	* (<code>'&#92;u0030'</code>); otherwise, the first character of
	* the representation of the magnitude will not be the zero
	* character.  The following ASCII characters are used as digits:
	*
	* <blockquote>
	*   {@code 0123456789abcdefghijklmnopqrstuvwxyz}
	* </blockquote>
	*
	* These are <code>'&#92;u0030'</code> through
	* <code>'&#92;u0039'</code> and <code>'&#92;u0061'</code> through
	* <code>'&#92;u007a'</code>. If {@code radix} is
	* <var>N</var>, then the first <var>N</var> of these characters
	* are used as radix-<var>N</var> digits in the order shown. Thus,
	* the digits for hexadecimal (radix 16) are
	* {@code 0123456789abcdef}. If uppercase letters are
	* desired, the {@link java.lang.String#toUpperCase()} method may
	* be called on the result:
	*
	* <blockquote>
	*  {@code Long.toString(n, 16).toUpperCase()}
	* </blockquote>
	*
	* @param   i       a {@code long} to be converted to a string.
	* @param   radix   the radix to use in the string representation.
	* @return  a string representation of the argument in the specified radix.
	* @see     java.lang.Character#MAX_RADIX
	* @see     java.lang.Character#MIN_RADIX
	*/
	@:native('toString') @:overload public static function _toString(i : haxe.Int64, radix : Int) : String;
	
	/**
	* Returns a string representation of the {@code long}
	* argument as an unsigned integer in base&nbsp;16.
	*
	* <p>The unsigned {@code long} value is the argument plus
	* 2<sup>64</sup> if the argument is negative; otherwise, it is
	* equal to the argument.  This value is converted to a string of
	* ASCII digits in hexadecimal (base&nbsp;16) with no extra
	* leading {@code 0}s.  If the unsigned magnitude is zero, it
	* is represented by a single zero character {@code '0'}
	* (<code>'&#92;u0030'</code>); otherwise, the first character of
	* the representation of the unsigned magnitude will not be the
	* zero character. The following characters are used as
	* hexadecimal digits:
	*
	* <blockquote>
	*  {@code 0123456789abcdef}
	* </blockquote>
	*
	* These are the characters <code>'&#92;u0030'</code> through
	* <code>'&#92;u0039'</code> and  <code>'&#92;u0061'</code> through
	* <code>'&#92;u0066'</code>.  If uppercase letters are desired,
	* the {@link java.lang.String#toUpperCase()} method may be called
	* on the result:
	*
	* <blockquote>
	*  {@code Long.toHexString(n).toUpperCase()}
	* </blockquote>
	*
	* @param   i   a {@code long} to be converted to a string.
	* @return  the string representation of the unsigned {@code long}
	*          value represented by the argument in hexadecimal
	*          (base&nbsp;16).
	* @since   JDK 1.0.2
	*/
	@:require(java0) @:overload public static function toHexString(i : haxe.Int64) : String;
	
	/**
	* Returns a string representation of the {@code long}
	* argument as an unsigned integer in base&nbsp;8.
	*
	* <p>The unsigned {@code long} value is the argument plus
	* 2<sup>64</sup> if the argument is negative; otherwise, it is
	* equal to the argument.  This value is converted to a string of
	* ASCII digits in octal (base&nbsp;8) with no extra leading
	* {@code 0}s.
	*
	* <p>If the unsigned magnitude is zero, it is represented by a
	* single zero character {@code '0'}
	* (<code>'&#92;u0030'</code>); otherwise, the first character of
	* the representation of the unsigned magnitude will not be the
	* zero character. The following characters are used as octal
	* digits:
	*
	* <blockquote>
	*  {@code 01234567}
	* </blockquote>
	*
	* These are the characters <code>'&#92;u0030'</code> through
	* <code>'&#92;u0037'</code>.
	*
	* @param   i   a {@code long} to be converted to a string.
	* @return  the string representation of the unsigned {@code long}
	*          value represented by the argument in octal (base&nbsp;8).
	* @since   JDK 1.0.2
	*/
	@:require(java0) @:overload public static function toOctalString(i : haxe.Int64) : String;
	
	/**
	* Returns a string representation of the {@code long}
	* argument as an unsigned integer in base&nbsp;2.
	*
	* <p>The unsigned {@code long} value is the argument plus
	* 2<sup>64</sup> if the argument is negative; otherwise, it is
	* equal to the argument.  This value is converted to a string of
	* ASCII digits in binary (base&nbsp;2) with no extra leading
	* {@code 0}s.  If the unsigned magnitude is zero, it is
	* represented by a single zero character {@code '0'}
	* (<code>'&#92;u0030'</code>); otherwise, the first character of
	* the representation of the unsigned magnitude will not be the
	* zero character. The characters {@code '0'}
	* (<code>'&#92;u0030'</code>) and {@code '1'}
	* (<code>'&#92;u0031'</code>) are used as binary digits.
	*
	* @param   i   a {@code long} to be converted to a string.
	* @return  the string representation of the unsigned {@code long}
	*          value represented by the argument in binary (base&nbsp;2).
	* @since   JDK 1.0.2
	*/
	@:require(java0) @:overload public static function toBinaryString(i : haxe.Int64) : String;
	
	/**
	* Returns a {@code String} object representing the specified
	* {@code long}.  The argument is converted to signed decimal
	* representation and returned as a string, exactly as if the
	* argument and the radix 10 were given as arguments to the {@link
	* #toString(long, int)} method.
	*
	* @param   i   a {@code long} to be converted.
	* @return  a string representation of the argument in base&nbsp;10.
	*/
	@:native('toString') @:overload public static function _toString(i : haxe.Int64) : String;
	
	/**
	* Parses the string argument as a signed {@code long} in the
	* radix specified by the second argument. The characters in the
	* string must all be digits of the specified radix (as determined
	* by whether {@link java.lang.Character#digit(char, int)} returns
	* a nonnegative value), except that the first character may be an
	* ASCII minus sign {@code '-'} (<code>'&#92;u002D'</code>) to
	* indicate a negative value or an ASCII plus sign {@code '+'}
	* (<code>'&#92;u002B'</code>) to indicate a positive value. The
	* resulting {@code long} value is returned.
	*
	* <p>Note that neither the character {@code L}
	* (<code>'&#92;u004C'</code>) nor {@code l}
	* (<code>'&#92;u006C'</code>) is permitted to appear at the end
	* of the string as a type indicator, as would be permitted in
	* Java programming language source code - except that either
	* {@code L} or {@code l} may appear as a digit for a
	* radix greater than 22.
	*
	* <p>An exception of type {@code NumberFormatException} is
	* thrown if any of the following situations occurs:
	* <ul>
	*
	* <li>The first argument is {@code null} or is a string of
	* length zero.
	*
	* <li>The {@code radix} is either smaller than {@link
	* java.lang.Character#MIN_RADIX} or larger than {@link
	* java.lang.Character#MAX_RADIX}.
	*
	* <li>Any character of the string is not a digit of the specified
	* radix, except that the first character may be a minus sign
	* {@code '-'} (<code>'&#92;u002d'</code>) or plus sign {@code
	* '+'} (<code>'&#92;u002B'</code>) provided that the string is
	* longer than length 1.
	*
	* <li>The value represented by the string is not a value of type
	*      {@code long}.
	* </ul>
	*
	* <p>Examples:
	* <blockquote><pre>
	* parseLong("0", 10) returns 0L
	* parseLong("473", 10) returns 473L
	* parseLong("+42", 10) returns 42L
	* parseLong("-0", 10) returns 0L
	* parseLong("-FF", 16) returns -255L
	* parseLong("1100110", 2) returns 102L
	* parseLong("99", 8) throws a NumberFormatException
	* parseLong("Hazelnut", 10) throws a NumberFormatException
	* parseLong("Hazelnut", 36) returns 1356099454469L
	* </pre></blockquote>
	*
	* @param      s       the {@code String} containing the
	*                     {@code long} representation to be parsed.
	* @param      radix   the radix to be used while parsing {@code s}.
	* @return     the {@code long} represented by the string argument in
	*             the specified radix.
	* @throws     NumberFormatException  if the string does not contain a
	*             parsable {@code long}.
	*/
	@:overload public static function parseLong(s : String, radix : Int) : haxe.Int64;
	
	/**
	* Parses the string argument as a signed decimal {@code long}.
	* The characters in the string must all be decimal digits, except
	* that the first character may be an ASCII minus sign {@code '-'}
	* (<code>&#92;u002D'</code>) to indicate a negative value or an
	* ASCII plus sign {@code '+'} (<code>'&#92;u002B'</code>) to
	* indicate a positive value. The resulting {@code long} value is
	* returned, exactly as if the argument and the radix {@code 10}
	* were given as arguments to the {@link
	* #parseLong(java.lang.String, int)} method.
	*
	* <p>Note that neither the character {@code L}
	* (<code>'&#92;u004C'</code>) nor {@code l}
	* (<code>'&#92;u006C'</code>) is permitted to appear at the end
	* of the string as a type indicator, as would be permitted in
	* Java programming language source code.
	*
	* @param      s   a {@code String} containing the {@code long}
	*             representation to be parsed
	* @return     the {@code long} represented by the argument in
	*             decimal.
	* @throws     NumberFormatException  if the string does not contain a
	*             parsable {@code long}.
	*/
	@:overload public static function parseLong(s : String) : haxe.Int64;
	
	/**
	* Returns a {@code Long} object holding the value
	* extracted from the specified {@code String} when parsed
	* with the radix given by the second argument.  The first
	* argument is interpreted as representing a signed
	* {@code long} in the radix specified by the second
	* argument, exactly as if the arguments were given to the {@link
	* #parseLong(java.lang.String, int)} method. The result is a
	* {@code Long} object that represents the {@code long}
	* value specified by the string.
	*
	* <p>In other words, this method returns a {@code Long} object equal
	* to the value of:
	*
	* <blockquote>
	*  {@code new Long(Long.parseLong(s, radix))}
	* </blockquote>
	*
	* @param      s       the string to be parsed
	* @param      radix   the radix to be used in interpreting {@code s}
	* @return     a {@code Long} object holding the value
	*             represented by the string argument in the specified
	*             radix.
	* @throws     NumberFormatException  If the {@code String} does not
	*             contain a parsable {@code long}.
	*/
	@:overload public static function valueOf(s : String, radix : Int) : Long;
	
	/**
	* Returns a {@code Long} object holding the value
	* of the specified {@code String}. The argument is
	* interpreted as representing a signed decimal {@code long},
	* exactly as if the argument were given to the {@link
	* #parseLong(java.lang.String)} method. The result is a
	* {@code Long} object that represents the integer value
	* specified by the string.
	*
	* <p>In other words, this method returns a {@code Long} object
	* equal to the value of:
	*
	* <blockquote>
	*  {@code new Long(Long.parseLong(s))}
	* </blockquote>
	*
	* @param      s   the string to be parsed.
	* @return     a {@code Long} object holding the value
	*             represented by the string argument.
	* @throws     NumberFormatException  If the string cannot be parsed
	*             as a {@code long}.
	*/
	@:overload public static function valueOf(s : String) : Long;
	
	/**
	* Returns a {@code Long} instance representing the specified
	* {@code long} value.
	* If a new {@code Long} instance is not required, this method
	* should generally be used in preference to the constructor
	* {@link #Long(long)}, as this method is likely to yield
	* significantly better space and time performance by caching
	* frequently requested values.
	*
	* Note that unlike the {@linkplain Integer#valueOf(int)
	* corresponding method} in the {@code Integer} class, this method
	* is <em>not</em> required to cache values within a particular
	* range.
	*
	* @param  l a long value.
	* @return a {@code Long} instance representing {@code l}.
	* @since  1.5
	*/
	@:require(java5) @:overload public static function valueOf(l : haxe.Int64) : Long;
	
	/**
	* Decodes a {@code String} into a {@code Long}.
	* Accepts decimal, hexadecimal, and octal numbers given by the
	* following grammar:
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
	* Long.parseLong} method with the indicated radix (10, 16, or 8).
	* This sequence of characters must represent a positive value or
	* a {@link NumberFormatException} will be thrown.  The result is
	* negated if first character of the specified {@code String} is
	* the minus sign.  No whitespace characters are permitted in the
	* {@code String}.
	*
	* @param     nm the {@code String} to decode.
	* @return    a {@code Long} object holding the {@code long}
	*            value represented by {@code nm}
	* @throws    NumberFormatException  if the {@code String} does not
	*            contain a parsable {@code long}.
	* @see java.lang.Long#parseLong(String, int)
	* @since 1.2
	*/
	@:require(java2) @:overload public static function decode(nm : String) : Long;
	
	/**
	* Constructs a newly allocated {@code Long} object that
	* represents the specified {@code long} argument.
	*
	* @param   value   the value to be represented by the
	*          {@code Long} object.
	*/
	@:overload public function new(value : haxe.Int64) : Void;
	
	/**
	* Constructs a newly allocated {@code Long} object that
	* represents the {@code long} value indicated by the
	* {@code String} parameter. The string is converted to a
	* {@code long} value in exactly the manner used by the
	* {@code parseLong} method for radix 10.
	*
	* @param      s   the {@code String} to be converted to a
	*             {@code Long}.
	* @throws     NumberFormatException  if the {@code String} does not
	*             contain a parsable {@code long}.
	* @see        java.lang.Long#parseLong(java.lang.String, int)
	*/
	@:overload public function new(s : String) : Void;
	
	/**
	* Returns the value of this {@code Long} as a
	* {@code byte}.
	*/
	@:overload override public function byteValue() : java.StdTypes.Int8;
	
	/**
	* Returns the value of this {@code Long} as a
	* {@code short}.
	*/
	@:overload override public function shortValue() : java.StdTypes.Int16;
	
	/**
	* Returns the value of this {@code Long} as an
	* {@code int}.
	*/
	@:overload override public function intValue() : Int;
	
	/**
	* Returns the value of this {@code Long} as a
	* {@code long} value.
	*/
	@:overload override public function longValue() : haxe.Int64;
	
	/**
	* Returns the value of this {@code Long} as a
	* {@code float}.
	*/
	@:overload override public function floatValue() : Single;
	
	/**
	* Returns the value of this {@code Long} as a
	* {@code double}.
	*/
	@:overload override public function doubleValue() : Float;
	
	/**
	* Returns a {@code String} object representing this
	* {@code Long}'s value.  The value is converted to signed
	* decimal representation and returned as a string, exactly as if
	* the {@code long} value were given as an argument to the
	* {@link java.lang.Long#toString(long)} method.
	*
	* @return  a string representation of the value of this object in
	*          base&nbsp;10.
	*/
	@:overload public function toString() : String;
	
	/**
	* Returns a hash code for this {@code Long}. The result is
	* the exclusive OR of the two halves of the primitive
	* {@code long} value held by this {@code Long}
	* object. That is, the hashcode is the value of the expression:
	*
	* <blockquote>
	*  {@code (int)(this.longValue()^(this.longValue()>>>32))}
	* </blockquote>
	*
	* @return  a hash code value for this object.
	*/
	@:overload public function hashCode() : Int;
	
	/**
	* Compares this object to the specified object.  The result is
	* {@code true} if and only if the argument is not
	* {@code null} and is a {@code Long} object that
	* contains the same {@code long} value as this object.
	*
	* @param   obj   the object to compare with.
	* @return  {@code true} if the objects are the same;
	*          {@code false} otherwise.
	*/
	@:overload public function equals(obj : Dynamic) : Bool;
	
	/**
	* Determines the {@code long} value of the system property
	* with the specified name.
	*
	* <p>The first argument is treated as the name of a system property.
	* System properties are accessible through the {@link
	* java.lang.System#getProperty(java.lang.String)} method. The
	* string value of this property is then interpreted as a
	* {@code long} value and a {@code Long} object
	* representing this value is returned.  Details of possible
	* numeric formats can be found with the definition of
	* {@code getProperty}.
	*
	* <p>If there is no property with the specified name, if the
	* specified name is empty or {@code null}, or if the
	* property does not have the correct numeric format, then
	* {@code null} is returned.
	*
	* <p>In other words, this method returns a {@code Long} object equal to
	* the value of:
	*
	* <blockquote>
	*  {@code getLong(nm, null)}
	* </blockquote>
	*
	* @param   nm   property name.
	* @return  the {@code Long} value of the property.
	* @see     java.lang.System#getProperty(java.lang.String)
	* @see     java.lang.System#getProperty(java.lang.String, java.lang.String)
	*/
	@:overload public static function getLong(nm : String) : Long;
	
	/**
	* Determines the {@code long} value of the system property
	* with the specified name.
	*
	* <p>The first argument is treated as the name of a system property.
	* System properties are accessible through the {@link
	* java.lang.System#getProperty(java.lang.String)} method. The
	* string value of this property is then interpreted as a
	* {@code long} value and a {@code Long} object
	* representing this value is returned.  Details of possible
	* numeric formats can be found with the definition of
	* {@code getProperty}.
	*
	* <p>The second argument is the default value. A {@code Long} object
	* that represents the value of the second argument is returned if there
	* is no property of the specified name, if the property does not have
	* the correct numeric format, or if the specified name is empty or null.
	*
	* <p>In other words, this method returns a {@code Long} object equal
	* to the value of:
	*
	* <blockquote>
	*  {@code getLong(nm, new Long(val))}
	* </blockquote>
	*
	* but in practice it may be implemented in a manner such as:
	*
	* <blockquote><pre>
	* Long result = getLong(nm, null);
	* return (result == null) ? new Long(val) : result;
	* </pre></blockquote>
	*
	* to avoid the unnecessary allocation of a {@code Long} object when
	* the default value is not needed.
	*
	* @param   nm    property name.
	* @param   val   default value.
	* @return  the {@code Long} value of the property.
	* @see     java.lang.System#getProperty(java.lang.String)
	* @see     java.lang.System#getProperty(java.lang.String, java.lang.String)
	*/
	@:overload public static function getLong(nm : String, val : haxe.Int64) : Long;
	
	/**
	* Returns the {@code long} value of the system property with
	* the specified name.  The first argument is treated as the name
	* of a system property.  System properties are accessible through
	* the {@link java.lang.System#getProperty(java.lang.String)}
	* method. The string value of this property is then interpreted
	* as a {@code long} value, as per the
	* {@code Long.decode} method, and a {@code Long} object
	* representing this value is returned.
	*
	* <ul>
	* <li>If the property value begins with the two ASCII characters
	* {@code 0x} or the ASCII character {@code #}, not followed by
	* a minus sign, then the rest of it is parsed as a hexadecimal integer
	* exactly as for the method {@link #valueOf(java.lang.String, int)}
	* with radix 16.
	* <li>If the property value begins with the ASCII character
	* {@code 0} followed by another character, it is parsed as
	* an octal integer exactly as by the method {@link
	* #valueOf(java.lang.String, int)} with radix 8.
	* <li>Otherwise the property value is parsed as a decimal
	* integer exactly as by the method
	* {@link #valueOf(java.lang.String, int)} with radix 10.
	* </ul>
	*
	* <p>Note that, in every case, neither {@code L}
	* (<code>'&#92;u004C'</code>) nor {@code l}
	* (<code>'&#92;u006C'</code>) is permitted to appear at the end
	* of the property value as a type indicator, as would be
	* permitted in Java programming language source code.
	*
	* <p>The second argument is the default value. The default value is
	* returned if there is no property of the specified name, if the
	* property does not have the correct numeric format, or if the
	* specified name is empty or {@code null}.
	*
	* @param   nm   property name.
	* @param   val   default value.
	* @return  the {@code Long} value of the property.
	* @see     java.lang.System#getProperty(java.lang.String)
	* @see java.lang.System#getProperty(java.lang.String, java.lang.String)
	* @see java.lang.Long#decode
	*/
	@:overload public static function getLong(nm : String, val : Long) : Long;
	
	/**
	* Compares two {@code Long} objects numerically.
	*
	* @param   anotherLong   the {@code Long} to be compared.
	* @return  the value {@code 0} if this {@code Long} is
	*          equal to the argument {@code Long}; a value less than
	*          {@code 0} if this {@code Long} is numerically less
	*          than the argument {@code Long}; and a value greater
	*          than {@code 0} if this {@code Long} is numerically
	*           greater than the argument {@code Long} (signed
	*           comparison).
	* @since   1.2
	*/
	@:require(java2) @:overload public function compareTo(anotherLong : Long) : Int;
	
	/**
	* Compares two {@code long} values numerically.
	* The value returned is identical to what would be returned by:
	* <pre>
	*    Long.valueOf(x).compareTo(Long.valueOf(y))
	* </pre>
	*
	* @param  x the first {@code long} to compare
	* @param  y the second {@code long} to compare
	* @return the value {@code 0} if {@code x == y};
	*         a value less than {@code 0} if {@code x < y}; and
	*         a value greater than {@code 0} if {@code x > y}
	* @since 1.7
	*/
	@:require(java7) @:overload public static function compare(x : haxe.Int64, y : haxe.Int64) : Int;
	
	/**
	* The number of bits used to represent a {@code long} value in two's
	* complement binary form.
	*
	* @since 1.5
	*/
	@:require(java5) public static var SIZE(default, null) : Int;
	
	/**
	* Returns a {@code long} value with at most a single one-bit, in the
	* position of the highest-order ("leftmost") one-bit in the specified
	* {@code long} value.  Returns zero if the specified value has no
	* one-bits in its two's complement binary representation, that is, if it
	* is equal to zero.
	*
	* @return a {@code long} value with a single one-bit, in the position
	*     of the highest-order one-bit in the specified value, or zero if
	*     the specified value is itself equal to zero.
	* @since 1.5
	*/
	@:require(java5) @:overload public static function highestOneBit(i : haxe.Int64) : haxe.Int64;
	
	/**
	* Returns a {@code long} value with at most a single one-bit, in the
	* position of the lowest-order ("rightmost") one-bit in the specified
	* {@code long} value.  Returns zero if the specified value has no
	* one-bits in its two's complement binary representation, that is, if it
	* is equal to zero.
	*
	* @return a {@code long} value with a single one-bit, in the position
	*     of the lowest-order one-bit in the specified value, or zero if
	*     the specified value is itself equal to zero.
	* @since 1.5
	*/
	@:require(java5) @:overload public static function lowestOneBit(i : haxe.Int64) : haxe.Int64;
	
	/**
	* Returns the number of zero bits preceding the highest-order
	* ("leftmost") one-bit in the two's complement binary representation
	* of the specified {@code long} value.  Returns 64 if the
	* specified value has no one-bits in its two's complement representation,
	* in other words if it is equal to zero.
	*
	* <p>Note that this method is closely related to the logarithm base 2.
	* For all positive {@code long} values x:
	* <ul>
	* <li>floor(log<sub>2</sub>(x)) = {@code 63 - numberOfLeadingZeros(x)}
	* <li>ceil(log<sub>2</sub>(x)) = {@code 64 - numberOfLeadingZeros(x - 1)}
	* </ul>
	*
	* @return the number of zero bits preceding the highest-order
	*     ("leftmost") one-bit in the two's complement binary representation
	*     of the specified {@code long} value, or 64 if the value
	*     is equal to zero.
	* @since 1.5
	*/
	@:require(java5) @:overload public static function numberOfLeadingZeros(i : haxe.Int64) : Int;
	
	/**
	* Returns the number of zero bits following the lowest-order ("rightmost")
	* one-bit in the two's complement binary representation of the specified
	* {@code long} value.  Returns 64 if the specified value has no
	* one-bits in its two's complement representation, in other words if it is
	* equal to zero.
	*
	* @return the number of zero bits following the lowest-order ("rightmost")
	*     one-bit in the two's complement binary representation of the
	*     specified {@code long} value, or 64 if the value is equal
	*     to zero.
	* @since 1.5
	*/
	@:require(java5) @:overload public static function numberOfTrailingZeros(i : haxe.Int64) : Int;
	
	/**
	* Returns the number of one-bits in the two's complement binary
	* representation of the specified {@code long} value.  This function is
	* sometimes referred to as the <i>population count</i>.
	*
	* @return the number of one-bits in the two's complement binary
	*     representation of the specified {@code long} value.
	* @since 1.5
	*/
	@:require(java5) @:overload public static function bitCount(i : haxe.Int64) : Int;
	
	/**
	* Returns the value obtained by rotating the two's complement binary
	* representation of the specified {@code long} value left by the
	* specified number of bits.  (Bits shifted out of the left hand, or
	* high-order, side reenter on the right, or low-order.)
	*
	* <p>Note that left rotation with a negative distance is equivalent to
	* right rotation: {@code rotateLeft(val, -distance) == rotateRight(val,
	* distance)}.  Note also that rotation by any multiple of 64 is a
	* no-op, so all but the last six bits of the rotation distance can be
	* ignored, even if the distance is negative: {@code rotateLeft(val,
	* distance) == rotateLeft(val, distance & 0x3F)}.
	*
	* @return the value obtained by rotating the two's complement binary
	*     representation of the specified {@code long} value left by the
	*     specified number of bits.
	* @since 1.5
	*/
	@:require(java5) @:overload public static function rotateLeft(i : haxe.Int64, distance : Int) : haxe.Int64;
	
	/**
	* Returns the value obtained by rotating the two's complement binary
	* representation of the specified {@code long} value right by the
	* specified number of bits.  (Bits shifted out of the right hand, or
	* low-order, side reenter on the left, or high-order.)
	*
	* <p>Note that right rotation with a negative distance is equivalent to
	* left rotation: {@code rotateRight(val, -distance) == rotateLeft(val,
	* distance)}.  Note also that rotation by any multiple of 64 is a
	* no-op, so all but the last six bits of the rotation distance can be
	* ignored, even if the distance is negative: {@code rotateRight(val,
	* distance) == rotateRight(val, distance & 0x3F)}.
	*
	* @return the value obtained by rotating the two's complement binary
	*     representation of the specified {@code long} value right by the
	*     specified number of bits.
	* @since 1.5
	*/
	@:require(java5) @:overload public static function rotateRight(i : haxe.Int64, distance : Int) : haxe.Int64;
	
	/**
	* Returns the value obtained by reversing the order of the bits in the
	* two's complement binary representation of the specified {@code long}
	* value.
	*
	* @return the value obtained by reversing order of the bits in the
	*     specified {@code long} value.
	* @since 1.5
	*/
	@:require(java5) @:overload public static function reverse(i : haxe.Int64) : haxe.Int64;
	
	/**
	* Returns the signum function of the specified {@code long} value.  (The
	* return value is -1 if the specified value is negative; 0 if the
	* specified value is zero; and 1 if the specified value is positive.)
	*
	* @return the signum function of the specified {@code long} value.
	* @since 1.5
	*/
	@:require(java5) @:overload public static function signum(i : haxe.Int64) : Int;
	
	/**
	* Returns the value obtained by reversing the order of the bytes in the
	* two's complement representation of the specified {@code long} value.
	*
	* @return the value obtained by reversing the bytes in the specified
	*     {@code long} value.
	* @since 1.5
	*/
	@:require(java5) @:overload public static function reverseBytes(i : haxe.Int64) : haxe.Int64;
	
	
}
