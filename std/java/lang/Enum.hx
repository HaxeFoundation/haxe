package java.lang;
/*
* Copyright (c) 2003, 2009, Oracle and/or its affiliates. All rights reserved.
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
* This is the common base class of all Java language enumeration types.
*
* More information about enums, including descriptions of the
* implicitly declared methods synthesized by the compiler, can be
* found in section 8.9 of
* <cite>The Java&trade; Language Specification</cite>.
*
* <p> Note that when using an enumeration type as the type of a set
* or as the type of the keys in a map, specialized and efficient
* {@linkplain java.util.EnumSet set} and {@linkplain
* java.util.EnumMap map} implementations are available.
*
* @param <E> The enum type subclass
* @author  Josh Bloch
* @author  Neal Gafter
* @see     Class#getEnumConstants()
* @see     java.util.EnumSet
* @see     java.util.EnumMap
* @since   1.5
*/
@:require(java5) extern class Enum<E> implements java.lang.Comparable<E> implements java.io.Serializable
{
	/**
	* Returns the name of this enum constant, exactly as declared in its
	* enum declaration.
	*
	* <b>Most programmers should use the {@link #toString} method in
	* preference to this one, as the toString method may return
	* a more user-friendly name.</b>  This method is designed primarily for
	* use in specialized situations where correctness depends on getting the
	* exact name, which will not vary from release to release.
	*
	* @return the name of this enum constant
	*/
	@:overload @:final public function name() : String;
	
	/**
	* Returns the ordinal of this enumeration constant (its position
	* in its enum declaration, where the initial constant is assigned
	* an ordinal of zero).
	*
	* Most programmers will have no use for this method.  It is
	* designed for use by sophisticated enum-based data structures, such
	* as {@link java.util.EnumSet} and {@link java.util.EnumMap}.
	*
	* @return the ordinal of this enumeration constant
	*/
	@:overload @:final public function ordinal() : Int;
	
	/**
	* Sole constructor.  Programmers cannot invoke this constructor.
	* It is for use by code emitted by the compiler in response to
	* enum type declarations.
	*
	* @param name - The name of this enum constant, which is the identifier
	*               used to declare it.
	* @param ordinal - The ordinal of this enumeration constant (its position
	*         in the enum declaration, where the initial constant is assigned
	*         an ordinal of zero).
	*/
	@:overload private function new(name : String, ordinal : Int) : Void;
	
	/**
	* Returns the name of this enum constant, as contained in the
	* declaration.  This method may be overridden, though it typically
	* isn't necessary or desirable.  An enum type should override this
	* method when a more "programmer-friendly" string form exists.
	*
	* @return the name of this enum constant
	*/
	@:overload public function toString() : String;
	
	/**
	* Returns true if the specified object is equal to this
	* enum constant.
	*
	* @param other the object to be compared for equality with this object.
	* @return  true if the specified object is equal to this
	*          enum constant.
	*/
	@:overload @:final public function equals(other : Dynamic) : Bool;
	
	/**
	* Returns a hash code for this enum constant.
	*
	* @return a hash code for this enum constant.
	*/
	@:overload @:final public function hashCode() : Int;
	
	/**
	* Throws CloneNotSupportedException.  This guarantees that enums
	* are never cloned, which is necessary to preserve their "singleton"
	* status.
	*
	* @return (never returns)
	*/
	@:overload @:final private function clone() : Dynamic;
	
	/**
	* Compares this enum with the specified object for order.  Returns a
	* negative integer, zero, or a positive integer as this object is less
	* than, equal to, or greater than the specified object.
	*
	* Enum constants are only comparable to other enum constants of the
	* same enum type.  The natural order implemented by this
	* method is the order in which the constants are declared.
	*/
	@:overload @:final public function compareTo(o : E) : Int;
	
	/**
	* Returns the Class object corresponding to this enum constant's
	* enum type.  Two enum constants e1 and  e2 are of the
	* same enum type if and only if
	*   e1.getDeclaringClass() == e2.getDeclaringClass().
	* (The value returned by this method may differ from the one returned
	* by the {@link Object#getClass} method for enum constants with
	* constant-specific class bodies.)
	*
	* @return the Class object corresponding to this enum constant's
	*     enum type
	*/
	@:overload @:final public function getDeclaringClass() : Class<E>;
	
	/**
	* Returns the enum constant of the specified enum type with the
	* specified name.  The name must match exactly an identifier used
	* to declare an enum constant in this type.  (Extraneous whitespace
	* characters are not permitted.)
	*
	* <p>Note that for a particular enum type {@code T}, the
	* implicitly declared {@code public static T valueOf(String)}
	* method on that enum may be used instead of this method to map
	* from a name to the corresponding enum constant.  All the
	* constants of an enum type can be obtained by calling the
	* implicit {@code public static T[] values()} method of that
	* type.
	*
	* @param <T> The enum type whose constant is to be returned
	* @param enumType the {@code Class} object of the enum type from which
	*      to return a constant
	* @param name the name of the constant to return
	* @return the enum constant of the specified enum type with the
	*      specified name
	* @throws IllegalArgumentException if the specified enum type has
	*         no constant with the specified name, or the specified
	*         class object does not represent an enum type
	* @throws NullPointerException if {@code enumType} or {@code name}
	*         is null
	* @since 1.5
	*/
	@:require(java5) @:overload public static function valueOf<T : Enum<T>>(enumType : Class<T>, name : String) : T;
	
	/**
	* enum classes cannot have finalize methods.
	*/
	@:overload @:final private function finalize() : Void;
	
	
}
