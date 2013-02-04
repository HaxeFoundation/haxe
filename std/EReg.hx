/*
 * Copyright (C)2005-2012 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
/**
	Regular expressions are a way to find regular patterns into
	Strings. Have a look at the tutorial on haXe website to learn
	how to use them.
**/
class EReg {

	/**
		Creates a new regular expression with pattern [r] and
		options [opt].
	**/
	public function new( r : String, opt : String ) {
		throw "Regular expressions are not implemented for this platform";
	}

	/**
		Tells if the regular expression matches the String.
		Updates the internal state accordingly.
	**/
	public function match( s : String ) : Bool {
		return false;
	}

	/**
		Returns a matched group or throw an expection if there
		is no such group. If [n = 0], the whole matched substring
		is returned.
	**/
	public function matched( n : Int ) : String {
		return null;
	}

	/**
		Returns the part of the string that was as the left of
		of the matched substring.
	**/
	public function matchedLeft() : String {
		return null;
	}

	/**
		Returns the part of the string that was at the right of
		of the matched substring.
	**/
	public function matchedRight() : String {
		return null;
	}

	/**
		Returns the position of the matched substring within the
		original matched string.
	**/
	public function matchedPos() : { pos : Int, len : Int } {
		return null;
	}

	/**
		Tells if the regular expression matches the String between pos and pos + len.
		Updates the internal state accordingly.
	**/
	public function matchSub( s : String, pos : Int, len : Int = 0):Bool {
		return false;
	}

	/**
		Split a string by using the regular expression to match
		the separators.
	**/
	public function split( s : String ) : Array<String> {
		return null;
	}

	/**
		Replaces a pattern by another string. The [by] format can
		contains [$1] to [$9] that will correspond to groups matched
		while replacing. [$$] means the [$] character.
	**/
	public function replace( s : String, by : String ) : String {
		return null;
	}

	/**
		For each occurence of the pattern in the string [s], the function [f] is called and
		can return the string that needs to be replaced. All occurences are matched anyway,
		and setting the [g] flag might cause some incorrect behavior on some platforms.
	**/
	public function map( s : String, f : EReg -> String ) : String {
		var buf = new StringBuf();
		while( true ) {
			if( !match(s) )
				break;
			buf.add(matchedLeft());
			buf.add(f(this));
			s = matchedRight();
		}
		buf.add(s);
		return buf.toString();
	}
}
