/*
 * Copyright (C)2005-2017 Haxe Foundation
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
	The EReg class represents regular expressions.

	While basic usage and patterns consistently work across platforms, some more
	complex operations may yield different results. This is a necessary trade-
	off to retain a certain level of performance.

	EReg instances can be created by calling the constructor, or with the
	special syntax `~/pattern/modifier`

	EReg instances maintain an internal state, which is affected by several of
	its methods.

	A detailed explanation of the supported operations is available at
	<https://haxe.org/manual/std-regex.html>
**/
class EReg {

	/**
		Creates a new regular expression with pattern `r` and modifiers `opt`.

		This is equivalent to the shorthand syntax `~/r/opt`

		If `r` or `opt` are null, the result is unspecified.
	**/
	public function new( r : String, opt : String ) {
		throw "Regular expressions are not implemented for this platform";
	}

	/**
		Tells if `this` regular expression matches String `s`.

		This method modifies the internal state.

		If `s` is `null`, the result is unspecified.
	**/
	public function match( s : String ) : Bool {
		return false;
	}

	/**
		Returns the matched sub-group `n` of `this` EReg.

		This method should only be called after `this.match` or
		`this.matchSub`, and then operates on the String of that operation.

		The index `n` corresponds to the n-th set of parentheses in the pattern
		of `this` EReg. If no such sub-group exists, an exception is thrown.

		If `n` equals 0, the whole matched substring is returned.
	**/
	public function matched( n : Int ) : String {
		return null;
	}

	/**
		Returns the part to the left of the last matched substring.

		If the most recent call to `this.match` or `this.matchSub` did not
		match anything, the result is unspecified.

		If the global g modifier was in place for the matching, only the
		substring to the left of the leftmost match is returned.

		The result does not include the matched part.
	**/
	public function matchedLeft() : String {
		return null;
	}

	/**
		Returns the part to the right of the last matched substring.

		If the most recent call to `this.match` or `this.matchSub` did not
		match anything, the result is unspecified.

		If the global g modifier was in place for the matching, only the
		substring to the right of the leftmost match is returned.

		The result does not include the matched part.
	**/
	public function matchedRight() : String {
		return null;
	}

	/**
		Returns the position and length of the last matched substring, within
		the String which was last used as argument to `this.match` or
		`this.matchSub`.

		If the most recent call to `this.match` or `this.matchSub` did not
		match anything, the result is unspecified.

		If the global g modifier was in place for the matching, the position and
		length of the leftmost substring is returned.
	**/
	public function matchedPos() : { pos : Int, len : Int } {
		return null;
	}

	/**
		Tells if `this` regular expression matches a substring of String `s`.

		This function expects `pos` and `len` to describe a valid substring of
		`s`, or else the result is unspecified. To get more robust behavior,
		`this.match(s.substr(pos,len))` can be used instead.

		This method modifies the internal state.

		If `s` is null, the result is unspecified.
	**/
	public function matchSub( s : String, pos : Int, len : Int = -1):Bool {
		return false;
	}

	/**
		Splits String `s` at all substrings `this` EReg matches.

		If a match is found at the start of `s`, the result contains a leading
		empty String "" entry.

		If a match is found at the end of `s`, the result contains a trailing
		empty String "" entry.

		If two matching substrings appear next to each other, the result
		contains the empty String `""` between them.

		By default, this method splits `s` into two parts at the first matched
		substring. If the global g modifier is in place, `s` is split at each
		matched substring.

		If `s` is null, the result is unspecified.
	**/
	public function split( s : String ) : Array<String> {
		return null;
	}

	/**
		Replaces the first substring of `s` which `this` EReg matches with `by`.

		If `this` EReg does not match any substring, the result is `s`.

		By default, this method replaces only the first matched substring. If
		the global g modifier is in place, all matched substrings are replaced.

		If `by` contains `$1` to `$9`, the digit corresponds to number of a
		matched sub-group and its value is used instead. If no such sub-group
		exists, the replacement is unspecified. The string `$$` becomes `$`.

		If `s` or `by` are null, the result is unspecified.
	**/
	public function replace( s : String, by : String ) : String {
		return null;
	}

	/**
		Calls the function `f` for the substring of `s` which `this` EReg matches
		and replaces that substring with the result of `f` call.

		The `f` function takes `this` EReg object as its first argument and should
		return a replacement string for the substring matched.

		If `this` EReg does not match any substring, the result is `s`.

		By default, this method replaces only the first matched substring. If
		the global g modifier is in place, all matched substrings are replaced.

		If `s` or `f` are null, the result is unspecified.
	**/
	public function map( s : String, f : EReg -> String ) : String {
		return null;
	}
}
