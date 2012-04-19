import java.util.regex.Regex;
/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */

/**
	Regular expressions are a way to find regular patterns into
	Strings. Have a look at the tutorial on haXe website to learn
	how to use them.
**/
class EReg {

	private var pattern:String;
	private var matcher:Matcher;
	private var cur:String;
	
	/**
		Creates a new regular expression with pattern [r] and
		options [opt].
	**/
	public function new( r : String, opt : String ) {
		//FIXME opt is ignored by now
		matcher = Pattern.compile(r).matcher("");
		pattern = r;
	}

	/**
		Tells if the regular expression matches the String.
		Updates the internal state accordingly.
	**/
	public function match( s : String ) : Bool {
		cur = s;
		matcher = matcher.reset(s);
		return matcher.find();
	}

	/**
		Returns a matched group or throw an expection if there
		is no such group. If [n = 0], the whole matched substring
		is returned.
	**/
	public function matched( n : Int ) : String 
	{
		return matcher.group(n);
	}

	/**
		Returns the part of the string that was as the left of
		of the matched substring.
	**/
	public function matchedLeft() : String 
	{
		return cur.substr(0, matcher.start());
	}

	/**
		Returns the part of the string that was at the right of
		of the matched substring.
	**/
	public function matchedRight() : String 
	{
		return cur.substr(matcher.end());
	}

	/**
		Returns the position of the matched substring within the
		original matched string.
	**/
	public function matchedPos() : { pos : Int, len : Int } {
		var start = matcher.start();
		return { pos : start, len : matcher.end() - start };
	}

	/**
		Split a string by using the regular expression to match
		the separators.
	**/
	@:functionBody('
		return new Array<String>(s.split(this.pattern));
	')
	public function split( s : String ) : Array<String> 
	{
		return null;
	}

	/**
		Replaces a pattern by another string. The [by] format can
		contains [$1] to [$9] that will correspond to groups matched
		while replacing. [$$] means the [$] character.
	**/
	public function replace( s : String, by : String ) : String {
		matcher.reset(s);
		return matcher.replaceAll(by);
	}

	/**
		For each occurence of the pattern in the string [s], the function [f] is called and
		can return the string that needs to be replaced. All occurences are matched anyway,
		and setting the [g] flag might cause some incorrect behavior on some platforms.
	**/
	public function customReplace( s : String, f : EReg -> String ) : String {
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
