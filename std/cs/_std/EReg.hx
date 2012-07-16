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

import cs.system.text.regularExpressions.Regex;

@:core_api class EReg {

	private var regex : Regex;
	private var m : Match;
	private var isGlobal : Bool;
	private var cur : String;
	
	public function new( r : String, opt : String ) : Void {
		var opts:Int = cast CultureInvariant;
		for (i in 0...opt.length) untyped {
			switch(cast(opt[i], Int))
			{
				case 'i'.code:
					opts |= cast(IgnoreCase, Int);
				case 'g'.code:
					isGlobal = true;
				case 'm'.code:
					opts |= cast(Multiline, Int);
				case 'c'.code:
					opts |= cast(Compiled, Int);
			}
		}
		
		this.regex = new Regex(r, cast(opts, RegexOptions));
	}

	public function match( s : String ) : Bool {
		m = regex.Match(s);
		cur = s;
		return m.Success;
	}

	public function matched( n : Int ) : String {
		if (m == null || cast(n, UInt) > m.Groups.Count)
			throw "EReg::matched";
		return m.Groups[n].Value;
	}

	public function matchedLeft() : String {
		return untyped cur.Substring(0, m.Index);
	}

	public function matchedRight() : String {
		return untyped cur.Substring(m.Index + m.Length);
	}

	public function matchedPos() : { pos : Int, len : Int } {
		return { pos : m.Index, len : m.Length };
	}

	public function split( s : String ) : Array<String> {
		if (isGlobal)
			return cs.Lib.array(regex.Split(s));
		var m = regex.Match(s);
		return untyped [s.Substring(0, m.Index), s.Substring(m.Index + m.Length)];
	}

	public function replace( s : String, by : String ) : String {
		if (isGlobal)
			return regex.Replace(s, by);
		var m = regex.Match(s);
		return untyped (s.Substring(0, m.Index) + by + s.Substring(m.Index + m.Length));
	}

	public function customReplace( s : String, f : EReg -> String ) : String {
		var buf = new StringBuf();
		while (true)
		{
			if (!match(s))
				break;
			buf.add(matchedLeft());
			buf.add(f(this));
			s = matchedRight();
		}
		buf.add(s);
		return buf.toString();
	}

}