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
package haxe.xml;

enum Filter {
	FInt;
	FBool;
	FEnum( values : Array<String> );
	FReg( matcher : EReg );
}

enum Attrib {
	Att( name : String, ?filter : Filter, ?defvalue : String );
}

enum Rule {
	RNode( name : String, ?attribs : Array<Attrib>, ?childs : Rule );
	RData( ?filter : Filter );
	RMulti( rule : Rule, ?atLeastOne : Bool );
	RList( rules : Array<Rule>, ?ordered : Bool );
	RChoice( choices : Array<Rule> );
	ROptional( rule : Rule );
}

private enum CheckResult {
	CMatch;
	CMissing( r : Rule );
	CExtra( x : Xml );
	CElementExpected( name : String, x : Xml );
	CDataExpected( x : Xml );
	CExtraAttrib( att : String, x : Xml );
	CMissingAttrib( att : String, x : Xml );
	CInvalidAttrib( att : String, x : Xml, f : Filter );
	CInvalidData( x : Xml, f : Filter );
	CInElement( x : Xml, r : CheckResult );
}

class Check {

	static var blanks = ~/^[ \r\n\t]*$/;

	static function isBlank( x : Xml ) {
		return( x.nodeType == Xml.PCData && blanks.match(x.nodeValue) ) || x.nodeType == Xml.Comment;
	}

	static function filterMatch( s : String, f : Filter ) {
		switch( f ) {
		case FInt: return filterMatch(s,FReg(~/[0-9]+/));
		case FBool: return filterMatch(s,FEnum(["true","false","0","1"]));
		case FEnum(values):
			for( v in values )
				if( s == v )
					return true;
			return false;
		case FReg(r):
			return r.match(s);
		}
	}

	static function isNullable( r : Rule ) {
		switch( r ) {
		case RMulti(r,one):
			return( one != true || isNullable(r) );
		case RList(rl,_):
			for( r in rl )
				if( !isNullable(r) )
					return false;
			return true;
		case RChoice(rl):
			for( r in rl )
				if( isNullable(r) )
					return true;
			return false;
		case RData(_):
			return false;
		case RNode(_,_,_):
			return false;
		case ROptional(_):
			return true;
		}
	}

	static function check( x : Xml, r : Rule ) {
		switch( r ) {
		// check the node validity
		case RNode(name,attribs,childs):
			if( x.nodeType != Xml.Element || x.nodeName != name )
				return CElementExpected(name,x);
			var attribs = if( attribs == null ) new Array() else attribs.copy();
			// check defined attributes
			for( xatt in x.attributes() ) {
				var found = false;
				for( att in attribs )
					switch( att ) {
					case Att(name,filter,_):
						if( xatt != name )
							continue;
						if( filter != null && !filterMatch(x.get(xatt),filter) )
							return CInvalidAttrib(name,x,filter);
						attribs.remove(att);
						found = true;
					}
				if( !found )
					return CExtraAttrib(xatt,x);
			}
			// check remaining unchecked attributes
			for( att in attribs )
				switch( att ) {
				case Att(name,_,defvalue):
					if( defvalue == null )
						return CMissingAttrib(name,x);
				}
			// check childs
			if( childs == null )
				childs = RList([]);
			var m = checkList(x.iterator(),childs);
			if( m != CMatch )
				return CInElement(x,m);
			// set default attribs values
			for( att in attribs )
				switch( att ) {
				case Att(name,_,defvalue):
					x.set(name,defvalue);
				}
			return CMatch;
		// check the data validity
		case RData(filter):
			if( x.nodeType != Xml.PCData && x.nodeType != Xml.CData )
				return CDataExpected(x);
			if( filter != null && !filterMatch(x.nodeValue,filter) )
				return CInvalidData(x,filter);
			return CMatch;
		// several choices
		case RChoice(choices):
			if( choices.length == 0 )
				throw "No choice possible";
			for( c in choices )
				if( check(x,c) == CMatch )
					return CMatch;
			return check(x,choices[0]);
		case ROptional(r):
			return check(x,r);
		default:
			throw "Unexpected "+Std.string(r);
		}
	}

	static function checkList( it : Iterator<Xml>, r : Rule ) {
		switch( r ) {
		case RList(rules,ordered):
			var rules = rules.copy();
			for( x in it ) {
				if( isBlank(x) )
					continue;
				var found = false;
				for( r in rules ) {
					var m = checkList([x].iterator(),r);
					if( m == CMatch ) {
						found = true;
						switch(r) {
						case RMulti(rsub,one):
							if( one ) {
								var i;
								for( i in 0...rules.length )
									if( rules[i] == r )
										rules[i] = RMulti(rsub);
							}
						default:
							rules.remove(r);
						}
						break;
					} else if( ordered && !isNullable(r) )
						return m;
				}
				if( !found )
					return CExtra(x);
			}
			for( r in rules )
				if( !isNullable(r) )
					return CMissing(r);
			return CMatch;
		case RMulti(r,one):
			var found = false;
			for( x in it ) {
				if( isBlank(x) )
					continue;
				var m = checkList([x].iterator(),r);
				if( m != CMatch )
					return m;
				found = true;
			}
			if( one && !found )
				return CMissing(r);
			return CMatch;
		default:
			var found = false;
			for( x in it ) {
				if( isBlank(x) )
					continue;
				var m = check(x,r);
				if( m != CMatch )
					return m;
				found = true;
				break;
			}
			if( !found ) {
				switch(r) {
				case ROptional(_):
				default: return CMissing(r);
				}
			}
			for( x in it ) {
				if( isBlank(x) )
					continue;
				return CExtra(x);
			}
			return CMatch;
		}
	}

	static function makeWhere( path : Array<Xml> ) {
		if( path.length == 0 )
			return "";
		var s = "In ";
		var first = true;
		for( x in path ) {
			if( first )
				first = false;
			else
				s += ".";
			s += x.nodeName;
		}
		return s+": ";
	}

	static function makeString( x : Xml ) {
		if( x.nodeType == Xml.Element )
			return "element "+x.nodeName;
		var s = x.nodeValue.split("\r").join("\\r").split("\n").join("\\n").split("\t").join("\\t");
		if( s.length > 20 )
			return s.substr(0,17)+"...";
		return s;
	}

	static function makeRule( r : Rule ) {
		switch( r ) {
		case RNode(name,_,_): return "element "+name;
		case RData(_): return "data";
		case RMulti(r,_): return makeRule(r);
		case RList(rules,_): return makeRule(rules[0]);
		case RChoice(choices): return makeRule(choices[0]);
		case ROptional(r): return makeRule(r);
		}
	}

	static function makeError(m,?path) {
		if( path == null )
			path = new Array();
		switch( m ) {
		case CMatch: throw "assert";
		case CMissing(r):
			return makeWhere(path)+"Missing "+makeRule(r);
		case CExtra(x):
			return makeWhere(path)+"Unexpected "+makeString(x);
		case CElementExpected(name,x):
			return makeWhere(path)+makeString(x)+" while expected element "+name;
		case CDataExpected(x):
			return makeWhere(path)+makeString(x)+" while data expected";
		case CExtraAttrib(att,x):
			path.push(x);
			return makeWhere(path)+"unexpected attribute "+att;
		case CMissingAttrib(att,x):
			path.push(x);
			return makeWhere(path)+"missing required attribute "+att;
		case CInvalidAttrib(att,x,_):
			path.push(x);
			return makeWhere(path)+"invalid attribute value for "+att;
		case CInvalidData(x,_):
			return makeWhere(path)+"invalid data format for "+makeString(x);
		case CInElement(x,m):
			path.push(x);
			return makeError(m,path);
		}
	}

	public static function checkNode( x : Xml, r : Rule ) {
		var m = checkList([x].iterator(),r);
		if( m == CMatch )
			return;
		throw makeError(m);
	}

	public static function checkDocument( x : Xml, r : Rule ) {
		if( x.nodeType != Xml.Document )
			throw "Document expected";
		var m = checkList(x.iterator(),r);
		if( m == CMatch )
			return;
		throw makeError(m);
	}

}
