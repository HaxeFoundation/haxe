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
enum XmlType {
}

@:coreApi class Xml {

	public static var Element(default,null) : XmlType;
	public static var PCData(default,null) : XmlType;
	public static var CData(default,null) : XmlType;
	public static var Comment(default,null) : XmlType;
	public static var DocType(default,null) : XmlType;
	public static var Prolog(default,null) : XmlType;
	public static var Document(default,null) : XmlType;


	public var nodeName(get,set) : String;
	public var nodeValue(get,set) : String;
	public var parent(get,null) : Xml;
	public var nodeType(default,null) : XmlType;

	private var __x : Dynamic;

	private static function convert( o : Dynamic ) : Xml {
		if( o == null ) return null;
		if( o.__w != null ) return o.__w;

		var r = new Xml();
		r.__x = o;
		o.__w = r;

		r.nodeType = switch( untyped o["nodeType"] ) {
			case 1:
				Xml.Element;
			case 3:
				Xml.PCData;
			default:
				throw "unknow nodeType: "+untyped o["nodeType"];
		}

		return untyped r;
	}

	public static function parse( str : String ) : Xml untyped {
		var x = __new__(_global["XML"]);
		x["parseXML"](str);
		if( x["status"] != 0 )
			throw ("Xml parse error #"+x["status"]);

		var r = convert(x);
		r.nodeType = Xml.Document;
		return r;
	}

	public static function createDocument() : Xml {
		var o = untyped __new__(_global["XML"])["createElement"]( "#document" );

		var r = convert(o);
		r.nodeType = Xml.Document;
		return r;
	}

	public static function createCData( data : String ) : Xml {
		var o = untyped __new__(_global["XML"])["createTextNode"]( data );
		var x = convert(o);
		x.nodeType = Xml.CData;
		return x;
	}

	public static function createPCData( data : String ) : Xml {
		var o = untyped __new__(_global["XML"])["createTextNode"]( data );
		return convert(o);
	}

	public static function createElement( name : String ) : Xml {
		var o = untyped __new__(_global["XML"])["createElement"]( name );
		return convert(o);
	}

	public static function createComment( data : String ) : Xml {
		throw "not implemented";
		return null;
	}

	public static function createDocType( data : String ) : Xml {
		var x = createPCData("");
		x.nodeType = Xml.DocType;
		x.nodeValue = data;
		return x;
	}

	public static function createProlog( data : String ) : Xml {
		var x = createPCData("");
		x.nodeType = Xml.Prolog;
		x.nodeValue = data;
		return x;
	}

	private function new() : Void {
	}

	public function firstChild() : Xml {
		if( nodeType != Xml.Element && nodeType != Xml.Document )
			throw "bad nodeType";
		return convert(this.__x[untyped "firstChild"]);
	}

	public function firstElement() : Xml {
		if( nodeType != Xml.Element && nodeType != Xml.Document )
			throw "bad nodeType";
		var e : Dynamic = __x[untyped "firstChild"];
		while( e != null && e[untyped "nodeType"] != 1 )
			e = e[untyped "nextSibling"];
		return convert(e);
	}

	private function set_nodeName( n : String ) : String {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		return __x[untyped "nodeName"] = n;
	}

	private function set_nodeValue( v : String ) : String {
		if( nodeType == Xml.Element || nodeType == Xml.Document )
			throw "bad nodeType";
		return __x[untyped "nodeValue"] = v;
	}

	private function get_nodeName() : String {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		return __x[untyped "nodeName"];
	}

	private function get_nodeValue() : String {
		if( nodeType == Xml.Element || nodeType == Xml.Document )
			throw "bad nodeType";
		return __x[untyped "nodeValue"];
	}

	private function get_parent() : Xml {
		return convert(__x[untyped "parentNode"]);
	}

	public function iterator() : Iterator<Xml> {
		if( nodeType != Xml.Document && nodeType != Xml.Element )
			throw "bad nodeType";
		return untyped {
			cur: this.__x[untyped "firstChild"],
			hasNext : function(){
				return __this__.cur != null;
			},
			next : function(){
				var r = convert(__this__.cur);
				__this__.cur = __this__.cur["nextSibling"];
				return r;
			}
		}
	}

	public function elements() : Iterator<Xml> {
		if( nodeType != Xml.Document && nodeType != Xml.Element )
			throw "bad nodeType";
		return untyped {
			cur: this.__x[untyped "firstChild"],
			hasNext : function() {
				var r = __this__.cur;
				while( r != null && r["nodeType"] != 1 )
					r = r["nextSibling"];
				__this__.cur = r;
				return r != null;
			},
			next : function(){
				var r = __this__.cur;
				while( r != null && r["nodeType"] != 1 )
					r = r["nextSibling"];
				if( r == null ) {
					__this__.cur = null;
					return null;
				}
				__this__.cur = r["nextSibling"];
				return convert(r);
			}
		}
	}

	public function elementsNamed( name : String ) : Iterator<Xml> {
		if( nodeType != Xml.Document && nodeType != Xml.Element )
			throw "bad nodeType";
		return untyped {
			cur: this.__x[untyped "firstChild"],
			hasNext : function() {
				var r = __this__.cur;
				while( r != null && (r["nodeType"] != 1 || r["nodeName"] != name) )
					r = r["nextSibling"];
				__this__.cur = r;
				return r != null;
			},
			next : function(){
				var r = __this__.cur;
				while( r != null && (r["nodeType"] != 1 || r["nodeName"] != name) )
					r = r["nextSibling"];
				if( r == null ) {
					__this__.cur = null;
					return null;
				}
				__this__.cur = r["nextSibling"];
				return convert(r);
			}
		}
	}

	public function get( att : String ) : String {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		return Reflect.field(__x[untyped "attributes"],att);
	}

	public function set( att : String, value : String ) : Void {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		return Reflect.setField(__x[untyped "attributes"],att,value);
	}

	public function exists( att : String ) : Bool {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		return Reflect.hasField(__x[untyped "attributes"],att);
	}

	public function remove( att : String ) : Void {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		Reflect.deleteField(__x[untyped "attributes"],att);
	}

	public function attributes() : Iterator<String> {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		return untyped __keys__(__x["attributes"])["iterator"]();
	}

	public function addChild( x : Xml ) : Void {
		if( nodeType != Xml.Document && nodeType != Xml.Element )
			throw "bad nodeType";
		untyped __x[untyped "appendChild"](x.__x);
	}

	public function removeChild( x : Xml ) : Bool {
		if( nodeType != Xml.Document && nodeType != Xml.Element )
			throw "bad nodeType";
		untyped if( x.__x["parentNode"] != __x )
			return false;
		untyped x.__x["removeNode"]();
		return true;
	}

	public function insertChild( x : Xml, pos : Int ) : Void {
		if( nodeType != Xml.Document && nodeType != Xml.Element )
			throw "bad nodeType";
		var c : Array<Dynamic> = __x[untyped "childNodes"];
		if( pos <= c.length )
			__x[untyped "insertBefore"](untyped x.__x,c[pos]);
	}

	public function toString() : String {
		if( nodeType == Xml.Document ){
			var s = "";
			for( c in iterator() )
				s += c.toString();
			return s;
		}
		// only works for toplevel elements
		if( nodeType == Xml.CData )
			return "<![CDATA["+__x[untyped "nodeValue"]+"]]>";
		if( nodeType == Xml.Prolog )
			return "<?"+__x[untyped "nodeValue"]+"?>";
		if( nodeType == Xml.DocType )
			return "<!DOCTYPE "+__x[untyped "nodeValue"]+">";
		var s : String = __x.toString();
		return s.split(" />").join("/>");
	}

	static function __init__() : Void untyped {
		Xml.Element = "element";
		Xml.PCData = "pcdata";
		Xml.CData = "cdata";
		Xml.Comment = "comment";
		Xml.DocType = "doctype";
		Xml.Prolog = "prolog";
		Xml.Document = "document";
		#if swf_mark
		flash.Lib.current["Xml"] = Xml;
		#end
	}

}
