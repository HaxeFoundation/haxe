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

enum XmlType {
}

@:core_api class Xml {

	public static var Element(default,null) : XmlType;
	public static var PCData(default,null) : XmlType;
	public static var CData(default,null) : XmlType;
	public static var Comment(default,null) : XmlType;
	public static var DocType(default,null) : XmlType;
	public static var Prolog(default,null) : XmlType;
	public static var Document(default,null) : XmlType;


	public var nodeName(get_nodeName,set_nodeName) : String;
	public var nodeValue(get_nodeValue,set_nodeValue) : String;
	public var parent(getParent,null) : Xml;
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

	private function getParent() : Xml {
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
