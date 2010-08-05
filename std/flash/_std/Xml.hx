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


	public var nodeName(getNodeName,setNodeName) : String;
	public var nodeValue(getNodeValue,setNodeValue) : String;
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

	public static function parse( xmlData : String ) : Xml untyped {
		var x = __new__(_global["XML"]);
		x["parseXML"](xmlData);
		if( x["status"] != 0 )
			throw ("Xml parse error #"+x["status"]);

		var r = convert(x);
		untyped r.nodeType = Xml.Document;
		return r;
	}

	public static function createDocument() : Xml {
		var o = untyped __new__(_global["XML"])["createElement"]( "#document" );

		var r = convert(o);
		untyped r.nodeType = Xml.Document;
		return r;
	}

	public static function createCData( s : String ) : Xml {
		var o = untyped __new__(_global["XML"])["createTextNode"]( s );
		var x = convert(o);
		untyped x.nodeType = Xml.CData;
		return x;
	}

	public static function createPCData( s : String ) : Xml {
		var o = untyped __new__(_global["XML"])["createTextNode"]( s );

		return convert(o);
	}

	public static function createElement( s : String ) : Xml {
		var o = untyped __new__(_global["XML"])["createElement"]( s );

		return convert(o);
	}

	public static function createComment( s : String ) : Xml {
		throw "not implemented";
		return null;
	}

	public static function createDocType( s : String ) : Xml {
		throw "not implemented";
		return null;
	}

	public static function createProlog( s : String ) : Xml {
		throw "not implemented";
		return null;
	}

	private function new() : Void {
	}

	public function firstChild() : Xml {
		return convert(this.__x[untyped "firstChild"]);
	}

	public function firstElement() : Xml {
		var e : Dynamic = __x[untyped "firstChild"];
		while( e != null && e[untyped "nodeType"] != 1 )
			e = e[untyped "nextSibling"];
		return convert(e);
	}

	private function setNodeName( n : String ) : String {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		return __x[untyped "nodeName"] = n;
	}

	private function setNodeValue( v : String ) : String {
		if( nodeType == Xml.Element || nodeType == Xml.Document )
			throw "bad nodeType";
		return __x[untyped "nodeValue"] = v;
	}

	private function getNodeName() : String {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		return __x[untyped "nodeName"];
	}

	private function getNodeValue() : String {
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
				return this.cur != null;
			},
			next : function(){
				var r = convert(this.cur);
				this.cur = this.cur["nextSibling"];
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
				var r = this.cur;
				while( r != null && r["nodeType"] != 1 )
					r = r["nextSibling"];
				this.cur = r;
				return r != null;
			},
			next : function(){
				var r = this.cur;
				while( r != null && r["nodeType"] != 1 )
					r = r["nextSibling"];
				if( r == null ) {
					this.cur = null;
					return null;
				}
				this.cur = r["nextSibling"];
				return convert(r);
			}
		}
	}

	public function elementsNamed( nodeName : String ) : Iterator<Xml> {
		if( nodeType != Xml.Document && nodeType != Xml.Element )
			throw "bad nodeType";
		return untyped {
			cur: this.__x[untyped "firstChild"],
			hasNext : function() {
				var r = this.cur;
				while( r != null && (r["nodeType"] != 1 || r["nodeName"] != nodeName) )
					r = r["nextSibling"];
				this.cur = r;
				return r != null;
			},
			next : function(){
				var r = this.cur;
				while( r != null && (r["nodeType"] != 1 || r["nodeName"] != nodeName) )
					r = r["nextSibling"];
				if( r == null ) {
					this.cur = null;
					return null;
				}
				this.cur = r["nextSibling"];
				return convert(r);
			}
		}
	}

	public function get( k : String ) : String {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		return Reflect.field(__x[untyped "attributes"],k);
	}

	public function set( k : String, v : String ) : Void {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		return Reflect.setField(__x[untyped "attributes"],k,v);
	}

	public function exists( k : String ) : Bool {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		return Reflect.hasField(__x[untyped "attributes"],k);
	}

	public function remove( k : String ) : Void {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		Reflect.deleteField(__x[untyped "attributes"],k);
	}

	public function attributes() : Iterator<String> {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		return untyped __keys__(__x["attributes"])["iterator"]();
	}

	public function addChild( child : Xml ) : Void {
		if( nodeType != Xml.Document && nodeType != Xml.Element )
			throw "bad nodeType";
		untyped __x[untyped "appendChild"](child.__x);
	}

	public function removeChild( child : Xml ) : Bool {
		if( nodeType != Xml.Document && nodeType != Xml.Element )
			throw "bad nodeType";
		untyped if( child.__x["parentNode"] != __x )
			return false;
		untyped child.__x["removeNode"]();
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
