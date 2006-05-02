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
package flash;
import Xml;

class FlashXml__ {

	public property nodeName(getNodeName,setNodeName) : String;
	public property nodeValue(getNodeValue,setNodeValue) : String;
	public property nodeType(default,null) : XmlType;

	private var __x : Dynamic;

	private static function convert( o : Dynamic ) : Xml {
		if( o == null ) return null;
		if( o.__w != null ) return o.__w;

		var r = new FlashXml__();
		r.__x = o;
		o.__w = r;

		r.nodeType = switch( o.nodeType ) {
			case 1:
				Xml.Element;
			case 3:
				Xml.PCData;
			default:
				throw "unknow nodeType: "+o.nodeType;
		}

		return untyped r;
	}

	public static function parse( xmlData : String ) : Xml untyped {
		var x = __new__(_global["XML"]);
		x.parseXML(xmlData);
		if( x.status != 0 )
			throw ("Xml parse error #"+x.status);

		var r = convert(x);
		untyped r.nodeType = Xml.Document;
		return r;
	}

	public static function createDocument() : Xml {
		var o = untyped __new__(_global["XML"]).createElement( "#document" );

		var r = convert(o);
		untyped r.nodeType = Xml.Document;
		return r;
	}

	public static function createCData( s : String ) : Xml {
		var o = untyped __new__(_global["XML"]).createTextNode( s );
		var x = convert(o);
		untyped x.nodeType = Xml.CData;
		return x;
	}

	public static function createPCData( s : String ) : Xml {
		var o = untyped __new__(_global["XML"]).createTextNode( s );

		return convert(o);
	}

	public static function createElement( s : String ) : Xml {
		var o = untyped __new__(_global["XML"]).createElement( s );

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

	private function new(){
	}

	public function firstChild(){
		return convert(this.__x.firstChild);
	}

	public function firstElement(){
		var e = __x.firstChild;
		while( e != null && e.nodeType != 3 )
			e = e.nextSibling;
		return convert(e);
	}

	private function setNodeName( n : String ) : String {
		if( nodeType != Xml.Element )
			throw "bad nodeType";

		untyped {
			return __x.nodeName = n;
		}
	}

	private function setNodeValue( v : String ) : String {
		if( nodeType == Xml.Element || nodeType == Xml.Document )
			throw "bad nodeType";

		untyped {
			return __x.nodeValue = v;
		}
	}

	private function getNodeName() : String {
		if( nodeType != Xml.Element )
			throw "bad nodeType";

		return __x.nodeName;
	}

	private function getNodeValue() : String {
		if( nodeType == Xml.Element || nodeType == Xml.Document )
			throw "bad nodeType";

		return __x.nodeValue;
	}

	public function iterator(){
		return untyped {
			cur: this.firstChild(),
			hasNext : function(){
				return this.cur != null;
			},
			next : function(){
				var r = this.cur;
				this.cur = convert(this.cur.__x.nextSibling);
				return r;
			}
		}
	}

	public function elements(){
		var nextElement = untyped function( e ) {
			while( e != null && e.nodeType != Xml.Element ){
				e = convert(e.__x.nextSibling);
			}
			return e;
		};

		return untyped {
			cur: this.firstChild(),
			hasNext : function(){
				this.cur = nextElement(this.cur);
				return this.cur != null;
			},
			next : function(){
				var r = nextElement(this.cur);
				this.cur = if( r == null ) null; else nextElement(convert(r.__x.nextSibling));
				return r;
			}
		}
	}

	public function elementsNamed( nodeName : String ){
		var nextElement = untyped function( e ) {
			while( e != null && (e.nodeType != Xml.Element || e.nodeName != nodeName) ){
				e = convert(e.__x.nextSibling);
			}
			return e;
		};

		return untyped {
			cur: this.__x.firstChild(),
			hasNext : function(){
				this.cur = nextElement(this.cur);
				return this.cur != null;
			},
			next : function(){
				var r = nextElement(this.cur);
				this.cur = if( r == null ) null; else nextElement((r.__x.nextSibling));
				return r;
			}
		}
	}

	public function get( k : String ) : String {
		if( nodeType != Xml.Element )
			throw "bad nodeType";

		return Reflect.field(__x.attributes,k);
	}

	public function set( k : String, v : String ) : Void {
		if( nodeType != Xml.Element )
			throw "bad nodeType";

		return Reflect.setField(__x.attributes,k,v);
	}

	public function exists( k : String ) : Bool {
		if( nodeType != Xml.Element )
			throw "bad nodeType";

		return Reflect.hasField(__x.attributes,k);
	}

	public function remove( k : String ) : Void {
		if( nodeType != Xml.Element )
			throw "bad nodeType";

		Reflect.deleteField(__x.attributes,k);
	}

	public function attributes() : Iterator<String> {
		if( nodeType != Xml.Element )
			throw "bad nodeType";

		return untyped __keys__(__x.attributes).iterator();
	}

	public function addChild( child : Xml ) {
		untyped __x.appendChild(child.__x);
	}

	public function removeChild( child : Xml ) : Bool {
		untyped if( child.__x.parentNode != __x )
			return false;

		untyped child.__x.removeNode();
		return true;
	}

	public function insertChild( x : Xml, pos : Int ) : Void {
		var i = 0;
		for( c in iterator() ){
			if( i == pos ){
				untyped __x.insertBefore(x.__x,c.__x);
			}
			i++;
		}
	}

	public function toString() {
		if( nodeType == Xml.Document ){
			var s = "";
			for( c in iterator() ){
				untyped s += c.__x.toString();
			}
			return s;
		}
		if( nodeType == Xml.CData )
			return "<![CDATA["+__x.nodeValue+"]]>";
		return __x.toString();
	}


}
