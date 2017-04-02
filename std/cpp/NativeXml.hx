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
package cpp;

@:enum abstract XmlType(Int) {
   /**
      Represents an XML element type.
   **/
   var Element = 0;
   /**
      Represents XML parsed character data type.
   **/
   var PCData = 1;
   /**
      Represents XML character data type.
   **/
   var CData = 2;
   /**
      Represents an XML comment type.
   **/
   var Comment = 3;
   /**
      Represents an XML doctype element type.
   **/
   var DocType = 4;
   /**
       Represents an XML processing instruction type.
   **/
   var ProcessingInstruction = 5;
   /**
      Represents an XML document type.
   **/
   var Document = 6;
}

class NativeXmlState
{
   var cur : Xml;

   public function new(x:Xml)
   {
      x._children = new Array<Xml>();
      cur = x;
   }


   @:keep
   public function xml(name:String, att:Dynamic<String>)
   {
      var x = new Xml();
      x._parent = cur;
      x.nodeType = Xml.Element;
      x._nodeName = name;
      x._attributes = att;
      x._children = new Array<Xml>();
      cur.addChild(x);
      cur = x;
   }

   @:keep
   public function cdata(text:String)
   {
      var x = new Xml();
      x._parent = cur;
      x.nodeType = Xml.CData;
      x._nodeValue = text;
      cur.addChild(x);
   }

   @:keep
   public function pcdata(text:String)
   {
      var x = new Xml();
      x._parent = cur;
      x.nodeType = Xml.PCData;
      x._nodeValue = text;
      cur.addChild(x);
   }

   @:keep
   public function comment(text:String)
   {
      var x = new Xml();
      x._parent = cur;
      if( text.length>1 && StringTools.fastCodeAt(text,0) == 63 )
      {
         x.nodeType = Xml.ProcessingInstruction;
         text = text.substr(1, text.length - 2);
      }
      else
      {
         x.nodeType = Xml.Comment;
      }
      x._nodeValue = text;
      cur.addChild(x);
   }

   @:keep
   public function doctype(text:String)
   {
      var x = new Xml();
      x._parent = cur;
      x.nodeType = Xml.DocType;
      x._nodeValue = text.substr(1);
      cur.addChild(x);
   }
   
   @:keep
   public function done()
   {
     cur = cur._parent;
   }
}

private class NativeXmlIterator
{
   var cur = 0;
   var children:Array<Xml>;

   public function new(inChildren:Array<Xml>)
   {
      children = inChildren;
      cur = 0;
   }

   public function hasNext() : Bool
   {
      var k = cur;
      var l = children.length;
      while( k < l )
      {
         if (children[k].nodeType == Xml.Element)
            break;
         k += 1;
      }
      cur = k;
      return k < l;
   }

   public function next():Xml
   {
      var k = cur;
      var l = children.length;
      while( k < l )
      {
         var n = children[k];
         k += 1;
         if( n.nodeType == Xml.Element )
         {
            cur = k;
            return n;
         }
      }
      return null;
   }
}

private class NativeXmlNamedIterator
{
   var cur = 0;
   var children:Array<Xml>;
   var name:String;

   public function new(inChildren:Array<Xml>, inName:String)
   {
      children = inChildren;
      name = inName;
      cur = 0;
   }


   public function hasNext() : Bool
   {
      var k = cur;
      var l = children.length;
      while( k < l )
      {
         var n = children[k];
         if( n.nodeType == Xml.Element && n._nodeName == name )
            break;
         k++;
     }
     cur = k;
     return k < l;
   }

   public function next():Xml
   {
      var k = cur;
      var l = children.length;
      while( k < l )
      {
         var n = children[k];
         k++;
         if( n.nodeType == Xml.Element && n._nodeName == name ) {
            cur = k;
            return n;
         }
      }
      return null;
   }
}




@:cppInclude("./NativeXmlImport.cpp")
@:allow(cpp.NativeXmlState) @:allow(cpp.NativeXmlIterator) @:allow(cpp.NativeXmlNamedIterator)
class Xml {
   static inline var Element = XmlType.Element;
   static inline var PCData = XmlType.PCData;
   static inline var CData = XmlType.CData;
   static inline var Comment = XmlType.Comment;
   static inline var DocType = XmlType.DocType;
   static inline var ProcessingInstruction = XmlType.ProcessingInstruction;
   static inline var Document = XmlType.Document;


   private var _nodeName : String;
   private var _nodeValue : String;
   private var _attributes : Dynamic<String>;
   private var _children : Array<Xml>;
   private var _parent : Xml;

   function new() : Void {
   }

   @:extern @:native("parse_xml")
   static function parse_xml(str:String, state:NativeXmlState) { }

   public static function parse( str : String ) : Xml
   {
      var x = new Xml();
      var state = new NativeXmlState(x);
      parse_xml(str,state);
      x.nodeType = Xml.Document;
      return x;
   }


   public static function createElement( name : String ) : Xml {
      var r = new Xml();
      r.nodeType = Xml.Element;
      r._nodeName = name;
      r._attributes = null;
      r._children = new Array();
      return r;
   }

   public static function createPCData( data : String ) : Xml {
      var r = new Xml();
      r.nodeType = Xml.PCData;
      r._nodeValue = data;
      return r;
   }

   public static function createCData( data : String ) : Xml {
      var r = new Xml();
      r.nodeType = Xml.CData;
      r._nodeValue = data;
      return r;
   }

   public static function createComment( data : String ) : Xml {
      var r = new Xml();
      r.nodeType = Xml.Comment;
      r._nodeValue = data;
      return r;
   }

   public static function createDocType( data : String ) : Xml {
      var r = new Xml();
      r.nodeType = Xml.DocType;
      r._nodeValue = data;
      return r;
   }

   public static function createProcessingInstruction( data : String ) : Xml {
      var r = new Xml();
      r.nodeType = Xml.ProcessingInstruction;
      r._nodeValue = data;
      return r;
   }

   public static function createDocument() : Xml {
      var r = new Xml();
      r.nodeType = Xml.Document;
      r._children = new Array();
      return r;
   }

   public var nodeType(default,null) : XmlType;

   public var nodeName(get,set) : String;

   public var nodeValue(get,set) : String;


   private function get_nodeName() : String {
      if( nodeType != Xml.Element )
         throw "bad nodeType";
      return _nodeName;
   }

   private function set_nodeName( n : String ) : String {
      if( nodeType != Xml.Element )
         throw "bad nodeType";
      return _nodeName = n;
   }

   private function get_nodeValue() : String {
      if( nodeType == Xml.Element || nodeType == Xml.Document )
         throw "bad nodeType";
      return _nodeValue;
   }

   private function set_nodeValue( v : String ) : String {
      if( nodeType == Xml.Element || nodeType == Xml.Document )
         throw "bad nodeType";
      return _nodeValue = v;
   }

   public var parent(get,null) : Xml;
   private function get_parent() : Xml {
      return _parent;
   }

   public function get( att : String ) : String {
      if( nodeType != Xml.Element )
         throw "bad nodeType";
      return Reflect.field( _attributes, att );
   }

   public function set( att : String, value : String ) : Void {
      if( nodeType != Xml.Element )
         throw "bad nodeType";
      if (_attributes==null)
         _attributes = {};
      Reflect.setField (_attributes, att, value );
      return null;
   }

   public function remove( att : String ) : Void{
      if( nodeType != Xml.Element )
         throw "bad nodeType";
      Reflect.deleteField( _attributes, att );
      return null;
   }

   public function exists( att : String ) : Bool {
      if( nodeType != Xml.Element )
         throw "bad nodeType";
      return Reflect.hasField( _attributes, att );
   }

   public function attributes() : Iterator<String> {
      if( nodeType != Xml.Element )
         throw "bad nodeType";
      return Reflect.fields( _attributes ).iterator();
   }

   public function iterator() : Iterator<Xml> {
      if( _children == null )
         throw "bad nodetype";
      return untyped _children.iterator();
   }


   public function elements(): Iterator<Xml>
   {
      if( _children == null )
         throw "bad nodetype";
      return new NativeXmlIterator(_children);
   }

   public function elementsNamed( name : String ) : Iterator<Xml>
   {
      if( _children == null )
         throw "bad nodetype";
      return new NativeXmlNamedIterator(_children,name);
   }

   public function firstChild() : Xml {
      if( _children == null )
         throw "bad nodetype";
      return _children[0];
   }

   public function firstElement() : Xml {
      if( _children == null )
         throw "bad nodetype";
      for( cur in 0..._children.length ) {
         var n:Xml = _children[cur];
         if( n.nodeType == Xml.Element )
            return n;
      }
      return null;
   }

   public function addChild( x : Xml ) : Void {
      if( _children == null )
         throw "bad nodetype";
      if( x._parent != null ) x._parent._children.remove(x);
      x._parent = this;
      _children.push( x );
      return null;
   }

   public function removeChild( x : Xml ) : Bool {
      if( _children == null )
         throw "bad nodetype";
      var b = _children.remove( x );
      if( b ) x._parent = null;
      return b;
   }

   public function insertChild( x : Xml, pos : Int ) : Void {
      if( _children == null )
         throw "bad nodetype";
      if( x._parent != null ) x._parent._children.remove(x);
      x._parent = this;
      _children.insert( pos, x );
      return null;
   }

   public function toString() : String {
      var s = new StringBuf();
      toStringRec(s);
      return s.toString();
   }

   private function toStringRec(s: StringBuf) : Void {
      switch( nodeType ) {
      case Xml.Document:
         for( x in _children )
            x.toStringRec(s);
      case Xml.Element:
         s.addChar("<".code);
         s.add(_nodeName);
         for( k in Reflect.fields(_attributes) ) {
            s.addChar(" ".code);
            s.add(k);
            s.addChar("=".code);
            s.addChar("\"".code);
            s.add(Reflect.field(_attributes,k));
            s.addChar("\"".code);
         }
         if( _children.length == 0 ) {
            s.addChar("/".code);
            s.addChar(">".code);
            return;
         }
         s.addChar(">".code);
         for( x in _children )
            x.toStringRec(s);
         s.addChar("<".code);
         s.addChar("/".code);
         s.add(_nodeName);
         s.addChar(">".code);
      case Xml.PCData:
         s.add(StringTools.htmlEscape(_nodeValue));
      case Xml.CData:
         s.add("<![CDATA[");
         s.add(_nodeValue);
         s.add("]]>");
      case Xml.Comment:
         s.add("<!--");
         s.add(_nodeValue);
         s.add("-->");
      case Xml.DocType:
         s.add("<!DOCTYPE ");
         s.add(_nodeValue);
         s.add(">");
      case Xml.ProcessingInstruction:
         s.add("<?");
         s.add(_nodeValue);
         s.add("?>");
      }
   }

}
