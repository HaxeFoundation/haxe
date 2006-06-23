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
package tools;

import neko.Lib;
import neko.Web;

private class Url {
	public static var base : String;
	public static var extension : String = "";
	public static var index : String;
	public static var buffer : StringBuf;
	public static function make( params, css, text ) {
		return "<a href=\""+base + params + extension+"\" class=\""+css+"\">"+text+"</a>";
	}
}

private enum DocType {
	tunknown;
	tclass( name : String, params : Array<DocType> );
	tenum( name : String, params : Array<DocType> );
	tanon( fields : Array<{ name : String, t : DocType  }> );
	tdynamic( t : DocType );
	tfunction( params : Array<{ name : String, t : DocType }>, ret : DocType );
	tparam( classpath : String, name : String );
	tconstr( fields : Array<{ name : String, t : DocType }> );
	tsign( name : String, params : Array<DocType> );
}

private class DocField {

	public var name : String;
	public var isStatic : Bool;
	public var type : DocType;
	public var doc : String;
	var parent : DocClass;

	public function new( name, s, t, p ) {
		this.name = name;
		isStatic = s;
		type = t;
		parent = p;
	}

	public function isVar() {
		if( type == null )
			return true;
		switch type {
		case tfunction(_,_):
			return false;
		default:
			return true;
		}
	}

	public function link(name : String ) {
		var path = name.split(".");
		var local = true;
		for( i in 0...path.length-1 ) {
			if( path[i] != parent.spath[i] ) {
				local = false;
				break;
			}
		}
		if( local ) {
			var url = path.join("/");
			return Url.make(url,"type",path.pop());
		}
		return Url.make(path.join("/"),"type",name);
	}

	function paramsToString( params : Array<DocType> ) {
		if( params.length == 0 )
			return "";
		var ps = new StringBuf();
		ps.add("&lt;");
		var first = true;
		for( p in params ) {
			if( first )
				first = false;
			else
				ps.add(",");
			ps.add(typeToString(p));
		}
		ps.add("&gt;");
		return ps.toString();
	}

	function typeToString(t) {
		switch t {
		case tunknown:
			return "Unknown";
		case tclass(name,params):
			return link(name)+paramsToString(params);
		case tsign(name,params):
			return link(name)+paramsToString(params);
		case tenum(name,params):
			return link(name)+paramsToString(params);
		case tanon(fields):
			var buf = new StringBuf();
			var first = true;
			buf.add("{");
			for( f in fields ) {
				if( first )
					first = false;
				else
					buf.add(", ");
				buf.add(f.name);
				buf.add(" : ");
				buf.add(typeToString(f.t));
			}
			buf.add(" }");
			return buf.toString();
		case tdynamic(t):
			if( t == null )
				return link("Dynamic");
			return link("Dynamic") + "&lt;" + typeToString(t) + "&gt;";
		case tfunction(params,ret):
			var buf = new StringBuf();
			if( params.length == 0 )
				buf.add("Void -> ");
			else {
				for( p in params ) {
					if( p.name != "" ) {
						buf.add(p.name);
						buf.add(" : ");
					}
					buf.add(funToString(p.t,true));
					buf.add(" -> ");
				}
			}
			buf.add(funToString(ret,false));
			return buf.toString();
		case tparam(cl,name):
			return if( cl != parent.path ) link(cl) + "." + name else name;
		case tconstr(params):
			var s = new StringBuf();
			s.add("(");
			var first = true;
			for( p in params ) {
				if( first )
					first = false;
				else
					s.add(", ");
				s.add(p.name);
				s.add(" : ");
				s.add(typeToString(p.t));
			}
			s.add(")");
			return s.toString();
		}
		return null;
	}

	function funToString( t, isarg ) {
		var parent =
		switch( t ) {
		case tfunction(_,_): true;
		case tenum(name,_): isarg && name == "Void";
		default: false;
		}
		if( parent )
			return "(" + typeToString(t) + ")";
		else
			return typeToString(t);
	}

	public function methToString( t ) {
		switch( t ) {
		case tfunction(params,ret):
			var s = new StringBuf();
			s.add("(");
			var first = true;
			for( p in params ) {
				if( first )
					first = false;
				else
					s.add(", ");
				if( p.name == "" )
					return typeToString(t);
				s.add(p.name);
				s.add(" : ");
				s.add(typeToString(p.t));
			}
			s.add(") : ");
			s.add(typeToString(ret));
			return s.toString();
		default:
			return typeToString(t);
		}
	}

}

private class DocClass {

	public var path : String;
	public var spath : Array<String>;
	public var module : String;
	public var name : String;
	public var doc : String;
	public var params : Array<String>;
	public var fields : Array<DocField>;
	public var isPrivate : Bool;

	public function new( path ) {
		this.path = path;
		spath = path.split(".");
		fields = new Array();
		params = new Array();
	}

	function genName( s : StringBuf ) {
		s.add("class ");
		s.add(path);
	}

	function genBody( s : StringBuf ) {
		for( f in fields ) {
			s.add("<dt>");
			if( f.isStatic )
				s.add("static ");
			if( f.isVar() )
				s.add("var ");
			else
				s.add("function ");
			s.add(f.name);
			if( f.isVar() )
				s.add(" : ");
			s.add(f.methToString(f.type));
			s.add("</dt>");
			s.add("<dd>");
			if( f.doc != null ) s.add(f.doc);
			s.add("</dd>");
		}
	}

	public function toString() {
		var s = new StringBuf();
		s.add("<div class=\"classname\">");
		if( isPrivate )
			s.add("private ");
		genName(s);
		if( params.length > 0 ) {
			s.add("&lt;");
			s.add(params.join(", "));
			s.add("&gt;");
		}
		s.add("</div>");
		if( module != null ) {
			s.add("<div class=\"importmod\">");
			s.add("import "+module);
			s.add("</div>");
		}
		if( doc != null ) {
			s.add("<div class=\"classdoc\">");
			s.add(doc);
			s.add("</div>");
		}
		s.add("<dl>");
		genBody(s);
		s.add("</dl>");
		return s.toString();
	}

}


private class DocEnum extends DocClass {

	override function genName( s : StringBuf ) {
		s.add("enum ");
		s.add(path);
	}

	override function genBody( s : StringBuf ) {
		for( f in fields ) {
			s.add("<dt>");
			s.add(f.name);
			if( f.type != null )
				s.add(f.methToString(f.type));
			s.add("</dt>");
			s.add("<dd>");
			if( f.doc != null ) s.add(f.doc);
			s.add("</dd>");
		}
	}

}

private class DocSign extends DocClass {

	public var t : DocType;

	override function genBody( s : StringBuf ) {
		if( t == null ) {
			super.genBody(s);
			return;
		}
		s.add("<dt> = ");
		s.add(new DocField("",false,t,this).methToString(t));
		s.add("</dt>");
	}

	override function genName( s : StringBuf ) {
		s.add("signature ");
		s.add(path);
	}
}

private enum DocEntry {
	eclass( c : DocClass );
	epackage( name : String, childs : Array<DocEntry> );
}

class DocView {

	static var entries = new Array();

	static function processType( x : Xml ) {
		var p = new Array();
		switch( x.nodeName )  {
		case "unknown":
			return tunknown;
		case "c":
			return tclass(x.get("path"),Lambda.amap(Lambda.array(x.elements()),processType));
		case "s":
			return tsign(x.get("path"),Lambda.amap(Lambda.array(x.elements()),processType));
		case "e":
			var path = x.get("path").split(".");
			if( path.length >= 2 ) {
				var c = path[path.length-2].charAt(0);
				if( c >= "A" && c <= "Z" ) {
					var name = path.pop();
					return tparam(path.join("."),name);
				}
			}
			return tenum(x.get("path"),Lambda.amap(Lambda.array(x.elements()),processType));
		case "f":
			var params = x.get("a").split(":");
			var it = x.elements();
			var pl = Lambda.amap(Lambda.array(params.iterator()),function(name) {
				return {
					name : name,
					t : processType(it.next())
				};
			});
			return tfunction(pl,processType(it.next()));
		case "a":
			var fields = Lambda.amap(Lambda.array(x.elements()),function(x : Xml) {
				return { name : x.nodeName, t : processType(x.firstElement()) };
			});
			return tanon(fields);
		case "d":
			var x = x.firstElement();
			return tdynamic( if( x == null) null else processType(x) );
		default:
			throw ("Unknown type "+x.nodeName);
		}
	}

	static function docFormat( doc : String ) : String {
		doc = ~/\[([^\]]+)\]/g.replace(doc,"<code>$1</code>");
		return doc;
	}

	static function processField( c : DocClass, x : Xml ) {
		var stat = x.get("static") == "1";
		var nl = x.elements();
		var t = processType(nl.next());
		var f = new DocField(x.nodeName,stat,t,c);
		var doc = nl.next();
		if( doc != null )
			f.doc = docFormat(doc.firstChild().nodeValue);
		return f;
	}

	static function processClass(x : Xml) {
		var path = x.get("path");
		if( StringTools.endsWith(path,"__") )
			return;
		if( findEntry(entries,path.split(".")) != null ) {
			// MERGE ?
			return;
		}
		var c : DocClass;
		switch( x.nodeName ) {
		case "class":
			c = new DocClass(path);
			for( m in x.elements() ) {
				if( m.nodeName == "haxe:doc" ) {
					c.doc = docFormat(m.firstChild().nodeValue);
					continue;
				}
				if( m.get("public") == "1" )
					c.fields.push(processField(c,m));
			}
		case "signature":
			var s = new DocSign(path);
			var t = processType(x.firstElement());
			switch( t ) {
			case tanon(fields):
				for( f in fields ) {
					var f = new DocField(f.name,false,f.t,s);
					s.fields.push(f);
				}
			default:
				s.t = t;
			}
			c = s;
		case "enum":
			var e = new DocEnum(path);
			c = e;
			for( m in x.elements() ) {
				if( m.nodeName == "haxe:doc" ) {
					c.doc = docFormat(m.firstChild().nodeValue);
					continue;
				}
				var l = Lambda.array(m.elements());
				var t = if( m.get("a") == null ) null else {
					var names = m.get("a").split(":");
					var params = Lambda.amap(names,function(name) {
						return {
							name : name,
							t : processType(l.pop())
						};
					});
					tconstr(params);
				}
				var f = new DocField(m.nodeName,false,t,c);
				c.fields.push(f);
			}
		default:
			throw x.nodeName;
		}
		c.isPrivate = x.get("private") == "1";
		c.module = x.get("module");
		c.params = x.get("params").split(":");
		c.fields.sort(function(f1 : DocField,f2 : DocField) {
			if( f1.isStatic && !f2.isStatic )
				return 1;
			var v1 = f1.isVar();
			var v2 = f2.isVar();
			if( v1 && !v2 )
				return -1;
			if( v2 && !v1 )
				return 1;
			if( f1.name > f2.name )
				return 1;
			return -1;
		});
		addEntry(c);
	}

	static function addEntry( c : DocClass ) {
		var path = c.path.split(".");
		var pack = entries;
		if( path.length > 0 ) {
			c.name = path.pop();
			for( x in path ) {
				var found = false;
				for( p in pack ) {
					switch p {
					case epackage(name,p):
						if( name == x ) {
							pack = p;
							found = true;
							break;
						}
					default:
					}
				}
				if( !found ) {
					var p = new Array();
					pack.push(epackage(x,p));
					pack = p;
				}
			}
		} else
			c.name = c.path;
		pack.push(eclass(c));
	}

	static function findEntry( pack : Array<DocEntry>, path : Array<String>) {
		for( p in path ) {
			var found = false;
			for( e in pack ) {
				switch e {
				case eclass(c): if( c.name.toLowerCase() == p.toLowerCase() ) return c;
				case epackage(name,newpack):
					if( name == p ) {
						found = true;
						pack = newpack;
						break;
					}
				}
			}
			if( !found )
				return null;
		}
		return null;
	}

	static function sortEntries( p : Array<DocEntry> ) {
		p.sort(function(e1 : DocEntry,e2 : DocEntry) {
			var n1 = switch e1 {
				case epackage(p,_) : " "+p;
				case eclass(c) : c.name;
			};
			var n2 = switch e2 {
				case epackage(p,_) : " "+p;
				case eclass(c) : c.name;
			};
			if( n1 > n2 )
				return 1;
			return -1;
		});
		for( e in p ) {
			switch e {
			case epackage(_,p):
				sortEntries(p);
			default:
			}
		}
	}

	static function display(p : Array<DocEntry> ) {
		print("<ul class=\"entry\">");
		for( e in p ) {
			switch e {
			case epackage(name,p):
				if( !filtered(name) )
					continue;
				print('<li><a href="#" class="package" onclick="toggle(\''+name+'\')">'+name+"</a><div id=\""+name+"\" class=\"package_content\">");
				display(p);
				print("</div></li>");
			case eclass(c):
				if( c.isPrivate || c.path == "@Main" || !filtered(c.path) )
					continue;
				print("<li>"+Url.make(c.path.split(".").join("/"),"entry",c.name)+"</li>");
			}
		}
		print("</ul>");
	}

	static function loadFile(file) {
		var data = neko.File.getContent(Web.getCwd()+file);
		var x = Xml.parse(data).firstChild();
		for( c in x.elements() )
			processClass(c);
	}

	static function print(s) {
		Url.buffer.add(s);
	}

	static function displayHtml(html : Xml,clname : String) {
		if( html.nodeType != Xml.Element ) {
			print(html.toString());
			return;
		}
		if( html.nodeName == "data" ) {
			if( clname == "index" )
				clname = null;
			if( clname == null )
				display(entries);
			else {
				clname = clname.split("/").join(".");
				var c = findEntry(entries,clname.split("."));
				if( c == null )
					throw ("Class not found : "+clname);
				print(Url.make(Url.index,"index","Index"));
				print(c.toString());
				print(Url.make(Url.index,"index","Index"));
			}
			return;
		}

		if( !html.iterator().hasNext() ) {
			print(html.toString());
			return;
		}
		print("<");
		print(html.nodeName);
		for( k in html.attributes() )
			print(" "+k+"=\""+html.get(k)+"\"");
		print(">");
		for( c in html )
			displayHtml(c,clname);
		print("</"+html.nodeName+">");
	}

	static var default_template = "<html><body><data/></body></html>";
	static var filters = new List();

	static function filtered(name) {
		if( filters.isEmpty() )
			return true;
		for( x in filters )
			if( StringTools.startsWith(name,x) )
				return true;
		return false;
	}

	static function save(html,clname,file) {
		Url.buffer = new StringBuf();
		displayHtml(html,clname);
		var f = neko.File.write(file,false);
		f.write(Url.buffer.toString());
		f.close();
		neko.Lib.print(".");
	}

	static function generateEntry(html,e,path) {
		switch( e ) {
		case eclass(c):
			if( !filtered(c.path) )
				return;
			save(html,c.path,path+c.name+".html");
		case epackage(name,entries):
			if( !filtered(name) )
				return;
			var old = Url.base;
			Url.base = "../"+Url.base;
			path += name + "/";
			try neko.FileSystem.createDir(path) catch( e : Dynamic ) { }
			for( e in entries )
				generateEntry(html,e,path);
			Url.base = old;
		}
	}

	static function generateAll(html) {
		Url.extension = ".html";
		Url.base = "content/";
		Url.index = "index";
		save(html,null,"index.html");
		Url.base = "";
		Url.index = "../index";
		try neko.FileSystem.createDir("content") catch( e : Dynamic ) { }
		for( e in entries )
			generateEntry(html,e,"content/");
	}

	public static function main() {
		var hdata =
			try
				neko.File.getContent(Web.getCwd()+"template.xml")
			catch( e : Dynamic ) try {
				var p = ~/[\/\\]/g.split(neko.Sys.executablePath());
				p.pop();
				neko.File.getContent(p.join("/")+"/std/tools/template.xml");
			} catch( e : Dynamic )
				default_template;
		var html = Xml.parse(hdata).firstChild();
		if( neko.Web.isModNeko ) {
			var baseDir = "../data/media/";
			Url.base = "/api/";
			Url.index = "";
			loadFile(baseDir+"flash.xml");
			loadFile(baseDir+"neko.xml");
			loadFile(baseDir+"js.xml");
			sortEntries(entries);

			var h = Web.getParams();
			var clname = h.get("class");
			Url.buffer = new StringBuf();
			displayHtml(html,clname);
			Lib.print(Url.buffer.toString());
		} else {
			var filter = false;
			for( x in neko.Sys.args() ) {
				if( x == "-f" )
					filter = true;
				else if( filter ) {
					filters.add(x);
					filter = false;
				} else
					loadFile(x);
			}
			sortEntries(entries);
			generateAll(html);
		}
	}

}
