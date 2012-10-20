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
package tools.haxedoc;
import haxe.rtti.CType;

class Main {

	static var parser = new haxe.rtti.XmlParser();

	static function loadFile(file,platform,?remap) {
		var data = sys.io.File.getContent(neko.Web.getCwd()+file);
		var x = Xml.parse(data).firstElement();
		if( remap != null )
			transformPackage(x,remap,platform);
		parser.process(x,platform);
	}

	static function transformPackage( x : Xml, p1, p2 ) {
		switch( x.nodeType ) {
		case Xml.Element:
			var p = x.get("path");
			if( p != null && p.substr(0,6) == p1 + "." )
				x.set("path",p2 + "." + p.substr(6));
			for( x in x.elements() )
				transformPackage(x,p1,p2);
		default:
		}
	}

	static function save(html : HtmlPrinter,x,file) {
		var f = sys.io.File.write(file,true);
		html.output = f.writeString;
		html.process(x);
		f.close();
		neko.Lib.print(".");
	}

	static function generateEntry(html : HtmlPrinter,e,path) {
		switch( e ) {
		case TPackage(name,full,entries):
			if( html.filtered(full,true) )
				return;
			var old = html.baseUrl;
			html.baseUrl = "../"+html.baseUrl;
			path += name + "/";
			try sys.FileSystem.createDirectory(path) catch( e : Dynamic ) { }
			for( e in entries )
				generateEntry(html,e,path);
			html.baseUrl = old;
		default:
			var inf = TypeApi.typeInfos(e);
			if( html.filtered(inf.path,false) )
				return;
			var pack = inf.path.split(".");
			var name = pack.pop();
			save(html,e,path+name+".html");
		}
	}

	static function generateAll(filters : List<String>) {
		var html = new HtmlPrinter("content/",".html","../index");
		for( f in filters )
			html.addFilter(f);
		save(html,TPackage("root","root",parser.root),"index.html");
		html.baseUrl = "";
		try sys.FileSystem.createDirectory("content") catch( e : Dynamic ) { }
		for( e in parser.root )
			generateEntry(html,e,"content/");
	}

	public static function main() {
		if( neko.Web.isModNeko ) {
			var h = neko.Web.getParams();
			var dataFile = neko.Web.getCwd()+".data";
			var data : TypeRoot = try neko.Lib.unserialize(sys.io.File.getBytes(dataFile)) catch( e : Dynamic ) null;
			if( h.get("reload") != null || data == null ) {
				var baseDir = "../data/media/";
				loadFile(baseDir+"flash.xml","flash");
				loadFile(baseDir+"flash9.xml","flash9","flash");
				loadFile(baseDir+"neko.xml","neko");
				loadFile(baseDir+"js.xml","js");
				loadFile(baseDir+"php.xml","php");
				parser.sort();
				data = parser.root;
				var bytes = neko.Lib.serialize(data);
				var f = sys.io.File.write(dataFile,true);
				f.write(bytes);
				f.close();
			}
			var html = new HtmlPrinter("/api/","","");
			var clname = h.get("class");
			if( clname == "index" )
				clname = null;
			if( clname == null )
				html.process(TPackage("root","root",data));
			else {
				var clpath = clname.toLowerCase().split("/").join(".").split(".");
				var f = html.find(data,clpath,0);
				if( f == null )
					throw "Class not found : "+clpath.join(".");
				html.process(f);
			}
		} else {
			var filter = false;
			var filters = new List();
			var pf = null;
			for( x in Sys.args() ) {
				if( x == "-f" )
					filter = true;
				else if( x == "-v" )
					parser.newField = function(c,f) {
						if( f.isPublic && !f.isOverride && !c.isPrivate )
							Sys.println("[API INCOMPATIBILITY] "+c.path+"."+f.name+" ["+pf+"]");
					};
				else if( filter ) {
					filters.add(x);
					filter = false;
				} else {
					var f = x.split(";");
					pf = f[1];
					loadFile(f[0],f[1],f[2]);
				}
			}
			parser.sort();
			if( parser.root.length == 0 ) {
				Sys.println("Haxe Doc Generator 2.0 - (c)2006 Motion-Twin");
				Sys.println(" Usage : haxedoc [xml files] [-f filter]");
				Sys.exit(1);
			}
			generateAll(filters);
		}
	}

}
