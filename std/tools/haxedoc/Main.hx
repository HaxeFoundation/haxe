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
import haxe.rtti.Type;

class Main {

	static var parser = new haxe.rtti.XmlParser();

	static function loadFile(file,platform) {
		var data = neko.io.File.getContent(neko.Web.getCwd()+file);
		var x = Xml.parse(data).firstElement();
		parser.process(x,platform);
	}

	static function save(html : HtmlPrinter,x,file) {
		var f = neko.io.File.write(file,true);
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
			try neko.FileSystem.createDirectory(path) catch( e : Dynamic ) { }
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
		try neko.FileSystem.createDirectory("content") catch( e : Dynamic ) { }
		for( e in parser.root )
			generateEntry(html,e,"content/");
	}

	static function findClass( t : TypeRoot, path : Array<String>, pos : Int ) {
		var name = path[pos];
		var pack = (pos != path.length - 1);
		var def = null;
		for( c in t )
			switch( c ) {
			case TPackage(pname,_,subs):
				if( name == pname ) {
					if( pack )
						return findClass(subs,path,pos+1);
					def = c;
				}
			default:
				if( pack ) continue;
				var inf = TypeApi.typeInfos(c);
				if( inf.path.toLowerCase() == path.join(".") )
					return c;
			}
		return def;
	}

	public static function main() {
		if( neko.Web.isModNeko ) {
			var h = neko.Web.getParams();
			var dataFile = neko.Web.getCwd()+".data";
			var data : TypeRoot = try neko.Lib.unserialize(neko.io.File.getContent(dataFile)) catch( e : Dynamic ) null;
			if( h.get("reload") != null || data == null ) {
				var baseDir = "../data/media/";
				loadFile(baseDir+"flash.xml","flash");
				loadFile(baseDir+"neko.xml","neko");
				loadFile(baseDir+"js.xml","js");
				parser.sort();
				data = parser.root;
				var str = neko.Lib.serialize(data);
				var f = neko.io.File.write(dataFile,true);
				f.writeString(str);
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
				var f = findClass(data,clpath,0);
				if( f == null )
					throw "Class not found : "+clpath.join(".");
				html.process(f);
			}
		} else {
			var filter = false;
			var filters = new List();
			for( x in neko.Sys.args() ) {
				if( x == "-f" )
					filter = true;
				else if( filter ) {
					filters.add(x);
					filter = false;
				} else {
					var f = x.split(";");
					loadFile(f[0],f[1]);
				}
			}
			parser.sort();
			if( parser.root.length == 0 ) {
				neko.Lib.println("Haxe Doc Generator 2.0 - (c)2006 Motion-Twin");
				neko.Lib.println(" Usage : haxedoc [xml files] [-f filter]");
				neko.Sys.exit(1);
			}
			generateAll(filters);
		}
	}

}
