package tools;

import neko.Lib;
import neko.Web;

private class Url {
	public static var base : String;
	public static function make( params, text ) {
		return "<a href=\""+base + params+"\">"+text+"</a>";
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
}

private class DocField {

	public var name : String;
	public var isStatic : Bool;
	public var type : DocType;
	public var doc : String;
	public var parent : DocClass;

	public function new( name, s, t ) {
		this.name = name;
		isStatic = s;
		type = t;
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

	public function link(name : String) {
		return Url.make(name.split(".").join("/"),name);
	}

	public function typeToString(t) {
		switch t {
		case tunknown:
			return "Unknown";
		case tclass(name,params):
			var ps = "";
			if( params.length != 0 ) {
				ps = "&lt;";
				var first = true;
				for( p in params ) {
					if( first )
						first = false;
					else
						ps += ",";
					ps += typeToString(p);
				}
				ps += "&gt;";
			}
			return link(name)+ps;
		case tenum(name,params):
			var ps = "";
			if( params.length != 0 ) {
				ps = "&lt;";
				var first = true;
				for( p in params ) {
					if( first )
						first = false;
					else
						ps += ",";
					ps += typeToString(p);
				}
				ps += "&gt;";
			}
			return link(name)+ps;
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
			s.add(") : ");
			return s.toString() + typeToString(ret);
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

}

private class DocClass {

	public var path : String;
	public var name : String;
	public var doc : String;
	public var isEnum : Bool;
	public var params : Array<String>;
	public var fields : Array<DocField>;

	public function new( path, e ) {
		this.path = path;
		isEnum = e;
		fields = new Array();
		params = new Array();
	}

	public function toString() {
		var s = new StringBuf();
		s.add("<h1>");
		s.add(path);
		if( params.length > 0 ) {
			s.add("&lt;");
			s.add(params.join(", "));
			s.add("&gt;");
		}
		s.add("</h1>");
		if( doc != null ) {
			s.add("<p>");
			s.add(doc);
			s.add("</p>");
		}
		s.add("<dl>");
		if( isEnum ) {
			for( f in fields ) {
				s.add("<dt>");
				s.add(f.name);
				if( f.type != null )
					s.add(f.typeToString(f.type));
				s.add("</dt>");
				s.add("<dd>");
				if( f.doc != null ) s.add(f.doc);
				s.add("</dd>");
			}
		} else {
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
				s.add(f.typeToString(f.type));
				s.add("</dt>");
				s.add("<dd>");
				if( f.doc != null ) s.add(f.doc);
				s.add("</dd>");
			}
		}
		s.add("</dl>");
		return s.toString();
	}

}

private enum DocEntry {
	eclass( c : DocClass );
	epackage( name : String, childs : Array<DocEntry> );
}

class DocView {

	static var entries = new Array();

	static function processType( x : Node ) {
		var p = new Array();
		switch( x.nodeName )  {
		case "unknown":
			return tunknown;
		case "c":
			return tclass(x.attributes.path,Lambda.amap(Lambda.array(x.nodes()),processType));
		case "e":
			var path = x.attributes.path.split(".");
			if( path.length >= 2 ) {
				var c = path[path.length-2].charAt(0);
				if( c >= "A" && c <= "Z" ) {
					var name = path.pop();
					return tparam(path.join("."),name);
				}
			}
			return tenum(x.attributes.path,Lambda.amap(Lambda.array(x.nodes()),processType));
		case "f":
			var params = x.attributes.a.split(":");
			var it = x.nodes();
			var pl = Lambda.amap(Lambda.array(params.iterator()),function(name) {
				return {
					name : name,
					t : processType(it.next())
				};
			});
			return tfunction(pl,processType(it.next()));
		case "a":
			var fields = Lambda.amap(Lambda.array(x.nodes()),function(x : Node) {
				return { name : x.nodeName, t : processType(x.nodes().next()) };
			});
			return tanon(fields);
		case "d":
			var x = x.nodes().next();
			return tdynamic( if( x == null) null else processType(x) );
		default:
			throw ("Unknown type "+x.nodeName);
		}
	}

	static function processField( c : DocClass, x : Node ) {
		var stat = try Reflect.field(x.attributes,"static") == "1" catch( e : Dynamic ) false;
		var nl = x.nodes();
		var t = processType(nl.next());
		var f = new DocField(x.nodeName,stat,t);
		f.parent = c;
		var doc = nl.next();
		if( doc != null )
			f.doc = doc.firstChild.nodeValue;
		return f;
	}

	static function processClass(x : Node) {
		var path = x.attributes.path;
		if( try Reflect.field(x.attributes,"private") == "1" catch( e : Dynamic ) false )
			return;
		if( StringTools.endWith(path,"__") )
			return;
		if( findEntry(entries,path.split(".")) != null )
			return;
		var c = new DocClass(path,x.nodeName != "class");
		c.params = x.attributes.params.split(":");
		if( c.isEnum ) {
			for( m in x.nodes() ) {
				if( m.nodeName == "haxe:doc" ) {
					c.doc = m.firstChild.nodeValue;
					continue;
				}
				var l = Lambda.array(m.nodes());
				var t = if( m.attributes.a == null ) null else {
					var names = m.attributes.a.split(":");
					var params = Lambda.amap(names,function(name) {
						return {
							name : name,
							t : processType(l.pop())
						};
					});
					tconstr(params);
				}
				var f = new DocField(m.nodeName,false,t);
				f.parent = c;
				c.fields.push(f);
			}
		} else {
			for( m in x.nodes() ) {
				if( m.nodeName == "haxe:doc" ) {
					c.doc = m.firstChild.nodeValue;
					continue;
				}
				if( try Reflect.field(m.attributes,"public") == "1" catch( e : Dynamic ) false )
					c.fields.push(processField(c,m));
			}
		}
		c.fields.sort(function(f1 : DocField,f2 : DocField) {
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
		Lib.print("<ul>");
		for( e in p ) {
			switch e {
			case epackage(name,p):
				Lib.print("<li>"+name);
				display(p);
				Lib.print("</li>");
			case eclass(c):
				if( c.fields.length > 0 )
					Lib.print("<li>"+Url.make(c.path.split(".").join("/"),c.name)+"</li>");
			}
		}
		Lib.print("</ul>");
	}

	static function loadFile(file) {
		var data = neko.File.getContent(Web.getCwd()+file);
		var x = XmlParser.parse(data).firstChild;
		for( c in x.nodes() )
			processClass(c);
	}

	public static function main() {
		var h = Web.params();
		loadFile("flash.xml");
		loadFile("neko.xml");
		sortEntries(entries);
		Url.base = "/api/";
		var clname = h.get("class");
		if( clname == "index" )
			clname = null;
		if( clname == null )
			display(entries);
		else {
			clname = clname.split("/").join(".");
			var c = findEntry(entries,clname.split("."));
			if( c == null )
				throw ("Class not found : "+clname);
			Lib.print(Url.make("index","Index"));
			Lib.print(c.toString());
			Lib.print(Url.make("index","Index"));
		}
	}

}
