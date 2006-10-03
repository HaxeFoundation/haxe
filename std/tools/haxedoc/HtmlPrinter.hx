package tools.haxedoc;
import tools.haxedoc.Type;

class HtmlPrinter {

	static function loadTemplate() {
		var hdata = try
			// load in current local/web directory
			neko.io.File.getContent(neko.Web.getCwd()+"template.xml")
		catch( e : Dynamic ) try {
			// load in haxe subdirectory (TODO : make it work on linux/osx)
			var p = ~/[\/\\]/g.split(neko.Sys.executablePath());
			p.pop();
			neko.io.File.getContent(p.join("/")+"/std/tools/template.xml");
		} catch( e : Dynamic )
			default_template;
		return Xml.parse(hdata);
	}

	static var default_template = "<html><body><data/></body></html>";
	static var template = loadTemplate();
	
	public var baseUrl : String;
	var indexUrl : String;
	var fileExtension : String;
	var curpackage : String;
	var filters : List<String>;
	var typeParams : TypeParams;
		
	public function new( baseUrl, fileExtension, indexUrl ) {
		this.baseUrl = baseUrl;
		this.fileExtension = fileExtension;
		this.indexUrl = indexUrl;
		filters = new List();
		typeParams = new Array();
	}
	
	public function output(str) {
		neko.Lib.print(str);
	}
	
	public function addFilter(f) {
		filters.add(f);
	}

	public function print(str, ?params : Dynamic ) {
		if( params != null )
			for( f in Reflect.fields(params) )
				str = StringTools.replace(str, "$"+f, Std.string(Reflect.field(params, f)));
		output(str);
	}
	
	public function process(t) {
		processHtml(t,template);
	}
	
	public function filtered( path : Path, isPackage : Bool ) {
		if( isPackage && path == "Remoting" )
			return true;
		if( StringTools.endsWith(path,"__") )
			return true;
		if( filters.isEmpty() )
			return false;
		for( x in filters )
			if( StringTools.startsWith(path,x) )
				return false;
		return true;
	}
	
	function makeUrl( url, text, css ) {
		return "<a href=\"" + baseUrl + url + fileExtension + "\" class=\""+css+"\">"+text+"</a>";
	}
	
	function makePathUrl( path : Path, css ) {
		var p = path.split(".");
		var name = p.pop();
		var local = (p.join(".") == curpackage);
		if( local )
			for( x in typeParams )
				if( x == name )
					return name;
		p.push(name);
		if( local )
			return makeUrl(p.join("/"),name,css);
		return makeUrl(p.join("/"),f9path(path),css);
	}
	
	function f9path(path : String) {
		if( path.substr(0,7) == "flash9." )
			return "flash."+path.substr(7);
		return path;
	}
	
	public function processHtml(t,html : Xml) {
		switch( html.nodeType ) {
		case Xml.Element:
			if( html.nodeName == "data" ) {
				processPage(t);
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
			for( x in html )
				processHtml(t,x);
			print("</"+html.nodeName+">");
		case Xml.Document:
			for( x in html )
				processHtml(t,x);
		default:
			print(html.toString());
		}
	}
	
	public function processPage(t) {
		switch(t) {
		case TPackage(p,full,list):
			processPackage(p,list);
		default:
			var head = '<a href="#" onclick="javascript:history.back(-1); return false" class="index">Back</a> | '+makeUrl(indexUrl,"Index","index");
			print(head);
			var inf = TypeApi.typeInfos(t);
			typeParams = inf.params;
			var p = inf.path.split(".");
			p.pop();
			curpackage = p.join(".");
			switch(t) {
			case TClassdecl(c): processClass(c);
			case TEnumdecl(e): processEnum(e);
			case TTypedecl(t): processTypedef(t);
			case TPackage(_,_,_): throw "ASSERT";
			}			
			print(head);
		}
	}
	
	function processPackage(name,list : Array<TypeTree> ) {
		print('<ul class="entry">');
		for( e in list ) {
			switch e {
			case TPackage(name,full,list):
				if( filtered(full,true) )					
					continue;
				print('<li><a href="#" class="package" onclick="toggle(\'$id\')">$name</a><div id="$id" class="package_content">', { id : full.split(".").join("_"), name : name });
				var old = curpackage;
				curpackage = full;
				processPackage(name,list);
				curpackage = old;
				print("</div></li>");
			default:
				var i = TypeApi.typeInfos(e);
				if( i.isPrivate || i.path == "@Main" || filtered(i.path,false) )
					continue;
				print("<li>"+makePathUrl(i.path,"entry")+"</li>");
			}
		}
		print("</ul>");
	}
	
	function processInfos(t : TypeInfos) {
		if( t.module != null )
			print('<div class="importmod">import $module</div>',{ module : t.module });
		if( !t.platforms.isEmpty() ) {
			print('<div class="platforms">Available in ');
			display(t.platforms,output,", ");
			print('</div>');
		}		
		if( t.doc != null ) {
			print('<div class="classdoc">');
			processDoc(t.doc);
			print('</div>');
		}
	}

	function processClass(c : Class) {		
		print('<div class="classname">');
		if( c.isExtern )
			keyword("extern");
		if( c.isPrivate )
			keyword("private");
		if( c.isInterface )
			keyword("interface");
		else
			keyword("class");
		print(f9path(c.path));
		if( c.params.length != 0 ) {
			print("&lt;");
			print(c.params.join(", "));
			print("&gt;");
		}
		print('</div>');
		if( c.superClass != null ) {
			print('<div class="extends">extends ');
			processPath(c.superClass.path,c.superClass.params);
			print('</div>');
		}
		for( i in c.interfaces ) {
			print('<div class="implements">implements ');
			processPath(i.path,i.params);
			print('</div>');
		}
		if( c.dynamic != null ) {
			var d = new List();
			d.add(c.dynamic);
			print('<div class="implements">implements ');
			processPath("Dynamic",d);
			print('</div>');
		}
		processInfos(c);
		print('<dl>');
		for( f in c.fields )
			processClassField(c.platforms,f,false);
		for( f in c.statics )
			processClassField(c.platforms,f,true);
		print('</dl>');
	}
	
	function processClassField(platforms : Platforms,f : ClassField,stat) {
		if( !f.isPublic )
			return;
		print('<dt>');
		if( stat ) keyword("static");
		var isMethod = false;
		switch( f.type ) {
		case TFunction(args,ret):
			if( f.get == RNormal && (f.set == RNormal || f.set == RF9Dynamic) ) {
				isMethod = true;
				if( f.set == RF9Dynamic )
					keyword("f9dynamic");
				keyword("function");
				print(f.name);				
				if( f.params != null )
					print("&lt;"+f.params.join(", ")+"&gt;");				
				print("(");
				var me = this;
				display(args,function(a) {
					if( a.opt )
						me.print("?");
					me.print(a.name);
					me.print(" : ");
					me.processType(a.t);					
				},", ");
				print(") : ");
				processType(ret);
			}
		default:
		}
		if( !isMethod ) {
			keyword("var");
			print(f.name);
			if( f.get != RNormal || f.set != RNormal )
				print("("+rightsStr(f.get)+","+rightsStr(f.set)+")");
			print(" : ");
			processType(f.type);			
		}
		if( f.platforms.length != platforms.length ) {
			print('<div class="platforms">Available in ');
			display(f.platforms,output,", ");
			print('</div>');
		}
		print('</dt>');
		print('<dd>');
		processDoc(f.doc);
		print('</dd>');
	}
	
	function processEnum(e : Enum) {
		print('<div class="classname">');
		if( e.isExtern )
			keyword("extern");
		if( e.isPrivate )
			keyword("private");
		keyword("enum");
		print(f9path(e.path));		
		if( e.params.length != 0 ) {
			print("&lt;");
			print(e.params.join(", "));
			print("&gt;");
		}
		print('</div>');
		processInfos(e);
		print('<dl>');
		for( c in e.constructors ) {
			print('<dt>');
			print(c.name);
			if( c.args != null ) {
				print("(");
				var me = this;
				display(c.args,function(a) {
					if( a.opt )
						me.print("?");
					me.print(a.name);
					me.print(" : ");
					me.processType(a.t);					
				},",");
				print(")");
			}
			print("</dt>");
			print("<dd>");
			processDoc(c.doc);			
			print("</dd>");
		}
		print('</dl>');
	}
	
	function processTypedef(t : Typedef) {
		print('<div class="classname">');
		if( t.isPrivate )
			keyword("private");
		keyword("typedef");
		print(f9path(t.path));
		if( t.params.length != 0 ) {
			print("&lt;");
			print(t.params.join(", "));
			print("&gt;");
		}
		print('</div>');
		processInfos(t);
		switch( t.type ) {
		case TAnonymous(fields):
			print('<dl>');
			for( f in fields ) {
				processClassField(t.platforms,{
					name : f.name,
					type : f.t,
					isPublic : true,
					doc : null,
					get : RNormal,
					set : RNormal,
					params : null,
					platforms : t.platforms,
				},false);
			}
			print('</dl>');
		default:
			print('<div class="typedef">= ');
			processType(t.type);
			print('</div>');
		}
	}
	
	function processPath( path : Path, ?params : List<Type> ) {
		print(makePathUrl(path,"type"));
		if( params != null && !params.isEmpty() ) {
			print("&lt;");
			for( t in params )
				processType(t);
			print("&gt;");
		}
	}
	
	function processType( t : Type ) {
		switch( t ) {
		case TUnknown:
			print("Unknown");
		case TEnum(path,params):
			processPath(path,params);
		case TClass(path,params):
			processPath(path,params);
		case TTypedef(path,params):
			processPath(path,params);
		case TFunction(args,ret):
			if( args.isEmpty() ) {
				processPath("Void");
				print(" -> ");
			}
			for( a in args ) {
				if( a.opt )
					print("?");
				print(a.name+" : ");
				processTypeFun(a.t,true);
				print(" -> ");
			}
			processTypeFun(ret,false);
		case TAnonymous(fields):
			print("{ ");
			var me = this;
			display(fields,function(f) {
				me.print(f.name+" : ");
				me.processType(f.t);				
			},", ");
			print("}");
		case TDynamic(t):
			if( t == null )
				processPath("Dynamic");
			else {
				var l = new List();
				l.add(t);
				processPath("Dynamic",l);
			}
		}
	}
	
	function processTypeFun( t : Type, isArg ) {
		var parent =  switch( t ) { case TFunction(_,_): true; case TEnum(n,_): isArg && n == "Void"; default : false; };
		if( parent )
			print("(");
		processType(t);
		if( parent )
			print(")");		
	}
	
	function rightsStr(r) {
		return switch(r) {
		case RNormal: "default";
		case RNo: "null";
		case RMethod(m): m;
		case RDynamic: "dynamic";
		case RF9Dynamic: "f9dynamic";
		}
	}
	
	function keyword(w) {
		print('<span class="kwd">'+w+' </span>');
	}
	
	function processDoc(doc) {
		if( doc == null )
			return;
		doc = ~/\[([^\]]+)\]/g.replace(doc,"<code>$1</code>");
		print(doc);
	}
	
	function display<T>( l : List<T>, f : T -> Void, sep : String ) {
		var first = true;
		for( x in l ) {
			if( first )
				first = false;
			else
				print(sep);
			f(x);
		}
	}

}
