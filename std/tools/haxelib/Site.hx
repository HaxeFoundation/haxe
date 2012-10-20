package tools.haxelib;
import tools.haxelib.SiteDb;
import haxe.rtti.CType;

class Site {

	static var db : neko.db.Connection;

	static var CWD = neko.Web.getCwd();
	static var DB_FILE = CWD+"haxelib.db";
	public static var TMP_DIR = CWD+"tmp";
	public static var REP_DIR = CWD+Datas.REPOSITORY;

	static function setup() {
		SiteDb.create(db);
	}

	static function initDatabase() {
		db = neko.db.Sqlite.open(DB_FILE);
		neko.db.Manager.cnx = db;
		neko.db.Manager.initialize();
	}

	static function run() {
		if( !sys.FileSystem.exists(TMP_DIR) )
			sys.FileSystem.createDirectory(TMP_DIR);
		if( !sys.FileSystem.exists(REP_DIR) )
			sys.FileSystem.createDirectory(REP_DIR);

		var ctx = new haxe.remoting.Context();
		ctx.addObject("api",new SiteApi(db));
		if( haxe.remoting.HttpConnection.handleRequest(ctx) )
			return;
		if( Sys.args()[0] == "setup" ) {
			setup();
			neko.Lib.print("Setup done\n");
			return;
		}
		var file = null;
		var sid = null;
		var bytes = 0;
		neko.Web.parseMultipart(function(p,filename) {
			if( p == "file" ) {
				sid = Std.parseInt(filename);
				file = sys.io.File.write(TMP_DIR+"/"+sid+".tmp",true);
			} else
				throw p+" not accepted";
		},function(data,pos,len) {
			bytes += len;
			file.writeFullBytes(data,pos,len);
		});
		if( file != null ) {
			file.close();
			neko.Lib.print("File #"+sid+" accepted : "+bytes+" bytes written");
			return;
		}
		display();
	}

	static function display() {
		var data = sys.io.File.getContent(CWD + "website.mtt");
		var page = new haxe.Template(data);
		var ctx : Dynamic = {};
		var macros = {
			download : function( res, p, v ) {
				return "/"+Datas.REPOSITORY+"/"+Datas.fileName(res(p).name,res(v).name);
			}
		};
		if( fillContent(ctx) )
			neko.Lib.print( page.execute(ctx,macros) );
	}

	static function fillContent( ctx : Dynamic ) {
		var uri = neko.Web.getURI().split("/");
		var error = function(msg) { ctx.error = StringTools.htmlEscape(msg); return true; }
		if( uri[0] == "" )
			uri.shift();
		var act = uri.shift();
		if( act == null || act == "" || act == "index.n" )
			act = "index";
		ctx.menuTags = Tag.manager.topTags(10);
		switch( act ) {
		case "p":
			var name = uri.shift();
			var p = Project.manager.search({ name : name }).first();
			if( p == null )
				return error("Unknown project '"+name+"'");
			ctx.p = p;
			ctx.owner = p.owner;
			ctx.version = p.version;
			ctx.versions = Version.manager.byProject(p);
			var tags = Tag.manager.search({ project : p.id });
			if( !tags.isEmpty() ) ctx.tags = tags;
		case "u":
			var name = uri.shift();
			var u = User.manager.search({ name : name }).first();
			if( u == null )
				return error("Unknown user '"+name+"'");
			ctx.u = u;
			ctx.uprojects = Developer.manager.search({ user : u.id }).map(function(d:Developer) { return d.project; });
		case "t":
			var tag = uri.shift();
			ctx.tag = StringTools.htmlEscape(tag);
			ctx.tprojects = Tag.manager.search({ tag : tag }).map(function(t) return t.project);
		case "d":
			var name = uri.shift();
			var p = Project.manager.search({ name : name }).first();
			if( p == null )
				return error("Unknown project '"+name+"'");
			var version = uri.shift();
			var v;
			if( version == null ) {
				v = p.version;
				version = v.name;
			} else {
				v = Version.manager.search({ project : p.id, name : version }).first();
				if( v == null ) return error("Unknown version '"+version+"'");
			}
			if( v.documentation == null )
				return error("Project "+p.name+" version "+version+" has no documentation");
			var root : TypeRoot = haxe.Unserializer.run(v.documentation);
			var buf = new StringBuf();
			var html = new tools.haxedoc.HtmlPrinter("/d/"+p.name+"/"+version+"/","","");
			html.output = function(str) buf.add(str);
			var path = uri.join(".").toLowerCase().split(".");
			if( path.length == 1 && path[0] == "" )
				path = [];
			if( path.length == 0 ) {
				ctx.index = true;
				html.process(TPackage("root","root",root));
			} else {
				var cl = html.find(root,path,0);
				if( cl == null ) {
					// we most likely clicked on a class which is part of the haxe core documentation
					neko.Web.redirect("http://haxe.org/api/"+path.join("/"));
					return false;
				}
				html.process(cl);
			}
			ctx.p = p;
			ctx.v = v;
			ctx.content = buf.toString();
		case "index":
			var vl = Version.manager.latest(10);
			for( v in vl ) {
				var p = v.project; // fetch
			}
			ctx.versions = vl;
		case "all":
			ctx.projects = Project.manager.allByName();
		case "search":
			var v = neko.Web.getParams().get("v");
			var p = Project.manager.search({ name : v }).first();
			if( p != null ) {
				neko.Web.redirect("/p/"+p.name);
				return false;
			}
			if( Tag.manager.count({ tag : v }) > 0 ) {
				neko.Web.redirect("/t/"+v);
				return false;
			}
			ctx.projects = Project.manager.containing(v).map(function(p) return Project.manager.get(p.id));
			ctx.act_all = true;
			ctx.search = StringTools.htmlEscape(v);
		case "rss":
			neko.Web.setHeader("Content-Type", "text/xml; charset=UTF-8");
			neko.Lib.println('<?xml version="1.0" encoding="UTF-8"?>');
			neko.Lib.print(buildRss().toString());
			return false;
		default:
			ctx.error = "Unknown action : "+act;
			return true;
		}
		Reflect.setField(ctx,"act_"+act,true);
		return true;
	}

	static function buildRss() : Xml {
		var createChild = function(root:Xml, name:String){
			var c = Xml.createElement(name);
			root.addChild(c);
			return c;
		}
		var createChildWithContent = function(root:Xml, name:String, content:String){
			var e = Xml.createElement(name);
			var c = Xml.createPCData(if (content != null) content else "");
			e.addChild(c);
			root.addChild(e);
			return e;
		}
		var createChildWithCdata = function(root:Xml, name:String, content:String){
			var e = Xml.createElement(name);
			var c = Xml.createCData(if (content != null) content else "");
			e.addChild(c);
			root.addChild(e);
			return e;
		}
		Sys.setTimeLocale("en_US.UTF8");
		var url = "http://"+neko.Web.getClientHeader("Host");
		var rss = Xml.createElement("rss");
		rss.set("version","2.0");
		var channel = createChild(rss, "channel");
		createChildWithContent(channel, "title", "haxe-libs");
		createChildWithContent(channel, "link", url);
		createChildWithContent(channel, "description", "lib.haxe.org RSS");
		createChildWithContent(channel, "generator", "haxe");
		createChildWithContent(channel, "language", "en");
		for (v in Version.manager.latest(10)){
			var project = v.project;
			var item = createChild(channel, "item");
			createChildWithContent(item, "title", StringTools.htmlEscape(project.name+" "+v.name));
			createChildWithContent(item, "link", url+"/p/"+project.name);
			createChildWithContent(item, "guid", url+"/p/"+project.name+"?v="+v.id);
			var date = DateTools.format(Date.fromString(v.date), "%a, %e %b %Y %H:%M:%S %z");
			createChildWithContent(item, "pubDate", date);
			createChildWithContent(item, "author", project.owner.name);
			createChildWithContent(item, "description", StringTools.htmlEscape(v.comments));
		}
		return rss;
	}

	static function main() {
		var error = null;
		initDatabase();
		try {
			run();
		} catch( e : Dynamic ) {
			error = { e : e };
		}
		db.close();
		neko.db.Manager.cleanup();
		if( error != null )
			neko.Lib.rethrow(error.e);
	}

}
