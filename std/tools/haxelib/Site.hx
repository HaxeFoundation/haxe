package tools.haxelib;
import tools.haxelib.SiteDb;

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
		if( !neko.FileSystem.exists(TMP_DIR) )
			neko.FileSystem.createDirectory(TMP_DIR);
		if( !neko.FileSystem.exists(REP_DIR) )
			neko.FileSystem.createDirectory(REP_DIR);

		var ctx = new haxe.remoting.Context();
		ctx.addObject("api",new SiteApi(db));
		if( haxe.remoting.HttpConnection.handleRequest(ctx) )
			return;
		if( neko.Sys.args()[0] == "setup" ) {
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
				file = neko.io.File.write(TMP_DIR+"/"+sid+".tmp",true);
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
		var data = neko.io.File.getContent(CWD + "website.mtt");
		var page = new haxe.Template(data);
		var ctx : Dynamic = {};
		var macros = {
			download : function( res, p, v ) {
				return "/"+Datas.REPOSITORY+"/"+Datas.fileName(res(p).name,res(v).name);
			}
		};
		if (fillContent(ctx))
			neko.Lib.print( page.execute(ctx,macros) );
	}

	static function fillContent( ctx : Dynamic ) {
		var uri = neko.Web.getURI().split("/");
		if( uri[0] == "" )
			uri.shift();
		var act = uri.shift();
		if( act == null || act == "" )
			act = "index";
		ctx.projects = Project.manager.allByName();
		switch( act ) {
		case "p":
			var name = uri.shift();
			var p = Project.manager.search({ name : name }).first();
			if( p == null ) {
				ctx.error = "Unknown project '"+name+"'";
				return true;
			}
			ctx.p = p;
			ctx.owner = p.owner;
			ctx.version = p.version;
			ctx.versions = Version.manager.byProject(p);
		case "u":
			var name = uri.shift();
			var u = User.manager.search({ name : name }).first();
			if( u == null ) {
				ctx.error = "Unknown user '"+name+"'";
				return true;
			}
			ctx.u = u;
			ctx.uprojects = Developer.manager.search({ user : u.id }).map(function(d:Developer) { return d.project; });
		case "index":
			var vl = Version.manager.latest(10);
			for( v in vl ) {
				var p = v.project;
			}
			ctx.versions = vl;
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
		neko.Sys.setTimeLocale("en_US.UTF8");
		var url = "http://"+neko.Web.getClientHeader("Host")+"/";
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
