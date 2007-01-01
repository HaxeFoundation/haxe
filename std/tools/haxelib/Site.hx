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

		var server = new neko.net.RemotingServer();
		var log = neko.io.File.append(TMP_DIR+"/log.txt",false);
		var api = new SiteApi(db);
		server.setPrivatePrefix("db");
		server.setLogger(log.write);
		server.addObject("api",api);
		var flag = server.handleRequest();
		log.close();
		if( flag )
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
		var ctx : Dynamic = Reflect.empty();
		var macros = {
			download : function( res, p, v ) {
				return "/"+Datas.REPOSITORY+"/"+Datas.fileName(res(p).name,res(v).name);
			}
		};
		fillContent(ctx);
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
				return;
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
				return;
			}
			ctx.u = u;
			ctx.uprojects = Developer.manager.search({ user : u.id }).map(function(d:Developer) { return d.project; });
		case "index":
			var vl = Version.manager.latest(10);
			for( v in vl ) {
				var p = v.project;
			}
			ctx.versions = vl;
		default:
			ctx.error = "Unknown action : "+act;
			return;
		}
		Reflect.setField(ctx,"act_"+act,true);
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
