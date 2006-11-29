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
		neko.Lib.print("I'm haXLib");
	}

	static function main() {
		initDatabase();
		run();
		db.close();
		neko.db.Manager.cleanup();
		return;
	}

}
