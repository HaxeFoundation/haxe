package tools.haxlib;
import tools.haxlib.SiteDb;

class Site {

	static var db : neko.db.Connection;

	static var CWD = neko.Web.getCwd();
	static var DB_FILE = CWD+"haxlib.db";
	public static var TMP_DIR = CWD+"tmp/";
	public static var REP_DIR = CWD+"files/";

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

		var server = new haxe.remoting.Server();
		var log = neko.io.File.write(TMP_DIR+"log.txt",false);
		var api =
		server.setPrivatePrefix("db");
		server.setLogger(log.write);
		server.addObject("api",new SiteApi(db));
		var flag = server.handleRequest();
		log.close();
		if( flag )
			return;
		if( neko.Sys.args()[0] == "setup" ) {
			setup();
			neko.Lib.print("Setup done\n");
			return;
		}
		var sid = Std.parseInt(neko.Web.getParams().get("submit"));
		if( sid != null ) {
			var file = neko.io.File.write(TMP_DIR+sid,true);
			var data = neko.Web.getPostData();
			file.write(data);
			file.close();
			neko.Lib.print("File #"+sid+" accepted : "+data.length+" bytes written");
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
