package neko.db;

class Mysql {

	public static function connect( params : {
		host : String,
		port : Int,
		user : String,
		pass : String,
		socket : String
	} ) : neko.db.Connection {
		var o = untyped {
			host : params.host.__s,
			port : params.port,
			user : params.user.__s,
			pass : params.pass.__s,
			socket : if( params.socket == null ) null else params.socket.__s
		};
		return untyped new Connection(sql_connect(o));
	}

	static var sql_connect = neko.Lib.load("mysql","connect",1);

}