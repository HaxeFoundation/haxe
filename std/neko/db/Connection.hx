package neko.db;

class Connection {

	private var __c : Void;

	private function new(c) {
		__c = c;
	}

	public function selectDB( db : String ) {
		sql_select_db(this.__c,untyped db.__s);
	}

	public function request( s : String ) : Result {
		var r = sql_request(this.__c,untyped s.__s);
		//Result.set_conv_date(r,function(d) { return new Date(d); });
		return untyped new Result(r);
	}

	public function close() {
		sql_close(__c);
	}

	public function escape( s : String ) {
		return new String(sql_escape(__c,untyped s.__s));
	}

	private static var __use_date = Date;
	private static var sql_select_db = neko.Lib.load("mysql","select_db",2);
	private static var sql_request = neko.Lib.load("mysql","request",2);
	private static var sql_close = neko.Lib.load("mysql","close",1);
	private static var sql_escape = neko.Lib.load("mysql","escape",2);

}