package mtwin.web;
import neko.Web;

class Request {

	var pathInfoParts  : Array<String>;
	var params         : Hash<String>;

	public function new() {
		pathInfoParts = Web.getURI().split( "/" );
		pathInfoParts.shift();
		params = Web.getParams();
	}

	public function getPathInfoPart ( level ) : String {
		if( pathInfoParts.length > level )
			return pathInfoParts[ level ];
		return "";
	}

	public function setParams( list : Hash<String> ) {
		params = new Hash();
		for( k in list.keys() ){
			params.set( k, list.get(k) );
		}
	}

	public function getParamsObject( ?keys : List<String> ) : Dynamic<String> {
		var ret = cast Reflect.empty();
		if( keys == null ){
			for( k in params.keys() ){
				Reflect.setField( ret, k, params.get(k) );
			}
		}else{
			for( k in keys ){
				Reflect.setField( ret, k, params.get(k) );
			}
		}
		return ret;
	}

	public function get( key : String , ?or : String ) : String {
		if( params.exists( key ) ) return params.get( key );
		return or;
	}

	public function getInt( key : String, ?or : Int ) : Int {
		if( params.exists(key) ){
			var v = params.get(key);
			if( v == "NULL" )
				return null;
			return Std.parseInt(v);
		}
		return or;
	}

	public function getBool( key:String ) : Bool {
		var val = params.get(key);
		return (val != null) && (val == "1" || val == "true");
	}

	public function getURI() : String {
		return Web.getURI();
	}

	public function getReferer() : String {
		return Web.getClientHeader("Referer");
	}

	public function exists( key ) {
		return params.exists( key );
	}

	public function toString() : String {
		var lst = new List();
		for (i in params.keys()){
			lst.add("['"+i+"'] => '"+params.get(i)+"'");
		}
		return lst.join(",\n");
	}
}
