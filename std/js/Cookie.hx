package js;

class Cookie {
	/**
		Create or update a cookie.
		expireDelay (seconds), if null, the cookie expires at end of session
	**/
	public static function set( name : String, value : String, ?expireDelay : Int, ?path : String, ?domain : String ){
		var s = name+"="+StringTools.urlEncode(value);
		if( expireDelay != null ){
			var d = DateTools.delta(Date.now(),expireDelay*1000);
			s += ";expires=" + untyped d.toGMTString();
		}
		if( path != null ){
			s += ";path="+path;
		}
		if( domain != null ){
			s += ";domain="+domain;
		}
		js.Lib.document.cookie = s;
	}

	/**
		Returns all cookies
	**/
	public static function all(){
		var h = new Hash();
		var a = js.Lib.document.cookie.split(";");
		for( e in a ){
			e = StringTools.ltrim(e);
			var t = e.split("=");
			h.set(t[0],StringTools.urlDecode(t[1]));
		}
		return h;
	}

	/**
		Returns value of a cookie.
	**/
	public static function get( name : String ){
		return all().get(name);
	}
	
	/**
		Returns true if a cookie [name] exists
	**/
	public static function exists( name : String ){
		return all().exists(name);
	}

	/**
		Remove a cookie
	**/
	public static function remove( name : String, ?path : String, ?domain : String ){
		set(name,"",-10,path,domain);
	}
}
