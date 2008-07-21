package php;
/**
* TODO: TEST IT!
*/
class Session {	
	public static function getCacheLimiter() {
		switch(untyped __call__("session_cache_limiter")) {
			case "public":
				return Public;
			case "private":
				return Private;
			case "nocache":
				return NoCache;
			case "private_no_expire":
				return PrivateNoExpire;
		}
		return null;
	}	
	
	public static function setCacheLimiter(l : CacheLimiter) {
		if(_started) throw "You can't set the cache limiter while the session is already in use";
		switch(l) {
			case Public:
				untyped __call__("session_cache_limiter", "public");
			case Private:
				untyped __call__("session_cache_limiter", "private");
			case NoCache:
				untyped __call__("session_cache_limiter", "nocache");
			case PrivateNoExpire:
				untyped __call__("session_cache_limiter", "private_no_expire");
		}
	}
	
	public static function getCacheExpire() : Int {
		return untyped __call__("session_cache_expire");
	}
	
	public static function setCacheExpire(minutes : Int) {
		if(_started) throw "You can't set the cache expire time while the session is already in use";
		untyped __call__("session_cache_expire", minutes);
	}
	
	public static function setName(name : String) {
		if(_started) throw "You can't set the name while the session is already in use";
		untyped __call__("session_name", name);
	}
	
	public static function getName() : String {
		return untyped __call__("session_name");
	}
	
	public static function getId() : String {
		return untyped __call__("session_id");
	}
	
	public static function setId(id : String) {
		if(_started) throw "You can't set the session id while the session is already in use";
		untyped __call__("session_id", id);
	}

	public static function getSavePath() : String {
		return untyped __call__("session_save_path");
	}
	
	public static function setSavePath(path : String) {
		if(_started) throw "You can't set the save path while the session is already in use";
		untyped __call__("session_save_path", path);
	}
	
	public static function getModule() : String {
		return untyped __call__("session_module_name");
	}
	
	public static function setModule(module : String) {
		if(_started) throw "You can't set the module while the session is already in use";
		untyped __call__("session_module_name", module);
	}
	
	public static function regenerateId(?deleteold : Bool) : Bool {
		return untyped __call__("session_regenerate_id", deleteold);
	}
	
	public static function get(name : String) : Dynamic {
		start();
		return untyped __var__("_SESSION", name);
	}
	
	public static function set(name : String, value : Dynamic) {
		start();
		return untyped __set__("_SESSION", name, value);
	}
	
	public static function setCookieParams(?lifetime : Int, ?path : String, ?domain : String, ?secure : Bool, ?httponly : Bool) {
		if(_started) throw "You can't set the cookie params while the session is already in use";
		untyped __call__("session_get_cookie_params", lifetime, path, domain, secure, httponly);
	}
	
	public static function getCookieParams() : { lifetime : Int, path : String, domain : String, secure : Bool, httponly : Bool} {
		return php.Boot.__anonymous(untyped __call__("session_get_cookie_params"));
	}
	
	// TODO: completely untested
	public static function setSaveHandler(open : String -> String -> Bool, close : Void -> Bool, read : String -> String, write : String -> String -> Bool, destroy, gc) : Bool {
		return untyped __call__("session_set_save_handler", open, close, read, write, destroy, gc);
	}
	
	public static function exists(name : String) {
		start();
		return untyped __call__("array_key_exists", name, __var__("_SESSION"));
	}
	
	public static function remove(name : String) {
		start();
		untyped __call__("unset", __var__("_SESSION", name));
	}
	
	private static var _started = false;
	private static function start() {
		if(_started) return;
		_started = true;
		untyped __call__("session_start");
	}
	
	public function clear() {
		untyped __call__("session_unset");
	}
	
	public function close() {
		untyped __call__("session_write_close");
		_started = false; // TODO: not sure this useful; test if a closed session can be re-opened (I doubt)
	}
}

enum CacheLimiter {
	Public;
	Private;
	NoCache;
	PrivateNoExpire;
}