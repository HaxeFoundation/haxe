/*
 * Copyright (C)2005-2017 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
package php;

/**
	Session consists of a way to preserve certain data across 
	subsequent accesses.
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
		if(started) throw "You can't set the cache limiter while the session is already in use";
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
		if(started) throw "You can't set the cache expire time while the session is already in use";
		untyped __call__("session_cache_expire", minutes);
	}

	public static function setName(name : String) {
		if(started) throw "You can't set the name while the session is already in use";
		untyped __call__("session_name", name);
	}

	public static function getName() : String {
		return untyped __call__("session_name");
	}

	public static function getId() : String {
		return untyped __call__("session_id");
	}

	public static function setId(id : String) {
		if(started) throw "You can't set the session id while the session is already in use";
		untyped __call__("session_id", id);
	}

	public static function getSavePath() : String {
		return untyped __call__("session_save_path");
	}

	public static function setSavePath(path : String) {
		if(started) throw "You can't set the save path while the session is already in use";
		untyped __call__("session_save_path", path);
	}

	public static function getModule() : String {
		return untyped __call__("session_module_name");
	}

	public static function setModule(module : String) {
		if(started) throw "You can't set the module while the session is already in use";
		untyped __call__("session_module_name", module);
	}

	public static function regenerateId(?deleteold : Bool) : Bool {
		return untyped __call__("session_regenerate_id", deleteold);
	}

	public static function get(name : String) : Dynamic {
		start();
		if(!untyped __call__('isset', __var__("_SESSION", name))) return null;
		return untyped __var__("_SESSION", name);
	}

	public static function set(name : String, value : Dynamic) {
		start();
		return untyped __set__("_SESSION", name, value);
	}

	public static function setCookieParams(?lifetime : Int, ?path : String, ?domain : String, ?secure : Bool, ?httponly : Bool) {
		if(started) throw "You can't set the cookie params while the session is already in use";
		untyped __call__("session_set_cookie_params", lifetime, path, domain, secure, httponly);
	}

	public static function getCookieParams() : { lifetime : Int, path : String, domain : String, secure : Bool, httponly : Bool} {
		return untyped __call__("_hx_anonymous", untyped __call__("session_get_cookie_params"));
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

	public static var started(default, null) : Bool;
	public static function start() {
		if(started) return;
		started = true;
		untyped __call__("session_start");
	}

	public static function clear() {
		untyped __call__("session_unset");
	}

	public static function close() {
		untyped __call__("session_write_close");
		started = false; // TODO: not sure this useful; test if a closed session can be re-opened (I doubt)
	}

	static function __init__()
	{
		started = untyped __call__("isset", __var__("_SESSION"));
	}
}

enum CacheLimiter {
	Public;
	Private;
	NoCache;
	PrivateNoExpire;
}
