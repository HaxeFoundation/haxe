package haxe.web;

class Request {

	/**
		Returns the current page GET and POST parameters (only GET parameters for Javascript)
	**/
	public static function getParams() : Hash<String> {
		#if neko
		return neko.Web.getParams();
		#elseif php
		return php.Web.getParams();
		#elseif js
		var get : String = untyped window.location.search.substr(1);
		var params = new Hash();
		for( p in ~/[&;]/g.split(get) ) {
			var pl = p.split("=");
			if( pl.length < 2 ) continue;
			var name = pl.shift();
			params.set(StringTools.urlDecode(name), StringTools.urlDecode(pl.join("=")));
		}
		return params;
		#end
	}

	/**
		Returns the local server host name
	**/
	public static function getHostName() : String {
		#if neko
		return neko.Web.getHostName();
		#elseif php
		return php.Web.getHostName();
		#elseif js
		return untyped window.location.host; // includes port
		#end
	}

	/**
		Returns the original request URL (before any server internal redirections)
	**/
	public static function getURI() : String {
		#if neko
		return neko.Web.getURI();
		#elseif php
		return php.Web.getURI();
		#elseif js
		return untyped window.location.pathname;
		#end
	}
	
}