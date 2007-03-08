package haxe.remoting;

class FlashJsConnection extends haxe.remoting.AsyncConnection {

	#if flash

	static var pendingCalls = new Array();

	override function __resolve( field : String ) : AsyncConnection {
		var c = new FlashJsConnection(__data,__path.copy());
		c.__error = __error;
		c.__path.push(field);
		return c;
	}

	override public function call( params, ?onData ) {
		var p = __path.copy();
		var f = p.pop();
		var path = p.join(".");
		var s = new haxe.Serializer();
		s.serialize(params);
		var cnx : { private function escapeString(s : String) : String; } = haxe.remoting.Connection;
		var params = cnx.escapeString(s.toString());
		var me = this;
		pendingCalls.push(function() {
			var s = flash.external.ExternalInterface.call("haxe.remoting.FlashJsConnection.flashCall",me.__data,path,f,params);
			var v = null;
			try {
				if( s == null )
					throw "Failed to call JS method "+path;
				v = { r : new haxe.Unserializer(s).unserialize() };
			} catch( e : Dynamic ) {
				me.onError(e);
			}
			if( v != null )
				onData(v.r);
		});
		haxe.Timer.delayed(function() {
			pendingCalls.shift()();
		},0)();
	}

	public static function flashConnect( objId : String ) : AsyncConnection {
		if( !flash.external.ExternalInterface.available )
			throw "External Interface not available";
		if( flash.external.ExternalInterface.call("haxe.remoting.FlashJsConnection"+".jsRemoting") != "yes" )
			throw "haxe.remoting.FlashJsConnection"+" is not available in JavaScript";
		return new FlashJsConnection(objId,[]);
	}

	#else js

	static function jsRemoting() {
		return "yes";
	}

	static function flashCall( flashObj : String, path : String, f : String, params : String ) : String {
		try {
			var cnx : { private var __data : Dynamic; } = haxe.remoting.Connection.flashConnect(flashObj);
			return cnx.__data.remotingCall(path,f,params);
		} catch( e : Dynamic ) {
			var s = new haxe.Serializer();
			s.serializeException(e);
			return s.toString();
		}
	}

	#end

}
