package unit;
import haxe.remoting.SocketProtocol;
#if flash
import haxe.remoting.SocketWrapper;
#end

class TestRemoting extends Test {

	static var HOST = "dev.unit-tests";
	static var PORT = 1999;

	static var _ : Dynamic = init();
	static var ecnx : haxe.remoting.ExternalConnection;
	static var ecnx2 : haxe.remoting.ExternalConnection;
	// static var ecnx3 : haxe.remoting.ExternalConnection;
	static var lcnx : haxe.remoting.LocalConnection;
	// static var fjscnx : haxe.remoting.FlashJsConnection;

	static function staticMethod( a : Int, b : Int ) {
		return a + b;
	}

	static function init() {
		var ctx = RemotingApi.context();
		#if flash
		if( !flash.external.ExternalInterface.available ) return;
		ecnx = haxe.remoting.ExternalConnection.jsConnect("cnx",ctx);
		ecnx3 = haxe.remoting.ExternalConnection.jsConnect("unknown",ctx);
		lcnx = haxe.remoting.LocalConnection.connect("local",ctx,[HOST]);
		// fjscnx = haxe.remoting.FlashJsConnection.connect("cnx",#if flash9 "haxeFlash8" #else "haxeFlash9" #end,ctx);
		#elseif js
		// ecnx = haxe.remoting.ExternalConnection.flashConnect("cnx","haxeFlash8",ctx);
		ecnx2 = haxe.remoting.ExternalConnection.flashConnect("cnx","haxeFlash9",ctx);
		// ecnx3 = haxe.remoting.ExternalConnection.flashConnect("nothing","haxeFlash8",ctx);
		#end
	}

	public function test() {
		#if flash
		if( !flash.external.ExternalInterface.available ) return;
		#end

		// external connection
		#if (flash || js)
		doTestConnection(ecnx);
		#end
		#if !php // accessing the properties of a null object generates a fatal error in php
		exc(function() ecnx3.api.add.call([1,3]));
		#end
		#if js
		doTestConnection(ecnx2);
		#end

		#if flash
		// local connection
		doTestAsyncConnection(lcnx);
		// flash-flash through-js connection
		// doTestAsyncConnection(fjscnx);
		#end
		#if (js || neko || php)
		// http sync connection
		var hcnx = haxe.remoting.HttpConnection.urlConnect("http://"+HOST+"/remoting.n");
		doTestConnection(hcnx);
		// test wrappers
		var dcnx = haxe.remoting.AsyncDebugConnection.create(haxe.remoting.AsyncAdapter.create(hcnx));
		dcnx.setErrorDebug(function(path,args,e) {});
		dcnx.setResultDebug(function(path,args,ret) {});
		dcnx.setCallDebug(function(path,args) {});
		doTestAsyncConnection(dcnx);
		#end

		// http async connection
		var hcnx = haxe.remoting.HttpAsyncConnection.urlConnect("http://"+HOST+"/remoting.n");
		doTestAsyncConnection(hcnx);
		var dcnx = haxe.remoting.DelayedConnection.create();
		dcnx.connection = hcnx;
		doTestAsyncConnection(dcnx);

		// socket connection
		#if (flash || neko || php)
		async( doConnect, new Socket(), true );
		#elseif js
		async( doConnect, new Socket("haxeFlash9"), true );
		#end

		var actx = new haxe.remoting.ContextAll();
		actx.addObject("fake",{ TestRemoting : TestRemoting },true);
		eq( actx.call(["unit","TestRemoting","staticMethod"],[2,3]), 5 );
		exc( function() actx.call(["unit2","TestRemoting","staticMethod"],[2,3]) );
		exc( function() actx.call(["unit","TestRemoting2","staticMethod"],[2,3]) );
		exc( function() actx.call(["unit","TestRemoting","staticMethod2"],[2,3]) );
		eq( actx.call(["fake","TestRemoting","staticMethod"],[2,3]), 5 );
	}

	function doConnect( s : Socket, onResult : Bool -> Void ) {
		var me = this;
		#if flash
		var connected = false;
		s.addEventListener(flash.events.Event.CONNECT,function(e) {
			connected = true;
			me.doTestSocket(s);
			onResult(true);
		});
		s.addEventListener(flash.events.SecurityErrorEvent.SECURITY_ERROR,function(e) {
			onResult(false);
		});
		s.addEventListener(flash.events.Event.CLOSE,function(e) {
			if( !connected )
				onResult(false);
		});
		s.connect(HOST,PORT);
		#elseif js
		s.onConnect = function(success) {
			if( success ) me.doTestSocket(s);
			onResult(success);
		};
		s.connect(HOST,PORT);
		#elseif neko
		var ret = try { s.connect(new neko.net.Host(HOST),PORT); true; } catch( e : Dynamic ) false;
		if( ret ) doTestSocket(s);
		onResult(ret);
		#elseif php
		var ret = try { s.connect(new php.net.Host(HOST),PORT); true; } catch( e : Dynamic ) false;
		if( ret ) doTestSocket(s);
		onResult(ret);
		#end
	}

	function doTestSocket( s : Socket ) {
		#if (neko || php)
		var scnx = haxe.remoting.SyncSocketConnection.create(s,new haxe.remoting.Context());
		doTestConnection(scnx);
		#else
		var scnx = haxe.remoting.SocketConnection.create(s,new haxe.remoting.Context());
		doTestAsyncConnection(scnx);
		#end
	}

	function doTestConnection( cnx : haxe.remoting.Connection ) {
		eq( cnx.api.add.call([1,2]), 3 );
		var strings = ["bla","\n","\r","\n\r","\t","    "," ","&","<",">","&nbsp;","&gt;","<br/>"];
		for( s in strings ) {
			infos("using "+s);
			eq( cnx.api.id.call([s]), s );
			eq( cnx.api.arr.call([[s,s,s]]), [s,s,s].join("#") );
		}
		infos(null);
		eq( cnx.api.exc.call([null]), null );
		exc( function() cnx.api.exc.call([5]) );

		exc( function() cnx.api.call([]) );
		exc( function() cnx.call([]) );

		exc( function() cnx.api.unknown.call([]) );
		exc( function() cnx.api.sub.add.call([1,2]) );
		eq( cnx.apirec.sub.add.call([1,2]), 3 );
	}

	function doTestAsyncConnection( cnx : haxe.remoting.AsyncConnection ) {
		var asyncExc = callback(asyncExc,cnx.setErrorHandler);

		async( cnx.api.add.call, [1,2], 3 );
		var strings = ["bla","\n","\r","\n\r","\t","    "," ","&","<",">","&nbsp;","&gt;","<br/>"];
		for( s in strings ) {
			async( cnx.api.id.call, [s], s );
			async( cnx.api.arr.call, [[s,s,s]], [s,s,s].join("#") );
		}
		async( cnx.api.exc.call, [null], null );
		asyncExc( cnx.api.exc.call, [5] );

		asyncExc( cnx.api.call, [] );
		asyncExc( cnx.call, [] );

		asyncExc( cnx.api.unknown.call, [] );
		asyncExc( cnx.api.sub.add.call, [1,2] );
		async( cnx.apirec.sub.add.call, [1,2], 3 );
	}

}