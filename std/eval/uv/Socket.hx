package eval.uv;

import haxe.NoData;
import haxe.async.Callback;
import haxe.io.Bytes;
import asys.net.*;

extern class Socket {
	function new();
	function connectTcp(address:Address, port:Int, cb:Callback<NoData>):Void;
	function bindTcp(host:Address, port:Int, ipv6only:Bool):Void;
	function accept():Socket;
	function close(cb:Callback<NoData>):Void;
	function setKeepAlive(enable:Bool, initialDelay:Int):Void;
	function setNoDelay(noDelay:Bool):Void;
	function getSockName():SocketAddress;
	function getPeerName():SocketAddress;
	function asStream():Stream;
}
