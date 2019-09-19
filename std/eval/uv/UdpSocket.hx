package eval.uv;

import haxe.NoData;
import haxe.async.Callback;
import haxe.io.Bytes;
import asys.net.*;

extern class UdpSocket {
	function new();
	function addMembership(multicastAddress:String, multicastInterface:String):Void;
	function dropMembership(multicastAddress:String, multicastInterface:String):Void;
	function send(msg:Bytes, offset:Int, length:Int, address:Address, port:Int, callback:Callback<NoData>):Void;
	function close(callback:Callback<NoData>):Void;
	function bindTcp(address:Address, port:Int, ipv6only:Bool):Void;
	function startRead(callback:Callback<{data:Bytes, remoteAddress:Address, remotePort:Int}>):Void;
	function stopRead():Void;
	function getSockName():SocketAddress;
	function setBroadcast(flag:Bool):Void;
	function setMulticastInterface(intfc:String):Void;
	function setMulticastLoopback(flag:Bool):Void;
	function setMulticastTTL(ttl:Int):Void;
	function setTTL(ttl:Int):Void;
	function getRecvBufferSize():Int;
	function getSendBufferSize():Int;
	function setRecvBufferSize(size:Int):Int;
	function setSendBufferSize(size:Int):Int;
	function asStream():Stream;
}
