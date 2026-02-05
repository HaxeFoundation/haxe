package cs.system.net;

@:native("System.Net.HttpListener.ExtendedProtectionSelector")
extern class HttpListener_ExtendedProtectionSelector extends cs.system.MulticastDelegate {
	function new(func:(request:cs.system.net.HttpListenerRequest)->cs.system.security.authentication.extendedprotection.ExtendedProtectionPolicy):Void;
	function Invoke(request:cs.system.net.HttpListenerRequest):cs.system.security.authentication.extendedprotection.ExtendedProtectionPolicy;
}
