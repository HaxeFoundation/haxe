package eval.vm;

extern class Tls<T> {
	public var value(get, set):T;
	function new():Void;
}