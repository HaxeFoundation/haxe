package rust.os;

@:native("os.Pipe") extern class Pipe {
	public var pin:Int;
	public var pout:Int;
}