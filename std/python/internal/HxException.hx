package python.internal;

@:keep
@:native("_HxException")
class HxException extends python.lib.Exceptions.Exception {
	public var val:Dynamic;
	public function new(val) {
		var message = Std.string(val);
		super(message);
		this.val = val;
	}
}