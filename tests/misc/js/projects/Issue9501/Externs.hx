@:expose @:keep
class BaseExtern {
	public var field:Int;
	public function new(i:Int) {
		field = i;
	}
}

@:expose
class ExtendedExtern extends BaseExtern {}