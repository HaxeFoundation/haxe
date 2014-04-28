package python.internal;

@:keep
@:native("Enum")
class EnumImpl {
	public var tag:String;
	public var index:Int;
	public var params:Array<Dynamic>;

	public function new(tag, index, params) {
		this.tag = tag;
		this.index = index;
		this.params = params;
	}

	function __str__() {
		return if (params == null) {
			tag;
		} else {
			tag + "(" + params.join(",") + ")";
		}
	}
}