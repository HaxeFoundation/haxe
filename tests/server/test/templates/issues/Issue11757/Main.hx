abstract State({}) from {} {
	public function new() {
		this = {};
	}

	@:op(a.b) function g{-1-}et(key:String):Any {
		return key;
	}

	@:op(a.b) function s{-2-}et(key:String, value:Any):Any {
		return "set" + key;
	}
}

function main() {}