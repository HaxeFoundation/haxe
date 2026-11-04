extern class Api2 {
	overload static function take(f:String->Void):Void;
	overload static function take(f:Int->Void):Void;
}

function main() {
	// No overload's signature matches; the overload failure is reported (body is clean).
	Api2.take((x:Bool) -> {});
}
