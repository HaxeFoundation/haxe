typedef Rec1<T> = {
	function then<TResult>():Rec1<TResult>;
}

abstract Rec2<T>(Rec2Struct<T>) from Rec2Struct<T> {}

typedef Rec2Struct<T> = {
	function then<TOut>():Rec2<TOut>;
}

class TypedefAbstract {
	static function main() {
		var r:Rec2<haxe.NoValue> = (null:Rec1<haxe.NoValue>);
	}
}