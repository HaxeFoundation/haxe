typedef Rec1<T> = {
	function then<TResult>():Rec1<TResult>;
}

typedef Rec2<T> = {
	function then<TOut>():Rec2<TOut>;
}

class TypedefTypedef {
	static function main() {
		var r:Rec1<Unit> = (null:Rec2<Unit>);
	}
}