typedef Rec1<T> = {
	function then<TResult>():Rec1<TResult>;
}

typedef Rec2<T> = {
	function then<TOut>():Rec2<TOut>;
}

class TypedefTypedef {
	static function main() {
		var r:Rec1<Void> = (null:Rec2<Void>);
	}
}