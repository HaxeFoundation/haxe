package haxe.ds;

@:listRepr enum ListRepr<T> {
	Tl;
	Hd( v : T, tl : ListRepr<T> );
}

/**
	Immutable list
**/
abstract ImmutableList<T>(ListRepr<T>) from ListRepr<T> to ListRepr<T> {

	@:op(a :: b) static inline function prepend<T>( v : T, a : ImmutableList<T> ) : ImmutableList<T> {
		return Hd(v,a);
	}
	
	@:to function toArray() : Array<T> {
		var a = [];
		var t = this;
		while( true ) {
			switch( t ) {
			case Tl: break;
			case Hd(v,tl): t = tl; a.push(v);
			}
		}
		return a;
	}

	@:from static function fromArray<T>( a : Array<T> ) : ImmutableList<T> {
		var l = Tl;
		var i = a.length - 1;
		while( i >= 0 )
			l = Hd(a[i--],l);
		return l;
	}
		
	function toString() {
		var a = toArray();
		return Std.string(a);
	}
	
}
