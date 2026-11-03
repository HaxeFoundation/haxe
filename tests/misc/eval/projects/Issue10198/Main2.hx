class Main2 {
	static function main() {
		var foo:Vector<Foo> = null;
		var bar:Vector<Bar> = Vector.fromIterable(foo);
	}
}

typedef Foo = {
	final id:String;
	final ?project:String;
}

typedef Bar = {
	final id:String;
	final createDate:Date;
}

abstract Vector<T>(Array<T>) {

	inline function new(a)
	  this = a;

	@:from static function fromVector<T, R:T>(v:Vector<R>):Vector<T>
	  return cast v;

	static public function fromIterable<T, R:T>(v:Iterable<R>):Vector<T>
	  return null;

	@:to public function toArray()
	  return this.copy();
}