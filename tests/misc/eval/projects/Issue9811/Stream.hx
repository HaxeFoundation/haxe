class Error {}

enum Outcome<Data, Failure> {
	Success(data:Data);
	Failure(failure:Failure);
}

interface Future<T> {
	function map<A>(f:T->A):Future<A>;
}

typedef Surprise<D, F> = Future<Outcome<D, F>>;

abstract Promise<T>(Surprise<T, Error>) from Surprise<T, Error> to Surprise<T, Error> {
	public inline function map<R>(f:Outcome<T, Error>->R):Future<R>
		return this.map(f);

	@:from static inline function ofError<T>(e:Error):Promise<T>
		return throw 1;
}

abstract Stream<Item, Quality>(StreamObject<Item, Quality>) from StreamObject<Item, Quality> to StreamObject<Item, Quality> {
	public var depleted(get, never):Bool;

	inline function get_depleted()
		return this.depleted;

	@:to function dirty():Stream<Item, Error>
		throw 1;

	@:from static public function flatten<Item, Quality>(f:Future<Stream<Item, Quality>>):Stream<Item, Quality>
		throw 1;

	@:from static public function promise<Item, Quality>(f:Promise<Stream<Item, Quality>>):Stream<Item, Error> {
		return flatten(f.map(function(o) return switch o {
			case Success(s): s.dirty();
			case Failure(e): throw 1;
		}));
	}
}

interface StreamObject<Item, Quality> {
	var depleted(get, never):Bool;
}
