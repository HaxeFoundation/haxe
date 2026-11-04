class Main {
	static function main() {
		getAnswer().next(v -> switch v {
			case 42: 'Perfect!';
			case 40, 41, 43, 44: 'Close enough!';
			default: new NotError();
		}).handle(o -> trace(Std.string(o)));
	}

	static function getAnswer():Promise<Int>
		return 3 + 39;
}

class NotError {
	public function new() {}
}

abstract Promise<T>((handler:(result:Outcome<T, NotError>) -> Void) -> Void) {
	inline function new(f)
		this = f;

	public function next<X>(transform:Next<T, X>):Promise<X>
		return new Promise(handler -> this(o -> switch o {
			case Success(v): transform(v).handle(handler);
			case Failure(e): handler(Failure(e));
		}));

	@:from static function ofOutcome<T>(o:Outcome<T, NotError>):Promise<T>
		return new Promise<T>(h -> h(o));

	@:from static function ofValue<T>(v:T):Promise<T>
		return ofOutcome(Success(v));

	@:from static function ofError<T>(e:NotError):Promise<T>
		return ofOutcome(Failure(e));

	public function handle(cb)
		this(cb);
}

@:callable
abstract Next<In, Out>(In->Promise<Out>) from In->Promise<Out> {
	@:from(ignoredByInference) static function ofSync<In, Out>(f:In->Out):Next<In, Out>
		return v -> (f(v) : Promise<Out>);
}

enum Outcome<T, E> {
	Success(data:T);
	Failure(error:E);
}
