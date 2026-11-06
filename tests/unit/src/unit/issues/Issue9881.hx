package unit.issues;

private class TypedError<T> {}

private enum Outcome<Data, Failure> {
	Success(data:Data);
	Failure(failure:Failure);
}

private abstract OutcomeMapper<DIn, FIn, DOut, FOut>({f:Outcome<DIn, FIn>->Outcome<DOut, FOut>}) {
	function new(f:Outcome<DIn, FIn>->Outcome<DOut, FOut>)
		this = {f: f};

	public function apply(o)
		return this.f(o);

	@:from static function withSameError<In, Out, Error>(f:In->Outcome<Out, Error>):OutcomeMapper<In, Error, Out, Error> {
		return new OutcomeMapper(function(o) return switch o {
			case Success(d): f(d);
			case Failure(f): Failure(f);
		});
	}
}

class Issue9881 extends unit.Test {
	function test() {
		switch flatMap(foo(), result -> bar(result.type, result.value)) {
			case Success(data):
				eq("foo:12", data);
			case Failure(_):
				assert();
		}
	}

	static function flatMap<DIn, FIn, DOut, FOut>(o:Outcome<DIn, FIn>, mapper:OutcomeMapper<DIn, FIn, DOut, FOut>):Outcome<DOut, FOut> {
		return mapper.apply(o);
	}

	static function foo():Outcome<{value:Int, type:String}, TypedError<Dynamic>> {
		return Success({value: 12, type: "foo"});
	}

	static function bar(type:String, value:Int):Outcome<String, TypedError<Dynamic>> {
		return Success(type + ":" + value);
	}
}
