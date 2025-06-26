package eval.luv;

/**
	Outcome of an operation.
**/
@:using(eval.luv.Result.ResultTools)
enum Result<T> {
	/** Operation completed successfully. **/
	Ok(value:T);
	/** Operation failed. **/
	Error(e:UVError);
}

enum abstract NoData(Dynamic) {
	var NoData = null;
}

class ResultTools {
	/**
		Returns the result value on success or throws `eval.luv.LuvException`
		on failure.
	**/
	static public inline function resolve<T>(result:Result<T>):T {
		switch result {
			case Ok(v): return v;
			case Error(e): throw new LuvException(e);
		}
	}

	/**
		Returns `true` if the result is `Ok`.
		Returns `false` if the result is `Error`.
	**/
	static public inline function isOk<T>(result:Result<T>):Bool {
		return switch result {
			case Ok(_): true;
			case Error(_): false;
		}
	}
}