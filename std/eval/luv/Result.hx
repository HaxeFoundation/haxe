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
		Returns the result value if on success or throws `eval.luv.LuvException`
		on failure.
	**/
	static public inline function resolve<T>(result:Result<T>):T {
		switch result {
			case Ok(v): return v;
			case Error(e): throw new LuvException(e);
		}
	}
}