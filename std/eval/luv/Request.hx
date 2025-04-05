package eval.luv;

/**
	Requests.

	@see https://aantron.github.io/luv/luv/Luv/Request
**/
@:coreType abstract Request {
	/**
		Tries to cancel a pending request.
	**/
	public function cancel():Result<Result.NoData>;
}