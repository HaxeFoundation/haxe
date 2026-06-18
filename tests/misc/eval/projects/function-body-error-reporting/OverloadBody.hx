extern class Api {
	overload static function take(f:Int->Int):Void;
	overload static function take(n:Int):Void;
}

function main() {
	// The lambda matches the (Int->Int) overload; its body error must be reported in
	// place, WITHOUT a spurious "For function argument" wrapper (it is a body error, not a
	// signature unification). DISABLED until the scoped-capture work lands -- the global
	// approach currently appends the wrapper. See CALL_ARG_ERRORS_TRANSACTIONAL_PLAN.md.
	Api.take(x -> {
		undefinedThing;
	});
}
