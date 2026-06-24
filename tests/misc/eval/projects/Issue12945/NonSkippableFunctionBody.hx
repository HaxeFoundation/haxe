function f( ?cb : Void -> Bool ) {}

function main() {
	// `cb` is the only parameter, so the optional argument cannot be skipped;
	// the function-literal body error must be reported in place.
	f(() -> {});
}
