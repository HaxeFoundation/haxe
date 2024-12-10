function main() {
	Lambda.fold([], function(a, acc) {
		a.v = acc.v;
	}, {v: 1});
}