package unit.issues;

private enum N {
	N1;
}

private class PR {}
private class VR {}

private enum C<X> {
	P:C<PR>;
	V:C<VR>;
}

class Issue6413 extends Test {

	function test () {
		add(N1, P);
	}

	public function add <X>(n:N, c:C<X>) {
		switch [n, c] {
			case [N1, P]:
				t(true);
			case [N1, _]:
				t(false);

		}
	}
}