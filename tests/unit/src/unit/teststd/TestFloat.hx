package unit.teststd;

class TestFloat extends unit.Test {
	public function test() {
		var nan = Math.NaN;
		var pinf = Math.POSITIVE_INFINITY;
		var ninf = Math.NEGATIVE_INFINITY;
		var fl:Float = 0.0;

		f(fl > nan);
		f(fl < nan);
		f(fl >= nan);
		f(fl <= nan);
		f(fl == nan);
		t(fl != nan);

		f(nan > nan);
		f(nan < nan);
		f(nan >= nan);
		f(nan <= nan);
		f(nan == nan);
		t(nan != nan);

		f(pinf > nan);
		f(pinf < nan);
		f(pinf >= nan);
		f(pinf <= nan);
		f(pinf == nan);
		t(pinf != nan);

		f(ninf > nan);
		f(ninf < nan);
		f(ninf >= nan);
		f(ninf <= nan);
		f(ninf == nan);
		t(ninf != nan);

		f(nan > fl);
		f(nan < fl);
		f(nan >= fl);
		f(nan <= fl);
		f(nan == fl);
		t(nan != fl);

		f(nan > pinf);
		f(nan < pinf);
		f(nan >= pinf);
		f(nan <= pinf);
		f(nan == pinf);
		t(nan != pinf);

		f(nan > ninf);
		f(nan < ninf);
		f(nan >= ninf);
		f(nan <= ninf);
		f(nan == ninf);
		t(nan != ninf);
	}
}
