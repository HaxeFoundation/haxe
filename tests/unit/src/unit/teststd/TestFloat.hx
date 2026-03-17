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
		t(fl != nan == true);

		f(nan > nan);
		f(nan < nan);
		f(nan >= nan);
		f(nan <= nan);
		f(nan == nan);
		t(nan != nan == true);

		f(pinf > nan);
		f(pinf < nan);
		f(pinf >= nan);
		f(pinf <= nan);
		f(pinf == nan);
		t(pinf != nan == true);

		f(ninf > nan);
		f(ninf < nan);
		f(ninf >= nan);
		f(ninf <= nan);
		f(ninf == nan);
		t(ninf != nan == true);

		f(nan > fl);
		f(nan < fl);
		f(nan >= fl);
		f(nan <= fl);
		f(nan == fl);
		t(nan != fl == true);

		f(nan > pinf);
		f(nan < pinf);
		f(nan >= pinf);
		f(nan <= pinf);
		f(nan == pinf);
		t(nan != pinf == true);

		f(nan > ninf);
		f(nan < ninf);
		f(nan >= ninf);
		f(nan <= ninf);
		f(nan == ninf);
		t(nan != ninf == true);
	}
}
