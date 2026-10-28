package unit.issues;

class Issue13086 extends unit.Test {
	#if (hl && hl_ver >= version("2.0.0"))
	// some tests that are disabled by #8600
	function test() {
		if (!hl.Api.is64()) {
			noAssert();
			return;
		}
		var referenceDate = new Date(1970, 0, 12, 2, 0, 0);

		// Y2038 (outside of signed 32-bit integer range)
		var date = new Date(2039, 0, 1, 1, 59, 59);
		eq(date.getHours(), 1);
		eq(date.getMinutes(), 59);
		eq(date.getSeconds(), 59);
		eq(date.getFullYear(), 2039);
		eq(date.getMonth(), 0);
		eq(date.getDate(), 1);
		eq(date.getDay(), 6);
		t(date.getTime() > referenceDate.getTime());

		// Y2112 (outside of unsigned 32-bit integer range)
		var date = new Date(2112, 0, 1, 1, 59, 59);
		eq(date.getHours(), 1);
		eq(date.getMinutes(), 59);
		eq(date.getSeconds(), 59);
		eq(date.getFullYear(), 2112);
		eq(date.getMonth(), 0);
		eq(date.getDate(), 1);
		eq(date.getDay(), 5);
		t(date.getTime() > referenceDate.getTime());

		// milliseconds are kept
		var date = Date.fromTime(405781340123.);
		eq(date.getTime(), 405781340123.);
		eq(date.getUTCSeconds(), 20);
		eq(Date.fromTime(date.getTime()).getTime(), 405781340123.);
	}
	#end
}
