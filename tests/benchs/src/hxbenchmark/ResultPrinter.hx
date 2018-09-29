package hxbenchmark;

using StringTools;

class ResultPrinter {
	public function new() {

	}

	public function print(result:SuiteResult) {
		result.cases.sort((cr1, cr2) -> Reflect.compare(cr2.numSamples, cr1.numSamples));
		var maxNameLength = 0;
		var maxSampleLength = 0;
		for (caseResult in result.cases) {
			if (caseResult.name.length > maxNameLength) {
				maxNameLength = caseResult.name.length;
			}
			var sampleLength = printNumber(caseResult.numSamples).length;
			if (sampleLength > maxSampleLength) {
				maxSampleLength = sampleLength;
			}
		}
		var best = result.cases[0].numSamples;
		var buf = new StringBuf();
		buf.add("  Suite: " + result.name + "\n");
		for (caseResult in result.cases) {
			buf.add("    ");
			buf.add(caseResult.name.lpad(" ", maxNameLength));
			buf.add(": ");
			buf.add(printNumber(caseResult.numSamples).lpad(" ", maxSampleLength));
			buf.add(" (");
			var percentage = round(Std.string(caseResult.numSamples * 100. / best), 2);
			buf.add(percentage.lpad(" ", 6));
			buf.add("%)\n");
		}
		return buf.toString();
	}

	function printNumber(i:Int) {
		var buf = new StringBuf();
		var s = Std.string(i);
		var k = s.length % 3;
		var i = k;
		buf.addSub(s, 0, k);
		while (i < s.length) {
			if (buf.length > 0) {
				buf.add(",");
			}
			buf.addSub(s, k, 3);
			i += 3;
		}
		return buf.toString();
	}

	function round(s:String, digits:Int) {
		var i = s.indexOf(".");
		if (i == -1) {
			if (digits > 0)
				return (s + ".").rpad("0", digits + s.length + 1);
			else
				return s;
		}
		return (s.substr(0, i) + s.substr(i, digits + 1)).rpad("0", i + digits + 1);
	}
}