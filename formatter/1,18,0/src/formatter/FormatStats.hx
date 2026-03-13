package formatter;

class FormatStats {
	public static var totalFiles(default, null):Int = 0;
	public static var successFiles(default, null):Int = 0;
	public static var failedFiles(default, null):Int = 0;
	public static var disabledFiles(default, null):Int = 0;
	public static var totalLinesOrig(default, null):Int = 0;
	public static var totalLinesFormatted(default, null):Int = 0;

	public static function reset() {
		totalFiles = 0;
		successFiles = 0;
		failedFiles = 0;
		disabledFiles = 0;
		totalLinesOrig = 0;
		totalLinesFormatted = 0;
	}

	public static inline function incSuccess() {
		totalFiles++;
		successFiles++;
	}

	public static inline function incFailed() {
		totalFiles++;
		failedFiles++;
	}

	public static inline function incDisabled() {
		totalFiles++;
		disabledFiles++;
	}

	public static inline function addOrigLines(count:Int) {
		totalLinesOrig += count;
	}

	public static inline function addFormattedLines(count:Int) {
		totalLinesFormatted += count;
	}
}
