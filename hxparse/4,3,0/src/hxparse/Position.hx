package hxparse;

/**
	The position information maintained by `Lexer`.
**/
class Position {
	/**
		Name of the source.
	**/
	public var psource : String;

	/**
		The first character position, counting from the beginning of the input.
	**/
	public var pmin : Int;

	/**
		The last character position, counting from the beginning of the input.
	**/
	public var pmax : Int;

	/**
		Creates a new `Position` from the given information.
	**/
	public function new(source, min, max) {
		psource = source;
		pmin = min;
		pmax = max;
	}

	/**
		Returns a readable representation of `this` position;
	**/
	public function toString() {
		return '$psource:characters $pmin-$pmax';
	}

	public function getLinePosition(input:byte.ByteData) {
		var lineMin = 1;
		var lineMax = 1;
		var posMin = 0;
		var posMax = 0;
		var cur = 0;
		while (cur < pmin) {
			if (input.readByte(cur) == "\n".code) {
				lineMin++;
				posMin = cur + 1;
			}
			cur++;
		}
		lineMax = lineMin;
		posMax = posMin;
		posMin = cur - posMin;
		while (cur < pmax) {
			if (input.readByte(cur) == "\n".code) {
				lineMax++;
				posMax = cur + 1;
			}
			cur++;
		}
		posMax = cur - posMax;
		return {
			lineMin: lineMin,
			lineMax: lineMax,
			posMin: posMin,
			posMax: posMax
		}
	}

	/**
		Formats `this` position by resolving line numbers within `input`.

		If `input` is null, the result is unspecified.
	**/
	public function format(input:byte.ByteData) {
		var linePos = getLinePosition(input);
		if (linePos.lineMin != linePos.lineMax) {
			return '${psource}:lines ${linePos.lineMin}-${linePos.lineMax}';
		} else {
			return '${psource}:${linePos.lineMin}: characters ${linePos.posMin}-${linePos.posMax}';
		}
	}

	/**
		Unifies two positions `p1` and `p2`, using the minimum `pmin` and
		maximum `pmax` of both.

		The resulting `psource` and `pline` are taken from `p1`.

		If `p1` or `p2` are null, the result is unspecified.
	**/
	static public function union(p1:Position, p2:Position) {
		return new Position(p1.psource, p1.pmin < p2.pmin ? p1.pmin : p2.pmin, p1.pmax > p2.pmax ? p1.pmax : p2.pmax);
	}
}

private typedef Position2 = {
	lineMin: Int,
	lineMax: Int,
	posMin: Int,
	posMax: Int
}
