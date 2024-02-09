package utils;

import haxe.Exception;
import haxe.display.Position;

/**
 * Parses a document with markers.
 * Marker format: `{-N-}`, where `N` is an integer number.
 */
class Markers {
	/** Parsed document with all markers removed. */
	public final source:String;

	final offsets:Array<Int>;
	final positions:Array<Position>;

	static public function parse(doc:String):Markers {
		var positions = [];
		var offsets = [];
		var line = 0;
		var lastNewLinePos = 0;
		var markersLengthSum = 0;
		var markersLengthSinceNewLine = 0;
		var source = ~/{-(\d+)-}|\n/g.map(doc, function(r) {
			var p = r.matchedPos();
			var replacement = switch r.matched(0) {
				case '\n':
					line++;
					lastNewLinePos = p.pos;
					markersLengthSinceNewLine = 0;
					'\n';
				case _:
					var name = r.matched(1);
					switch Std.parseInt(name) {
						case null:
							throw new Exception('Invalid marker name: {-$name-}');
						case n:
							offsets[n] = p.pos - markersLengthSum;
							var character = p.pos - (lastNewLinePos + 1) - markersLengthSinceNewLine;
							positions[n] = {line: line, character: character};
					}
					markersLengthSum += p.len;
					markersLengthSinceNewLine += p.len;
					"";
			}
			return replacement;
		});
		return new Markers(source, offsets, positions);
	}

	function new(source:String, offsets:Array<Int>, positions:Array<Position>) {
		this.source = source;
		this.offsets = offsets;
		this.positions = positions;
	}

	/**
	 * Returns an offset of the n-th marker.
	 * Amount of characters from the beginning of the parsed document excluding markers.
	 */
	public function offset(n:Int):Int {
		return switch offsets[n] {
			case null: throw new Exception('Marker {-$n-} not found');
			case pos: pos;
		}
	}

	/**
	 * Returns a position of n-th marker.
	 * Line number and character number from the beginning of the line.
	 */
	public function pos(n:Int):Position {
		return switch positions[n] {
			case null: throw new Exception('Marker {-$n-} not found');
			case pos: pos;
		}
	}

	/**
	 * Returns a range between positions of `startMarker` and `endMarker`
	 */
	public function range(startMarker:Int, endMarker:Int):Range {
		return {
			start: pos(startMarker),
			end: pos(endMarker)
		}
	}
}
