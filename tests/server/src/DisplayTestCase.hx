import haxe.display.Position;
import haxe.Exception;
import utils.Markers;
import haxe.display.FsPath;

/**
	Display test should have a snippet with `{-N-}` markers
	in the doc block.
	The snippet will be automatically parsed.
**/
class DisplayTestCase extends TestCase {
	/** The snippet with markers removed */
	var source(get,never):String;
	/** A file created for the snippet */
	final file = new FsPath("Main.hx");
	/** Data extracted from the snippet */
	var markers(get, never):Markers;
	@:noCompletion var _markers:Null<Markers>;

	inline function get_markers():Markers
		return switch _markers {
			case null: throw new Exception('Markers are not initialized');
			case m: m;
		}

	inline function get_source():String
		return markers.source;

	/**
	 * Returns an offset of the n-th marker.
	 * Amount of characters from the beginning of the parsed document excluding markers.
	 */
	public function offset(n:Int):Int
		return markers.offset(n);

	/**
	 * Returns a position of n-th marker.
	 * Line number and character number from the beginning of the line.
	 */
	public function pos(n:Int):Position
		return markers.pos(n);

	/**
	 * Returns a range between positions of `startMarker` and `endMarker`
	 */
	public function range(startMarker:Int, endMarker:Int):Range
		return markers.range(startMarker, endMarker);
}
