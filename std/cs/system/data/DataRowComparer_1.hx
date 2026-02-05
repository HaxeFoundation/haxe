package cs.system.data;

@:native("System.Data.DataRowComparer`1")
extern class DataRowComparer_1<TRow> {
	static var Default(default, never):cs.system.data.DataRowComparer_1<Dynamic>;
	function Equals(leftRow:TRow, rightRow:TRow):Bool;
	function GetHashCode(row:TRow):Int;
}
