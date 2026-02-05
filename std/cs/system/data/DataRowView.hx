package cs.system.data;

/** Represents a customized view of a . */
@:native("System.Data.DataRowView")
extern class DataRowView {
	/**
	 * Gets the  to which this row belongs.
	 * @return The  to which this row belongs.
	 */
	var DataView(default, never):cs.system.data.DataView;
	/**
	 * Indicates whether the row is in edit mode.
	 * @return if the row is in edit mode; otherwise .
	 */
	var IsEdit(default, never):Bool;
	/**
	 * Indicates whether a  is new.
	 * @return if the row is new; otherwise .
	 */
	var IsNew(default, never):Bool;
	/**
	 * Gets the  being viewed.
	 * @return The  being viewed by the .
	 */
	var Row(default, never):cs.system.data.DataRow;
	/**
	 * Gets the current version description of the .
	 * @return One of the  values. Possible values for the  property are , , , and .
	 */
	var RowVersion(default, never):cs.system.data.DataRowVersion;
	@:overload(function(index0:Int):Dynamic {})
	@:native("get_Item")
	function get_Item(index0:String):Dynamic;
	@:overload(function(index0:Int, value:Dynamic):Void {})
	@:native("set_Item")
	function set_Item(index0:String, value:Dynamic):Void;
	/** Begins an edit procedure. */
	function BeginEdit():Void;
	/** Cancels an edit procedure. */
	function CancelEdit():Void;
	@:overload(function(relation:cs.system.data.DataRelation):cs.system.data.DataView {})
	@:overload(function(relationName:String):cs.system.data.DataView {})
	@:overload(function(relation:cs.system.data.DataRelation, followParent:Bool):cs.system.data.DataView {})
	/**
	 * Returns a  for the child  with the specified child .
	 * @param relation The  object.
	 * @return a  for the child .
	 */
	function CreateChildView(relationName:String, followParent:Bool):cs.system.data.DataView;
	/** Deletes a row. */
	function Delete():Void;
	/** Commits changes to the underlying  and ends the editing session that was begun with .  Use  to discard the changes made to the . */
	function EndEdit():Void;
	/**
	 * Gets a value indicating whether the current  is identical to the specified
	 * object.
	 * @param other An  to be compared.
	 * @return if  is a  and it returns the same row as the current ; otherwise .
	 */
	function Equals(other:Dynamic):Bool;
	/**
	 * Returns the hash code of the  object.
	 * @return A 32-bit signed integer hash code 1, which represents Boolean  if the
	 * value of this instance is nonzero; otherwise the integer zero, which represents
	 * Boolean .
	 */
	function GetHashCode():Int;
}
