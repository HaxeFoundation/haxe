package cs.system.data;

@:native("System.Data.DataRowExtensions")
extern class DataRowExtensions {
	@:overload(function<T>(row:cs.system.data.DataRow, column:cs.system.data.DataColumn):T {})
	@:overload(function<T>(row:cs.system.data.DataRow, columnIndex:Int):T {})
	@:overload(function<T>(row:cs.system.data.DataRow, columnName:String):T {})
	@:overload(function<T>(row:cs.system.data.DataRow, column:cs.system.data.DataColumn, version:cs.system.data.DataRowVersion):T {})
	@:overload(function<T>(row:cs.system.data.DataRow, columnIndex:Int, version:cs.system.data.DataRowVersion):T {})
	static function Field<T>(row:cs.system.data.DataRow, columnName:String, version:cs.system.data.DataRowVersion):T;
	@:overload(function<T>(row:cs.system.data.DataRow, column:cs.system.data.DataColumn, value:T):Void {})
	@:overload(function<T>(row:cs.system.data.DataRow, columnIndex:Int, value:T):Void {})
	static function SetField<T>(row:cs.system.data.DataRow, columnName:String, value:T):Void;
}
