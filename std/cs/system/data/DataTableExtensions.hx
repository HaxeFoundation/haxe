package cs.system.data;

@:native("System.Data.DataTableExtensions")
extern class DataTableExtensions {
	@:overload(function(table:cs.system.data.DataTable):cs.system.data.DataView {})
	static function AsDataView<T>(source:cs.system.data.EnumerableRowCollection_1<T>):cs.system.data.DataView;
	static function AsEnumerable(source:cs.system.data.DataTable):cs.system.data.EnumerableRowCollection_1<cs.system.data.DataRow>;
	@:overload(function<T>(source:cs.system.collections.generic.IEnumerable<T>):cs.system.data.DataTable {})
	@:overload(function<T>(source:cs.system.collections.generic.IEnumerable<T>, table:cs.system.data.DataTable, options:cs.system.data.LoadOption):Void {})
	static function CopyToDataTable<T>(source:cs.system.collections.generic.IEnumerable<T>, table:cs.system.data.DataTable, options:cs.system.data.LoadOption, errorHandler:cs.system.data.FillErrorEventHandler):Void;
}
