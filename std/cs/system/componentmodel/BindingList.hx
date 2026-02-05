package cs.system.componentmodel;

@:native("System.ComponentModel.BindingList")
extern class BindingList<T> extends cs.system.collections.objectmodel.Collection<T0> {
	var AllowEdit(default, default):Bool;
	var AllowNew(default, default):Bool;
	var AllowRemove(default, default):Bool;
	var IsSortedCore(default, never):Bool;
	var RaiseListChangedEvents(default, default):Bool;
	var SortDirectionCore(default, never):cs.system.componentmodel.ListSortDirection;
	var SortPropertyCore(default, never):cs.system.componentmodel.PropertyDescriptor;
	var SupportsChangeNotificationCore(default, never):Bool;
	var SupportsSearchingCore(default, never):Bool;
	var SupportsSortingCore(default, never):Bool;
	@:overload(function():Void {})
	function new(list:cs.system.collections.generic.IList<T>):Void;
	function AddNew():T;
	function CancelNew(itemIndex:Int):Void;
	function EndNew(itemIndex:Int):Void;
	function ResetBindings():Void;
	function ResetItem(position:Int):Void;
}
