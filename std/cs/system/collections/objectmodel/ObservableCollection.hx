package cs.system.collections.objectmodel;

@:native("System.Collections.ObjectModel.ObservableCollection")
extern class ObservableCollection<T> extends cs.system.collections.objectmodel.Collection<T0> {
	@:overload(function():Void {})
	@:overload(function(collection:cs.system.collections.generic.IEnumerable<T>):Void {})
	function new(list:cs.system.collections.generic.List<T>):Void;
	function Move(oldIndex:Int, newIndex:Int):Void;
}
