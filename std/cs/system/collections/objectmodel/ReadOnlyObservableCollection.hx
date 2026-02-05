package cs.system.collections.objectmodel;

@:native("System.Collections.ObjectModel.ReadOnlyObservableCollection")
extern class ReadOnlyObservableCollection<T> extends cs.system.collections.objectmodel.ReadOnlyCollection<T0> {
	function new(list:cs.system.collections.objectmodel.ObservableCollection<T>):Void;
}
