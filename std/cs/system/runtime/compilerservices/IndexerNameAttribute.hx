package cs.system.runtime.compilerservices;

/** Indicates the name by which an indexer is known in programming languages that do not support indexers directly. */
@:native("System.Runtime.CompilerServices.IndexerNameAttribute")
extern class IndexerNameAttribute extends cs.system.Attribute {
	function new(indexerName:String):Void;
}
