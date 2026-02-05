package cs.system.componentmodel;

/** Defines members that data entity classes can implement to provide custom synchronous and asynchronous validation support. */
@:native("System.ComponentModel.INotifyDataErrorInfo")
extern interface INotifyDataErrorInfo {
	/**
	 * Gets a value that indicates whether the entity has validation errors.
	 * @return if the entity currently has validation errors; otherwise, .
	 */
	var HasErrors(default, never):Bool;
	/**
	 * Gets the validation errors for a specified property or for the entire entity.
	 * @param propertyName The name of the property to retrieve validation errors for;
	 * or  or , to retrieve entity-level errors.
	 * @return The validation errors for the property or entity.
	 */
	function GetErrors(propertyName:String):cs.system.collections.IEnumerable;
}
