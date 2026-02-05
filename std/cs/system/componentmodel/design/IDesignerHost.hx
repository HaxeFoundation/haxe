package cs.system.componentmodel.design;

/** Provides an interface for managing designer transactions and components. */
@:native("System.ComponentModel.Design.IDesignerHost")
extern interface IDesignerHost extends cs.system.componentmodel.design.IServiceContainer extends cs.system.IServiceProvider {
	/**
	 * Gets the container for this designer host.
	 * @return The  for this host.
	 */
	var Container(default, never):cs.system.componentmodel.IContainer;
	/**
	 * Gets a value indicating whether the designer host is currently in a transaction.
	 * @return if a transaction is in progress; otherwise, .
	 */
	var InTransaction(default, never):Bool;
	/**
	 * Gets a value indicating whether the designer host is currently loading the
	 * document.
	 * @return if the designer host is currently loading the document; otherwise, .
	 */
	var Loading(default, never):Bool;
	/**
	 * Gets the instance of the base class used as the root component for the current
	 * design.
	 * @return The instance of the root component class.
	 */
	var RootComponent(default, never):cs.system.componentmodel.IComponent;
	/**
	 * Gets the fully qualified name of the class being designed.
	 * @return The fully qualified name of the base component class.
	 */
	var RootComponentClassName(default, never):String;
	/**
	 * Gets the description of the current transaction.
	 * @return A description of the current transaction.
	 */
	var TransactionDescription(default, never):String;
	/** Activates the designer that this host is hosting. */
	function Activate():Void;
	@:overload(function(componentClass:cs.system.Type):cs.system.componentmodel.IComponent {})
	/**
	 * Creates a component of the specified type and adds it to the design document.
	 * @param componentClass The type of the component to create.
	 * @return The newly created component.
	 */
	function CreateComponent(componentClass:cs.system.Type, name:String):cs.system.componentmodel.IComponent;
	@:overload(function():cs.system.componentmodel.design.DesignerTransaction {})
	/**
	 * Creates a  that can encapsulate event sequences to improve performance and
	 * enable undo and redo support functionality.
	 * @return A new instance of . When you complete the steps in your transaction, you
	 * should call  on this object.
	 */
	function CreateTransaction(description:String):cs.system.componentmodel.design.DesignerTransaction;
	/**
	 * Destroys the specified component and removes it from the designer container.
	 * @param component The component to destroy.
	 */
	function DestroyComponent(component:cs.system.componentmodel.IComponent):Void;
	/**
	 * Gets the designer instance that contains the specified component.
	 * @param component The  to retrieve the designer for.
	 * @return An , or  if there is no designer for the specified component.
	 */
	function GetDesigner(component:cs.system.componentmodel.IComponent):cs.system.componentmodel.design.IDesigner;
	/**
	 * Gets an instance of the specified, fully qualified type name.
	 * @param typeName The name of the type to load.
	 * @return The type object for the specified type name, or  if the type cannot be
	 * found.
	 */
	function GetType(typeName:String):cs.system.Type;
}
