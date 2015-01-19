// Hoist certain common attributes up into Element for convenience
partial interface Element {
  readonly attribute CSSStyleDeclaration style;

  attribute EventHandler oncopy;
  attribute EventHandler oncut;
  attribute EventHandler onpaste;
};

Element implements GlobalEventHandlers;
Element implements TouchEventHandlers;
Element implements OnErrorEventHandlerForNodes;
