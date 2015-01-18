// Hoist certain common attributes up into Element for convenience
partial interface Element {
  readonly attribute CSSStyleDeclaration style;
};
Element implements GlobalEventHandlers;
Element implements TouchEventHandlers;
Element implements OnErrorEventHandlerForNodes;
