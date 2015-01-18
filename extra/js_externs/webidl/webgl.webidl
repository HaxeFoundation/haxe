// Missing from mozilla WebIDLs

[Constructor(DOMString type, optional WebGLContextEventInit eventInit)]
interface WebGLContextEvent : Event {
    readonly attribute DOMString statusMessage;
};

// EventInit is defined in the DOM4 specification.
dictionary WebGLContextEventInit : EventInit {
    DOMString statusMessage;
};
