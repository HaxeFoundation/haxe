// Add window.orientation
partial interface Window {
    readonly attribute long orientation;
    attribute EventHandler onorientationchange;
};
