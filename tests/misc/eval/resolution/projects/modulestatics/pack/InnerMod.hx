package pack.inner;

// this module should NOT shadow InnerMod when called from within pack.inner

function lowerCase() {
    return "pack.InnerMod.lowerCase";
}

function UpperCase() {
    return "pack.InnerMod.UpperCase";
}
