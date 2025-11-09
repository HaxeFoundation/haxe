open EvalValue
open EvalEncode
open EvalDecode
open EvalExceptions
open EvalHash

let as_atomic_bool vthis = match vthis with
	| VInstance {ikind = IAtomicBool i} -> i
	| _ -> unexpected_value vthis "AtomicBool"
let as_atomic_int vthis = match vthis with
	| VInstance {ikind = IAtomicInt i} -> i
	| _ -> unexpected_value vthis "AtomicInt"
let as_atomic_object vthis = match vthis with
	| VInstance {ikind = IAtomicObject i} -> i
	| _ -> unexpected_value vthis "AtomicObject"

let atomic_object_fields = [
        "_hx_new", vfun1 (fun v -> encode_instance key_haxe_atomic_AtomicObject ~kind:(IAtomicObject (Atomic.make v)));
        "load", vfun1 (fun v -> Atomic.get (as_atomic_object v));
        "store", vfun2 (fun a v -> Atomic.set (as_atomic_object a) v; v);
        "exchange", vfun2 (fun a v -> Atomic.exchange (as_atomic_object a) v);
        "compareExchange", vfun3 (fun vthis a b ->
            let vthis = as_atomic_object vthis in
            let rec loop () = begin
                let original = Atomic.get vthis in
                let real_replacement = if EvalValue.equals original a then b else original in
                if not (Atomic.compare_and_set vthis original real_replacement) then 
                    loop ()
                else
                    original
            end
            in loop ());
]

let atomic_bool_fields = [
        "_hx_new", vfun1 (fun v -> encode_instance key_haxe_atomic_AtomicBool ~kind:(IAtomicBool (Atomic.make (decode_bool v))));
        "load", vfun1 (fun v -> vbool (Atomic.get (as_atomic_bool v)));
        "store", vfun2 (fun a v -> Atomic.set (as_atomic_bool a) (decode_bool v); v);
        "exchange", vfun2 (fun a v -> vbool (Atomic.exchange (as_atomic_bool a) (decode_bool v)));
        "compareExchange", vfun3 (fun vthis a b ->
            let vthis = as_atomic_bool vthis in
            let a = decode_bool a and b = decode_bool b in
            let rec loop () = begin
                let original = Atomic.get vthis in
                let real_replacement = if original = a then b else original in
                if not (Atomic.compare_and_set vthis original real_replacement) then 
                    loop ()
                else
                    vbool original
            end
            in loop ());
]

let fetch_update (this: 'a Atomic.t) (f: 'a -> 'a) =
    let rec loop () = begin
        let original = Atomic.get this in
        let replacement = f original in
        if not (Atomic.compare_and_set this original replacement) then 
            loop ()
        else
            original
    end
    in loop ()

let atomic_int_fields = [
        "_hx_new", vfun1 (fun v -> encode_instance key_haxe_atomic_AtomicInt ~kind:(IAtomicInt (Atomic.make (decode_int v))));
        "load", vfun1 (fun v -> vint (Atomic.get (as_atomic_int v)));
        "store", vfun2 (fun a v -> Atomic.set (as_atomic_int a) (decode_int v); v);
        "exchange", vfun2 (fun a v -> vint (Atomic.exchange (as_atomic_int a) (decode_int v)));
        "compareExchange", vfun3 (fun vthis a b ->
            let vthis = as_atomic_int vthis in
            let a = decode_int a and b = decode_int b in
            let rec loop () = begin
                let original = Atomic.get vthis in
                let real_replacement = if original = a then b else original in
                if not (Atomic.compare_and_set vthis original real_replacement) then 
                    loop ()
                else
                    vint original
            end
            in loop ());
        "add", vfun2 (fun a v -> vint (Atomic.fetch_and_add (as_atomic_int a) (decode_int v)));
        "sub", vfun2 (fun a v -> vint (Atomic.fetch_and_add (as_atomic_int a) (-(decode_int v))));
        "and", vfun2 (fun a v -> let v = decode_int v in vint (fetch_update (as_atomic_int a) (fun orig -> orig land v)));
        "or", vfun2 (fun a v -> let v = decode_int v in vint (fetch_update (as_atomic_int a) (fun orig -> orig lor v)));
        "xor", vfun2 (fun a v -> let v = decode_int v in vint (fetch_update (as_atomic_int a) (fun orig -> orig lxor v)));
]