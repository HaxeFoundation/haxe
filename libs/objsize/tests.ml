open Printf
;;

let print title vl =
  let i = Objsize.objsize vl in
  printf "%S : data_words=%i headers=%i depth=%i\n    \
bytes_without_headers=%i bytes_with_headers=%i\n%!"
    title i.Objsize.data i.Objsize.headers i.Objsize.depth
    (Objsize.size_without_headers i)
    (Objsize.size_with_headers i)
in

print "string of 13 chars" ("0123456" ^ "789012")
;
print "some object"
  ( object method x = 123; method y = print_int; end
  )
;

print "some float" (Random.float 1.)
;
(*
let rec cyc = [1 :: [2 :: [3 :: cyc]]] in

print "cyclic list" cyc
;
*)
let genlist n =
  let rec inner acc n =
    if n <= 0
    then acc
    else inner (n :: acc) (n-1)
  in
    inner [] n
in

print "big list" (genlist 300000)
;

print "big array" (Array.make 30000 true)
;

print "statically created value" [1; 2; 3]
;

print "objsize 0.14 bug"
  (let rec val_a = (val_z, val_z)
   and val_z = (123, val_y)
   and val_y = (234, 345)
   in
     val_a
  )
;

print "objsize 0.15 bug"
  (let val_z = ((), ()) in
   let val_y = (val_z, val_z, fun x -> x) in
   val_y
  )
;
