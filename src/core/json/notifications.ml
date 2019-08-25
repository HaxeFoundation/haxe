open JsonRpc
open Genjson

let notification name fl =
	notification name (Some (jobject fl))

module Progress = struct
	let get_id =
		let id = ref 0 in
		(fun () ->
			incr id;
			!id
		)

	class t = object(self)
		val id = get_id();

		method start ?cancellable ?message ?percentage title = "progress/start",[
			"id",jint id;
			"title",jstring title;
			"cancellable",jopt jbool cancellable;
			"message",jopt jstring message;
			"percentage",jopt jfloat percentage;
		]

		method done' = "progress/done",[
			"id",jint id;
		]

		method report ?message ?percentage () = "progress/report",[
			"id",jint id;
			"message",jopt jstring message;
			"percentage",jopt jfloat percentage;
		]

		method cancel = "progress/cancel",[
			"id",jint id;
		]
	end
end