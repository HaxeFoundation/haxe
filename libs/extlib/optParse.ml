(*
 * optParse - Functions for parsing command line arguments.
 * Copyright (C) 2004 Bardur Arantsson
 *
 * Heavily influenced by the optparse.py module from the Python
 * standard library, but with lots of adaptation to the 'Ocaml Way'
 *
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)
open Printf
open ExtString
open ExtList


let terminal_width =
  try 
    int_of_string (Sys.getenv "COLUMNS")    (* Might as well use it if it's there... *)
  with
    Failure _ -> 80
  | Not_found -> 80

module GetOpt =
  struct

    type action = string -> string list -> unit
    type long_opt = string * int * action
    type short_opt = char * int * action

    exception Error of (string * string)

    let split1 haystack needle =
      try 
        let (h, x) = String.split haystack needle in h, [x] 
      with
        Invalid_string -> haystack, []

    let find_opt format_name options s =
      let rec loop l =
        match l with
          (x, y, z) :: t -> if x = s then x, y, z else loop t
        | [] -> raise (Error (format_name s, "no such option"))
      in
      loop options

    let find_short_opt options = find_opt (fun c -> sprintf "-%c" c) options

    let find_long_opt options = find_opt (fun s -> "--" ^ s) options

    let parse other find_short_opt find_long_opt args =
      let rec loop args =
        let rec gather_args name n args =
          try 
            List.split_nth n args 
          with
            List.Invalid_index _ ->
              raise (Error (name, "missing required arguments"))
        in
        let gather_long_opt s args =
          let (h, t) = split1 s "=" in
          let (_, nargs, action) = find_long_opt (String.slice ~first:2 h) in
          let (accum, args') = gather_args h (nargs - List.length t) args in
          action h (t @ accum); args'
        in
        let rec gather_short_opt_concat seen_args s k args =
          if k < String.length s then
            let ostr = sprintf "-%c" s.[k]
            and (_, nargs, action) = find_short_opt s.[k] in
            if nargs = 0 then
              begin
                action ostr [];
                gather_short_opt_concat seen_args s (k + 1) args
              end
            else if not seen_args then
              let (accum, args') = gather_args ostr nargs args in
              action ostr accum; gather_short_opt_concat true s (k + 1) args'
            else
              raise
                (Error
                   (sprintf "-%c" s.[k],
                    sprintf "option list '%s' already contains an option requiring an argument"
                      s))
          else args
        in
        let gather_short_opt s k args =
          let ostr = sprintf "-%c" s.[k] in
          let (_, nargs, action) = find_short_opt s.[k] in
          if nargs = 0 then gather_short_opt_concat false s k args
          else
            let (accum, args') =
              let h = String.slice ~first:(k+1) s in
              if String.length h = 0 then gather_args ostr nargs args
              else
                let (t, args'') = gather_args ostr (nargs - 1) args in
                h :: t, args''
            in
            action ostr accum; args'
        in
        match args with
          [] -> []
        | arg :: args' ->
            if arg = "--" then args'
            else if String.starts_with arg "--" then
              loop (gather_long_opt arg args')
            else if arg = "-" then begin other arg; loop args' end
            else if String.starts_with arg "-" then
              loop (gather_short_opt arg 1 args')
            else begin other arg; loop args' end
      in
      let args' = loop args in List.iter other args'
  end


module Opt =
  struct

    exception No_value
    exception Option_error of string * string
    exception Option_help

    type 'a t = { 
      option_set : string -> string list -> unit;
      option_set_value : 'a -> unit;
      option_get : unit -> 'a option;
      option_metavars : string list;
      option_defhelp : string option 
    }

    let get opt =
      match opt.option_get () with
        Some x -> x
      | None -> raise No_value

    let set opt v =
      opt.option_set_value v

    let is_set opt = Option.is_some (opt.option_get ())

    let opt opt = opt.option_get ()

    let value_option metavar default coerce errfmt =
      let data = ref default in
      {
        option_metavars = [metavar]; 
        option_defhelp = None;
        option_get = (fun _ -> !data);
        option_set_value = (fun x -> data := Some x);
        option_set =
         (fun option args ->
            let arg = List.hd args in
              try 
                data := Some (coerce arg)
              with
                  exn -> raise (Option_error (option, errfmt exn arg)))
      }

    let callback_option metavar coerce errfmt f =
      {
        option_metavars = [metavar]; 
        option_defhelp = None;
        option_get = (fun _ -> Some ());
        option_set_value = (fun () -> ());
        option_set =
         (fun option args ->
            let arg = List.hd args in
            let datum = ref None in
              begin 
              try 
                datum := Some (coerce arg)
              with
                  exn -> raise (Option_error (option, errfmt exn arg))
              end;

              Option.may f !datum)
      }
  end

module StdOpt =
  struct

    open Opt

    let store_const ?default const =
      let data = ref default in
      {
        option_metavars = []; 
        option_defhelp = None;
        option_get = (fun _ -> !data);
        option_set_value = (fun x -> data := Some x);
        option_set = fun _ _ -> data := Some const
      }

    let store_true () = store_const ~default:false true

    let store_false () = store_const ~default:true false

    let int_option ?default ?(metavar = "INT") () =
      value_option metavar default int_of_string
        (fun _ s -> sprintf "invalid integer value '%s'" s)

    let int_callback ?(metavar = "INT") =
      callback_option metavar int_of_string
        (fun _ s -> sprintf "invalid integer value '%s'" s)

    let float_option ?default ?(metavar = "FLOAT") () =
      value_option metavar default float_of_string
        (fun _ s -> sprintf "invalid floating point value '%s'" s)

    let float_callback ?(metavar = "FLOAT") =
      callback_option metavar float_of_string
        (fun _ s -> sprintf "invalid floating point value '%s'" s)

    let str_option ?default ?(metavar = "STR") () =
      value_option metavar default (fun s -> s) (fun _ _ -> "cannot happen")

    let str_callback ?(metavar = "STR") =
      callback_option metavar (fun s -> s) (fun _ _ -> "cannot happen")

    let count_option ?(dest = ref 0) ?(increment = 1) () =
      {
        option_metavars = []; 
        option_defhelp = None;
        option_get = (fun _ -> Some !dest);
        option_set_value = (fun x -> dest := x);
        option_set = fun _ _ -> dest := !dest + increment
      }

    let incr_option ?(dest = ref 0) = 
      count_option ~dest ~increment:1

    let decr_option ?(dest = ref 0) = 
      count_option ~dest ~increment:(-1)

    let help_option () =
      {
        option_metavars = [];
        option_defhelp = Some "show this help message and exit";
        option_get = (fun _ -> raise No_value);
        option_set_value = (fun _ -> ());
        option_set = fun _ _ -> raise Option_help
      }

    let version_option vfunc =
      {
        option_metavars = [];
        option_defhelp = Some "show program's version and exit";
        option_get = (fun _ -> raise No_value);
        option_set_value = (fun _ -> ());
        option_set = fun _ _ -> print_endline (vfunc ()); exit 0
      }
  end




module Formatter =
  struct

    (* Note that the whitespace regexps must NOT treat the non-breaking
       space character as whitespace. *)
    let whitespace = "\t\n\013\014\r "

    let split_into_chunks s =
      let buf = Buffer.create (String.length s) in
      let flush () =
        let s = Buffer.contents buf
        in
          Buffer.clear buf;
          s
      in
      let rec loop state accum i =
        if (i<String.length s) then
          if ((state && not (String.contains whitespace s.[i])) || 
              ((not state) && String.contains whitespace s.[i])) then
            if Buffer.length buf > 0 then
               loop (not state) (flush () :: accum) i 
             else 
               loop (not state) accum i
          else
            begin
              Buffer.add_char buf s.[i];
              loop state accum (i+1)
            end
        else
          if Buffer.length buf > 0 then
            flush () :: accum
          else 
            accum
      in
        List.rev (loop false [] 0)

    let is_whitespace s =
      let rec loop i =
        if i<String.length s then
          if String.contains whitespace s.[i] then
            loop (i+1)
          else 
            false
        else 
          true
      in
        loop 0

    let expand_tabs ?(tab_size = 8) s =
      let len = String.length s in
      let spaces n = String.make n ' '
      and b = Buffer.create len in
      let rec expand i col =
        if i < len then
          match s.[i] with
            '\t' ->
              let n = tab_size - col mod tab_size in
              Buffer.add_string b (spaces n);
              expand (i + 1) (col + n)
          | '\n' -> 
              Buffer.add_string b "\n";
              expand (i + 1) 0
          | c -> 
              Buffer.add_char b c;
              expand  (i + 1) (col + 1)
      in
      expand 0 0; 
      Buffer.contents b

    let wrap ?(initial_indent = 0) ?(subsequent_indent = 0) text _width =
      let wrap_chunks_line width acc =
        let rec wrap (chunks, cur_line, cur_len) =
          match chunks with
            [] -> [], cur_line, cur_len
          | hd :: tl ->
              let l = String.length hd in
              if cur_len + l <= width then
                wrap (tl, hd :: cur_line, cur_len + l)
              else chunks, cur_line, cur_len
        in
        wrap acc
      in
      let wrap_long_last_word width (chunks, cur_line, cur_len) =
        match chunks with
          [] -> [], cur_line, cur_len
        | hd :: tl ->
            let l = String.length hd in
            if l > width then
              match cur_line with
                [] -> tl, [hd], cur_len + l
              | _ -> chunks, cur_line, cur_len
            else chunks, cur_line, cur_len
      in
      let wrap_remove_last_ws (chunks, cur_line, cur_len) =
        match cur_line with
          [] -> chunks, cur_line, cur_len
        | hd :: tl ->
            if is_whitespace hd then chunks, tl, cur_len - String.length hd
            else chunks, cur_line, cur_len
      in
      let rec wrap_chunks_lines chunks lines =
        let indent =
          match lines with
            [] -> initial_indent
          | _ -> subsequent_indent
        in
        let width = _width - indent in
        match chunks with
          hd :: tl ->
            if is_whitespace hd && lines <> [] then wrap_chunks_lines tl lines
            else (* skip *)
              let (chunks', cur_line, _) =
                wrap_remove_last_ws
                  (wrap_long_last_word width
                     (wrap_chunks_line width (chunks, [], 0)))
              in
              wrap_chunks_lines chunks'
                ((String.make indent ' ' ^
                    String.concat "" (List.rev cur_line)) ::
                   lines)
        | [] -> List.rev lines
      in
      let chunks = split_into_chunks (expand_tabs text) in
      wrap_chunks_lines chunks []


    let fill ?(initial_indent = 0) ?(subsequent_indent = 0) text width =
      String.concat "\n" (wrap ~initial_indent ~subsequent_indent text width)



    type t = { 
      indent : unit -> unit;
      dedent : unit -> unit;
      format_usage : string -> string;
      format_heading : string -> string;
      format_description : string -> string;
      format_option : char list * string list -> string list -> 
                                             string option -> string
    }

    let format_option_strings short_first (snames, lnames) metavars =
      let metavar = String.concat " " metavars in
      let lopts =
        List.map
          (match metavar with
             "" -> (fun z -> sprintf "--%s" z)
           | _ -> fun z -> sprintf "--%s=%s" z metavar)
          lnames
      and sopts = List.map (fun x -> sprintf "-%c%s" x metavar) snames in
      match short_first with
        true -> String.concat ", " (sopts @ lopts)
      | false -> String.concat ", " (lopts @ sopts)


    let indented_formatter ?level:(extlevel = ref 0)
      ?indent:(extindent = ref 0) ?(indent_increment = 2) 
      ?(max_help_position = 24) ?(width = terminal_width - 1) 
      ?(short_first = true) () =
      let indent = ref 0
      and level = ref 0 in
      let help_position = ref max_help_position
      and help_width = ref (width - max_help_position) in
      {
        indent =
         (fun () ->
            indent := !indent + indent_increment;
            level := !level + 1;
            extindent := !indent;
            extlevel := !level);

        dedent =
         (fun () ->
            indent := !indent - indent_increment;
            level := !level - 1;
            assert (!level >= 0);
            extindent := !indent;
            extlevel := !level);
        
        format_usage = (fun usage -> sprintf "usage: %s\n" usage);
        
        format_heading =
         (fun heading -> sprintf "%*s%s:\n\n" !indent "" heading);
        
        format_description =
         (fun description ->
            let x =
              fill ~initial_indent:(!indent) ~subsequent_indent:(!indent)
                description (width - !indent)
            in
              if not (String.ends_with x "\n") then x ^ "\n\n" else x ^ "\n");
        
        format_option =
         fun names metavars help ->
           let opt_width = !help_position - !indent - 2 in
           let opt_strings =
             format_option_strings short_first names metavars
           in
           let buf = Buffer.create 256 in
           let indent_first =
             if String.length opt_strings > opt_width then
               begin
                 bprintf buf "%*s%s\n" !indent "" opt_strings; !help_position
               end
             else
               begin
                 bprintf buf "%*s%-*s  " !indent "" opt_width opt_strings; 0
               end
           in
           Option.may
             (fun option_help ->
                let lines = wrap option_help !help_width in
                match lines with
                  h :: t ->
                    bprintf buf "%*s%s\n" indent_first "" h;
                    List.iter
                      (fun x -> bprintf buf "%*s%s\n" !help_position "" x) t
                | [] -> ())
             help;

           let contents =
             Buffer.contents buf
           in
             if String.length contents > 0 && not (String.ends_with contents "\n") then
               contents ^ "\n"
             else
               contents
      }

    let titled_formatter ?(level = ref 0) ?(indent = ref 0) 
      ?(indent_increment = 0) ?(max_help_position = 24) 
      ?(width = terminal_width - 1) ?(short_first = true) 
      () =
      let formatter =
        indented_formatter ~level ~indent ~indent_increment ~max_help_position
          ~width ~short_first ()
      in
      let format_heading h =
        let c =
          match !level with
            0 -> '='
          | 1 -> '-'
          | _ -> failwith "titled_formatter: Too much indentation"
        in
        sprintf "%*s%s\n%*s%s\n\n" !indent "" (String.capitalize h) !indent ""
          (String.make (String.length h) c)
      in
      let format_usage usage =
        sprintf "%s  %s\n" (format_heading "Usage") usage
      in
      { formatter with 
          format_usage = format_usage;
          format_heading = format_heading
      }
  end



open Opt
open Formatter

module OptParser =
  struct

    exception Option_conflict of string

    type group = { 
      og_heading : string;
      og_description : string option;
      og_options :
        ((char list * string list) * string list * string option) RefList.t;
      og_children : group RefList.t 
    }

    type t = { 
      op_usage : string;
      op_suppress_usage : bool;
      op_prog : string;

      op_formatter : Formatter.t;
      
      op_long_options : GetOpt.long_opt RefList.t;
      op_short_options : GetOpt.short_opt RefList.t;
      
      op_groups : group 
    }

    let unprogify optparser s =
      (snd (String.replace ~str:s ~sub:"%prog" ~by:optparser.op_prog))

    let add optparser ?(group = optparser.op_groups) ?help ?(hide = false)
      ?short_name ?(short_names = []) ?long_name ?(long_names = []) opt =
      let lnames =
        match long_name with
            None -> long_names
          | Some x -> x :: long_names
      and snames =
        match short_name with
            None -> short_names
          | Some x -> x :: short_names
      in
      if lnames = [] && snames = [] then
        failwith "Options must have at least one name"
      else
        (* Checking for duplicates: *)
        let snames' =
          List.fold_left (fun r (x, _, _) -> x :: r) []
            (RefList.to_list optparser.op_short_options)
        and lnames' =
          List.fold_left (fun r (x, _, _) -> x :: r) []
            (RefList.to_list optparser.op_long_options)
        in
        let sconf =
          List.filter (fun e -> List.exists (( = ) e) snames') snames
        and lconf =
          List.filter (fun e -> List.exists (( = ) e) lnames') lnames
        in
        if List.length sconf > 0 then
          raise (Option_conflict (sprintf "-%c" (List.hd sconf)))
        else if List.length lconf > 0 then
          raise (Option_conflict (sprintf "--%s" (List.hd lconf)));
          
        (* Add to display list. *)
        if not hide then
          RefList.add group.og_options
            ((snames, lnames), opt.option_metavars,
             (match help with
                  None -> opt.option_defhelp
                | Some _ -> help));
          
        (* Getopt: *)
        let nargs = List.length opt.option_metavars in
          List.iter
            (fun short ->
               RefList.add optparser.op_short_options
               (short, nargs, opt.option_set))
            snames;
          List.iter
            (fun long ->
               RefList.add optparser.op_long_options
               (long, nargs, opt.option_set))
            lnames
            
    let add_group optparser ?(parent = optparser.op_groups) ?description heading =
      let g =
        {
          og_heading = heading; 
          og_description = description;
          og_options = RefList.empty (); 
          og_children = RefList.empty ()
        }
      in
      RefList.add parent.og_children g; g

    let make ?(usage = "%prog [options]") ?description ?version
      ?(suppress_usage = false) ?(suppress_help = false) ?prog 
      ?(formatter = Formatter.indented_formatter ()) () =
      let optparser =
        {
          op_usage = usage; 
          op_suppress_usage = suppress_usage;
          op_prog = Option.default (Filename.basename Sys.argv.(0)) prog;
          op_formatter = formatter; 
          op_short_options = RefList.empty ();
          op_long_options = RefList.empty ();
          op_groups = {
            og_heading = "options"; 
            og_options = RefList.empty ();
            og_children = RefList.empty ();
            og_description = description
          }
        }
      in
      Option.may                         (* Add version option? *)
        (fun version ->
           add optparser ~long_name:"version"
             (StdOpt.version_option
                (fun () -> unprogify optparser version)))
        version;
      if not suppress_help then              (* Add help option? *)
        add optparser ~short_name:'h' ~long_name:"help"
          (StdOpt.help_option ());

      optparser

    let format_usage optparser eol =
      match optparser.op_suppress_usage with
        true -> ""
      | false ->
          unprogify optparser
            (optparser.op_formatter.format_usage optparser.op_usage) ^ eol

    let error optparser ?(chn = stderr) ?(status = 1) message =
      fprintf chn "%s%s: %s\n" (format_usage optparser "\n") optparser.op_prog
        message;
      flush chn;
      exit status

    let usage optparser ?(chn = stdout) () =
      let rec loop g =
        (* Heading: *)
        output_string chn
          (optparser.op_formatter.format_heading g.og_heading);

        optparser.op_formatter.indent ();
        (* Description: *)
        Option.may
          (fun x ->
             output_string chn (optparser.op_formatter.format_description x))
          g.og_description;
        (* Options: *)
        RefList.iter
          (fun (names, metavars, help) ->
             output_string chn
               (optparser.op_formatter.format_option names metavars help))
          g.og_options;
        (* Child groups: *)
        output_string chn "\n";
        RefList.iter loop g.og_children;

        optparser.op_formatter.dedent ()
      in
      output_string chn (format_usage optparser "\n");
      loop optparser.op_groups;
      flush chn

    let parse optparser ?(first = 0) ?last argv =
      let args = RefList.empty ()
      and n =
        match last with
          None -> Array.length argv - first
        | Some m -> m - first + 1
      in
      begin 
        try
          GetOpt.parse (RefList.push args)
            (GetOpt.find_short_opt
               (RefList.to_list optparser.op_short_options))
            (GetOpt.find_long_opt (RefList.to_list optparser.op_long_options))
            (Array.to_list (Array.sub argv first n))
        with
            GetOpt.Error (opt, errmsg) ->
              error optparser (sprintf "option '%s': %s" opt errmsg)
          | Option_error (opt, errmsg) ->
              error optparser (sprintf "option '%s': %s" opt errmsg)
          | Option_help -> usage optparser (); exit 0
      end;
      List.rev (RefList.to_list args)

    let parse_argv optparser = 
      parse optparser ~first:1 Sys.argv
  end
