(***********************************************************************)
(*                                                                     *)
(*                         The CamlZip library                         *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                  adapted to Extc lib by Caue Waneck                 *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License, with     *)
(*  the special exception on linking described in file LICENSE.        *)
(*                                                                     *)
(***********************************************************************)

(* $Id: zip.ml,v 1.5 2008/12/07 09:23:08 xleroy Exp $ *)

(* Module [Zip]: reading and writing ZIP archives *)

exception Error of string * string * string

let read1 = input_byte
let read2 ic =
  let lb = read1 ic in let hb = read1 ic in lb lor (hb lsl 8)
let read4 ic =
  let lw = read2 ic in let hw = read2 ic in
  Int32.logor (Int32.of_int lw) (Int32.shift_left (Int32.of_int hw) 16)
let read4_int ic =
  let lw = read2 ic in let hw = read2 ic in
  if hw > max_int lsr 16 then raise (Error("", "", "32-bit data too large"));
  lw lor (hw lsl 16)
let readstring ic n =
  let s = String.create n in
  really_input ic s 0 n; s

let write1 = output_byte
let write2 oc n =
  write1 oc n; write1 oc (n lsr 8)
let write4 oc n =
  write2 oc (Int32.to_int n);
  write2 oc (Int32.to_int (Int32.shift_right_logical n 16))
let write4_int oc n =
  write2 oc n;
  write2 oc (n lsr 16)
let writestring oc s =
  output oc s 0 (String.length s)

type compression_method = Stored | Deflated

type entry =
  { filename: string;
    extra: string;
    comment: string;
    methd: compression_method;
    mtime: float;
    crc: int32;
    uncompressed_size: int;
    compressed_size: int;
    is_directory: bool;
    file_offset: int64 }

type in_file =
  { if_filename: string;
    if_channel: Pervasives.in_channel;
    if_entries: entry list;
    if_directory: (string, entry) Hashtbl.t;
    if_comment: string }

let entries ifile = ifile.if_entries
let comment ifile = ifile.if_comment

type out_file =
  { of_filename: string;
    of_channel: Pervasives.out_channel;
    mutable of_entries: entry list;
    of_comment: string }

exception Error of string * string * string

(* Return the position of the last occurrence of s1 in s2, or -1 if not
   found. *)

let strrstr pattern buf ofs len =
  let rec search i j =
    if i < ofs then -1
    else if j >= String.length pattern then i
    else if pattern.[j] = buf.[i + j] then search i (j+1)
    else search (i-1) 0
  in search (ofs + len - String.length pattern) 0

(* Determine if a file name is a directory (ends with /) *)

let filename_is_directory name =
  String.length name > 0 && name.[String.length name - 1] = '/'

(* Convert between Unix dates and DOS dates *)

let unixtime_of_dostime time date =
  fst(Unix.mktime
        { Unix.tm_sec = (time lsl 1) land 0x3e;
          Unix.tm_min = (time lsr 5) land 0x3f;
          Unix.tm_hour = (time lsr 11) land 0x1f;
          Unix.tm_mday = date land 0x1f;
          Unix.tm_mon = ((date lsr 5) land 0xf) - 1;
          Unix.tm_year = ((date lsr 9) land 0x7f) + 80;
          Unix.tm_wday = 0;
          Unix.tm_yday = 0;
          Unix.tm_isdst = false })

let dostime_of_unixtime t =
  let tm = Unix.localtime t in
  (tm.Unix.tm_sec lsr 1
     + (tm.Unix.tm_min lsl 5)
     + (tm.Unix.tm_hour lsl 11),
   tm.Unix.tm_mday
     + (tm.Unix.tm_mon + 1) lsl 5
     + (tm.Unix.tm_year - 80) lsl 9)

(* Read end of central directory record *)

let read_ecd filename ic =
  let buf = String.create 256 in
  let filelen = in_channel_length ic in
  let rec find_ecd pos len =
    (* On input, bytes 0 ... len - 1 of buf reflect what is at pos in ic *)
    if pos <= 0 || filelen - pos >= 0x10000 then
      raise (Error(filename, "",
                   "end of central directory not found, not a ZIP file"));
    let toread = min pos 128 in
    (* Make room for "toread" extra bytes, and read them *)
    String.blit buf 0 buf toread (256 - toread);
    let newpos = pos - toread in
    seek_in ic newpos;
    really_input ic buf 0 toread;
    let newlen = min (toread + len) 256 in
    (* Search for magic number *)
    let ofs = strrstr "PK\005\006" buf 0 newlen in
    if ofs < 0 || newlen < 22 || 
       (let comment_len = 
          Char.code buf.[ofs + 20] lor (Char.code buf.[ofs + 21] lsl 8) in
        newpos + ofs + 22 + comment_len <> filelen) then
      find_ecd newpos newlen
    else
      newpos + ofs in
  seek_in ic (find_ecd filelen 0);
  let magic = read4 ic in
  let disk_no = read2 ic in
  let cd_disk_no = read2 ic in
  let _disk_entries = read2 ic in
  let cd_entries = read2 ic in
  let cd_size = read4 ic in
  let cd_offset = read4 ic in
  let comment_len = read2 ic in
  let comment = readstring ic comment_len in
  assert (magic = Int32.of_int 0x06054b50);
  if disk_no <> 0 || cd_disk_no <> 0 then
    raise (Error(filename, "", "multi-disk ZIP files not supported"));
  (cd_entries, cd_size, cd_offset, comment)

(* Read central directory *)

let read_cd filename ic cd_entries cd_offset cd_bound =
  let cd_bound = Int64.of_int32 cd_bound in
  try
    LargeFile.seek_in ic (Int64.of_int32 cd_offset);
    let e = ref [] in
    let entrycnt = ref 0 in
    while (LargeFile.pos_in ic < cd_bound) do
      incr entrycnt;
      let magic = read4 ic in
      let _version_made_by = read2 ic in
      let _version_needed = read2 ic in
      let flags = read2 ic in
      let methd = read2 ic in
      let lastmod_time = read2 ic in
      let lastmod_date = read2 ic in
      let crc = read4 ic in
      let compr_size = read4_int ic in
      let uncompr_size = read4_int ic in
      let name_len = read2 ic in
      let extra_len = read2 ic in
      let comment_len = read2 ic in
      let _disk_number = read2 ic in
      let _internal_attr = read2 ic in
      let _external_attr = read4 ic in
      let header_offset = Int64.of_int32(read4 ic) in
      let name = readstring ic name_len in
      let extra = readstring ic extra_len in
      let comment = readstring ic comment_len in
      if magic <> Int32.of_int 0x02014b50 then
        raise (Error(filename, name,
                     "wrong file header in central directory"));
      if flags land 1 <> 0 then
        raise (Error(filename, name, "encrypted entries not supported"));

      e := { filename = name;
             extra = extra;
             comment = comment;
             methd = (match methd with
                         0 -> Stored
                       | 8 -> Deflated
                       | _ -> raise (Error(filename, name,
                                           "unknown compression method")));
             mtime = unixtime_of_dostime lastmod_time lastmod_date;
             crc = crc;
             uncompressed_size = uncompr_size;
             compressed_size = compr_size;
             is_directory = filename_is_directory name;
             file_offset = header_offset
           } :: !e
    done;
    assert((cd_bound = (LargeFile.pos_in ic)) &&
           (cd_entries = 65535 || !entrycnt = cd_entries));
    List.rev !e
  with End_of_file ->
    raise (Error(filename, "", "end-of-file while reading central directory"))

(* Open a ZIP file for reading *)

let open_in filename =
  let ic = Pervasives.open_in_bin filename in
  let (cd_entries, cd_size, cd_offset, cd_comment) = read_ecd filename ic in
  let entries =
    read_cd filename ic cd_entries cd_offset (Int32.add cd_offset cd_size) in
  let dir = Hashtbl.create (cd_entries / 3) in
  List.iter (fun e -> Hashtbl.add dir e.filename e) entries;
  { if_filename = filename;
    if_channel = ic;
    if_entries = entries;
    if_directory = dir;
    if_comment = cd_comment }

(* Close a ZIP file opened for reading *)

let close_in ifile =
  Pervasives.close_in ifile.if_channel

(* Return the info associated with an entry *)

let find_entry ifile name =
  Hashtbl.find ifile.if_directory name

(* Position on an entry *)

let goto_entry ifile e =
  try
    let ic = ifile.if_channel in
    LargeFile.seek_in ic e.file_offset;
    let magic = read4 ic in
    let _version_needed = read2 ic in
    let _flags = read2 ic in
    let _methd = read2 ic in
    let _lastmod_time = read2 ic in
    let _lastmod_date = read2 ic in
    let _crc = read4 ic in
    let _compr_size = read4_int ic in
    let _uncompr_size = read4_int ic in
    let filename_len = read2 ic in
    let extra_len = read2 ic in
    if magic <> Int32.of_int 0x04034b50 then
       raise (Error(ifile.if_filename, e.filename, "wrong local file header"));
    (* Could validate information read against directory entry, but
       what the heck *)
    LargeFile.seek_in ifile.if_channel
      (Int64.add e.file_offset (Int64.of_int (30 + filename_len + extra_len)))
  with End_of_file ->
    raise (Error(ifile.if_filename, e.filename, "truncated local file header"))

(* Read the contents of an entry as a string *)

let read_entry ifile e =
  try
    goto_entry ifile e;
    let res = String.create e.uncompressed_size in
    match e.methd with
      Stored ->
        if e.compressed_size <> e.uncompressed_size then
          raise (Error(ifile.if_filename, e.filename,
                       "wrong size for stored entry"));
        really_input ifile.if_channel res 0 e.uncompressed_size;
        res
    | Deflated ->
        let in_avail = ref e.compressed_size in
        let out_pos = ref 0 in
        begin try

          Zlib.uncompress ~header:false
            (fun buf ->
              let read = input ifile.if_channel buf 0
                               (min !in_avail (String.length buf)) in
              in_avail := !in_avail - read;
              read)
            (fun buf len ->
              if !out_pos + len > String.length res then
                raise (Error(ifile.if_filename, e.filename,
                             "wrong size for deflated entry (too much data)"));
              String.blit buf 0 res !out_pos len;
              out_pos := !out_pos + len)
        with Failure(_) ->
          raise (Error(ifile.if_filename, e.filename, "decompression error"))
        end;
        if !out_pos <> String.length res then
          raise (Error(ifile.if_filename, e.filename,
                       "wrong size for deflated entry (not enough data)"));
        let crc = Zlib.update_crc Int32.zero res 0 (String.length res) in
        if crc <> e.crc then
          raise (Error(ifile.if_filename, e.filename, "CRC mismatch"));
        res
  with End_of_file ->
    raise (Error(ifile.if_filename, e.filename, "truncated data"))
    
(* Write the contents of an entry into an out channel *)

let copy_entry_to_channel ifile e oc =
  try
    goto_entry ifile e;
    match e.methd with
      Stored ->
        if e.compressed_size <> e.uncompressed_size then
          raise (Error(ifile.if_filename, e.filename,
                       "wrong size for stored entry"));
        let buf = String.create 4096 in
        let rec copy n =
          if n > 0 then begin
            let r = input ifile.if_channel buf 0 (min n (String.length buf)) in
            output oc buf 0 r;
            copy (n - r)
          end in
        copy e.uncompressed_size
    | Deflated ->
        let in_avail = ref e.compressed_size in
        let crc = ref Int32.zero in
        begin try
          Zlib.uncompress ~header:false
            (fun buf ->
              let read = input ifile.if_channel buf 0
                               (min !in_avail (String.length buf)) in
              in_avail := !in_avail - read;
              read)
            (fun buf len ->
              output oc buf 0 len;
              crc := Zlib.update_crc !crc buf 0 len)
        with Failure _ ->
          raise (Error(ifile.if_filename, e.filename, "decompression error"))
        end;
        if !crc <> e.crc then
          raise (Error(ifile.if_filename, e.filename, "CRC mismatch"))
  with End_of_file ->
    raise (Error(ifile.if_filename, e.filename, "truncated data"))
    
(* Write the contents of an entry to a file *)

let copy_entry_to_file ifile e outfilename =
  let oc = open_out_bin outfilename in
  try
    copy_entry_to_channel ifile e oc;
    close_out oc;
    begin try
      Unix.utimes outfilename e.mtime e.mtime
    with Unix.Unix_error(_, _, _) | Invalid_argument _ -> ()
    end
  with x ->
    close_out oc;
    Sys.remove outfilename;
    raise x

(* Open a ZIP file for writing *)

let open_out ?(comment = "") filename =
  if String.length comment >= 0x10000 then
    raise(Error(filename, "", "comment too long"));
  { of_filename = filename;
    of_channel = Pervasives.open_out_bin filename;
    of_entries = [];
    of_comment = comment }

(* Close a ZIP file for writing.  Add central directory. *)

let write_directory_entry oc e =
  write4 oc (Int32.of_int 0x02014b50);  (* signature *)
  let version = match e.methd with Stored -> 10 | Deflated -> 20 in
  write2 oc version;                    (* version made by *)
  write2 oc version;                    (* version needed to extract *)
  write2 oc 8;                          (* flags *)
  write2 oc (match e.methd with Stored -> 0 | Deflated -> 8); (* method *)
  let (time, date) = dostime_of_unixtime e.mtime in
  write2 oc time;                       (* last mod time *)
  write2 oc date;                       (* last mod date *)
  write4 oc e.crc;                      (* CRC32 *)
  write4_int oc e.compressed_size;      (* compressed size *)
  write4_int oc e.uncompressed_size;    (* uncompressed size *)
  write2 oc (String.length e.filename); (* filename length *)
  write2 oc (String.length e.extra);    (* extra length *)
  write2 oc (String.length e.comment);  (* comment length *)
  write2 oc 0;                          (* disk number start *)
  write2 oc 0;                          (* internal attributes *)
  write4_int oc 0;                      (* external attributes *)
  write4 oc (Int64.to_int32 e.file_offset); (* offset of local header *)
  writestring oc e.filename;            (* filename *)
  writestring oc e.extra;               (* extra info *)
  writestring oc e.comment              (* file comment *)

let close_out ofile =
  let oc = ofile.of_channel in
  let start_cd = pos_out oc in
  List.iter (write_directory_entry oc) (List.rev ofile.of_entries);
  let cd_size = pos_out oc - start_cd in
  let num_entries = List.length ofile.of_entries in
  if num_entries >= 0x10000 then
    raise(Error(ofile.of_filename, "", "too many entries"));
  write4 oc (Int32.of_int 0x06054b50);  (* signature *)
  write2 oc 0;                          (* disk number *)
  write2 oc 0;                          (* number of disk with central dir *)
  write2 oc num_entries;                (* # entries in this disk *)
  write2 oc num_entries;                (* # entries in central dir *)
  write4_int oc cd_size;                (* size of central dir *)
  write4_int oc start_cd;               (* offset of central dir *)
  write2 oc (String.length ofile.of_comment); (* length of comment *)
  writestring oc ofile.of_comment;         (* comment *)
  Pervasives.close_out oc

(* Write a local file header and return the corresponding entry *)

let add_entry_header ofile extra comment level mtime filename =
  if level < 0 || level > 9 then
    raise(Error(ofile.of_filename, filename, "wrong compression level"));
  if String.length filename >= 0x10000 then
    raise(Error(ofile.of_filename, filename, "filename too long"));
  if String.length extra >= 0x10000 then
    raise(Error(ofile.of_filename, filename, "extra data too long"));
  if String.length comment >= 0x10000 then
    raise(Error(ofile.of_filename, filename, "comment too long"));
  let oc = ofile.of_channel in
  let pos = LargeFile.pos_out oc in
  write4 oc (Int32.of_int 0x04034b50);  (* signature *)
  let version = if level = 0 then 10 else 20 in
  write2 oc version;                    (* version needed to extract *)
  write2 oc 8;                          (* flags *)
  write2 oc (if level = 0 then 0 else 8); (* method *)
  let (time, date) = dostime_of_unixtime mtime in
  write2 oc time;                       (* last mod time *)
  write2 oc date;                       (* last mod date *)
  write4 oc Int32.zero;                 (* CRC32 - to be filled later *)
  write4_int oc 0;                      (* compressed size - later *)
  write4_int oc 0;                      (* uncompressed size - later *)
  write2 oc (String.length filename);   (* filename length *)
  write2 oc (String.length extra);      (* extra length *)
  writestring oc filename;              (* filename *)
  writestring oc extra;                 (* extra info *)
  { filename = filename;
    extra = extra;
    comment = comment;
    methd = (if level = 0 then Stored else Deflated);
    mtime = mtime;
    crc = Int32.zero;
    uncompressed_size = 0;
    compressed_size = 0;
    is_directory = filename_is_directory filename;
    file_offset = pos }

(* Write a data descriptor and update the entry *)

let add_data_descriptor ofile crc compr_size uncompr_size entry =
  let oc = ofile.of_channel in
  write4 oc (Int32.of_int 0x08074b50);  (* signature *)
  write4 oc crc;                        (* CRC *)
  write4_int oc compr_size;             (* compressed size *)
  write4_int oc uncompr_size;           (* uncompressed size *)
  { entry with crc = crc;
               uncompressed_size = uncompr_size;
               compressed_size = compr_size }

(* Add an entry with the contents of a string *)

let add_entry data ofile ?(extra = "") ?(comment = "") 
                         ?(level = 6) ?(mtime = Unix.time()) name =
  let e = add_entry_header ofile extra comment level mtime name in
  let crc = Zlib.update_crc Int32.zero data 0 (String.length data) in
  let compr_size =
    match level with
      0 ->
        output ofile.of_channel data 0 (String.length data);
        String.length data
    | _ ->
        let in_pos = ref 0 in
        let out_pos = ref 0 in
        try
          Zlib.compress ~level ~header:false
            (fun buf ->
               let n = min (String.length data - !in_pos)
                           (String.length buf) in
               String.blit data !in_pos buf 0 n;
               in_pos := !in_pos + n;
               n)
            (fun buf n ->
                output ofile.of_channel buf 0 n;
                out_pos := !out_pos + n);
          !out_pos
        with Failure _ ->
          raise (Error(ofile.of_filename, name, "compression error")) in
  let e' = add_data_descriptor ofile crc compr_size (String.length data) e in
  ofile.of_entries <- e' :: ofile.of_entries

(* Add an entry with the contents of an in channel *)

let copy_channel_to_entry ic ofile ?(extra = "") ?(comment = "") 
                                   ?(level = 6) ?(mtime = Unix.time()) name =
  let e = add_entry_header ofile extra comment level mtime name in
  let crc = ref Int32.zero in
  let (compr_size, uncompr_size) =
    match level with
      0 ->
        let buf = String.create 4096 in
        let rec copy sz =
          let r = input ic buf 0 (String.length buf) in
          if r = 0 then sz else begin
            crc := Zlib.update_crc !crc buf 0 r;
            output ofile.of_channel buf 0 r;
            copy (sz + r)
          end in
        let size = copy 0 in
        (size, size)
    | _ ->
        let in_pos = ref 0 in
        let out_pos = ref 0 in
        try
          Zlib.compress ~level ~header:false
            (fun buf ->
               let r = input ic buf 0 (String.length buf) in
               crc := Zlib.update_crc !crc buf 0 r;
               in_pos := !in_pos + r;
               r)
            (fun buf n ->            
               output ofile.of_channel buf 0 n;
               out_pos := !out_pos + n);
          (!out_pos, !in_pos)
        with Failure( _) ->
          raise (Error(ofile.of_filename, name, "compression error")) in
  let e' = add_data_descriptor ofile !crc compr_size uncompr_size e in
  ofile.of_entries <- e' :: ofile.of_entries

(* Add an entry with the contents of a file *)

let copy_file_to_entry infilename ofile ?(extra = "") ?(comment = "") 
                                        ?(level = 6) ?mtime name =
  let ic = open_in_bin infilename in
  let mtime' =
    match mtime with 
      Some t -> mtime
    | None ->
        try Some((Unix.stat infilename).Unix.st_mtime)
        with Unix.Unix_error(_,_,_) -> None in
  try
    copy_channel_to_entry ic ofile ~extra ~comment ~level ?mtime:mtime' name;
    Pervasives.close_in ic
  with x ->
    Pervasives.close_in ic; raise x


(* Add an entry whose content will be produced by the caller *)

let add_entry_generator ofile ?(extra = "") ?(comment = "")
                         ?(level = 6) ?(mtime = Unix.time()) name =
  let e = add_entry_header ofile extra comment level mtime name in
  let crc = ref Int32.zero in
  let compr_size = ref 0 in
  let uncompr_size = ref 0 in
  let finished = ref false in
  let check () =
    if !finished then
      raise (Error(ofile.of_filename, name, "entry already finished"))
  in
  let finish () =
    finished := true;
    let e' = add_data_descriptor ofile !crc !compr_size !uncompr_size e in
    ofile.of_entries <- e' :: ofile.of_entries
  in
  match level with
  | 0 ->
      (fun buf pos len ->
        check ();
        output ofile.of_channel buf pos len;
        compr_size := !compr_size + len;
        uncompr_size := !uncompr_size + len
      ),
      (fun () ->
        check ();
        finish ()
      )
  | _ ->
      let (send, flush) = Zlib.compress_direct ~level ~header:false
          (fun buf n ->
            output ofile.of_channel buf 0 n;
            compr_size := !compr_size + n)
      in
      (fun buf pos len ->
        check ();
        try
          send buf pos len;
          uncompr_size := !uncompr_size + len;
          crc := Zlib.update_crc !crc buf pos len
        with Failure(_) ->
          raise (Error(ofile.of_filename, name, "compression error"))
      ),
      (fun () ->
        check ();
        try
          flush ();
          finish ()
        with Failure(_) ->
          raise (Error(ofile.of_filename, name, "compression error"))
      )
