open Memgraph.Repr
(*
let l1 = [10; 11]
let l2 = [10; 11; 12]


let get_pblock cell_value =
  match cell_value with
  | Int i -> failwith "Invalid value"
  | Pointer addr -> Memgraph.Repr.follow addr
  | _ -> failwith "Invalid value"


let addr_table = Hashtbl.create 10

let print_data d =
  match d with
  | Int i -> Printf.printf "%d; " i
  | _ -> ()


let load_data_with_addr d =
  match d.data with
  | Abstract -> d.addr
  | Block b -> failwith "Invalid value"
  | Fields f -> Hashtbl.replace addr_table d.addr d.data; d.addr

let print_addr_list l =
  Printf.printf "[ ";
  List.iter (fun a -> print_data (Hashtbl.find addr_table a)) l;
  Printf.printf " ]"


let () =
  let test = Memgraph.Repr.repr l1 in
  let test2 = Memgraph.Repr.repr l2 in
  let pblock_test = get_pblock test in
  let pblock_test = get_pblock test2 in
  let a = ref [] in
  Memgraph.Repr.walk (fun pb -> a := (load_data_with_addr pb.block) :: !a) pblock_test;
  Printf.printf "\n"
  *)