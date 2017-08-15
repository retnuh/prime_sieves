open Core
open CFStream
open Core_bench

let is_prime_mod n primes = 
  not (Doubly_linked.exists primes ~f:(fun p -> n mod p = 0))

let string_of_dlist dlist =
  dlist
  |> Doubly_linked.sexp_of_t Int.sexp_of_t
  |> Sexp.to_string_hum

let sieve_mod upto =
  let primes = Doubly_linked.create () in
  let rec acc count x = function
    | 0 ->
       (* Printf.printf "Upto: %d Primes: %s\n" upto (string_of_dlist primes); *)
       count + 1
    | n -> if is_prime_mod x primes then
             begin
               (* Printf.printf "Found prime: %d\n" x; *)
               ignore(Doubly_linked.insert_last primes x);
               acc (count + 1) (x + 2)  (n - 1)
             end
           else
             acc count (x + 2) (n - 1)
  in acc 0 3 (upto/2 - 1)

type heap_entry = Composite of int * int | Empty
                                 
let heap_entry_cmp a b = match (a, b) with
  | _, Empty -> -1
  | Empty, _ -> 1
  | Composite (x, xp), Composite (y, yp) -> compare x y

let heap_entry_to_list accum = function
  | Empty -> List.rev accum
  | Composite (_, p) -> p :: accum

let string_of_heap heap =
  Heap.fold heap ~init:[] ~f:heap_entry_to_list
  |> (List.sexp_of_t Int.sexp_of_t)
  |> Sexp.to_string_hum

let rec bump_heap n heap = match Heap.top_exn heap with
  | Composite (x, p) when x <= n  -> 
     (* Printf.printf "Bumping heap entry: %d %d %d\n" n x p; *)
     ignore(Heap.pop_exn heap);
     Heap.add heap (Composite ((x + 2*p), p));
     bump_heap n heap
  | _ -> ()
      
let rec is_prime_heap ?(bump=false) n heap =
  match Heap.top_exn heap with
  | Empty -> (true, (bump || false))
  | Composite (x, _) when n < x -> (true, bump || false)
  | Composite (x, _) when n = x -> (false, true)
  | Composite (x, p) ->
     (* Printf.printf "Skipping heap entry: %d %d\n" x p; *)
     is_prime_heap ~bump:true n heap


(* Simple Estimate from https://en.wikipedia.org/wiki/Prime-counting_function *)

let sieve_heap upto =
  let upto_float = float_of_int upto in
  let estimate = 1.25506 *. upto_float /. (log upto_float) |> Float.round_up |> int_of_float in
  let heap = (Heap.create ?min_size:(Some estimate) ~cmp:heap_entry_cmp) () in
  Heap.add heap Empty;
  let rec acc count x = function
    | 0 ->
       (* Printf.printf "Upto: %d Primes: %s\n" upto (string_of_heap heap);  *)
       count + 1
    | n -> let is_prime, bump = is_prime_heap x heap in
           if bump then
             ignore(bump_heap x heap);
           if is_prime then
             begin
               (* Printf.printf "Heap Found prime: %d\n" x; *)
               Heap.add heap (Composite (3*x, x));
               acc (count + 1) (x + 2)  (n - 1)
             end
           else
             acc count (x + 2) (n - 1)
  in
  acc 0 3 (upto/2 - 1)
                         
               
let bench sizes =
  Command.run (Bench.make_command [
                   Bench.Test.create_indexed
                     ~name:"Mod sieve"
                     ~args:sizes
                     (fun upto ->
                       Staged.stage (fun () -> ignore(sieve_mod upto)));
                   Bench.Test.create_indexed
                     ~name:"Heap sieve"
                     ~args:sizes
                     (fun upto ->
                       Staged.stage (fun () -> ignore(sieve_heap upto)));
              ])
              

let run_mod x = Printf.printf "Mod Primes upto %d: %d\n" x (sieve_mod x)
let run_heap x = Printf.printf "Heap Primes upto %d: %d\n" x (sieve_heap x)
(* let run () = run_heap 1000 *)
let run () = bench [1000; 10_000; 100_000; 1_000_000; 10_000_000] 
