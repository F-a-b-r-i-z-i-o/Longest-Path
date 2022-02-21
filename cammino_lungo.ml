(*Definizione delle eccezioni usate*)
exception CamminoNonTrovato;;
exception Errore;;

type 'a graph = Graph of ('a -> 'a list);;
(*
Definizione archi del grafo1 mediante tupla di interi 

1)valore nodo iniziale
2)valore peso arco
3)valore nodo di arrivo
*)
let pesi1 = [(0,1,1);(0,1,5);(1,1,3);(2,1,0);(3,1,4);(3,1,2)];;

(*Definizione successori primo grafo mediante la funzione dei successori*)
let f1 = function
    0 -> [1;2]
  | 1 -> [3]
  | 2 -> [3]
  | 3 -> [4] 
  | 4 -> []
  | _ -> [];;



let grafo1 = Graph f1;; 

(*
Definizione archi del grafo2 mediante tupla di interi 

1)valore nodo iniziale
2)valore peso arco
3)valore nodo di arrivo
*)
let pesi2 =[(0,1,1);(0,1,5);(1,1,3);(2,1,0);(3,1,4);(3,1,2)];;


(*Definizione successori secondo grafo mediante la lista dei successori*)
let f2 = function
    0-> [1;5]
  | 1 -> [3]
  | 2 -> [0]
  | 3 -> [4;2]
  | 4 -> [] 
  | 5 -> []
  | _ -> [];;
      
let grafo2 = Graph f2;; 

(*
Definizione archi del grafo3 mediante tupla di interi 

1)valore nodo iniziale
2)valore peso arco
3)valore nodo di arrivo
*)

let pesi3 = [(0,2,1);(1,3,3);(2,1,1);(3,2,4);(3,1,2);(4,1,5)];;

(*Definizione successori terzo grafo mediante la lista dei successori*)
let f3 = function
    0-> [1]
  | 1 -> [3]
  | 2 -> [1]
  | 3 -> [4;2]
  | 4 -> [5] 
  | 5 -> []
  | _ -> [];; 

let grafo3 = Graph f3;; 

(*
La funzione cerca, prende due coppie di interi (x,y), e una lista di lunghezze,
restituisce la lunghezza associata all'arco che ha come estremi i due nodi
*)
let rec cerca x y= function
    []->0
  |(a,b,c)::resto -> if((a=x && c=y))then b else cerca x y resto;;

(*
La funzione costo_cammino si avvale della funzione  ausiliaria 
costocamm_ottimo che grazie alla ricorsione di coda permette di calcolare
il costo del cammino passato come parametro basandosi sui pesi che sono passati come secondo parametro.
*)
let costo_cammino cammino pesi= 
  let rec cammino_ottimo costo=function
      []->raise Errore
    |x::y::rest -> cammino_ottimo ( costo + cerca x y pesi ) (y::rest)
    |_::[]->costo 
  in cammino_ottimo 0 cammino;;

(*
questa funzione stampa la lista del cammino ogni volta che questa viene espansa, usata e lasciata per debug
*)
let rec stampalista = function [] -> print_newline()
                             | x::rest -> print_int(x); print_string("; "); stampalista rest;;

(*
La funzione dfs prende come parametri il nodo iniziale e il nodo finale del grafo, insieme al grafo stesso
ed esegue l'algoritmo di dfs sul grafo. Utilizza le funzioni ausiliarie estendia cammino ed search_auxa.
*)
let dfs inizio fine (Graph succ) =
  let estendia cammino = (*stampalista cammino*)
    List.map (function x -> x::cammino)
      (List.filter (function x -> not (List.mem x cammino)) (succ (List.hd cammino))) 
  in let rec search_auxa fine = function
        [] -> raise CamminoNonTrovato
      | cammino::rest -> 
          if ((fine = List.hd cammino) ) then List.rev cammino         
          else search_auxa fine ((estendia cammino) @ rest)
  in search_auxa fine [[inizio]];;

(*
La funzione dfs_bool prende come parametri il nodo iniziale e il nodo finale del grafo, insieme al grafo stesso
ed esegue l'algoritmo di dfs sul grafo, ritornando un valore true o flase se è presente la soluzione. 
Utilizza le funzioni ausiliarie estendia cammino ed search_auxa.
*)
let dfs_bool inizio fine (Graph succ) =
  let estendia cammino = (*stampalista cammino*)
    List.map (function x -> x::cammino)
      (List.filter (function x -> not (List.mem x cammino)) (succ (List.hd cammino)))
  in let rec search_auxa fine = function
        [] -> false
      | cammino::rest -> 
          if ((fine = List.hd cammino) ) then true       
          else search_auxa fine ((estendia cammino) @ rest)
  in search_auxa fine [[inizio]];;

(*la funzione dfs_cerca esegue una ricerca in profondità considerando anche i cicli, non appena soddisfa il k, 
controlla se il cammino è esattamente la soluzione andando anche a controllare che sia presente la soluzione, 
altrimenti, ritorna alla fine chiamando la dfs*)
let dfs_cerca inizio fine k pesi (Graph succ)=
  let estendi cammino = stampalista cammino;
    List.map (function x -> x::cammino)
      (succ (List.hd cammino))        
  in let rec search_aux fine = function
        [] -> raise CamminoNonTrovato 
      | cammino::rest -> 
          if ((costo_cammino (List.rev cammino) pesi >=k) && (List.hd cammino = fine) )then List.rev cammino 
          else if((costo_cammino (List.rev cammino) pesi) >=k) && (dfs_bool (List.hd cammino) fine (Graph succ))
          then (List.rev (List.tl cammino)) @ dfs (List.hd cammino) (fine) (Graph succ) 
          else if((costo_cammino (List.rev cammino) pesi) < k)
          then search_aux fine ((estendi cammino) @ rest) 
          else search_aux fine rest
  in search_aux fine [[inizio]];;

(*la funzione cerca_cammino serve per richiamare l'esecuzione della funzione dfs_cerca*)
let trova_cammino inizio fine k pesi (Graph succ)=
  let risultato = (dfs_cerca inizio fine k pesi (Graph succ)) in 
  print_string("il risultato trovato è il seguente = "); stampalista risultato ;
  print_string("il costo del cammino è il seguente = ");   print_int (costo_cammino risultato pesi); print_newline();;

