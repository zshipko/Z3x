open Z3x

let _ =
    let open Infix in
    let a = var INT "a" in
    let b = var INT "b" in
    let c = var INT "c" in
    let sat, s = solve [
        a > b;
        b > c;
        a > c + b;
        a - b * int 2 < c
    ] in
    if sat = Z3.Solver.SATISFIABLE then
        let model = match get_model s with
            | Some m -> m
            | None ->
                print_endline "No model";
                failwith "no model" in
        match eval model (a > c) with
        | Some x -> print_endline (Z3.Expr.to_string x)
        | None -> ()
    else print_endline "UNSAT"
