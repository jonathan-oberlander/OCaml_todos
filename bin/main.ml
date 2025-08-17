type task =
  { description : string
  ; completed : bool
  }

let todo_list = List.[]

let print_task index task =
  let status = if task.completed then "[x]" else "[ ]" in
  Printf.printf "%d. %s %s\n" index status task.description
;;

let print_all_tasks todo_list =
  List.iteri (fun i task -> print_task (i + 1) task) todo_list
;;

let add_task tasks description =
  let new_task = { description; completed = false } in
  List.cons new_task tasks
;;

let delete_task tasks idx = List.filteri (fun i _ -> i <> idx) tasks

let mark_completed tasks idx =
  List.mapi (fun i task -> if i = idx then { task with completed = true } else task) tasks
;;

let rec main_loop todo_list =
  print_endline "\n--- To-Do List Menu ---";
  print_endline "1. Add Task";
  print_endline "2. List Tasks";
  print_endline "3. Complete Task";
  print_endline "4. Delete Task";
  print_endline "5. Quit";
  print_string "Choose an option: ";
  let choice = read_line () in
  match choice with
  | "1" ->
    print_string "Enter task description: ";
    let desc = read_line () in
    let todo_list = add_task todo_list desc in
    main_loop todo_list
  | "2" ->
    print_all_tasks todo_list;
    main_loop todo_list
  | "3" ->
    print_string "Enter task number to mark as complete (starting from 1): ";
    let num = int_of_string (read_line ()) in
    let todo_list = mark_completed todo_list (num - 1) in
    main_loop todo_list
  | "4" ->
    print_string "Enter task number to delete (startin from 1): ";
    let num = int_of_string (read_line ()) in
    let todo_list = delete_task todo_list (num - 1) in
    main_loop todo_list
  | "5" -> print_endline "Goodbye!"
  | _ ->
    print_endline "Invalid option.";
    main_loop todo_list
;;

main_loop todo_list
