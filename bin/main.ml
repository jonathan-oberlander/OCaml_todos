type task =
  { description : string
  ; completed : bool
  }

type error = IndexOutOfBounds of int

(* ---------------------------------------------------------------- *)

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
  Ok (new_task :: tasks)
;;

let update_task tasks idx description =
  if idx < 0 || idx >= List.length tasks
  then Error (IndexOutOfBounds idx)
  else
    Ok
      (List.mapi
         (fun i task -> if i = idx then { task with description } else task)
         tasks)
;;

let delete_task tasks idx =
  if idx < 0 || idx >= List.length tasks
  then Error (IndexOutOfBounds idx)
  else Ok (List.filteri (fun i _ -> i <> idx) tasks)
;;

let mark_completed tasks idx =
  if idx < 0 || idx >= List.length tasks
  then Error (IndexOutOfBounds idx)
  else
    Ok
      (List.mapi
         (fun i task -> if i = idx then { task with completed = true } else task)
         tasks)
;;

(* ---------------------------------------------------------------- *)

let add_task_handler todo_list =
  print_endline "Enter task description:";
  let desc = read_line () in
  match add_task todo_list desc with
  | Ok list -> list
  | Error _ ->
    print_endline "Failed to add task.";
    todo_list
;;

let print_all_tasks_handler todo_list =
  print_all_tasks todo_list;
  todo_list
;;

let complete_task_handler todo_list =
  print_string "Enter task number to mark as complete (starting from 1): ";
  match int_of_string_opt (read_line ()) with
  | None ->
    print_endline "Please enter a valid number.";
    todo_list
  | Some num ->
    (match mark_completed todo_list (num - 1) with
     | Ok list -> list
     | Error (IndexOutOfBounds n) ->
       Printf.printf "Failed to complete task %d. Is the task index correct?\n" n;
       todo_list)
;;

let update_task_handler todo_list =
  print_string "Enter task number to update (starting from 1): ";
  match int_of_string_opt (read_line ()) with
  | None ->
    print_endline "Please enter a valid number.";
    todo_list
  | Some num ->
    print_string "Enter new description: ";
    let desc = read_line () in
    (match update_task todo_list (num - 1) desc with
     | Ok updated -> updated
     | Error (IndexOutOfBounds n) ->
       Printf.printf "Invalid task number: %d." n;
       todo_list)
;;

let delete_task_handler todo_list =
  print_string "Enter task number to delete (startin from 1): ";
  match int_of_string_opt (read_line ()) with
  | None ->
    print_endline "Please enter a valid number.";
    todo_list
  | Some num ->
    (match delete_task todo_list (num - 1) with
     | Ok list -> list
     | Error (IndexOutOfBounds n) ->
       Printf.printf "Failed to delete task %d. Is the task index correct?\n" n;
       todo_list)
;;

(* ---------------------------------------------------------------- *)

let rec main_loop todo_list =
  print_endline "\n--- To-Do List Menu ---";
  print_endline "1. List Tasks";
  print_endline "2. Add Task";
  print_endline "3. Update Task";
  print_endline "4. Complete Task";
  print_endline "5. Delete Task";
  print_endline "6. Quit";
  print_string "Choose an option: ";
  let choice = read_line () in
  match choice with
  | "1" -> main_loop (print_all_tasks_handler todo_list)
  | "2" -> main_loop (add_task_handler todo_list)
  | "3" -> main_loop (update_task_handler todo_list)
  | "4" -> main_loop (complete_task_handler todo_list)
  | "5" -> main_loop (delete_task_handler todo_list)
  | "6" -> print_endline "Goodbye!"
  | _ ->
    print_endline "Invalid option.";
    main_loop todo_list
;;

main_loop todo_list
