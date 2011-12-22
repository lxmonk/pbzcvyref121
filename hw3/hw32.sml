Control.Print.printDepth := 500000;
Control.Print.printLength := 500000;
Control.Print.stringDepth := 500000;
Control.polyEqWarn := false;

datatype instruction = Inst of string * (int list) * (int list);

structure IntKey =
struct
    type ord_key = int
    val compare = Int.compare
end;

structure IMap = RedBlackMapFn(IntKey);
val find = IMap.find;
val insert = IMap.insert;
         
signature REMOVE_WW =
sig
val remww : (instruction list) -> (instruction list);
end;
          
exception NotYetImplemented;

structure RemoveWW : REMOVE_WW =
struct
local
    exception RemError;
              
    fun updateReads (dict, [], line) = dict
      | updateReads (dict, r::Rs, line : int) =
        (case (find(dict, r)) of
             NONE => updateReads(insert(dict, r,
                                        {R = line, W = ~1}),
                                 Rs, line)
           | SOME(record) =>
             updateReads(insert(dict, r, {R=line,
                                          W=(#W record)}),
                         Rs, line));
    fun updateWrites (dict, [], line) = dict
      | updateWrites (dict, w::Ws, line) =
        (case (find(dict, w)) of
             NONE => updateWrites(insert(dict, w, {R = ~1,
                                                   W = line}),
                                  Ws, line)
           | SOME(record) =>
             updateWrites(insert(dict, w, {W = line,
                                           R = (#R record)}),
                          Ws, line))

    fun needsWriting (dict : {R:int, W:int} IMap.map, []) =
        false
      | needsWriting (dict, w::Ws) =
        (case (find(dict, w)) of
             NONE => true
           | SOME(record) =>
             (((#W record) <= (#R record)) orelse 
              (needsWriting(dict, Ws))));


    fun rem ([], dict, result, line) = result
      | rem ((this as Inst(opname, w::Ws, Rs)) :: lst,
             dict, result, line) =
        (case (find(dict, w)) of
             NONE => rem(lst,
                         updateReads(updateWrites(
                                     insert(dict, w,
                                            {R = ~1,
                                             W = line}),
                                     Ws, line),
                                     Rs, line),
                         this::result, line + 1)
           | SOME(record) =>
             if (#W record) <= (#R record) orelse
                needsWriting(dict, Ws)
             then
                 rem (lst,
                      updateReads(updateWrites(
                                  insert(dict, w,
                                         {W = line,
                                          R = (#R record)}),
                                  Ws, line),
                                  Rs, line),
                      this :: result, line+1)
             else 
                 rem (lst, dict, result, line + 1))
      | rem _ = raise RemError;
    
in
fun remww (instList) = rem(List.rev (instList),
                           IMap.empty, [], 0);
end;
end;


