let xml_child name xdata =
  let finder prev xchld =
    match prev with
    | Some(x) -> prev
    | None -> if Xml.tag xchld = name then Some(xchld) else None
  in
  Xml.fold finder None xdata


let xml_child_pcdata name xdata default =
  let child = xml_child name xdata in
  match child with
  | Some(x) -> Xml.pcdata (List.hd (Xml.children x))
  | None -> default

let xml_child_map name xdata f = 
  let do_fold lst xchld =
    if Xml.tag xchld = name then 
      (f xchld) :: lst
    else
      lst
  in
  List.rev (Xml.fold do_fold [] xdata)
