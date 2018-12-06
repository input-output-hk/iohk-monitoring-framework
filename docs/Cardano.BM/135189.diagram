format 221

classinstance 128021 class_ref 129301 // Counters
  name ""   xyz 313 4 2000 life_line_z 2000
classinstance 128149 class_ref 128789 // STMObserver
  name ""   xyz 139 4 2000 life_line_z 2000
classinstancecanvas 128789 classinstance_ref 128149 // STM
  xyz 470 4 2000 life_line_z 2000
end
classinstance 130453 class_ref 128021 // Trace
  name ""   xyz 635 4 2000 life_line_z 2000
durationcanvas 128405 classinstance_ref 128149 // :STMObserver
  xyzwh 185 105 2010 11 408
end
durationcanvas 128533 classinstance_ref 128021 // :Counters
  xyzwh 344 103 2010 11 43
end
durationcanvas 129301 classinstance_ref 128789 // STM:STM
  xyzwh 502 187 2010 11 92
end
durationcanvas 129685 classinstance_ref 128021 // :Counters
  xyzwh 344 310 2010 11 45
end
durationcanvas 130069 classinstance_ref 128021 // :Counters
  xyzwh 344 382 2010 11 35
end
durationcanvas 130581 classinstance_ref 130453 // :Trace
  xyzwh 655 468 2010 11 35
end
durationcanvas 130837 classinstance_ref 130453 // :Trace
  xyzwh 655 158 2010 11 26
end
durationcanvas 131093 classinstance_ref 130453 // :Trace
  xyzwh 655 422 2010 11 25
end
msg 128661 synchronous
  from durationcanvas_ref 128405
  to durationcanvas_ref 128533
  yz 105 2015 msg operation_ref 128789 // "readCounters() : Counter"
  show_full_operations_definition default show_class_of_operation default drawing_language default show_context_mode default
  label_xy 223 91
msg 129173 return
  from durationcanvas_ref 128533
  to durationcanvas_ref 128405
  yz 135 2015 unspecifiedmsg
  show_full_operations_definition default show_class_of_operation default drawing_language default show_context_mode default
msg 129429 synchronous
  from durationcanvas_ref 128405
  to durationcanvas_ref 129301
  yz 188 2015 explicitmsg "atomically"
  show_full_operations_definition default show_class_of_operation default drawing_language default show_context_mode default
  label_xy 318 174
msg 129557 return
  from durationcanvas_ref 129301
  to durationcanvas_ref 128405
  yz 268 2015 explicitmsg "(t, [LogObject])"
  show_full_operations_definition default show_class_of_operation default drawing_language default show_context_mode default
  label_xy 303 254
msg 129813 synchronous
  from durationcanvas_ref 128405
  to durationcanvas_ref 129685
  yz 311 2015 msg operation_ref 128789 // "readCounters() : Counter"
  show_full_operations_definition default show_class_of_operation default drawing_language default show_context_mode default
  label_xy 223 297
msg 129941 return
  from durationcanvas_ref 129685
  to durationcanvas_ref 128405
  yz 344 2015 unspecifiedmsg
  show_full_operations_definition default show_class_of_operation default drawing_language default show_context_mode default
msg 130197 synchronous
  from durationcanvas_ref 128405
  to durationcanvas_ref 130069
  yz 383 2015 msg operation_ref 136981 // "calcDiff(in other : Counter) : Counters"
  show_full_operations_definition default show_class_of_operation default drawing_language default show_context_mode default
  label_xy 243 369
msg 130325 return
  from durationcanvas_ref 130069
  to durationcanvas_ref 128405
  yz 406 2015 unspecifiedmsg
  show_full_operations_definition default show_class_of_operation default drawing_language default show_context_mode default
msg 130709 asynchronous
  from durationcanvas_ref 128405
  to durationcanvas_ref 130581
  yz 468 2015 msg operation_ref 129301 // "logInfo()"
  show_full_operations_definition default show_class_of_operation default drawing_language default show_context_mode default
  label_xy 399 454
msg 130965 asynchronous
  from durationcanvas_ref 128405
  to durationcanvas_ref 130837
  yz 158 3005 msg operation_ref 137109 // "logOpening()"
  show_full_operations_definition default show_class_of_operation default drawing_language default show_context_mode default
  label_xy 385 144
msg 131221 asynchronous
  from durationcanvas_ref 128405
  to durationcanvas_ref 131093
  yz 424 3005 msg operation_ref 137237 // "logClosing()"
  show_full_operations_definition default show_class_of_operation default drawing_language default show_context_mode default
  label_xy 388 410
end
